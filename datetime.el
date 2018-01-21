;;; datetime.el --- Parsing, formatting and matching timestamps  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2018 Paul Pogonyshev

;; Author:     Paul Pogonyshev <pogonyshev@gmail.com>
;; Maintainer: Paul Pogonyshev <pogonyshev@gmail.com>
;; Version:    0.3.2
;; Keywords:   lisp, i18n
;; Homepage:   https://github.com/doublep/datetime
;; Package-Requires: ((emacs "24.1") (extmap "1.0"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses.


;;; Commentary:

;; Library that provides support for formatting, parsing and matching
;; timestamps in certain format.


;;; Code:


;; Internally any date-time pattern is parsed to a list of value pairs
;; (type . details).  Type is a symbol, while details are either nil,
;; another symbol or a number that represents minimum number of
;; characters in formatted number (left padded with zeros).  The only
;; exception is "as-is" part: it is just a string, not a cons cell.
;;
;; Here are all currently used types with details in parentheses,
;; grouped roughly by represented date-time value.  Context/standalone
;; is meaningful for languages that involve flexing, for English they
;; are the same.
;;
;; In all cases these should be seen as internals and can be changed
;; in a future library versions without prior notice.
;;
;;   era (full | abbreviated) --- AD or BC
;;
;;   year (add-century-when-parsing | always-two-digits | NUMBER)
;;     - add-century-when-parsing: format as-is, but when parsing add
;;       century if exactly two digits;
;;   year-for-week (same as for year)
;;
;;   month (NUMBER)
;;   month-context-name (full | abbreviated)
;;   month-standalone-name (full | abbreviated)
;;
;;   week-in-year (NUMBER)
;;   week-in-month (NUMBER)
;;
;;   day-in-year (NUMBER)
;;   day-in-month (NUMBER)
;;   weekday-in-month (NUMBER)
;;       e.g. would be 2 for 2015-09-09, because it is the second
;;       Wednesday that month;
;;   weekday (NUMBER)
;;   weekday-context-name (full | abbreviated)
;;   weekday-standalone-name (full | abbreviated)
;;
;;   am-pm (full | abbreviated)
;;
;;   hour-0-23 (NUMBER)
;;   hour-1-24 (NUMBER)
;;   hour-am-pm-0-11 (NUMBER)
;;   hour-am-pm-1-12 (NUMBER)
;;
;;   minute (NUMBER)
;;   second (NUMBER)
;;   millisecond (NUMBER)
;;   second-fractional (NUMBER)
;;       this is a generalization used internally: (second-fractional . 3)
;;       means millis, (second-fractional . 6) -- micros, and so on;
;;
;;   decimal-separator (PREFERRED)
;;       either dot or comma;
;;
;;   timezone (?) -- currently not supported further than pattern parsing


(require 'extmap)


(if (fboundp 'define-error)
    (define-error 'datetime-unsupported-timezone "Timezones are currently not supported")
  (put 'datetime-unsupported-timezone 'error-conditions '(datetime-unsupported-timezone error))
  (put 'datetime-unsupported-timezone 'error-message "Timezones are currently not supported"))


(defconst datetime--directory (file-name-directory (or load-file-name (buffer-file-name))))

;; Extracted from Java using `dev/HarvestData.java'.  All patterns are
;; obviously of `java' type.
;;
;; There are many fallbacks involved to reduce size:
;;   - for locale XX-YY value for any property defaults to that of
;;     locale XX;
;;   - `:decimal-separator' defaults to dot;
;;   - `:eras' and `:am-pm' default to English version;
;;   - month/dayweek standalone abbreviations or names default to
;;     the corresponding context-aware property;
;;   - date-time patterns are not stored, instead they are built from
;;     date and time parts for that locale; corresponding field is a
;;     cons with car determining what should be in the beginning (t
;;     for date, nil for time), and cdr being the separator string;
;;     the cons defaults to (t . " ");
;;   - all patterns have the following fallbacks: `:short' defaults to
;;     `:medium', `:long' defaults to `:medium', `:full' defaults to
;;     `:long'.
(defvar datetime--locale-extmap (extmap-init (expand-file-name "locale-data.extmap" datetime--directory)))

(defvar datetime--pattern-parsers '((parsed . (lambda (pattern options) pattern))
                                    (java   . datetime--parse-java-pattern)))

(defvar datetime--pattern-formatters '((parsed . (lambda (parts options) parts))
                                       (java   . datetime--format-java-pattern)))


(defun datetime--parse-pattern (type pattern options)
  (let ((parser (cdr (assq type datetime--pattern-parsers))))
    (if parser
        (funcall parser pattern options)
      (error "Unknown pattern type `%s'" type))))

(defun datetime--format-pattern (type parts options)
  (let ((formatter (cdr (assq type datetime--pattern-formatters))))
    (if formatter
        (funcall formatter parts options)
      (error "Unknown pattern type `%s'" type))))


;; Appending character-by-character is slow, but pretty sure it
;; doesn't matter for generally short date-time patterns.
(defmacro datetime--extend-as-is-part (parts text)
  `(let ((text ,text))
     (if (stringp (car ,parts))
         (setcar parts (concat (car ,parts) text))
       (push text ,parts))))


(defun datetime--parse-java-pattern (pattern options)
  (let ((scan   0)
        (length (length pattern))
        parts)
    (while (< scan length)
      (let ((character       (aref pattern scan))
            (num-repetitions 1))
        (setq scan (1+ scan))
        (cond ((= character ?')
               (when (= scan length)
                 (error "Unterminated quote"))
               ;; Ugly code to parse single-quoted string.
               (if (= (aref pattern scan) ?')
                   (progn
                     (datetime--extend-as-is-part parts "'")
                     (setq scan (1+ scan)))
                 (while (progn
                          (when (= scan length)
                            (error "Unterminated quote"))
                          (setq character (aref pattern scan)
                                scan      (1+ scan))
                          (if (/= character ?')
                              (datetime--extend-as-is-part parts (string character))
                            (when (and (< scan length) (= (aref pattern scan) ?'))
                              (datetime--extend-as-is-part parts (string ?'))
                              (setq scan (1+ scan))))))))
              ((or (and (<= ?A character) (<= character ?Z))  (and (<= ?a character) (<= character ?z)))
               (while (and (< scan length) (eq (aref pattern scan) character))
                 (setq scan            (1+ scan)
                       num-repetitions (1+ num-repetitions)))
               (pcase character
                 ((or ?G ?E ?a)
                  (push (cons (pcase character
                                (?G 'era)
                                (?E 'weekday-context-name)
                                (?a 'am-pm))
                              (if (>= num-repetitions 4) 'full 'abbreviated))
                        parts))
                 ((or ?y ?Y)
                  (push (cons (if (= character ?y) 'year 'year-for-week)
                              (pcase num-repetitions
                                (1 'add-century-when-parsing)
                                (2 'always-two-digits)
                                (_ num-repetitions)))
                        parts))
                 ((or ?M ?L)
                  (push (if (<= num-repetitions 2)
                            (cons 'month num-repetitions)
                          (cons (if (= character ?M) 'month-context-name 'month-standalone-name)
                                (if (>= num-repetitions 4) 'full 'abbreviated)))
                        parts))
                 (?w (push (cons 'week-in-year     num-repetitions) parts))
                 (?W (push (cons 'week-in-month    num-repetitions) parts))
                 (?D (push (cons 'day-in-year      num-repetitions) parts))
                 (?d (push (cons 'day-in-month     num-repetitions) parts))
                 (?F (push (cons 'weekday-in-month num-repetitions) parts))
                 (?u (push (cons 'weekday          num-repetitions) parts))
                 (?H (push (cons 'hour-0-23        num-repetitions) parts))
                 (?k (push (cons 'hour-1-24        num-repetitions) parts))
                 (?K (push (cons 'hour-am-pm-0-11  num-repetitions) parts))
                 (?h (push (cons 'hour-am-pm-1-12  num-repetitions) parts))
                 (?m (push (cons 'minute           num-repetitions) parts))
                 (?s (push (cons 'second           num-repetitions) parts))
                 (?S (push (cons (if (plist-get options :second-fractional-extension) 'second-fractional 'millisecond)
                                 num-repetitions)
                           parts))
                 (?z (push (cons 'timezone         'general)        parts))
                 (?Z (push (cons 'timezone         'rfc-822)        parts))
                 (?X (push (cons 'timezone         'iso-8601)       parts))
                 (_
                  (error "Illegal pattern character `%c'" character))))
              (t
               (if (and (or (= character ?.) (= character ?,))
                        (plist-get options :any-decimal-separator)
                        (eq (car-safe (car parts)) 'second)
                        (< scan length) (= (aref pattern scan) ?S))
                   (push (cons 'decimal-separator character) parts)
                 (datetime--extend-as-is-part parts (string character)))))))
    (nreverse parts)))

(defun datetime--format-java-pattern (parts options)
  (let ((case-fold-search nil)
        strings)
    (dolist (part parts)
      (if (stringp part)
          (progn
            (when (string-match "\\`'+" part)
              (push (concat (match-string-no-properties 0) (match-string-no-properties 0)) strings)
              (setq part (substring part (match-end 0))))
            (when (> (length part) 0)
              (push (if (string-match "['[:alpha:]]" part)
                        ;; TODO: Might want to prettify a bit.
                        (concat "'" (replace-regexp-in-string "'" "''" part t t) "'")
                      part)
                    strings)))
        (let* ((type    (car part))
               (details (cdr part))
               (string  (pcase type
                          (`era              "G")
                          ((or `year `year-for-week)
                           (let ((base (if (eq type 'year) ?y ?Y)))
                             (pcase details
                               (`add-century-when-parsing base)
                               (`always-two-digits        (cons base 2))
                               (_                         (cons base details)))))
                          (`month            (cons ?M details))
                          ((or `month-context-name `month-standalone-name `weekday-context-name)
                           (cons (pcase type
                                   (`month-context-name    ?M)
                                   (`month-standalone-name ?L)
                                   (`weekday-context-name  ?E))
                                 (pcase details
                                   (`abbreviated 3)
                                   (`full        4)
                                   (_            (error "Unexpected details %s" details)))))
                          (`week-in-year      (cons ?w details))
                          (`week-in-month     (cons ?W details))
                          (`day-in-year       (cons ?D details))
                          (`day-in-month      (cons ?d details))
                          (`weekday-in-month  (cons ?F details))
                          (`hour-0-23         (cons ?H details))
                          (`hour-1-24         (cons ?k details))
                          (`hour-am-pm-0-11   (cons ?K details))
                          (`hour-am-pm-1-12   (cons ?h details))
                          (`minute            (cons ?m details))
                          (`second            (cons ?s details))
                          (`decimal-separator details)
                          (`millisecond       (cons ?S details))
                          (`second-fractional (if (plist-get options :second-fractional-extension)
                                                  (cons ?S details)
                                                (error "`second-fractional' extension is not enabled")))
                          (`am-pm             "a")
                          (_                  (error "Unexpected part type %s" type)))))
          (push (cond ((integerp string)
                       (string string))
                      ((consp string)
                       (unless (integerp (cdr string))
                         (error "Unexpected details %s" (cdr string)))
                       (make-string (cdr string) (car string)))
                      (t
                       string))
                strings))))
    (apply #'concat (nreverse strings))))


(defun datetime-matching-regexp (type pattern &rest options)
  "Return a regexp that matches date-time according to the PATTERN.
Argument TYPE defines how the pattern should be interpreted, see
library documentation.  Rest of the arguments must be a property
list, i.e. keywords interleaved with values.

Returned regexp contains only \"shy\" groups, so it can be
inserted into a larger one without screwing group ordering.  Note
that the returned regexp as a whole is not enclosed in a group;
when inserting you need to take that into account where
necessarily.

Note that the returned regexp will match some incorrect dates
too.  It is supposed to be used as a good and fast estimation if
a string represents date-time formatted according to PATTERN, but
it is not strict enough to be used as a validator.

OPTIONS should be any keyword arguments understood by
`datetime-recode-pattern' plus any from the list below, specific
to this function.

The function understands several keyword arguments to subtly
tweak the produced regexp.  Many of these flags can be used to
discard valid date-time strings.  They are still useful because
\"can be parsed\" is not necessarily equal to \"valid in this
context\".  Default value of keyword arguments is nil unless
specified otherwise.

  :locale

    Locale (language) used for month, weekday etc. names.  Always
    defaults to English, even if system locale is different.

  :only-4-digit-years

    Match only four consecutive digits as a year assuming the
    pattern contains a 4-digit year placeholder.  By default any
    number of digits will be accepted.  This can be seen as a
    special case of :require-leading-zeros for year field only.

  :lax-whitespace

    Match any whitespace in PATTERN against any whitespace in
    date-time string.  For this purpose \"whitespace\" is defined
    as space and tab characters only.

  :accept-leading-space

    Make variable-width numbers (e.g. day number without leading
    zero) match also if there is a leading space.

  :require-leading-zeros

    Make numbers that are formatted with leading zeros in PATTERN
    only match when there are corresponding zeros in the string.

  :forbid-unnecessary-zeros

    Don't match more leading zeros than required by the pattern.
    E.g. \"030 September\" is a valid date, but no-one writes it
    like that and with this flag such strings are not matched."
  (let* ((lax-whitespace (plist-get options :lax-whitespace))
         (locale         (or (plist-get options :locale) 'en))
         regexp-parts)
    (dolist (part (datetime--parse-pattern type pattern options))
      (if (stringp part)
          (push (if lax-whitespace
                    (replace-regexp-in-string (rx (1+ (any blank))) (rx (1+ (any blank))) (regexp-quote part) t t)
                  (regexp-quote part))
                regexp-parts)
        (let* ((type    (car part))
               (details (cdr part))
               (regexp  (pcase type
                          (`era (regexp-opt (append (datetime-locale-field locale :era) nil)))
                          ((or `year `year-for-week)
                           (cond ((and (plist-get options :only-4-digit-years) (eq details 4))
                                  (rx (= 4 (any "0-9"))))
                                 ((or (memq details '(1 add-century-when-parsing)) (not (plist-get options :require-leading-zeros)))
                                  (rx (1+ (any "0-9"))))
                                 ((memq details '(2 always-two-digits))
                                  (rx (any "0-9") (1+ (any "0-9"))))
                                 (t
                                  (format "[0-9]\\{%d\\}[0-9]+" (1- details)))))
                          (`month                12)
                          (`month-context-name
                           (regexp-opt (append (datetime-locale-field locale (if (eq details 'abbreviated)
                                                                                 :month-context-abbr
                                                                               :month-context-names))
                                               nil)))
                          (`month-standalone-name
                           (regexp-opt (append (datetime-locale-field locale (if (eq details 'abbreviated)
                                                                                 :month-standalone-abbr
                                                                               :month-standalone-names))
                                               nil)))
                          (`week-in-year     53)
                          (`week-in-month     5)
                          (`day-in-month     31)
                          (`weekday-in-month  5)
                          (`weekday           7)
                          (`weekday-context-name
                           (regexp-opt (append (datetime-locale-field locale (if (eq details 'abbreviated)
                                                                                 :weekday-context-abbr
                                                                               :weekday-context-names))
                                               nil)))
                          (`weekday-standalone-name
                           (regexp-opt (append (datetime-locale-field locale (if (eq details 'abbreviated)
                                                                                 :weekday-standalone-abbr
                                                                               :weekday-standalone-names))
                                               nil)))
                          (`am-pm
                           (regexp-opt (append (datetime-locale-field locale :am-pm) nil)))
                          (`hour-0-23        23)
                          (`hour-1-24        24)
                          (`hour-am-pm-0-11  11)
                          (`hour-am-pm-1-12  12)
                          (`minute           59)
                          (`second           59)
                          (`decimal-separator (rx (or "." ",")))
                          ((or `millisecond `second-fractional)
                           (apply #'concat (make-list details (rx (any "0-9")))))
                          (`timezone
                           (signal 'datetime-unsupported-timezone nil))
                          ((pred stringp)
                           (regexp-quote type))
                          (_ (error "Unexpected value %s" type)))))
          (when (integerp regexp)
            ;; REGEXP is really the maximum value of this one- or
            ;; two-digit number.
            (setq regexp (if (<= regexp 9)
                             (cond ((and (>= details 2) (plist-get options :require-leading-zeros)
                                         (format "%s[1-%d]" (make-string (- details 1) ?0) regexp)))
                                   ((plist-get options :forbid-unnecessary-zeros)
                                    (format "[1-%d]" regexp))
                                   (t
                                    (format "0*[1-%d]" regexp)))
                           (cond ((and (= details 1) (plist-get options :accept-leading-space))
                                  (format "[ 0-%d]?[0-9]" (/ regexp 10)))
                                 ((and (>= details 2) (plist-get options :require-leading-zeros)
                                       (format "%s[0-%d][0-9]" (make-string (- details 2) ?0) (/ regexp 10))))
                                 ((plist-get options :forbid-unnecessary-zeros)
                                  (format "[0-%d]?[0-9]" (/ regexp 10)))
                                 ((>= regexp 20)
                                  (format "0*[1-%d]?[0-9]" (/ regexp 10)))
                                 (t
                                  "0*1?[0-9]")))))
          (push regexp regexp-parts))))
    (apply #'concat (nreverse regexp-parts))))


(defun datetime-recode-pattern (from to pattern &rest options)
  "Recode PATTERN between two supported types.
As a special case, either of FROM and TO can be set to \\='parsed.
This is useful as a speed optimization in a few cases where you
perform several transformations on the same pattern.

Options can be a list of the following keyword arguments:

  :second-fractional-extension

    In Java patterns any number of \"S\" stand for milliseconds.
    With this extension they are instead interpreted according to
    how many \"S\" there is, e.g. \"SSSSSS\" means microseconds.

  :any-decimal-separator

    Treat a decimal dot or comma in pattern between seconds and
    milli- or microseconds (etc.) as a placeholder for _any_
    decimal separator and also accept commas in this place.  This
    only works if TO is \\='parsed."
  (datetime--format-pattern to (datetime--parse-pattern from pattern options) options))


;; Arguments are expected to be atoms.
(defmacro datetime--pattern-includes-p (type pattern &rest part-types)
  `(let ((parts (datetime--parse-pattern ,type ,pattern nil))
         includes)
     (while parts
       (let ((part (car parts)))
         (if (and (consp part) ,(if (= (length part-types) 1)
                                    `(eq (car part) ',(car part-types))
                                  `(memq (car part) ',part-types)))
             (setq parts    nil
                   includes t)
           (setq parts (cdr parts)))))
     includes))

(defun datetime-pattern-locale-dependent-p (type pattern)
  "Determine if PATTERN includes any locale-based parts.
In other words, return non-nil if PATTERN includes any textual
names."
  (datetime--pattern-includes-p type pattern era month-context-name month-standalone-name weekday-context-name weekday-standalone-name am-pm))

(defun datetime-pattern-includes-era-p (type pattern)
  "Determine if PATTERN includes the date era."
  (datetime--pattern-includes-p type pattern era))

(defun datetime-pattern-includes-year-p (type pattern)
  "Determine if PATTERN includes the year."
  (datetime--pattern-includes-p type pattern year year-for-week))

(defun datetime-pattern-includes-month-p (type pattern)
  "Determine if PATTERN includes the month."
  (datetime--pattern-includes-p type pattern month month-context-name month-standalone-name))

(defun datetime-pattern-includes-day-p (type pattern)
  "Determine if PATTERN includes the day."
  (datetime--pattern-includes-p type pattern day-in-year day-in-month))

(defun datetime-pattern-includes-hour-p (type pattern)
  "Determine if PATTERN includes hours."
  (datetime--pattern-includes-p type pattern hour-0-23 hour-1-24 hour-am-pm-0-11 hour-am-pm-1-12))

(defun datetime-pattern-includes-minute-p (type pattern)
  "Determine if PATTERN includes minutes."
  (datetime--pattern-includes-p type pattern minute))

(defun datetime-pattern-includes-second-p (type pattern)
  "Determine if PATTERN includes seconds."
  (datetime--pattern-includes-p type pattern second))

(defun datetime-pattern-includes-millisecond-p (type pattern)
  "Determine if PATTERN includes fractions of seconds."
  ;; Without enabled :second-fractional-extension consecutive "S" are
  ;; just always parsed to milliseconds.  Check for
  ;; `second-fractional' just in case of another pattern type.
  (datetime--pattern-includes-p type pattern millisecond second-fractional))

(defun datetime-pattern-includes-timezone-p (type pattern)
  "Determine if PATTERN includes timezone."
  (datetime--pattern-includes-p type pattern timezone))


(defun datetime-list-locales (&optional include-variants)
  "List all locales for which the library has information.
If INCLUDE-VARIANTS is nil, only include “base” locales (in
format \"xx\"), if it is t then also include “variants” in format
\"xx-YY\".

Return value is a list of symbols in no particular order; it can
be modified freely."
  (if include-variants
      (extmap-keys datetime--locale-extmap)
    (let (locales)
      (extmap-mapc datetime--locale-extmap (lambda (locale data) (unless (plist-get (cdr data) :parent) (push locale locales))))
      locales)))


(defsubst datetime--do-get-locale-pattern (patterns variant)
  (or (plist-get patterns variant)
      (unless (eq variant :medium) (plist-get patterns :medium))
      (when (eq variant :full) (plist-get patterns :long))))

(defun datetime-locale-date-pattern (locale &optional variant)
  "Get given date pattern for the LOCALE.
Supported variants are `:short', `:medium', `:long' and `:full'.
If no VARIANT is specified, it defaults to `:medium'.

Returned pattern is always of type \\\='java."
  (datetime--do-get-locale-pattern (datetime-locale-field locale :date-patterns) (or variant :medium)))

(defun datetime-locale-time-pattern (locale &optional variant)
  "Get given time pattern for the LOCALE.
Supported variants are `:short', `:medium', `:long' and `:full'.
If no VARIANT is specified, it defaults to `:medium'.

Returned pattern is always of type \\\='java."
  (datetime--do-get-locale-pattern (datetime-locale-field locale :time-patterns) (or variant :medium)))

(defun datetime-locale-date-time-pattern (locale &optional date-variant time-variant)
  "Get given date-time pattern for the LOCALE.
Supported variants are `:short', `:medium', `:long' and `:full'.
If DATE-VARIANT is not specified, it defaults to `:medium'.  If
TIME-VARIANT is not specified, it defaults to DATE-VARIANT (or
`:medium' it that's missing too).

Returned pattern is always of type \\\='java.

This function exists not just for completeness: while in most
cases the result is just corresponding date and time patterns
separated by a space, for a few locales it is different."
  (let ((date-time-pattern-rule (or (datetime-locale-field locale :date-time-pattern-rule) '(t . " ")))
        (date-part              (datetime-locale-date-pattern locale date-variant))
        (time-part              (datetime-locale-time-pattern locale (or time-variant date-variant))))
    (if (car date-time-pattern-rule)
        (concat date-part (cdr date-time-pattern-rule) time-part)
      (concat time-part (cdr date-time-pattern-rule) date-part))))


(defconst datetime--english-eras  ["BC" "AD"])
(defconst datetime--english-am-pm ["AM" "PM"])

(defsubst datetime--do-get-locale-field (locale-data field)
  (or (plist-get locale-data field)
      ;; See `datetime--locale-extmap' for description of fallbacks.
      (pcase field
        (:month-standalone-abbr    (plist-get locale-data :month-context-abbr))
        (:month-standalone-names   (plist-get locale-data :month-context-names))
        (:weekday-standalone-abbr  (plist-get locale-data :weekday-context-abbr))
        (:weekday-standalone-names (plist-get locale-data :weekday-context-names)))))

(defun datetime-locale-field (locale field)
  "Get a FIELD of data for the LOCALE.
Supported fields:

  :decimal-separator
  :eras
  :month-context-abbr
  :month-context-names
  :weekday-context-abbr
  :weekday-context-names
  :month-standalone-abbr
  :month-standalone-names
  :weekday-standalone-abbr
  :weekday-standalone-names
  :am-pm"
  ;; Additionally `:date-patterns', `:time-patterns' and
  ;; `:date-time-pattern-rule' are supported for internal use.
  (let ((data (extmap-get datetime--locale-extmap locale t)))
    (or (datetime--do-get-locale-field data field)
        (let ((parent (plist-get data :parent)))
          (when parent
            (datetime--do-get-locale-field (extmap-get datetime--locale-extmap parent) field)))
        (pcase field
          (:decimal-separator ?.)
          (:eras              datetime--english-eras)
          (:am-pm             datetime--english-am-pm)))))


(provide 'datetime)

;;; datetime.el ends here
