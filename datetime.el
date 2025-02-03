;;; datetime.el --- Parsing, formatting and matching timestamps  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2025 Paul Pogonyshev

;; Author:     Paul Pogonyshev <pogonyshev@gmail.com>
;; Maintainer: Paul Pogonyshev <pogonyshev@gmail.com>
;; Version:    0.10.2snapshot
;; Keywords:   lisp, i18n
;; Homepage:   https://github.com/doublep/datetime
;; Package-Requires: ((emacs "25.1") (extmap "1.1.1"))

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

;; Library for generic timestamp handling.  It is targeted at bulk
;; processing, therefore many functions are optimized for speed, but
;; not necessarily for ease of use.  For example, formatting is done
;; in two steps: first you need to generate a formatting function for
;; given pattern, and only using it obtain formatted strings.
;;
;; Package's main feature is timestamp parsing and formatting based on
;; Java pattern.  Arbitrary timezones and locales (i.e. not
;; necessarily those used by the system) are supported.  However,
;; specifying timezone in the input string to the parser function is
;; not implemented yet.  See functions `datetime-parser-to-float' and
;; `datetime-float-formatter' for details.
;;
;; Library also supports timestamp matching.  It can generate regular
;; expressions that match timestamps corresponding to given pattern.
;; These regular expressions can give false positives, but for most
;; purposes are good enough to detect timestamps in text files,
;; e.g. in various application logs.  See `datetime-matching-regexp'.
;;
;; Finally, library provides functions to select an appropriate
;; timestamp format for given locale.  For example, function
;; `datetime-locale-date-pattern' returns a Java pattern suitable for
;; formatting (or parsing) date only, without time part.  However, it
;; is not required that patterns are generated this way.


;;; Code:


;; Internally any date-time pattern is parsed to a list of value pairs
;; (TYPE . DETAILS).  Type is a symbol, while details are either nil,
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
;;   era (short | full | narrow) --- AD or BC
;;
;;   year (add-century-when-parsing | always-two-digits | NUMBER)
;;     - add-century-when-parsing: format as-is, but when parsing add
;;       century if exactly two digits;
;;   year-for-week (same as for year)
;;
;;   month (NUMBER)
;;   month-context-name (short | full | narrow)
;;   month-standalone-name (short | full | narrow)
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
;;   weekday-context-name (short | full | narrow)
;;   weekday-standalone-name (short | full | narrow)
;;
;;   am-pm (full | abbreviated)
;;   day-period (short | full | narrow)
;;
;;   hour-0-23 (NUMBER)
;;   hour-1-24 (NUMBER)
;;   hour-am-pm-0-11 (NUMBER)
;;   hour-am-pm-1-12 (NUMBER)
;;
;;   minute (NUMBER)
;;   second (NUMBER)
;;   second-fractional (NUMBER)
;;       parts of a second: (second-fractional . 3) means millis,
;;       (second-fractional . 6) -- micros, and so on;
;;
;;   decimal-separator (PREFERRED)
;;       either dot or comma;
;;
;;   timezone (SYMBOL)
;;       abbreviated, full --- timezone name, as reported by Java
;;           (abbreviated is by far more useful, as full is too
;;           verbose for most usecases);
;;       offset-* -- different representations of timezone (search
;;           the source code for a full list) offset to GMT.


(require 'extmap)


(defun datetime--define-error (name message)
  (if (fboundp #'define-error)
      (define-error name message)
    (put name 'error-conditions `(,name error))
    (put name 'error-message    message)))

(datetime--define-error 'datetime-invalid-string       "Date-time string is invalid")
(datetime--define-error 'datetime-unsupported-timezone "Timezones are currently not supported")


;; Defining the three extmap database variables like this so that values _are_ replaced if these
;; variable declaration is reevaluated (rather, the whole buffer).  Otherwise e.g. reinstalling
;; `datetime' package could leave unusable extmap objects referring to removed files.
;;
;; FIXME: Would be nice to add ERT test(s).

;; Extracted from Java using `dev/HarvestData.java'.  All patterns are
;; obviously of `java' type.
;;
;; There are many fallbacks involved to reduce size:
;;   - for locale XX-YY value for any property defaults to that for
;;     locale XX;
;;   - `:decimal-separator' defaults to dot;
;;   - both `:eras-full' and `:eras-narrow' fall back to
;;     `:eras-short';
;;   - `:eras-short' and `:am-pm' default to English version;
;;   - month/dayweek standalone abbreviations or names default to
;;     the corresponding context-aware property;
;;   - for day period strings, both `:full' and `:narrow' variants
;;     fall back to `:short';
;;   - date-time patterns are not stored, instead they are built from
;;     date and time parts for that locale; corresponding field is a
;;     cons with car determining what should be in the beginning (t
;;     for date, nil for time), and cdr being the separator string;
;;     the cons defaults to (t . " ");
;;   - all patterns have the following fallbacks: `:short' defaults to
;;     `:medium', `:long' defaults to `:medium', `:full' defaults to
;;     `:long'.
(defvar datetime--locale-extmap nil)

;; Extracted from Java using `dev/HarvestData.java'.
(defvar datetime--timezone-extmap nil)

;; Extracted from Java using `dev/HarvestData.java'.
;;
;; Fallbacks:
;;   - for locale XX-YY names defaults to those for locale XX;
;;   - for locale XX names default to those in English locale;
;;   - names themselves can be in several formats (individual values
;;     are always strings):
;;         FULL -- abbreviated name is taken from the English locale,
;;                 no special for DST;
;;         (FULL-STD . FULL-DST) -- abbreviated names are taken from the
;;                                  English locale;
;;         [ABBREVIATED FULL] -- no special for DST;
;;         [ABBREVIATED-STD ABBREVIATED-DST FULL-STD FULL-DST].
(defvar datetime--timezone-name-extmap nil)

(dolist (entry
         '((datetime--locale-extmap "locale-data.extmap" :auto-reload t)
           (datetime--timezone-extmap "timezone-data.extmap" :weak-data t :auto-reload t)
           (datetime--timezone-name-extmap "timezone-name-data.extmap" :weak-data t :auto-reload t)))
  (let ((directory (file-name-directory (or load-file-name (buffer-file-name))))
        (variable  (nth 0 entry))
        (filename  (nth 1 entry))
        (options   (nthcdr 2 entry)))
    (unless (ignore-errors (file-equal-p (file-name-directory (cdr (assq 'filename (extmap-statistics (symbol-value variable)))))
                                         directory))
      (set variable (apply #'extmap-init (expand-file-name filename directory) options)))))

(defvar datetime--pattern-parsers '((parsed . (lambda (pattern options) pattern))
                                    (java   . datetime--parse-java-pattern)))

(defvar datetime--pattern-formatters '((parsed . (lambda (parts options) parts))
                                       (java   . datetime--format-java-pattern)))

;; Floating-point offset is our internal mark of a transition to DST.
(defvar datetime--last-conversion-offset nil)

(defvar datetime--locale-timezone-name-lookup-cache nil)
(defvar datetime--locale-timezone-name-lookup-cache-version 0)


;; `datetime-list-*' must be defined here, since they are used in
;; `defcustom' forms below.
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
      (extmap-mapc datetime--locale-extmap (lambda (locale data) (unless (plist-get data :parent) (push locale locales))))
      locales)))

(defun datetime-list-timezones ()
  "List all timezones for which the library has information.

Return value is a list of symbols in no particular order; it can
be modified freely."
  (delq :aliases (extmap-keys datetime--timezone-extmap)))


(defgroup datetime nil
  "Date-time handling library."
  :group 'i18n)

(defcustom datetime-locale nil
  "Default locale for date-time formatting and parsing.
Leave unset to let the library auto-determine it from your OS
when necessary.

You can see the list of locales supported by the library by
evaluating this form:

    (prin1-to-string (sort (datetime-list-locales t) #\\='string<))"
  ;; The only minor problem is the type won't be rebuilt if `datetime--locale-extmap' is
  ;; autoreloaded, but oh well.
  :type  `(choice (const nil) ,@(mapcar (lambda (locale) `(const ,locale)) (datetime-list-locales t))))

(defcustom datetime-timezone nil
  "Default timezone for date-time formatting and parsing.
Leave unset to let the library auto-determine it from your OS
when necessary.

You can see the list of supported timezones by evaluating this
form:

    (prin1-to-string (sort (datetime-list-timezones) #\\='string<))"
  :type  `(choice (const nil) ,@(mapcar (lambda (locale) `(const ,locale)) (datetime-list-timezones))))


(defun datetime--get-locale (options)
  (let ((locale (plist-get options :locale)))
    (if (eq locale 'system)
        (or (when datetime-locale
              (if (extmap-contains-key datetime--locale-extmap datetime-locale)
                  datetime-locale
                (warn "Locale `%S' (value of `datetime-locale' variable) is not known" datetime-locale)
                nil))
            (let ((system-locale (or (getenv "LC_ALL") (getenv "LC_TIME") (getenv "LANG")))
                  as-symbol)
              (when system-locale
                (save-match-data
                  (when (string-match "^[a-zA-Z_]+" system-locale)
                    (setq as-symbol (intern (replace-regexp-in-string "_" "-" (match-string 0 system-locale) t t))))))
              (if (extmap-contains-key datetime--locale-extmap as-symbol)
                  as-symbol
                (error "Failed to determine system locale%s; consider customizing `datetime-locale' variable"
                       (if as-symbol (format-message " (found raw value: `%s')" as-symbol) "")))))
      (or locale 'en))))

(defun datetime--get-timezone (options)
  (let ((timezone (plist-get options :timezone)))
    (if (eq timezone 'system)
        (or (when datetime-timezone
              (if (and (not (eq datetime-timezone :aliases)) (extmap-contains-key datetime--timezone-extmap datetime-timezone))
                  datetime-timezone
                (warn "Timezone `%S' (value of `datetime-timezone' variable) is not known" datetime-timezone)
                nil))
            (datetime--determine-system-timezone))
      (or timezone 'UTC))))

(defun datetime--determine-system-timezone ()
  ;; Unfortunately, there is no simple way.  `current-time-zone' might
  ;; look as one, but it often returns a name that is not understood
  ;; by this library.  These heuristics are certainly incomplete.
  (save-match-data
    (let ((system-timezone (intern (or (pcase system-type
                                         ((or `gnu `gnu/linux `gnu/kfreebsd `darwin)
                                          (or ;; For Debian-based distros.
                                              (when (file-exists-p "/etc/timezone")
                                                (condition-case nil
                                                    (with-temp-buffer
                                                      (insert-file-contents-literally "/etc/timezone")
                                                      (when (looking-at "\\S-+")
                                                        (match-string-no-properties 0)))
                                                  (error)))
                                              ;; Freedesktop standard (?).
                                              (let ((localtime (file-symlink-p "/etc/localtime")))
                                                ;; The link normally points to `/usr/share/...', but at least
                                                ;; on macOS the target is `/var/db/timezone...', see
                                                ;; https://github.com/doublep/datetime/issues/11.  To make
                                                ;; this more robust, just accept any target with "zoneinfo"
                                                ;; just before the name.
                                                (when (and localtime (string-match "^/.+/zoneinfo/\\(.+\\)$" localtime))
                                                  (match-string-no-properties 1 localtime)))))
                                         ;; FIXME: On Windows we could (probably) use "tzutil /g" command to get
                                         ;;        timezone identifier, but then it still needs to be mapped to what we
                                         ;;        have in `timezone-data.extmap' (i.e. Java format)...  So, currently
                                         ;;        Windows users have to set `datetime-timezone' manually.
                                         )
                                       (cadr (current-time-zone))
                                       "?"))))
      (if (and (not (eq system-timezone :aliases)) (extmap-contains-key datetime--timezone-extmap system-timezone))
          system-timezone
        (let* ((aliases (extmap-get datetime--timezone-extmap :aliases t))
               (entry   (assoc (symbol-name system-timezone) aliases)))
          (if entry
              (cdr entry)
            (error "Failed to determine system timezone%s; consider customizing `datetime-timezone' variable"
                   (if (eq system-timezone '\?) "" (format-message " (found raw value: `%s')" system-timezone)))))))))


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
         (setf (car parts) (concat (car ,parts) text))
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
               (push (pcase character
                       (?G (cons 'era (pcase num-repetitions
                                        ((or 1 2 3) 'short)
                                        (4          'full)
                                        (5          'narrow)
                                        (_ (error "Pattern character `%c' must come in 1-5 repetitions" character)))))
                       ((or ?y ?Y)
                        (cons (if (= character ?y) 'year 'year-for-week)
                              (pcase num-repetitions
                                (1 'add-century-when-parsing)
                                (2 'always-two-digits)
                                (_ num-repetitions))))
                       ((or ?M ?L)
                        (if (<= num-repetitions 2)
                            (cons 'month num-repetitions)
                          (cons (if (= character ?M) 'month-context-name 'month-standalone-name)
                                (pcase num-repetitions
                                  (3 'short)
                                  (4 'full)
                                  (5 'narrow)
                                  (_ (error "Pattern character `%c' must come in 1-5 repetitions" character))))))
                       ((or ?E ?c ?e)
                        (if (or (cond ((= character ?e) (<= num-repetitions 2))
                                      ((= character ?c) (=  num-repetitions 1))))
                            (cons 'weekday num-repetitions)
                          (cons (if (= character ?c) 'weekday-standalone-name 'weekday-context-name)
                                (pcase num-repetitions
                                  ((or 1 2 3) 'short)
                                  (4 'full)
                                  (5 'narrow)
                                  (_ (error "Pattern character `%c' must come in 1-5 repetitions" character))))))
                       (?w (cons 'week-in-year      num-repetitions))
                       (?W (cons 'week-in-month     num-repetitions))
                       (?D (cons 'day-in-year       num-repetitions))
                       (?d (cons 'day-in-month      num-repetitions))
                       (?F (cons 'weekday-in-month  num-repetitions))
                       (?u (cons 'weekday           num-repetitions))
                       (?a (cons 'am-pm             (pcase num-repetitions
                                                      (1 'abbreviated)
                                                      (_ (error "Pattern character `%c' must come in exactly 1 repetition" character)))))
                       (?H (cons 'hour-0-23         num-repetitions))
                       (?k (cons 'hour-1-24         num-repetitions))
                       (?K (cons 'hour-am-pm-0-11   num-repetitions))
                       (?h (cons 'hour-am-pm-1-12   num-repetitions))
                       (?B (cons 'day-period        (pcase num-repetitions
                                                      (1 :short)
                                                      (4 :full)
                                                      (5 :narrow)
                                                      (_ (error "Pattern character `%c' must come in exactly 1, 4 or 5 repetitions" character)))))
                       (?m (cons 'minute            num-repetitions))
                       (?s (cons 'second            num-repetitions))
                       (?S (cons 'second-fractional num-repetitions))
                       (?z (cons 'timezone          (if (>= num-repetitions 4) 'full 'abbreviated)))
                       (?O (cons 'timezone          (pcase num-repetitions
                                                      (1 'offset-localized-short)
                                                      (4 'offset-localized-full)
                                                      (_ (error "Pattern character `%c' must come in exactly 1 or 4 repetitions" character)))))
                       ((or ?x ?X)
                        (cons 'timezone             (let ((details (pcase num-repetitions
                                                                     (1 'offset-hh?mm)
                                                                     (2 'offset-hhmm)
                                                                     (3 'offset-hh:mm)
                                                                     (4 'offset-hhmm?ss)
                                                                     (5 'offset-hh:mm?:ss)
                                                                     (_ (error "Pattern character `%c' must come in 1-5 repetitions" character)))))
                                                      (if (= character ?x) details (intern (format "%s-or-z" (symbol-name details)))))))
                       (?Z (cons 'timezone          (pcase num-repetitions
                                                      ((or 1 2 3) 'offset-hhmm)
                                                      (4          'offset-localized-full)
                                                      (5          'offset-hh:mm?:ss-or-z)
                                                      (_ (error "Pattern character `%c' must come in 1-5 repetitions" character)))))
                       (_
                        (error "Illegal pattern character `%c'" character)))
                     parts))
              ;; FIXME: Optional pattern sections are currently treated the same as
              ;;        mandatory (brackets are just discarded).  May want to treat them
              ;;        as optional at least for parsing purposes later.
              ((or (= character ?\[) (= character ?\])))
              (t
               (if (and (or (= character ?.) (= character ?,))
                        (plist-get options :any-decimal-separator)
                        (eq (car-safe (car parts)) 'second)
                        (< scan length) (= (aref pattern scan) ?S))
                   (push (cons 'decimal-separator character) parts)
                 (datetime--extend-as-is-part parts (string character)))))))
    (nreverse parts)))

(defun datetime--format-java-pattern (parts options)
  (ignore options)
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
                          (`era              (pcase details
                                               (`short  "G")
                                               (`full   "GGGG")
                                               (`narrow "GGGGG")))
                          ((or `year `year-for-week)
                           (let ((base (if (eq type 'year) ?y ?Y)))
                             (pcase details
                               (`add-century-when-parsing base)
                               (`always-two-digits        (cons base 2))
                               (_                         (cons base details)))))
                          (`month            (cons ?M details))
                          ((or `month-context-name `month-standalone-name `weekday-context-name `weekday-standalone-name)
                           (cons (pcase type
                                   (`month-context-name      ?M)
                                   (`month-standalone-name   ?L)
                                   (`weekday-context-name    ?E)
                                   (`weekday-standalone-name ?c))
                                 (pcase details
                                   (`short  3)
                                   (`full   4)
                                   (`narrow 5)
                                   (_       (error "Unexpected details %s" details)))))
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
                          (`second-fractional (cons ?S details))
                          (`am-pm             "a")
                          (`day-period        (pcase details
                                                (:short  "B")
                                                (:full   "BBBB")
                                                (:narrow "BBBBB")
                                                (_       (error "Unexpected details for `%s' part: %s" type details))))
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


(defsubst datetime--gregorian-leap-year-mod-400-p (year-mod-400)
  (aref (eval-when-compile (let (result)
                             (dotimes (year 400)
                               (push (and (= (% year 4) 0) (or (/= (% year 100) 0) (= (% year 400) 0))) result))
                             (with-no-warnings (apply (if (fboundp #'bool-vector) #'bool-vector #'vector) (nreverse result)))))
        year-mod-400))

(defsubst datetime--gregorian-leap-year-p (year)
  (datetime--gregorian-leap-year-mod-400-p (mod year 400)))

(defconst datetime--gregorian-cumulative-year-days (let ((days 0)
                                                         result)
                                                     (dotimes (year 400)
                                                       (push days result)
                                                       (setq days (+ days (if (datetime--gregorian-leap-year-mod-400-p year) 366 365))))
                                                     (push days result)
                                                     (apply #'vector (nreverse result))))
(defconst datetime--gregorian-days-in-400-years    (aref datetime--gregorian-cumulative-year-days 400))
(defconst datetime--gregorian-days-in-1970-years   (+ (* datetime--gregorian-days-in-400-years (/ 1970 400))
                                                      (aref datetime--gregorian-cumulative-year-days (% 1970 400))))

;; Conveniently, this also has a loop size of 400 years.
(defconst datetime--gregorian-first-day-of-year (let ((first-day 5)
                                                      result)
                                                  (dotimes (year 400)
                                                    (push first-day result)
                                                    (setq first-day (% (+ first-day (if (datetime--gregorian-leap-year-mod-400-p year) 2 1)) 7)))
                                                  (apply #'vector (nreverse result))))

(defconst datetime--average-seconds-in-year (/ (* datetime--gregorian-days-in-400-years 24 60 60) 400))

;; For non-leap years.
(defconst datetime--gregorian-month-days            [31 28 31 30 31 30 31 31 30 31 30 31])
(defconst datetime--gregorian-cumulative-month-days (let ((days   0)
                                                          (result (list 0)))
                                                      (dolist (month-days (append datetime--gregorian-month-days nil))
                                                        (push (setq days (+ days month-days)) result))
                                                      (apply #'vector (nreverse result))))

;; FIXME: Maybe use binary lookup or something?  Not terribly important.
(defsubst datetime--day-period-index (thresholds minute)
  (let ((index 0))
    (while (and thresholds (<= (car thresholds) minute))
      (setq thresholds (cdr thresholds)
            index      (1+ index)))
    index))


;; In functions below we rely on form arguments being evaluated from left to right.  This
;; is documented in Elisp manual.  Important as we use `(setf offset ...)' in the first
;; argument's of `format'.

(defsubst datetime--format-offset-hhmm (offset)
  (format (if (>= offset 0)
              "+%02d%02d"
            (setf offset (- offset))
            "-%02d%02d")
          (/ offset (* 60 60)) (/ (% offset (* 60 60)) 60)))

(defsubst datetime--format-offset-hh?mm (offset)
  (let ((sign    (if (>= offset 0) ?+ ?-))
        (hours   (/ (if (>= offset 0) offset (setf offset (- offset))) (* 60 60)))
        (minutes (/ (% offset (* 60 60)) 60)))
      (if (= minutes 0)
          (format "%c%02d" sign hours)
        (format "%c%02d%02d" sign hours minutes))))

(defsubst datetime--format-offset-hhmm?ss (offset)
  (let ((sign    (if (>= offset 0) ?+ ?-))
        (seconds (% (if (>= offset 0) offset (setf offset (- offset))) 60)))
    (if (= seconds 0)
        (format "%c%02d%02d" sign (/ offset (* 60 60)) (/ (% offset (* 60 60)) 60))
      (format "%c%02d%02d%02d" sign (/ offset (* 60 60)) (/ (% offset (* 60 60)) 60) seconds))))

(defsubst datetime--format-offset-hh:mm (offset)
  (format (if (>= offset 0)
              "+%02d:%02d"
            (setf offset (- offset))
            "-%02d:%02d")
          (/ offset (* 60 60)) (/ (% offset (* 60 60)) 60)))

(defsubst datetime--format-offset-hh:mm?:ss (offset)
  (let ((sign    (if (>= offset 0) ?+ ?-))
        (seconds (% (if (>= offset 0) offset (setf offset (- offset))) 60)))
    (if (= seconds 0)
        (format "%c%02d:%02d" sign (/ offset (* 60 60)) (/ (% offset (* 60 60)) 60))
      (format "%c%02d:%02d:%02d" sign (/ offset (* 60 60)) (/ (% offset (* 60 60)) 60) seconds))))

(defsubst datetime--format-offset-hhmm-or-z (offset)
  (if (= offset 0) "Z" (datetime--format-offset-hhmm offset)))

(defsubst datetime--format-offset-hh?mm-or-z (offset)
  (if (= offset 0) "Z" (datetime--format-offset-hh?mm offset)))

(defsubst datetime--format-offset-hhmm?ss-or-z (offset)
  (if (= offset 0) "Z" (datetime--format-offset-hhmm?ss offset)))

(defsubst datetime--format-offset-hh:mm-or-z (offset)
  (if (= offset 0) "Z" (datetime--format-offset-hh:mm offset)))

(defsubst datetime--format-offset-hh:mm?:ss-or-z (offset)
  (if (= offset 0) "Z" (datetime--format-offset-hh:mm?:ss offset)))

(defsubst datetime--format-offset-localized-short (offset)
  (if (= offset 0)
      "GMT"
    (let ((sign                (if (>= offset 0) ?+ ?-))
          (minutes-and-seconds (% (if (>= offset 0) offset (setf offset (- offset))) (* 60 60))))
      (if (= minutes-and-seconds 0)
          (format "GMT%c%d" sign (/ offset (* 60 60)))
        (let ((seconds (% minutes-and-seconds 60)))
          (if (= seconds 0)
              (format "GMT%c%d:%02d" sign (/ offset (* 60 60)) (/ minutes-and-seconds 60))
            (format "GMT%c%d:%02d:%02d" sign (/ offset (* 60 60)) (/ minutes-and-seconds 60) seconds)))))))

(defsubst datetime--format-offset-localized-full (offset)
  (if (= offset 0)
      "GMT"
    (let ((sign    (if (>= offset 0) ?+ ?-))
          (seconds (% (if (>= offset 0) offset (setf offset (- offset))) 60)))
      (if (= seconds 0)
          (format "GMT%c%02d:%02d" sign (/ offset (* 60 60)) (/ (% offset (* 60 60)) 60))
        (format "GMT%c%02d:%02d:%02d" sign (/ offset (* 60 60)) (/ (% offset (* 60 60)) 60) seconds)))))

(defvar datetime--timezone-offset-matching-regexps
  '((offset-hhmm            . "[-+][01][0-9][0-5][0-9]")
    (offset-hh?mm           . "[-+][01][0-9]\\(?:[0-5][0-9]\\)?")
    (offset-hhmm?ss         . "[-+][01][0-9][0-5][0-9]\\(?:[0-5][0-9]\\)?")
    (offset-hh:mm           . "[-+][01][0-9]:[0-5][0-9]")
    (offset-hh:mm?:ss       . "[-+][01][0-9]:[0-5][0-9]\\(?::[0-5][0-9]\\)?")
    (offset-hhmm-or-z       . "[-+][01][0-9][0-5][0-9]\\|Z")
    (offset-hh?mm-or-z      . "[-+][01][0-9]\\(?:[0-5][0-9]\\)?\\|Z")
    (offset-hhmm?ss-or-z    . "[-+][01][0-9][0-5][0-9]\\(?:[0-5][0-9]\\)?\\|Z")
    (offset-hh:mm-or-z      . "[-+][01][0-9]:[0-5][0-9]\\|Z")
    (offset-hh:mm?:ss-or-z  . "[-+][01][0-9]:[0-5][0-9]\\(?::[0-5][0-9]\\)?\\|Z")
    (offset-localized-short . "GMT\\(?:[-+]\\([0-9]\\|1[0-9]\\)\\(?::[0-5][0-9]\\(?::[0-5][0-9]\\)?\\)?\\)?")
    (offset-localized-full  . "GMT\\(?:[-+][01][0-9]:[0-5][0-9]\\(?::[0-5][0-9]\\)?\\)?")))

(defun datetime--timezone-offset-matching-regexp (details)
  (cdr (assq details datetime--timezone-offset-matching-regexps)))


(defsubst datetime--digits-format (num-repetitions)
  (if (> num-repetitions 1) (format "%%0%dd" num-repetitions) "%d"))

(defsubst datetime--format-escape-string (string)
  (replace-regexp-in-string "%" "%%" string t t))

(defun datetime-float-formatter (type pattern &rest options)
  "Return a function that formats date-time expressed as a float.
The returned function accepts single argument---a floating-point
number---and returns a string with given time formatted according
to given PATTERN of given TYPE.  Rest of the arguments must be a
property list, i.e. keywords interleaved with values.

OPTIONS should be any keyword arguments understood by
`datetime-recode-pattern' plus any from the list below, specific
to this function.

  :locale

    Locale (language) used for month, weekday etc. names.  Always
    defaults to English, even if system locale is different.  You
    can use special value \\='system to let the library find it.

  :timezone

    Timezone for time values to be formatted in.  Always defaults
    to UTC.  You can use special value \\='system to let the
    library find the value, suitable for the current machine.

  :debug

    Don't byte-compile the formatter function, leave it in the
    form of a Lisp lambda."
  (let* ((locale        (datetime--get-locale options))
         (timezone      (datetime--get-timezone options))
         (timezone-data (or (unless (eq timezone :aliases) (extmap-get datetime--timezone-extmap timezone t))
                            (error "Unknown timezone `%s'" timezone)))
         need-year need-month need-weekday need-day need-hour need-time
         format-parts
         format-arguments)
    (dolist (part (datetime--parse-pattern type pattern options))
      (if (stringp part)
          (push (datetime--format-escape-string part) format-parts)
        (let ((type    (car part))
              (details (cdr part)))
          (pcase type
            (`era
             (setq need-year t)
             (push "%s" format-parts)
             (push `(aref ,(datetime-locale-field locale (datetime--era-field details)) (if (> year 0) 1 0)) format-arguments))
            (`year
             (setq need-year t)
             (push (pcase details
                     (`add-century-when-parsing "%d")
                     (`always-two-digits        "%02d")
                     (_                         (datetime--digits-format details)))
                   format-parts)
             (push (if (eq type 'year)
                       `(if (> year 0) year (- 1 year))
                     (error "Formatting `%s' is currently not implemented" type))
                   format-arguments)
             (when (eq details 'always-two-digits)
               (setf (car format-arguments) `(mod ,(car format-arguments) 100))))
            (`year-for-week
             (error "Formatting `%s' is currently not implemented" type))
            (`month
             (setq need-month t)
             (push (datetime--digits-format details) format-parts)
             (push `(1+ month) format-arguments))
            ((or `month-context-name `month-standalone-name)
             (setq need-month t)
             (push "%s" format-parts)
             (push `(aref ,(datetime-locale-field locale
                                                  (if (eq type 'month-context-name)
                                                      (datetime--month-context-name-field details)
                                                    (datetime--month-standalone-name-field details)))
                          month)
                   format-arguments))
            (`week-in-year
             (error "Formatting `%s' is currently not implemented" type))
            (`week-in-month
             (error "Formatting `%s' is currently not implemented" type))
            (`day-in-year
             (setq need-day t)
             (push (datetime--digits-format details) format-parts)
             (push `(1+ year-day) format-arguments))
            (`day-in-month
             (setq need-day t)
             (push (datetime--digits-format details) format-parts)
             (push `(1+ day) format-arguments))
            (`weekday-in-month
             (error "Formatting `%s' is currently not implemented" type))
            (`weekday
             (setq need-weekday t)
             (push (datetime--digits-format details) format-parts)
             (let ((first-day-of-week (datetime-locale-field locale :first-day-of-week)))
               (push (if (= first-day-of-week 0)
                         `(1+ weekday)
                       `(1+ (mod (- weekday ,first-day-of-week) 7)))
                     format-arguments)))
            ((or `weekday-context-name `weekday-standalone-name)
             (setq need-weekday t)
             (push "%s" format-parts)
             (push `(aref ,(datetime-locale-field locale
                                                  (if (eq type 'weekday-context-name)
                                                      (datetime--weekday-context-name-field details)
                                                    (datetime--weekday-standalone-name-field details)))
                          weekday)
                   format-arguments))
            (`am-pm
             (setq need-hour t)
             (push "%s" format-parts)
             (push `(aref ,(datetime-locale-field locale :am-pm) (if (>= hour 12) 1 0)) format-arguments))
            (`day-period
             (setq need-time t)
             (push "%s" format-parts)
             (let* ((day-period-data (datetime-locale-field locale :day-periods))
                    (thresholds      (plist-get day-period-data :thresholds))
                    (strings         (or (plist-get day-period-data details) (plist-get day-period-data :short))))
               (push `(aref ,strings (datetime--day-period-index ',thresholds (/ time 60))) format-arguments)))
            ((or `hour-0-23 `hour-1-24 `hour-am-pm-0-11 `hour-am-pm-1-12)
             (setq need-hour t)
             (push (datetime--digits-format details) format-parts)
             (push (pcase type
                     (`hour-0-23       `hour)
                     (`hour-1-24       `(if (> hour 0) hour 24))
                     (`hour-am-pm-0-11 `(% hour 12))
                     (`hour-am-pm-1-12 `(let ((hour (% hour 12))) (if (> hour 0) hour 12))))
                   format-arguments))
            (`minute
             (setq need-time t)
             (push (datetime--digits-format details) format-parts)
             (push `(/ (mod time ,(* 60 60)) 60) format-arguments))
            (`second
             (setq need-time t)
             (push (datetime--digits-format details) format-parts)
             (push `(mod time 60) format-arguments))
            (`second-fractional
             (setq need-time t)
             (push (datetime--digits-format details) format-parts)
             (let ((scale (expt 10 details)))
               (push `(mod (* time ,scale) ,scale) format-arguments)))
            (`timezone
             (pcase details
               ((or `abbreviated `full)
                (let* ((name     (datetime-locale-timezone-name locale timezone nil (eq details 'full)))
                       (dst-name (pcase timezone-data
                                   (`(,_constant-offset) name)
                                   (_ (datetime-locale-timezone-name locale timezone t (eq details 'full))))))
                  (if (string= name dst-name)
                      (push (datetime--format-escape-string name) format-parts)
                    (push "%s" format-parts)
                    ;; See comments for the variable for explanation of `floatp'.
                    (push `(if (floatp datetime--last-conversion-offset) ,dst-name ,name) format-arguments))))
               ((or `offset-localized-short `offset-localized-full
                    `offset-hh?mm `offset-hhmm `offset-hh:mm `offset-hhmm?ss `offset-hh:mm?:ss
                    `offset-hh?mm-or-z `offset-hhmm-or-z `offset-hh:mm-or-z `offset-hhmm?ss-or-z `offset-hh:mm?:ss-or-z
                    `offset-hhmm)
                (let ((formatter-function (intern (format "datetime--format-%s" (symbol-name details)))))
                  (pcase timezone-data
                    (`(,constant-offset)
                     (push (funcall formatter-function constant-offset) format-parts))
                    (_
                     ;; At least `offset-hhmm' and `offset-hh:mm' could in principle be
                     ;; inlined since they use (or could use) fixed format substring.
                     ;; Hardly terribly important.
                     (push "%s" format-parts)
                     (push `(,formatter-function (round datetime--last-conversion-offset)) format-arguments)))))
               (_
                (error "Unexpected timezone details `%s'" details))))
            (_ (error "Unexpected value `%s'" type))))))
    ;; 400 is the size of Gregorian calendar leap year loop.
    (let* ((days-in-400-years datetime--gregorian-days-in-400-years)
           (formatter `(lambda (date-time)
                         (setq date-time ,(pcase timezone-data
                                            (`(,constant-offset)
                                             (if (/= constant-offset 0)
                                                 `(+ (float date-time) ,constant-offset)
                                               `(float date-time)))
                                            (_
                                             `(datetime--convert-to-utc-float (float date-time) ,(datetime--macroexp-quote timezone-data)))))
                         (let* (,@(when (or need-year need-month need-weekday need-day)
                                    ;; Date in days, rebased from 1970-01-01 to 0000-01-01.
                                    `((date-0           (+ (floor (/ date-time ,(* 24 60 60))) ,datetime--gregorian-days-in-1970-years))
                                      (date-%-400-years (mod date-0 ,days-in-400-years))
                                      (full-400-years   (/ (- date-0 date-%-400-years) ,days-in-400-years))
                                      (year-%-400       (/ date-%-400-years 366))
                                      (year             (+ (* full-400-years 400)
                                                           (progn
                                                             (if (< date-%-400-years (aref ,datetime--gregorian-cumulative-year-days (1+ year-%-400)))
                                                                 year-%-400
                                                               (setq year-%-400 (1+ year-%-400))))))))
                                ,@(when (or need-month need-weekday need-day)
                                    `((year-day         (- date-0 (* full-400-years ,days-in-400-years) (aref ,datetime--gregorian-cumulative-year-days (mod year 400))))))
                                ,@(when (or need-month need-day)
                                    `((day              year-day)
                                      ;; Using variable `_month' to avoid byte-compilation warnings if day is
                                      ;; needed, but month is not.  Let's hope byte-compiler elides unneeded
                                      ;; code then (only side-effect of `(setq day ...)' is important in that
                                      ;; case), such patterns are too uncommon to bother ourselves.
                                      (,(if need-month 'month '_month)
                                       (let ((july-days (if (datetime--gregorian-leap-year-mod-400-p year-%-400)
                                                            ,(+ 31 29 31 30 31 30)
                                                          ,(+ 31 28 31 30 31 30))))
                                         (if (>= day july-days)
                                             (if (>= (setq day (- day july-days)) ,(+ 31 31 30))
                                                 (cond ((< (setq day (- day ,(+ 31 31 30))) 31)  9)           ; October
                                                       ((< (setq day (- day 31)) 30)            10)           ; November
                                                       (t  (setq day (- day 30))                11))          ; December
                                               (cond ((< day 31)                                 6)           ; July
                                                     ((< (setq day (- day 31)) 31)               7)           ; August
                                                     (t  (setq day (- day 31))                   8)))         ; September
                                           (let ((february-days (- july-days ,(+ 31 30 31 30))))
                                             (cond ((< day february-days)
                                                    (cond ((< day 31)                            0)           ; January
                                                          (t (setq day (- day 31))               1)))         ; February
                                                   ((< (setq day (- day february-days)) ,(+ 31 30))
                                                    (cond ((< day 31)                            2)           ; March
                                                          (t (setq day (- day 31))               3)))         ; April
                                                   (t
                                                    (cond ((< (setq day (- day ,(+ 31 30))) 31)  4)           ; May
                                                          (t (setq day (- day 31))               5))))))))))  ; June
                                ,@(when need-weekday
                                    `((weekday          (% (+ year-day (aref ,datetime--gregorian-first-day-of-year (mod year 400))) 7))))
                                ,@(when (or need-time need-hour)
                                    `((time (mod date-time ,(* 24 60 60)))))
                                ,@(when need-hour
                                    `((hour (/ (mod (floor time) ,(* 24 60 60)) ,(* 60 60))))))
                           (format ,(apply #'concat (nreverse format-parts)) ,@(nreverse format-arguments))))))
      (unless (plist-get options :debug)
        (setf formatter (datetime--do-byte-compile formatter "the generated formatter")))
      formatter)))

;; Not available on older Emacs versions.  Copied from recent Emacs source.
(defun datetime--macroexp-quote (v)
  (if (and (not (consp v))
	   (or (keywordp v)
	       (not (symbolp v))
	       (memq v '(nil t))))
      v
    (list 'quote v)))

(defun datetime--do-byte-compile (function description)
  (or (byte-compile function)
      (error "Internal error: unable to byte-compile %s" description)))

(defun datetime--convert-to-utc-float (date-time timezone-data)
  (let ((year-offset          (floor (/ (- date-time (car timezone-data)) datetime--average-seconds-in-year)))
        (all-year-transitions (nth 1 timezone-data))
        offset)
    (if (>= year-offset 0)
        (let ((year-transitions (or (when (< year-offset (length all-year-transitions))
                                      (aref all-year-transitions year-offset))
                                    (datetime--calculate-year-transitions timezone-data year-offset))))
          (setf offset (pop year-transitions))
          (when year-transitions
            (let ((offset-in-year (floor (- date-time (car timezone-data) (* year-offset datetime--average-seconds-in-year)))))
              (while (and (>= offset-in-year (car year-transitions))
                          (setf offset           (cadr year-transitions)
                                year-transitions (cddr year-transitions)))))))
      ;; Offset before the very first transition.
      (setf offset (car (aref all-year-transitions 0))))
    (+ date-time (setf datetime--last-conversion-offset offset))))

;; 146097 is the value of `datetime--gregorian-days-in-400-years'.
;; `eval-when-compile' doesn't allow referring to the mnemonic name.
;;
;; Likewise, 135140 is the value of
;; `(aref datetime--gregorian-cumulative-year-days (mod 1970 400))'.
(defsubst datetime--start-of-day (year year-day)
  (* (eval-when-compile (* 24 60 60.0))
     (+ (* (floor (/ (float year) 400)) (eval-when-compile 146097))
        (aref datetime--gregorian-cumulative-year-days (mod year 400))
        (eval-when-compile (- (+ (* (floor (/ (float 1970) 400)) 146097) 135140)))
        year-day)))

(defun datetime--calculate-year-transitions (timezone-data year-offset)
  (let* ((all-year-transitions (nth 1 timezone-data))
         (num-years            (length all-year-transitions))
         transitions)
    (when (>= year-offset num-years)
      (setf (cadr timezone-data) (setq all-year-transitions (vconcat all-year-transitions (make-vector (max (1+ (- year-offset num-years)) (/ num-years 2) 10) nil)))))
    (let ((year      (+ (nth 2 timezone-data) year-offset))
          (year-base (+ (nth 0 timezone-data) (* year-offset datetime--average-seconds-in-year)))
          (rules     (nth 3 timezone-data)))
      (if rules
          (dolist (rule rules)
            (let* ((month           (plist-get rule :month))
                   (day-of-month    (plist-get rule :day-of-month))
                   (effective-month (if (< day-of-month 0) month (1- month)))
                   (day-of-week     (plist-get rule :day-of-week))
                   (year-day        (+ (aref datetime--gregorian-cumulative-month-days effective-month)
                                       (if (and (>= effective-month 2) (datetime--gregorian-leap-year-p year)) 1 0)
                                       day-of-month -1))
                   (offset-before   (plist-get rule :before)))
              (unless transitions
                ;; Preserve our DST "flag" across year boundary.
                (push (if (floatp (car (last (aref all-year-transitions (1- year-offset)))))
                          (float offset-before)
                        offset-before)
                      transitions))
              (when day-of-week
                (let ((current-weekday (% (+ year-day (aref datetime--gregorian-first-day-of-year (mod year 400))) 7)))
                  (setq year-day (if (< day-of-month 0) (- year-day (mod (- day-of-week current-weekday) 7)) (+ year-day (mod (- day-of-week current-weekday) 7))))))
              (when (plist-get rule :end-of-day)
                (setq year-day (1+ year-day)))
              (push (round (- (+ (datetime--start-of-day year year-day) (plist-get rule :time))
                              (pcase (plist-get rule :time-definition)
                                (`utc      0)
                                (`standard (plist-get rule :standard-offset))
                                (`wall     offset-before)
                                (type      (error "Unhandled time definition type `%s'" type)))
                              year-base))
                    transitions)
              (let ((after (plist-get rule :after)))
                ;; Mark transitions to DST by making offset a float.
                (push (if (plist-get rule :dst) (float after) after) transitions))))
        ;; No transition rules.  Take the offset after the last historical transition.
        (let ((k (length all-year-transitions)))
          (while (null transitions)
            (let ((historic-transitions (aref all-year-transitions (setf k (1- k)))))
              (when historic-transitions
                (setf transitions `(,(car (last historic-transitions))))))))))
    (aset all-year-transitions year-offset (nreverse transitions))))


;; There is horribly unreadable level of backquoting/unquoting inside this macro...
(defmacro datetime--parser-computation (pattern value-name validating min max &rest arguments)
  (let ((computations    (make-symbol "$computations"))
        (computation     (make-symbol "$computation"))
        (range-validated (make-symbol "$range-validated"))
        loops)
    (setq arguments (reverse arguments))
    (while arguments
      (let* ((set             (pop arguments))
             (part-indices    (nth 0 set))
             (builder         (nth 1 set))
             (self-validating (nth 2 set))
             (new-loop        `(while ,part-indices
                                 (push (,(if (consp builder) (car builder) builder) (pop ,part-indices) ,@(when (consp builder) (cdr builder)))
                                       ,computations))))
        (when (and self-validating (or min max))
          (setq new-loop `(progn (when ,part-indices (setq ,range-validated t)) ,new-loop)))
        (setq loops
              (if loops
                  `(,@(macroexp-unprogn new-loop)
                    (when (or ,validating (null ,computations))
                      ,@loops))
                `(,new-loop)))))
    `(let (,computations
           ,@(when (or min max) `(,range-validated)))
       ,@loops
       (when ,computations
         (let ((,computation (if (cdr ,computations)
                                 `(let ((x ,(car ,computations)))
                                    ,@(mapcar (lambda (computation)
                                                `(unless (eq ,computation x)
                                                   (signal 'datetime-invalid-string (list string ,,pattern ,,(format "inconsistent %s" value-name)))))
                                              (cdr ,computations))
                                    x)
                               (car ,computations))))
           ,@(when (or min max)
               `((when (and ,validating (not ,range-validated))
                   (setq ,computation `(let ((x ,,computation))
                                         (unless ,',(cond ((and min max) `(<= ,min x ,max))
                                                          (min           `(<= ,min x))
                                                          (t             `(<= x ,max)))
                                           (signal 'datetime-invalid-string (list string ,,pattern ,,(format "%s is out of range" value-name))))
                                         x)))))
           ,computation)))))

(defun datetime-parser-to-float (type pattern &rest options)
  "Return a function that parses date-time according to the PATTERN.
Argument TYPE defines how the pattern should be interpreted, see
library documentation.  Rest of the arguments must be a property
list, i.e. keywords interleaved with values.

The resulting function transforms a string to a float number of
seconds since the epoch (0:00:00 of 1st of January 1970), in UTC
timezone.  The function is byte-compiled, unless you specify
:debug option.  Behavior for invalid strings depends on whether
:non-validating option is specified.

OPTIONS should be any keyword arguments understood by
`datetime-recode-pattern' plus any from the list below, specific
to this function.  Default value of keyword arguments is nil
unless specified otherwise.

  :locale

    Locale (language) used for month, weekday etc. names.  Always
    defaults to English, even if system locale is different.

  :timezone

    The timezone for parsing input strings in.  Always defaults
    to UTC.  You can use special value \\='system to let the
    library find the value suitable for the current machine.

    If input string explicitly specifies a timezone (i.e. if
    PATTERN does), this value is essentially ignored.

  :defaults

    A plist of values for those date/time part that are not
    specified in the input.  Accepted keys:

      year   -- defaults to 1970 (the year of UNIX epoch);
      month  -- must be in the range 1 to 12, defaults to 1;
      day    -- must be in the range 1 to 31, defaults to 1; will
                cause validation errors if used and is too large
                for the parsed month and year;
      hour   -- must be in the range 0 to 23, defaults to 0;
      minute -- must be in the range 0 to 59, defaults to 0;
      second -- must be in the range 0 to 59, defaults to 0.

    Note that the set of accepted keys is substantially smaller
    than that of all understood pattern parts.  For example, eras
    are not supported (use negative years), or 12-hour clock time
    (convert to 24-hour).

    If PATTERN specifies a way certain value is encoded in input
    strings, corresponding value from this plist is ignored.

  :non-validating

    Validating parsers always signal a `datetime-invalid-string'
    error if given strings that cannot be parsed or contain
    invalid values like 30th of February.  Non-validating parsers
    can either return unspecified numeric result or signal
    arbitrary errors in such cases.  (But it is guaranteed they
    don't fall into an infinite loop or perform any other
    action.)

    Non-validating parsers are more efficient, for some patterns
    considerably so.

  :case-insensitive

    Accept text in any case.  This works both for literal text
    included in the pattern and for month etc. names.

  :lax-whitespace

    Match any whitespace in PATTERN against any whitespace in
    date-time string.  For this purpose \"whitespace\" is defined
    as space and tab characters only.

  :accept-leading-space

    Make variable-width numbers (e.g. day number without leading
    zero) match also if there is a leading space.

  :debug

    Don't byte-compile the parser function, leave it in the form
    of Lisp lambda."
  (let* ((locale           (datetime--get-locale options))
         (timezone         (datetime--get-timezone options))
         (timezone-data    (or (unless (eq timezone :aliases) (extmap-get datetime--timezone-extmap timezone t))
                               (error "Unknown timezone `%s'" timezone)))
         (defaults         (plist-get options :defaults))
         (validating       (not (plist-get options :non-validating)))
         (case-insensitive (and (plist-get options :case-insensitive) t))
         (lax-whitespace   (plist-get options :lax-whitespace))
         (part-index       0)
         regexp-part-sources
         last-part-was-numeric
         regexp-parts
         ;; To handle excessive information patterns (e.g. "Mon 16 Sep 2018" is excessive,
         ;; since day of the week can be found from the day of the year), we keep track of
         ;; all the various groups and decide which to use later.  Groups are also stored
         ;; as a list (or alist in certain cases), though this is hardly necessary, since
         ;; normally patterns wouldn't repeat the same group.
         era-part-indices
         year-part-indices
         month-number-part-indices
         month-name-part-indices
         day-of-month-part-indices
         am-pm-part-indices
         day-period-part-indices
         hour-0-23-part-indices
         hour-1-24-part-indices
         hour-am-pm-1-12-part-indices
         hour-am-pm-0-11-part-indices
         minute-part-indices
         second-part-indices
         second-fractional-part-indices
         timezone-offset-part-indices
         have-case-sensitive-parts)
    ;; Doing this in two loops, so that the second can look ahead and easily find out if
    ;; the next regexp part is going to be a numeric value.
    (dolist (part (datetime--parse-pattern type pattern options))
      (push (if (stringp part)
                (let ((quoted (regexp-quote part)))
                  (when (not (or have-case-sensitive-parts (string= (upcase part) (downcase part))))
                    (setq have-case-sensitive-parts t))
                  (cons (if lax-whitespace
                            (replace-regexp-in-string (rx (1+ (any blank))) (rx (1+ (any blank))) quoted t t)
                          quoted)
                        nil))
              (let* ((type    (car part))
                     (details (cdr part)))
                (cons (pcase type
                        (`era                     (let ((field (datetime--era-field details)))
                                                    (when (or validating (null era-part-indices))
                                                      (push (cons part-index field) era-part-indices))
                                                    (datetime-locale-field locale field)))
                        (`year                    (when (or validating (null year-part-indices))
                                                    (push (cons part-index details) year-part-indices))
                                                  ;; Magic number for the next loop.
                                                  0)
                        (`year-for-week           (error "Parsing `%s' is currently not implemented" type))
                        (`month                   (when (or validating (null month-number-part-indices))
                                                    (push part-index month-number-part-indices))
                                                  12)
                        (`month-context-name      (let ((field (datetime--month-context-name-field details)))
                                                    (when (or validating (null month-name-part-indices))
                                                      (push (cons part-index field) month-name-part-indices))
                                                    (datetime-locale-field locale field)))
                        (`month-standalone-name   (let ((field (datetime--month-standalone-name-field details)))
                                                    (when (or validating (null month-name-part-indices))
                                                      (push (cons part-index field) month-name-part-indices))
                                                    (datetime-locale-field locale field)))
                        (`week-in-year            (error "Parsing `%s' is currently not implemented" type))
                        (`week-in-month           (error "Parsing `%s' is currently not implemented" type))
                        (`day-in-month            (when (or validating (null day-of-month-part-indices))
                                                    (push part-index day-of-month-part-indices))
                                                  31)
                        (`weekday-in-month        (error "Parsing `%s' is currently not implemented" type))
                        (`weekday                  7)
                        (`weekday-context-name    (datetime-locale-field locale (datetime--weekday-context-name-field details)))
                        (`weekday-standalone-name (datetime-locale-field locale (datetime--weekday-standalone-name-field details)))
                        (`am-pm                   (when (or validating (null am-pm-part-indices))
                                                    (push part-index am-pm-part-indices))
                                                  (datetime-locale-field locale :am-pm))
                        (`day-period              (when (or validating (null day-period-part-indices))
                                                    (push part-index day-period-part-indices))
                                                  (let ((day-period-data (datetime-locale-field locale :day-periods)))
                                                    (or (plist-get day-period-data details) (plist-get day-period-data :short))))
                        (`hour-0-23               (when (or validating (null hour-0-23-part-indices))
                                                    (push part-index hour-0-23-part-indices))
                                                  23)
                        (`hour-1-24               (when (or validating (null hour-1-24-part-indices))
                                                    (push part-index hour-1-24-part-indices))
                                                  24)
                        (`hour-am-pm-0-11         (when (or validating (null hour-am-pm-0-11-part-indices))
                                                    (push part-index hour-am-pm-0-11-part-indices))
                                                  11)
                        (`hour-am-pm-1-12         (when (or validating (null hour-am-pm-1-12-part-indices))
                                                    (push part-index hour-am-pm-1-12-part-indices))
                                                  12)
                        (`minute                  (when (or validating (null minute-part-indices))
                                                    (push part-index minute-part-indices))
                                                  59)
                        (`second                  (when (or validating (null second-part-indices))
                                                    (push part-index second-part-indices))
                                                  59)
                        (`decimal-separator       (rx (or "." ",")))
                        (`second-fractional       (push (cons part-index (expt 10.0 details)) second-fractional-part-indices)
                                                  (apply #'concat (make-list details (rx (any "0-9")))))
                        (`timezone
                         (pcase details
                           ((or `abbreviated `full)
                            (signal 'datetime-unsupported-timezone nil))
                           ((or `offset-localized-short `offset-localized-full
                                `offset-hh?mm `offset-hhmm `offset-hh:mm `offset-hhmm?ss `offset-hh:mm?:ss
                                `offset-hh?mm-or-z `offset-hhmm-or-z `offset-hh:mm-or-z `offset-hhmm?ss-or-z `offset-hh:mm?:ss-or-z
                                `offset-hhmm)
                            ;; FIXME: Use the most specific (not just the first) offset if there are several.
                            (when (or validating (null timezone-offset-part-indices))
                              (push (cons part-index details) timezone-offset-part-indices))
                            ;; t means that this is supposed to match numbers.
                            (cons (datetime--timezone-offset-matching-regexp details) t))
                           (_
                            (error "Unexpected timezone details `%s'" details))))
                        (_ (error "Unexpected value %s" type)))
                      details)))
            regexp-part-sources)
      (setf part-index (1+ part-index)))
    (setf regexp-part-sources (nreverse regexp-part-sources))
    ;; Not using `dolist' since we need access to the next entry.
    (while regexp-part-sources
      (let* ((entry   (pop regexp-part-sources))
             (regexp  (car entry))
             (details (cdr entry)))
        (push (if (integerp regexp)
                  (let ((run-together-numeric-groups (or last-part-was-numeric (integerp (caar regexp-part-sources)))))
                    (setf last-part-was-numeric t)
                    (if (= regexp 0)
                        ;; Magic number for years.
                        (cond ((or (memq details '(1 add-century-when-parsing)) (not (plist-get options :require-leading-zeros)))
                               (rx (1+ (any "0-9"))))
                              ((memq details '(2 always-two-digits))
                               (rx (any "0-9") (1+ (any "0-9"))))
                              (t
                               (format "[0-9]\\{%d\\}[0-9]+" (1- details))))
                      ;; REGEXP is really the maximum value of this one- or two-digit
                      ;; number.  However, we don't include it in the regexp in most of
                      ;; the cases (unlike in `datetime-matching-regexp').
                      (if (<= regexp 9)
                          (format (if run-together-numeric-groups "[1-%d]" "0*[1-%d]") regexp)
                        (cond ((and (= details 1) (plist-get options :accept-leading-space))
                               (format (if run-together-numeric-groups "[0-%d][0-9]" "[ 0-%d]?[0-9]") (/ regexp 10)))
                              ((>= regexp 20)
                               (format (if run-together-numeric-groups "[0-%d][0-9]" "0*[1-%d]?[0-9]") (/ regexp 10)))
                              (t
                               (if run-together-numeric-groups "[01][0-9]" "0*1?[0-9]"))))))
                (if (consp regexp)
                    (setf last-part-was-numeric (cdr regexp)
                          regexp                (car regexp))
                  (setf last-part-was-numeric nil))
                (cond ((vectorp regexp)
                       ;; A vector of options returned by `datetime-locale-field'.
                       (setq have-case-sensitive-parts t)
                       (regexp-opt (append regexp nil)))
                      (t
                       regexp)))
              regexp-parts)))
    (setq era-part-indices               (nreverse era-part-indices)
          year-part-indices              (nreverse year-part-indices)
          month-number-part-indices      (nreverse month-number-part-indices)
          month-name-part-indices        (nreverse month-name-part-indices)
          day-of-month-part-indices      (nreverse day-of-month-part-indices)
          am-pm-part-indices             (nreverse am-pm-part-indices)
          day-period-part-indices        (nreverse day-period-part-indices)
          hour-0-23-part-indices         (nreverse hour-0-23-part-indices)
          hour-1-24-part-indices         (nreverse hour-1-24-part-indices)
          hour-am-pm-1-12-part-indices   (nreverse hour-am-pm-1-12-part-indices)
          hour-am-pm-0-11-part-indices   (nreverse hour-am-pm-0-11-part-indices)
          regexp-parts                   (nreverse regexp-parts)
          minute-part-indices            (nreverse minute-part-indices)
          second-part-indices            (nreverse second-part-indices)
          second-fractional-part-indices (nreverse second-fractional-part-indices)
          timezone-offset-part-indices   (nreverse timezone-offset-part-indices))
    (unless validating
      (when month-number-part-indices
        (setq month-name-part-indices nil))
      (cond (hour-0-23-part-indices
             (setq hour-1-24-part-indices       nil
                   hour-am-pm-1-12-part-indices nil
                   hour-am-pm-0-11-part-indices nil))
            (hour-1-24-part-indices
             (setq hour-am-pm-1-12-part-indices nil
                   hour-am-pm-0-11-part-indices nil))
            (hour-am-pm-0-11-part-indices
             (setq hour-am-pm-1-12-part-indices nil))))
    (let* ((regexp-parts            regexp-parts)
           (substituting-indices-in (list era-part-indices
                                          year-part-indices month-number-part-indices month-name-part-indices day-of-month-part-indices
                                          am-pm-part-indices day-period-part-indices
                                          hour-0-23-part-indices hour-1-24-part-indices hour-am-pm-1-12-part-indices hour-am-pm-0-11-part-indices
                                          minute-part-indices second-part-indices second-fractional-part-indices timezone-offset-part-indices))
           (part-index              0)
           (group-index             1))
      (while regexp-parts
        (let ((substituting-indices-in-scan substituting-indices-in))
          (while substituting-indices-in-scan
            (let ((listed-element (car substituting-indices-in-scan)))
              (when listed-element
                ;; To handle alists.
                (unless (numberp (car listed-element))
                  (setq listed-element (car listed-element)))
                (when (eq part-index (car listed-element))
                  (setf (car listed-element)               group-index
                        (car substituting-indices-in-scan) (cdar substituting-indices-in-scan)
                        (car regexp-parts)                 (concat "\\(" (car regexp-parts) "\\)")
                        group-index                        (1+ group-index)
                        substituting-indices-in-scan       nil))))
            (setq substituting-indices-in-scan (cdr substituting-indices-in-scan))))
        (setq regexp-parts (cdr regexp-parts)
              part-index   (1+ part-index))))
    (let* ((downcased          (and have-case-sensitive-parts case-insensitive))
           (year-computation   (datetime--parser-computation pattern "year" validating nil nil
                                                             (year-part-indices datetime--parser-year-computation)))
           (constant-year      (unless year-computation
                                 (or (plist-get 'year defaults) 1970)))
           (era-correction     (when (and year-computation era-part-indices)
                                 (let ((only-one-part (null (cdr era-part-indices))))
                                   (datetime--parser-computation pattern "era" validating nil nil
                                                                 (era-part-indices (datetime--parser-era-correction locale nil downcased (not only-one-part)) t)))))
           (month-computation  (or (datetime--parser-computation pattern "month" validating 0 11
                                                                 (month-number-part-indices (datetime--parser-int-computation t))
                                                                 (month-name-part-indices (datetime--parser-string-index-computation locale nil downcased) t))
                                   (let ((default (plist-get 'month defaults)))
                                     (if default (1- default) 0))))
           (day-computation    (or (datetime--parser-computation pattern "day of month" validating 0 nil
                                                                 (day-of-month-part-indices (datetime--parser-int-computation t)))
                                   (let ((default (plist-get 'day defaults)))
                                     (if default (1- default) 0))))
           (am-pm-computation  (or (datetime--parser-computation pattern "am-pm" validating nil nil (am-pm-part-indices (datetime--parser-string-if-computation locale :am-pm downcased 0 12) t))
                                   (plist-get 'am-pm 0)
                                   0))
           (_day-period-computation)  ;; FIXME: Write; currently not validated for consistency with actual time.
           (hour-computation   (or (datetime--parser-computation pattern "hour" validating nil 23
                                                                 (hour-0-23-part-indices datetime--parser-int-computation)
                                                                 (hour-1-24-part-indices datetime--parser-hour-1-24-computation)
                                                                 (hour-am-pm-1-12-part-indices (datetime--parser-hour-am-pm-computation am-pm-computation t))
                                                                 (hour-am-pm-0-11-part-indices (datetime--parser-hour-am-pm-computation am-pm-computation nil)))
                                   (plist-get 'hour defaults) 0))
           (minute-computation (or (datetime--parser-computation pattern "minute" validating nil 59
                                                                 (minute-part-indices datetime--parser-int-computation))
                                   (plist-get 'minute defaults) 0))
           (second-computation (or (datetime--parser-computation pattern "second" validating nil 59
                                                                 (second-part-indices datetime--parser-int-computation))
                                   (plist-get 'second defaults) 0))
           (timezone-offset-computation
                               (datetime--parser-computation pattern "timezone offset" validating nil nil
                                                             (timezone-offset-part-indices datetime--parser-timezone-offset-computation)))
           (parser            `(+ ,@(when (or year-computation
                                              (not (memq constant-year     '(nil 1970)))
                                              (not (memq month-computation '(nil 0)))
                                              (not (memq day-computation   '(nil 0))))
                                      ;; FIXME: Optimize for constant year.
                                      `((* (let ((year  ,(or year-computation constant-year))
                                                 (month ,month-computation))
                                             ,@(when era-correction
                                                 `(,era-correction))
                                             (let ((year-mod-400 (mod year 400)))
                                               (+ (* (/ (- year year-mod-400) 400) ,datetime--gregorian-days-in-400-years)
                                                  (aref ,datetime--gregorian-cumulative-year-days year-mod-400)
                                                  ,(- datetime--gregorian-days-in-1970-years)
                                                  (aref ,datetime--gregorian-cumulative-month-days month)
                                                  (if (and (>= month 2) (datetime--gregorian-leap-year-mod-400-p year-mod-400)) 1 0)
                                                  ,(if validating
                                                       `(let ((day ,day-computation))
                                                          (unless (and (<= 0 day)
                                                                       (or (< day (aref ,datetime--gregorian-month-days month))
                                                                           (and (= month 1) (= day 28)
                                                                                (datetime--gregorian-leap-year-mod-400-p year-mod-400))))
                                                            (signal 'datetime-invalid-string (list string ,pattern "day is out of range")))
                                                          day)
                                                     day-computation))))
                                           ,(* 24 60 60))))
                                  (* ,hour-computation ,(* 60 60))
                                  (* ,minute-computation 60)
                                  ,second-computation
                                  ,@(when timezone-offset-computation
                                      `((- ,timezone-offset-computation)))
                                  ,@(when second-fractional-part-indices
                                      `((/ (string-to-number (match-string ,(caar second-fractional-part-indices) string))
                                           ,(cdar second-fractional-part-indices)))))))
      ;; Apply the timezone from `options', but only if one is not specified in the argument.
      (unless timezone-offset-part-indices
        (pcase timezone-data
          (`(,constant-offset)
           (unless (= constant-offset 0)
             (setq parser `(- ,parser ,constant-offset))))
          (_
           (setq parser `(datetime--convert-from-utc-float ,parser ,(datetime--macroexp-quote timezone-data))))))
      (setq parser `(save-match-data
                      (if (string-match ,(concat "^" (apply #'concat regexp-parts) "$") ,(if downcased `(downcase string) 'string))
                          ,parser
                        (signal 'datetime-invalid-string (list string ,pattern "doesn't match the pattern")))))
      (when have-case-sensitive-parts
        (setq parser `(let ((case-fold-search ,case-insensitive))
                        ,parser)))
      (setq parser `(lambda (string) ,parser))
      (unless (plist-get options :debug)
        (setf parser (datetime--do-byte-compile parser "ths generated parser")))
      parser)))

(defun datetime--parser-year-computation (argument)
  (pcase (cdr argument)
    (`add-century-when-parsing `(let ((year ,(datetime--parser-int-computation (car argument))))
                                  (if (= (length (match-string ,(car argument) string)) 2)
                                      (+ year 2000)
                                    year)))
    (`always-two-digits        `(+ ,(datetime--parser-int-computation (car argument)) 2000))
    (_                         (datetime--parser-int-computation (car argument)))))

(defun datetime--parser-era-correction (argument locale field downcased can-be-called-multiple-times)
  (datetime--parser-string-if-computation argument locale (or field (cdr argument)) downcased
                                          (if can-be-called-multiple-times
                                              `(if (>= year 0) (setq year (- 1 year)) year)
                                            `(setq year (- 1 year)))
                                          nil))

(defun datetime--parser-hour-1-24-computation (argument)
  `(let ((hour-1-24 ,(datetime--parser-int-computation argument)))
     (if (< hour-1-24 24) hour-1-24 0)))

(defun datetime--parser-hour-am-pm-computation (argument am-pm-computation expect-1-12)
  (if expect-1-12
      `(let ((hour-am-pm-1-12 ,(datetime--parser-int-computation argument)))
         (+ (if (< hour-am-pm-1-12 12) hour-am-pm-1-12 0) ,am-pm-computation))
    `(+ ,(datetime--parser-int-computation argument) ,am-pm-computation)))

(defun datetime--parser-int-computation (argument &optional off-by-one)
  (let ((computation `(string-to-number (match-string ,(if (consp argument) (car argument) argument) string))))
    (if off-by-one
        `(1- ,computation)
      computation)))

(defun datetime--parser-string-index-computation (argument locale field downcased)
  (let ((strings (datetime-locale-field locale (or field (cdr argument)))))
    (when downcased
      (setq strings (vconcat (mapcar #'downcase (append strings nil)))))
    `(let ((match (match-string ,(if (consp argument) (car argument) argument) string))
           (n     0))
       (while (not (string= match (aref ,strings n)))
         (setq n (1+ n)))
       n)))

(defun datetime--parser-string-if-computation (argument locale field downcased if-first-form if-second-form)
  (let ((strings (datetime-locale-field locale field)))
    (unless (= (length strings) 2)
      (error "Must be called only for two-string fields, called for `%S' instead" strings))
    `(if (string= (match-string ,(if (consp argument) (car argument) argument) string)
                  ,(if downcased (downcase (aref strings 0)) (aref strings 0)))
         ,if-first-form
       ,if-second-form)))

;; Where ARGUMENT is expected to be `(STRING-INDEX . OFFSET-TYPE)'.
(defun datetime--parser-timezone-offset-computation (argument)
  (let* ((type             (cdr argument))
         (localized        (memq type '(offset-localized-short offset-localized-full)))
         ;; For "localized" offset types the prefix is "GMT".
         (prefix-length    (if localized 3 0))
         (separator-length (if (or localized (memq type '(offset-hh:mm offset-hh:mm?:ss offset-hh:mm-or-z offset-hh:mm?:ss-or-z))) 1 0))
         (has-seconds      (or localized (memq type '(offset-hhmm?ss offset-hh:mm?:ss offset-hhmm?ss-or-z offset-hh:mm?:ss-or-z))))
         (hour-form        `(* (string-to-number (substring offset-string
                                                            ,(+ prefix-length 1)
                                                            ,@(if (eq type 'offset-localized-short) '(hour-end) `(,(+ prefix-length 1 2)))))
                               ,(* 60 60)))
         (minutes-form     `(* (string-to-number (substring offset-string
                                                            ,(if (eq type 'offset-localized-short)
                                                                 '(1+ hour-end)
                                                               (+ prefix-length 1 2 separator-length))
                                                            ,@(when has-seconds
                                                                `(,(if (eq type 'offset-localized-short)
                                                                       '(+ hour-end 3)
                                                                     (+ prefix-length 1 2 separator-length 2))))))
                               60))
         (seconds-form     (when has-seconds
                             `(string-to-number (substring offset-string
                                                           ,(if (eq type 'offset-localized-short)
                                                                `(+ hour-end 4)
                                                              (+ prefix-length 1 2 separator-length 2 separator-length))))))
         (form             `(+ ,hour-form
                               ,(if (eq type 'offset-localized-short)
                                    `(if (> (length offset-string) 6) ,minutes-form 0)
                                  minutes-form)
                               ,@(when has-seconds
                                   ;; Condition works for `offset-localized-short' too.
                                   `((if (> (length offset-string) ,(+ prefix-length 1 2 separator-length 2)) ,seconds-form 0))))))
    ;; For all other types `hour-end' is constant.
    (when (eq type 'offset-localized-short)
      (setf form `(let ((hour-end (if (or (= (length offset-string) 5) (eq (aref offset-string 5) ?:)) 5 6))) ,form)))
    (setf form `(let ((offset ,form)) (if (= (aref offset-string ,prefix-length) ?+) offset (- offset))))
    (unless (memq type '(offset-hhmm offset-hh?mm offset-hhmm?ss offset-hh:mm offset-hh:mm?:ss))
      ;; For all types, there is a fixed length of the match that implies zero offset
      ;; (either for "Z" or "GMT") and cannot happen in any other case.
      (setf form `(if (= (length offset-string) ,(if localized 3 1)) 0 ,form)))
    `(let ((offset-string (match-string ,(car argument) string)))
       ,form)))

;; Pretty similar to `datetime--convert-to-utc-float', but not quite.
(defun datetime--convert-from-utc-float (date-time timezone-data)
  (let ((year-offset          (floor (/ (- date-time (car timezone-data)) datetime--average-seconds-in-year)))
        (all-year-transitions (nth 1 timezone-data)))
    (if (>= year-offset 0)
        (let* ((year-transitions (or (when (< year-offset (length all-year-transitions))
                                       (aref all-year-transitions year-offset))
                                     (datetime--calculate-year-transitions timezone-data year-offset)))
               (offset           (pop year-transitions)))
          (when year-transitions
            (let ((offset-in-year (floor (- date-time (car timezone-data) (* year-offset datetime--average-seconds-in-year)))))
              (while (and (>= (- offset-in-year offset) (car year-transitions))
                          (setq offset           (cadr year-transitions)
                                year-transitions (cddr year-transitions))))))
          (- date-time offset))
      ;; Offset before the very first transition.
      (- date-time (car (aref all-year-transitions 0))))))


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
    defaults to English, even if system locale is different.  You
    can use special value \\='system to let the library find it.

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
         (locale         (datetime--get-locale options))
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
                          (`era                     (datetime-locale-field locale (datetime--era-field details)))
                          ((or `year `year-for-week)
                           (cond ((and (plist-get options :only-4-digit-years) (eq details 4))
                                  (rx (= 4 (any "0-9"))))
                                 ((or (memq details '(1 add-century-when-parsing)) (not (plist-get options :require-leading-zeros)))
                                  (rx (1+ (any "0-9"))))
                                 ((memq details '(2 always-two-digits))
                                  (rx (any "0-9") (1+ (any "0-9"))))
                                 (t
                                  (format "[0-9]\\{%d\\}[0-9]+" (1- details)))))
                          (`month                   12)
                          (`month-context-name      (datetime-locale-field locale (datetime--month-context-name-field details)))
                          (`month-standalone-name   (datetime-locale-field locale (datetime--month-standalone-name-field details)))
                          (`week-in-year            53)
                          (`week-in-month            5)
                          (`day-in-month            31)
                          (`weekday-in-month         5)
                          (`weekday                  7)
                          (`weekday-context-name    (datetime-locale-field locale (datetime--weekday-context-name-field details)))
                          (`weekday-standalone-name (datetime-locale-field locale (datetime--weekday-standalone-name-field details)))
                          (`am-pm                   (datetime-locale-field locale :am-pm))
                          (`day-period              (let ((day-period-data (datetime-locale-field locale :day-periods)))
                                                      (or (plist-get day-period-data details) (plist-get day-period-data :short))))
                          (`hour-0-23               23)
                          (`hour-1-24               24)
                          (`hour-am-pm-0-11         11)
                          (`hour-am-pm-1-12         12)
                          (`minute                  59)
                          (`second                  59)
                          (`decimal-separator       (rx (or "." ",")))
                          (`second-fractional       (apply #'concat (make-list details (rx (any "0-9")))))
                          (`timezone
                           (pcase details
                             ((or `abbreviated `full)
                              (signal 'datetime-unsupported-timezone nil))
                             ((or `offset-localized-short `offset-localized-full
                                  `offset-hh?mm `offset-hhmm `offset-hh:mm `offset-hhmm?ss `offset-hh:mm?:ss
                                  `offset-hh?mm-or-z `offset-hhmm-or-z `offset-hh:mm-or-z `offset-hhmm?ss-or-z `offset-hh:mm?:ss-or-z
                                  `offset-hhmm)
                              (datetime--timezone-offset-matching-regexp details))
                             (_
                              (error "Unexpected timezone details `%s'" details))))
                          (_ (error "Unexpected value %s" type)))))
          (push (cond ((integerp regexp)
                       ;; REGEXP is really the maximum value of this one- or two-digit
                       ;; number.
                       (if (<= regexp 9)
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
                                "0*1?[0-9]"))))
                      ((vectorp regexp)
                       ;; A vector of options returned by `datetime-locale-field'.
                       (regexp-opt (append regexp nil)))
                      (t
                       regexp))
                regexp-parts))))
    (apply #'concat (nreverse regexp-parts))))


(defun datetime-recode-pattern (from to pattern &rest options)
  "Recode PATTERN between two supported types.
As a special case, either of FROM and TO can be set to \\='parsed.
This is useful as a speed optimization in a few cases where you
perform several transformations on the same pattern.

Options can be a list of the following keyword arguments:

  :any-decimal-separator

    Treat a decimal dot or comma in pattern between seconds and
    milli- or microseconds (etc.) as a placeholder for _any_
    decimal separator and also accept commas in this place.  This
    only works if TO is \\='parsed.

  :second-fractional-extension

    Obsolete since 0.6.6: this is now always enabled."
  (datetime--format-pattern to (datetime--parse-pattern from pattern options) options))


;; Arguments are expected to be atoms; however, PART-TYPES may also consist of full cons-cells.
(defmacro datetime--pattern-includes-p (type pattern options &rest part-types)
  (declare (indent 3))
  (let ((only-atoms (not (memq nil (mapcar #'atom part-types)))))
    `(let ((parts (datetime--parse-pattern ,type ,pattern ,options))
           includes)
       (while parts
         (let ((part (car parts)))
           (if (and (consp part) ,(if only-atoms
                                      (if (= (length part-types) 1)
                                          `(eq (car part) ',(car part-types))
                                        `(memq (car part) ',part-types))
                                    (if (= (length part-types) 1)
                                        `(equal part ',(car part-types))
                                      `(member part ',part-types))))
               (setq parts    nil
                     includes t)
             (setq parts (cdr parts)))))
       includes)))

(defun datetime-pattern-locale-dependent-p (type pattern &rest options)
  "Determine if PATTERN includes any locale-based parts.
In other words, return non-nil if PATTERN includes any textual
names.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options era month-context-name month-standalone-name weekday-context-name weekday-standalone-name am-pm))

(defun datetime-pattern-includes-date-p (type pattern &rest options)
  "Determine if PATTERN includes any date parts.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options
                                era year year-for-week month month-context-name month-standalone-name week-in-year week-in-month
                                day-in-year day-in-month weekday-in-month weekday weekday-context-name weekday-standalone-name))

(defun datetime-pattern-includes-time-p (type pattern &rest options)
  "Determine if PATTERN includes any time parts.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options
                                am-pm day-period hour-0-23 hour-1-24 hour-am-pm-0-11 hour-am-pm-1-12 minute second second-fractional))

(defun datetime-pattern-includes-era-p (type pattern &rest options)
  "Determine if PATTERN includes the date era.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options era))

(defun datetime-pattern-includes-year-p (type pattern &rest options)
  "Determine if PATTERN includes the year.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options year year-for-week))

(defun datetime-pattern-includes-month-p (type pattern &rest options)
  "Determine if PATTERN includes the month.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options month month-context-name month-standalone-name))

(defun datetime-pattern-includes-week-p (type pattern &rest options)
  "Determine if PATTERN includes the week.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options week-in-year week-in-month))

(defun datetime-pattern-includes-day-p (type pattern &rest options)
  "Determine if PATTERN includes the day.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options day-in-year day-in-month))

(defun datetime-pattern-includes-weekday-p (type pattern &rest options)
  "Determine if PATTERN includes the weekday.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options weekday-in-month weekday weekday-context-name weekday-standalone-name))

(defun datetime-pattern-includes-hour-p (type pattern &rest options)
  "Determine if PATTERN includes hours.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options hour-0-23 hour-1-24 hour-am-pm-0-11 hour-am-pm-1-12))

(defun datetime-pattern-includes-minute-p (type pattern &rest options)
  "Determine if PATTERN includes minutes.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options minute))

(defun datetime-pattern-includes-second-p (type pattern &rest options)
  "Determine if PATTERN includes seconds.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options second))

(defun datetime-pattern-includes-second-fractionals-p (type pattern &rest options)
  "Determine if PATTERN includes fractions of seconds.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options second-fractional))

(define-obsolete-function-alias 'datetime-pattern-includes-millisecond-p #'datetime-pattern-includes-second-fractionals-p "0.6.1")

(defun datetime-pattern-num-second-fractionals (type pattern &rest options)
  "Determine number of second fractional digits in the PATTERN.
E.g. if PATTERN includes milliseconds, result will be 3, for
microseconds it will be 6 and so on.  OPTIONS are passed to
`datetime-recode-pattern'."
  (let ((parts           (datetime--parse-pattern type pattern options))
        (num-fractionals 0))
    (while parts
      (let ((part (pop parts)))
        (when (consp part)
          (pcase (car part)
            (`second-fractional (setq num-fractionals (max num-fractionals (cdr part))))))))
    num-fractionals))

(defun datetime-pattern-includes-timezone-p (type pattern &rest options)
  "Determine if PATTERN includes timezone.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options timezone))

(defun datetime-pattern-includes-timezone-name-p (type pattern &rest options)
  "Determine if PATTERN includes timezone name.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options (timezone . full) (timezone . abbreviated)))

(defun datetime-pattern-includes-timezone-offset-p (type pattern &rest options)
  "Determine if PATTERN includes timezone offset.

OPTIONS are passed to `datetime-recode-pattern'.  Currently no
options can affect result of this function."
  (datetime--pattern-includes-p type pattern options
    (timezone . offset-hhmm) (timezone . offset-hh?mm) (timezone . offset-hhmm?ss) (timezone . offset-hh:mm) (timezone . offset-hh:mm?:ss)
    (timezone . offset-hhmm-or-z) (timezone . offset-hh?mm-or-z) (timezone . offset-hhmm?ss-or-z) (timezone . offset-hh:mm-or-z) (timezone . offset-hh:mm?:ss-or-z)
    (timezone . offset-localized-short) (timezone . offset-localized-full)))


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
separated by a space, for quite a few locales it is different."
  (unless date-variant
    (setq date-variant :medium))
  (unless time-variant
    (setq time-variant date-variant))
  ;; Some ugly parsing of compressed data follows; see the harvesting tool.
  (let* ((date-part              (datetime-locale-date-pattern locale date-variant))
         (time-part              (datetime-locale-time-pattern locale time-variant))
         (date-time-pattern-rule (or (datetime-locale-field locale :date-time-pattern-rule) '(t . " ")))
         (date-part-first        (car date-time-pattern-rule))
         constant-strings)
    (if (consp date-part-first)
        (let ((style-data (cdr (assoc (list date-variant time-variant) date-time-pattern-rule))))
          (setq date-part-first  (car style-data)
                constant-strings (cdr style-data)))
      (unless (stringp (setq constant-strings (cdr date-time-pattern-rule)))
        (setq constant-strings (cdr (assoc (list date-variant time-variant) constant-strings)))))
    (if (stringp constant-strings)
        (if date-part-first
            (concat date-part constant-strings time-part)
          (concat time-part constant-strings date-part))
      (if date-part-first
          (concat (nth 0 constant-strings) date-part (nth 1 constant-strings) time-part (nth 2 constant-strings))
        (concat (nth 0 constant-strings) time-part (nth 1 constant-strings) date-part (nth 2 constant-strings))))))


(defconst datetime--english-eras  ["BC" "AD"])
(defconst datetime--english-am-pm ["AM" "PM"])

(defsubst datetime--do-get-locale-field (locale-data field)
  (or (plist-get locale-data field)
      ;; See `datetime--locale-extmap' for description of fallbacks.
      (pcase field
        ((or :eras-full :eras-narrow) (plist-get locale-data :eras-short))
        (:month-standalone-short      (plist-get locale-data :month-context-short))
        (:month-standalone-full       (plist-get locale-data :month-context-full))
        (:month-standalone-narrow     (plist-get locale-data :month-context-narrow))
        (:weekday-standalone-short    (plist-get locale-data :weekday-context-short))
        (:weekday-standalone-full     (plist-get locale-data :weekday-context-full))
        (:weekday-standalone-narrow   (plist-get locale-data :weekday-context-narrow)))))

(defun datetime-locale-field (locale field)
  "Get a FIELD of data for the LOCALE.
Supported fields:

  :decimal-separator         (a character, usually dot or comma)
  :eras-short                (alias: :eras)
  :eras-full
  :eras-narrow
  :month-context-short       (alias: :month-context-abbr)
  :month-context-full        (alias: :month-context-names)
  :month-context-narrow
  :month-standalone-short    (alias: :month-standalone-abbr)
  :month-standalone-full     (alias: :month-standalone-names)
  :month-standalone-narrow
  :weekday-context-short     (alias: :weekday-context-abbr)
  :weekday-context-full      (alias: :weekday-context-names)
  :weekday-context-narrow
  :weekday-standalone-short  (alias: :weekday-standalone-abbr)
  :weekday-standalone-full   (alias: :weekday-standalone-names)
  :weekday-standalone-narrow
  :first-day-of-week         (a number, with 0 standing for Monday)
  :am-pm

Unless something else is stated explicitly, values are arrays of
strings.  Lengths of arrays for the same field are the same
across all locales (12 months, 7 weekdays etc.)."
  ;; Additionally `:day-periods', `:date-patterns', `:time-patterns' and
  ;; `:date-time-pattern-rule' are supported for internal use.
  (let ((data (extmap-get datetime--locale-extmap locale t)))
    ;; Resolve aliases, mostly for compatibility with older versions.
    (setf field (pcase field
                  (:eras                     :eras-short)
                  (:month-context-abbr       :month-context-short)
                  (:month-context-names      :month-context-full)
                  (:month-standalone-abbr    :month-standalone-short)
                  (:month-standalone-names   :month-standalone-full)
                  (:weekday-context-abbr     :weekday-context-short)
                  (:weekday-context-names    :weekday-context-full)
                  (:weekday-standalone-abbr  :weekday-standalone-short)
                  (:weekday-standalone-names :weekday-standalone-full)
                  (_                         field)))
    (or (datetime--do-get-locale-field data field)
        (let ((parent (plist-get data :parent)))
          (when parent
            (datetime--do-get-locale-field (extmap-get datetime--locale-extmap parent) field)))
        (pcase field
          (:decimal-separator                       ?.)
          ((or :eras-short :eras-full :eras-narrow) datetime--english-eras)
          (:first-day-of-week                       6)  ; See comments in `HarvestData.java'.
          (:am-pm                                   datetime--english-am-pm)))))

(defun datetime--era-field (details)
  (pcase-exhaustive details
    (`short  :eras-short)
    (`full   :eras-full)
    (`narrow :eras-narrow)))

(defun datetime--month-context-name-field (details)
  (pcase-exhaustive details
    (`short  :month-context-short)
    (`full   :month-context-full)
    (`narrow :month-context-narrow)))

(defun datetime--month-standalone-name-field (details)
  (pcase-exhaustive details
    (`short  :month-standalone-short)
    (`full   :month-standalone-full)
    (`narrow :month-standalone-narrow)))

(defun datetime--weekday-context-name-field (details)
  (pcase-exhaustive details
    (`short  :weekday-context-short)
    (`full   :weekday-context-full)
    (`narrow :weekday-context-narrow)))

(defun datetime--weekday-standalone-name-field (details)
  (pcase-exhaustive details
    (`short  :weekday-standalone-short)
    (`full   :weekday-standalone-full)
    (`narrow :weekday-standalone-narrow)))

(defun datetime-locale-timezone-name (locale timezone dst &optional full)
  "Get name of TIMEZONE in given LOCALE.
For timezones that don't have daylight saving time, parameter DST
is ignored.

By default, abbreviated name (like \"UTC\") suitable for use in
date-time strings is returned.  However, if FULL is non-nil, a
non-abbreviated name (e.g. \"Coordinated Universal Time\") is
returned instead."
  ;; See `datetime--timezone-name-extmap' for description of fallbacks.
  (let ((names (plist-get (extmap-get datetime--timezone-name-extmap locale t) timezone)))
    (cond ((vectorp names)
           (aref names (if (= (length names) 4)
                           (+ (if full 2 0) (if dst 1 0))
                         (if full 1 0))))
          ((consp names)
           (if full
               (if dst (cdr names) (car names))
             (datetime-locale-timezone-name 'en timezone dst)))
          ((stringp names)
           (if full
               names
             (datetime-locale-timezone-name 'en timezone nil)))
          (t
           (let ((locale-data (extmap-get datetime--locale-extmap locale t)))
             (when locale-data
               (datetime-locale-timezone-name (or (plist-get locale-data :parent) 'en) timezone dst full)))))))


(defun datetime-locale-database-version ()
  "Return locale database version, a simple integer.
This version will be incremented each time locale database of the
package is updated.  It can be used e.g. to invalidate caches you
create based on locales `datetime' knows about.

Note that this database doesn't include timezone names.  See
`datetime-timezone-name-database-version'."
  9)

(defun datetime-timezone-database-version ()
  "Return timezone database version, a simple integer.
This version will be incremented each time timezone database of the
package is updated.  It can be used e.g. to invalidate caches you
create based on timezones `datetime' knows about and their rules.

Locale-specific timezone names are contained in a different
database.  See `datetime-timezone-name-database-version'."
  10)

(defun datetime-timezone-name-database-version ()
  "Return timezone name database version, a simple integer.
This version will be incremented each time timezone name database
of the package is updated.  It can be used e.g. to invalidate
caches.

This database includes only locale-specific timezone names.
Other locale-specific data as well as locale-independent data
about timezones is contained in different databases.  See
`datetime-locale-database-version' and
`datetime-timezone-database-version'."
  5)


(provide 'datetime)

;;; datetime.el ends here
