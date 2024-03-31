;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2024 Paul Pogonyshev

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


(require 'test/base)


(defun datetime--~= (our-result java-result &optional epsilon as-string)
  (cond ((and our-result java-result)
         (let ((error (abs (- our-result java-result))))
           (if (<= error (or epsilon 0.0000001))
               t
             (message "Error of %s" (funcall (datetime-float-formatter 'java "HH:mm:ss.SSSSSS") error))
             nil)))
        (our-result
         ;; Apparently there is a bug in Java in that `x' cannot parse offsets between
         ;; +0001 and +0059 (all other offsets are fine).  Tried reporting it, only for
         ;; `bugreport.java.com' to die on me, so fuck it.
         (if (and (string-match-p "[^x]x$" datetime--test-pattern)
                  as-string (string-match-p "\\+00\\(0[1-9]\\|[1-5][0-9]\\)$" as-string))
             t
           (message "Successfully parsed by us, but not by Java")
           nil))
        (java-result
         (message "Successfully parsed by Java, but not by us")
         nil)
        (t
         t)))

(defun datetime--test-parser (as-strings)
  (unless (listp as-strings)
    (setq as-strings (list as-strings)))
  (let ((parsed (datetime--test 'parse as-strings)))
    (while as-strings
      (let ((as-string (pop as-strings))
            (time      (pop parsed)))
        (eval `(should (progn ',datetime--test-timezone ',datetime--test-locale ,datetime--test-pattern ,as-string
                              (datetime--~= ,(funcall datetime--test-parser as-string) ,time nil ,as-string)))
              t)))))

(defun datetime--test-parser-around-transition (time)
  (datetime--test-parser (datetime--test 'format (list time
                                                       (+ time  0.5) (- time  0.5)      ; half a second
                                                       (+ time   30) (- time   30)      ; half a minute
                                                       (+ time 1800) (- time 1800)      ; half an hour
                                                       (+ time 3600) (- time 3600)      ; one hour
                                                       (+ time 7200) (- time 7200)))))  ; two hours


(ert-deftest datetime-parsing-now ()
  (datetime--test-set-up-parser 'UTC 'en "yyyy-MM-dd HH:mm:ss.SSS"
    (datetime--test-parser (datetime--test 'format (float-time)))))

(ert-deftest datetime-parsing-now-standard-formats ()
  (let ((now (float-time)))
    (dolist (locale (datetime-list-locales t))
      (dolist (variant '(:short :medium :long :full))
        (let ((pattern (datetime-locale-date-time-pattern locale variant)))
          (unless (datetime-pattern-includes-timezone-p 'java pattern)
            (datetime--test-set-up-parser 'UTC locale pattern
              (datetime--test-parser (datetime--test 'format now)))))))))

(ert-deftest datetime-parsing-various-timestamps-1 ()
  (datetime--test-set-up-parser 'UTC 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-parser (datetime--test 'format (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000))))))

(ert-deftest datetime-parsing-various-timestamps-with-fixed-offset-timezone-1 ()
  (datetime--test-set-up-parser 'Etc/GMT+1 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-parser (datetime--test 'format (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000))))))

(ert-deftest datetime-parsing-various-timestamps-with-shifting-timezone-1 ()
  (datetime--test-set-up-parser 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-parser (datetime--test 'format (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000))))))

(ert-deftest datetime-parsing-various-timestamps-with-shifting-timezone-2 ()
  (datetime--test-set-up-parser 'America/Anchorage 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-parser (datetime--test 'format (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000))))))

(ert-deftest datetime-parsing-various-timestamps-with-shifting-timezone-3 ()
  (datetime--test-set-up-parser 'Australia/Hobart 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-parser (datetime--test 'format (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000))))))

(ert-deftest datetime-parsing-text-1 ()
  (datetime--test-set-up-parser 'UTC 'en "'on' EEEE 'the' d MMMM 'of' yyyy G, 'at' h:mm:ss a"
    ;; Roughly from 1200 BC till 5100 AD with 6 and a half year step.
    (datetime--test-parser (datetime--test 'format (mapcar (lambda (k) (* k 200000000.123)) (number-sequence -500 500))))))

(ert-deftest datetime-parsing-around-offset-transition-1 ()
  (datetime--test-set-up-parser 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; First historical transition.
    (datetime--test-parser-around-transition -2177452800)))

(ert-deftest datetime-parsing-around-offset-transition-2 ()
  (datetime--test-set-up-parser 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2010-03-25.
    (datetime--test-parser-around-transition 1269738000)))

(ert-deftest datetime-parsing-around-offset-transition-3 ()
  (datetime--test-set-up-parser 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Future transition on 2480-10-27 (according to the rules as of 2018).
    (datetime--test-parser-around-transition 16119997200)))

(ert-deftest datetime-parsing-around-offset-transition-4 ()
  (datetime--test-set-up-parser 'America/Anchorage 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2009-03-08.
    (datetime--test-parser-around-transition 1236510000)))

(ert-deftest datetime-parsing-around-offset-transition-5 ()
  (datetime--test-set-up-parser 'Australia/Hobart 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2014-10-05.
    (datetime--test-parser-around-transition 1412438400)))

(ert-deftest datetime-parser-validating-1 ()
  (datetime--test-set-up-parser 'UTC 'en "yyyy-MM-dd HH:mm:ss.SSS"
    (should-error (funcall datetime--test-parser "lol") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-00-01 00:00:00.000") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-13-01 00:00:00.000") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-01-00 00:00:00.000") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-01-32 00:00:00.000") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-01-01 24:00:00.000") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-01-01 00:60:00.000") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-01-01 00:00:60.000") :type 'datetime-invalid-string)))

(ert-deftest datetime-parser-validating-excessive-patterns ()
  (datetime--test-set-up-parser 'UTC 'en "dd 'of' MMMM '(month' M')'"
    (datetime--test-parser "12 of March (month 3)")
    (should-error (funcall datetime--test-parser "12 of March (month 1)") :type 'datetime-invalid-string)))


(ert-deftest datetime-parsing-future-timestamp-1 ()
  ;; Real failure: would cause an exception on certain timezones
  ;; without transition rules (e.g. `Africa/Algiers').
  (dolist (timezone (datetime-list-timezones))
    (datetime--test-set-up-parser timezone 'en "yyyy-MM-dd HH:mm:ss"
      (datetime--test-parser '("2100-01-01 00:00:00")))))


(ert-deftest datetime-parsing-run-together-1 ()
  ;; Real failure: run-together digit groups in the pattern would confuse the parser, see
  ;; https://github.com/doublep/datetime/issues/6.
  (dolist (timezone (datetime-list-timezones))
    (datetime--test-set-up-parser timezone 'en "yyyyMMdd"
      (datetime--test-parser '("20220506")))))

(ert-deftest datetime-parsing-run-together-2 ()
  (dolist (timezone (datetime-list-timezones))
    (datetime--test-set-up-parser timezone 'en "yyyyMMddHHmmss"
      (datetime--test-parser '("20220506123000")))))


(ert-deftest datetime-parsing-timezone-offset-1 ()
  (dolist (offset-format-specifier datetime--test-offset-format-specifiers)
    (let ((pattern (format "yyyy-MM-dd HH:mm:ss%s" offset-format-specifier)))
      ;; Parser should not need a fixed timezone, instead it will get the offset from the
      ;; argument upon call.
      (datetime--test-set-up-parser nil 'en pattern
        (dolist (timezone (datetime-list-timezones))
          (datetime--test-set-up-formatter timezone 'en pattern
            (datetime--test-parser (list (funcall datetime--test-formatter 1694863500)
                                         ;; For ancient times many timezones yield offsets that
                                         ;; include seconds.  Make sure we can parse those too.
                                         (funcall datetime--test-formatter -3000000000)))))))))


(ert-deftest datetime-parsing-era-specified-several-times ()
  (datetime--test-set-up-parser 'UTC 'en "yyyy G+G"
    (datetime--test-parser (list "2000 AD+AD"))
    ;; Used to die with "inconsistent era", even though it is consistent.  Used to invert
    ;; year two times, resulting in wrong return value.
    (datetime--test-parser (list "2000 BC+BC")))
  (datetime--test-set-up-parser 'UTC 'en "yyyy G / GGGG / GGGGG"
    (datetime--test-parser (list "2000 AD / Anno Domini / A"))
    (datetime--test-parser (list "2000 BC / Before Christ / B"))))


(provide 'test/parse)
