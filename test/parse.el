;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Paul Pogonyshev

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

(defvar datetime--test-parser nil)


(defun datetime--~= (a b &optional epsilon)
  (when (and a b)
    (unless (<= (abs (- a b)) (or epsilon 0.0000001))
      (message "Error of %s" (funcall (datetime-float-formatter 'java "HH:mm:ss.SSSSSS" :second-fractional-extension t) (abs (- a b))))))
  (if (and a b)
      (<= (abs (- a b)) (or epsilon 0.0000001))
    (not (or a b))))

(defmacro datetime--test-set-up-parser (timezone locale pattern &rest body)
  (declare (debug (form form form body))
           (indent 3))
  `(datetime--test-set-up ,timezone ,locale ,pattern
     (let ((datetime--test-parser (datetime-parser-to-float 'java datetime--test-pattern :timezone datetime--test-timezone :locale datetime--test-locale)))
       ,@body)))

(defun datetime--test-parser (as-strings)
  (unless (listp as-strings)
    (setq as-strings (list as-strings)))
  (let ((parsed (datetime--test 'parse as-strings)))
    (while as-strings
      (let ((as-string (pop as-strings))
            (time      (pop parsed)))
        (eval `(should (progn ',datetime--test-timezone ',datetime--test-locale ,datetime--test-pattern ,as-string
                              (datetime--~= ,(funcall datetime--test-parser as-string) ,time))))))))

(defun datetime--test-parser-around-transition (time)
  (datetime--test-parser (datetime--test 'format (list time
                                                       (+ time  0.5) (- time  0.5)      ; half a second
                                                       (+ time   30) (- time   30)      ; half a minute
                                                       (+ time 1800) (- time 1800)      ; half an hour
                                                       (+ time 3600) (- time 3600)      ; one hour
                                                       (+ time 7200) (- time 7200)))))  ; two hours


(ert-deftest datetime-test-parsing-now ()
  (datetime--test-set-up-parser 'UTC 'en "yyyy-MM-dd HH:mm:ss.SSS"
    (datetime--test-parser (datetime--test 'format (float-time)))))

(ert-deftest datetime-test-parsing-now-standard-formats ()
  (let ((now (float-time)))
    (dolist (locale (datetime-list-locales t))
      (dolist (variant '(:short :medium :long :full))
        (let ((pattern (datetime-locale-date-time-pattern locale variant)))
          (unless (datetime-pattern-includes-timezone-p 'java pattern)
            (datetime--test-set-up-parser 'UTC locale pattern
              (datetime--test-parser (datetime--test 'format now)))))))))

(ert-deftest datetime-test-parsing-various-timestamps-1 ()
  (datetime--test-set-up-parser 'UTC 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-parser (datetime--test 'format (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000))))))

(ert-deftest datetime-test-parsing-various-timestamps-with-fixed-offset-timezone-1 ()
  (datetime--test-set-up-parser 'Etc/GMT+1 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-parser (datetime--test 'format (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000))))))

(ert-deftest datetime-test-parsing-various-timestamps-with-shifting-timezone-1 ()
  (datetime--test-set-up-parser 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-parser (datetime--test 'format (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000))))))

(ert-deftest datetime-test-parsing-various-timestamps-with-shifting-timezone-2 ()
  (datetime--test-set-up-parser 'America/Anchorage 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-parser (datetime--test 'format (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000))))))

(ert-deftest datetime-test-parsing-various-timestamps-with-shifting-timezone-3 ()
  (datetime--test-set-up-parser 'Australia/Hobart 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-parser (datetime--test 'format (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000))))))

(ert-deftest datetime-test-parsing-text-1 ()
  (datetime--test-set-up-parser 'UTC 'en "'on' EEEE 'the' d MMMM 'of' yyyy G, 'at' h:mm:ss a"
    ;; Roughly from 1200 BC till 5100 AD with 6 and a half year step.
    (datetime--test-parser (datetime--test 'format (mapcar (lambda (k) (* k 200000000.123)) (number-sequence -500 500))))))

(ert-deftest datetime-test-parsing-around-offset-transition-1 ()
  (datetime--test-set-up-parser 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; First historical transition.
    (datetime--test-parser-around-transition -2177452800)))

(ert-deftest datetime-test-parsing-around-offset-transition-2 ()
  (datetime--test-set-up-parser 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2010-03-25.
    (datetime--test-parser-around-transition 1269738000)))

(ert-deftest datetime-test-parsing-around-offset-transition-3 ()
  (datetime--test-set-up-parser 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Future transition on 2480-10-27 (according to the rules as of 2018).
    (datetime--test-parser-around-transition 16119997200)))

(ert-deftest datetime-test-parsing-around-offset-transition-4 ()
  (datetime--test-set-up-parser 'America/Anchorage 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2009-03-08.
    (datetime--test-parser-around-transition 1236510000)))

(ert-deftest datetime-test-parsing-around-offset-transition-5 ()
  (datetime--test-set-up-parser 'Australia/Hobart 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2014-10-05.
    (datetime--test-parser-around-transition 1412438400)))

(ert-deftest datetime-test-parser-validating-1 ()
  (datetime--test-set-up-parser 'UTC 'en "yyyy-MM-dd HH:mm:ss.SSS"
    (should-error (funcall datetime--test-parser "lol") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-00-01 00:00:00.000") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-13-01 00:00:00.000") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-01-00 00:00:00.000") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-01-32 00:00:00.000") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-01-01 24:00:00.000") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-01-01 00:60:00.000") :type 'datetime-invalid-string)
    (should-error (funcall datetime--test-parser "2000-01-01 00:00:60.000") :type 'datetime-invalid-string)))

(ert-deftest datetime-test-parser-validating-excessive-patterns ()
  (datetime--test-set-up-parser 'UTC 'en "dd 'of' MMMM '(month' M')'"
    (datetime--test-parser "12 of March (month 3)")
    (should-error (funcall datetime--test-parser "12 of March (month 1)") :type 'datetime-invalid-string)))
