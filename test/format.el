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

(defvar datetime--test-formatter nil)


(defmacro datetime--test-set-up-formatter (timezone locale pattern &rest body)
  (declare (debug (form form form body))
           (indent 3))
  `(datetime--test-set-up ,timezone ,locale ,pattern
     (let ((datetime--test-formatter (datetime-float-formatter 'java datetime--test-pattern :timezone datetime--test-timezone :locale datetime--test-locale)))
       ,@body)))

(defun datetime--test-formatter (times)
  (unless (listp times)
    (setq times (list times)))
  (let ((formatted (datetime--test 'format times)))
    (while times
      (let ((time     (pop times))
            (expected (pop formatted)))
        (eval `(should (progn ',datetime--test-timezone ',datetime--test-locale ,datetime--test-pattern ,time
                              (string= ,(funcall datetime--test-formatter time) ,expected))))))))

(defun datetime--test-formatter-around-transition (time)
  (datetime--test-formatter (list time
                                  (+ time  0.5) (- time  0.5)     ; half a second
                                  (+ time   30) (- time   30)     ; half a minute
                                  (+ time 1800) (- time 1800)     ; half an hour
                                  (+ time 3600) (- time 3600)     ; one hour
                                  (+ time 7200) (- time 7200))))  ; two hours


(ert-deftest datetime-test-formatting-now ()
  (datetime--test-set-up-formatter 'UTC 'en "yyyy-MM-dd HH:mm:ss.SSS"
    (datetime--test-formatter (float-time))))

(ert-deftest datetime-test-formatting-now-standard-formats ()
  (let ((now (float-time)))
    (dolist (locale (datetime-list-locales t))
      (dolist (variant '(:short :medium :long :full))
        (let ((pattern (datetime-locale-date-time-pattern locale variant)))
          (unless (datetime-pattern-includes-timezone-p 'java pattern)
            (datetime--test-set-up-formatter 'UTC locale pattern
              (datetime--test-formatter now))))))))

(ert-deftest datetime-test-formatting-various-timestamps-1 ()
  (datetime--test-set-up-formatter 'UTC 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-formatter (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-test-formatting-various-timestamps-with-fixed-offset-timezone-1 ()
  (datetime--test-set-up-formatter 'Etc/GMT+1 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-formatter (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-test-formatting-various-timestamps-with-shifting-timezone-1 ()
  (datetime--test-set-up-formatter 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-formatter (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-test-formatting-various-timestamps-with-shifting-timezone-2 ()
  (datetime--test-set-up-formatter 'America/Anchorage 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-formatter (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-test-formatting-various-timestamps-with-shifting-timezone-3 ()
  (datetime--test-set-up-formatter 'Australia/Hobart 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-formatter (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-test-formatting-text-1 ()
  (datetime--test-set-up-formatter 'UTC 'en "'on' EEEE 'the' d MMMM 'of' yyyy G, 'at' h:mm:ss a"
    ;; Roughly from 1200 BC till 5100 AD with 6 and a half year step.
    (datetime--test-formatter (mapcar (lambda (k) (* k 200000000.123)) (number-sequence -500 500)))))

(ert-deftest datetime-test-formatting-around-offset-transition-1 ()
  (datetime--test-set-up-formatter 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; First historical transition.
    (datetime--test-formatter-around-transition -2177452800)))

(ert-deftest datetime-test-formatting-around-offset-transition-2 ()
  (datetime--test-set-up-formatter 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2010-03-25.
    (datetime--test-formatter-around-transition 1269738000)))

(ert-deftest datetime-test-formatting-around-offset-transition-3 ()
  (datetime--test-set-up-formatter 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Future transition on 2480-10-27 (according to the rules as of 2018).
    (datetime--test-formatter-around-transition 16119997200)))

(ert-deftest datetime-test-formatting-around-offset-transition-4 ()
  (datetime--test-set-up-formatter 'America/Anchorage 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2009-03-08.
    (datetime--test-formatter-around-transition 1236510000)))

(ert-deftest datetime-test-formatting-around-offset-transition-5 ()
  (datetime--test-set-up-formatter 'Australia/Hobart 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2014-10-05.
    (datetime--test-formatter-around-transition 1412438400)))
