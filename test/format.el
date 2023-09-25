;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2023 Paul Pogonyshev

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


(defun datetime--test-formatter (times)
  (unless (listp times)
    (setq times (list times)))
  (let ((all-formatted (datetime--test 'format times)))
    (while times
      (let* ((time      (pop times))
             (expected  (pop all-formatted))
             (formatted (funcall datetime--test-formatter time)))
        (eval `(should (progn ',datetime--test-timezone ',datetime--test-locale ,datetime--test-pattern ,time
                              (string= ,formatted ,expected)))
              t)
        (when datetime--test-matcher
          (eval `(should (progn ',datetime--test-timezone ',datetime--test-locale ,datetime--test-pattern ,time
                                (string-match-p ,datetime--test-matcher ,formatted)))
                t))))))

(defun datetime--test-formatter-around-transition (time)
  (datetime--test-formatter (list time
                                  (+ time  0.5) (- time  0.5)     ; half a second
                                  (+ time   30) (- time   30)     ; half a minute
                                  (+ time 1800) (- time 1800)     ; half an hour
                                  (+ time 3600) (- time 3600)     ; one hour
                                  (+ time 7200) (- time 7200))))  ; two hours


(ert-deftest datetime-formatting-now ()
  (datetime--test-set-up-formatter 'UTC 'en "yyyy-MM-dd HH:mm:ss.SSS"
    (datetime--test-formatter (float-time))))

(ert-deftest datetime-formatting-now-standard-formats ()
  (let ((now (float-time)))
    (dolist (locale (datetime-list-locales t))
      (dolist (variant '(:short :medium :long :full))
        (let ((pattern (datetime-locale-date-time-pattern locale variant)))
          (datetime--test-set-up-formatter 'UTC locale pattern
            (datetime--test-formatter now)))))))

(ert-deftest datetime-formatting-various-timestamps-1 ()
  (datetime--test-set-up-formatter 'UTC 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-formatter (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-formatting-various-timestamps-with-fixed-offset-timezone-1 ()
  (datetime--test-set-up-formatter 'Etc/GMT+1 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-formatter (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-formatting-various-timestamps-with-shifting-timezone-1 ()
  (datetime--test-set-up-formatter 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-formatter (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-formatting-various-timestamps-with-shifting-timezone-2 ()
  (datetime--test-set-up-formatter 'America/Anchorage 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-formatter (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-formatting-various-timestamps-with-shifting-timezone-3 ()
  (datetime--test-set-up-formatter 'Australia/Hobart 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test-formatter (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-formatting-text-1 ()
  (datetime--test-set-up-formatter 'UTC 'en "'on' EEEE 'the' d MMMM 'of' yyyy G, 'at' h:mm:ss a"
    ;; Roughly from 1200 BC till 5100 AD with 6 and a half year step.
    (datetime--test-formatter (mapcar (lambda (k) (* k 200000000.123)) (number-sequence -500 500)))))

(ert-deftest datetime-formatting-around-offset-transition-1 ()
  (datetime--test-set-up-formatter 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; First historical transition.
    (datetime--test-formatter-around-transition -2177452800)))

(ert-deftest datetime-formatting-around-offset-transition-2 ()
  (datetime--test-set-up-formatter 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2010-03-25.
    (datetime--test-formatter-around-transition 1269738000)))

(ert-deftest datetime-formatting-around-offset-transition-3 ()
  (datetime--test-set-up-formatter 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Future transition on 2480-10-27 (according to the rules as of 2018).
    (datetime--test-formatter-around-transition 16119997200)))

(ert-deftest datetime-formatting-around-offset-transition-4 ()
  (datetime--test-set-up-formatter 'America/Anchorage 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2009-03-08.
    (datetime--test-formatter-around-transition 1236510000)))

(ert-deftest datetime-formatting-around-offset-transition-5 ()
  (datetime--test-set-up-formatter 'Australia/Hobart 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2014-10-05.
    (datetime--test-formatter-around-transition 1412438400)))

(ert-deftest datetime-formatting-with-timezone-name-1 ()
  (dolist (timezone (datetime-list-timezones))
    (datetime--test-set-up-formatter timezone 'en "yyyy-MM-dd HH:mm:ss z"
      ;; Rule-based transition on 2014-10-26.  In some timezones should also result in name
      ;; changing, e.g. between CEST and CET.
      (datetime--test-formatter-around-transition 1414285200))))

(ert-deftest datetime-formatting-with-timezone-name-2 ()
  ;; Many timezones had special relations with DST (see comments in 'HarvestData.java'), so
  ;; resulting name varies a lot.  Make sure we handle all that correctly.  Too much to test all
  ;; timezones, only some selected.
  (dolist (timezone '(Africa/Algiers Africa/Tripoli Africa/Windhoek
                      America/Anchorage America/Argentina/Buenos_Aires America/Argentina/Ushuaia America/Chihuahua America/Dawson America/Indiana/Knox America/Iqaluit America/Kentucky/Louisville America/Whitehorse
                      Antarctica/McMurdo Antarctica/Palmer
                      Asia/Almaty Asia/Ashkhabad Asia/Baku Asia/Bishkek Asia/Damascus Asia/Istanbul Asia/Kamchatka Asia/Omsk Asia/Singapore Asia/Tashkent Asia/Tbilisi Asia/Vladivostok Asia/Yerevan
                      Atlantic/Azores
                      Canada/Yukon
                      Chile/EasterIsland
                      Europe/Belfast Europe/Kaliningrad Europe/Lisbon Europe/Minsk Europe/Moscow Europe/Simferopol Europe/Tallinn Europe/Warsaw
                      GB Libya NZ
                      Pacific/Auckland Pacific/Norfolk
                      Poland Portugal
                      US/Alaska US/Aleutian US/Pacific
                      W-SU))
    (datetime--test-set-up-formatter timezone 'en "yyyy-MM-dd HH:mm:ss z"
      ;; Exact numbers don't matter much, we just need to skip a few months each time.
      (datetime--test-formatter (mapcar (lambda (k) (* k 7000000)) (number-sequence -300 400))))))


(ert-deftest datetime-formatting-with-timezone-offset-1 ()
  (dolist (timezone (datetime-list-timezones))
    (dolist (offset-format-specifier datetime--test-offset-format-specifiers)
      (datetime--test-set-up-formatter timezone 'en (format "yyyy-MM-dd HH:mm:ss%s" offset-format-specifier)
        (datetime--test-formatter-around-transition 1414285200)))))

;; Test with offsets that include seconds.  This was true for most real timezones in ye older times.
(ert-deftest datetime-formatting-with-timezone-offset-2 ()
  (dolist (timezone '(Africa/Lusaka America/Asuncion Asia/Dushanbe Asia/Tehran Atlantic/Bermuda Australia/Sydney
                      Brazil/East Canada/Pacific Europe/Athens Europe/Rome Europe/Zurich Indian/Antananarivo
                      Mexico/General Pacific/Samoa US/Central))
    (dolist (offset-format-specifier datetime--test-offset-format-specifiers)
      (datetime--test-set-up-formatter timezone 'en (format "yyyy-MM-dd HH:mm:ss%s" offset-format-specifier)
        (datetime--test-formatter -3000000000)))))

(ert-deftest datetime-formatting-day-periods ()
  (let (times)
    (dotimes (minute (* 24 60))
      (push (* minute 60) times))
    (setf times (nreverse times))
    (dolist (pattern '("HH:mm:ss B" "HH:mm:ss BBBB" "HH:mm:ss BBBBB"))
      (datetime--test-set-up-formatter 'UTC 'en pattern
        (datetime--test-formatter times)))))


;; A real bug fixed in 0.9.1.  A more thorough test for it is `datetime-locale-database-sanity'.
(ert-deftest datetime-formatting-hebrew-1 ()
  (datetime--test-set-up-formatter 'UTC 'he "yyyy G"
    (datetime--test-formatter 0)))


(provide 'test/format)
