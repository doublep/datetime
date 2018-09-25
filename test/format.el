;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018 Paul Pogonyshev

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


(require 'datetime)
(require 'ert)


(defvar datetime--test-timezone  nil)
(defvar datetime--test-locale    nil)
(defvar datetime--test-pattern   nil)
(defvar datetime--test-formatter nil)

(defvar datetime--test-java-formatting-process nil)

(defvar datetime--test-directory (file-name-directory (or load-file-name (buffer-file-name))))


(defmacro datetime--test-set-up (timezone locale pattern &rest body)
  (declare (debug (form form form body))
           (indent 3))
  `(let* ((datetime--test-timezone  ,timezone)
          (datetime--test-locale    ,locale)
          (datetime--test-pattern   ,pattern)
          (datetime--test-formatter (datetime-float-formatter 'java datetime--test-pattern :timezone datetime--test-timezone :locale datetime--test-locale)))
     ,@body))

;; We assume that the Java program is already compiled externally (see `run-tests.sh').
(defun datetime--test (times)
  (unless (listp times)
    (setq times (list times)))
  (unless (process-live-p datetime--test-java-formatting-process)
    (let ((default-directory datetime--test-directory))
      (setq datetime--test-java-formatting-process (make-process :name "java-formatter" :buffer "java-formatter" :stderr "java-formatter/stderr"
                                                                 :command '("java" "FormatTimestamp")))))
  (let* ((marker        (process-mark datetime--test-java-formatting-process))
         (position      (marker-position marker))
         (num-times     (length times))
         (num-formatted 0)
         formatted)
    (save-excursion
      (set-buffer (marker-buffer marker))
      ;; It is much faster to give "tasks" to the remote process in
      ;; batch, then fetch the results.
      (dolist (time times)
        (process-send-string datetime--test-java-formatting-process
                             (format "%s %s %s %s\n" time datetime--test-timezone datetime--test-locale datetime--test-pattern)))
      (while (< num-formatted num-times)
        (while (or (= (marker-position marker) position) (/= (char-before marker) ?\n))
          (accept-process-output datetime--test-java-formatting-process))
        (while (> (marker-position marker) position)
          (goto-char position)
          (end-of-line)
          (push (buffer-substring position (point)) formatted)
          (beginning-of-line 2)
          (setq position      (point)
                num-formatted (1+ num-formatted))))
      (setq formatted (nreverse formatted))
      (while times
        (let ((time     (pop times))
              (expected (pop formatted)))
          (eval `(should (progn ',datetime--test-timezone ',datetime--test-locale ,datetime--test-pattern ,time
                                (string= ,(funcall datetime--test-formatter time) ,expected)))))))))

(defun datetime--test-transition (time)
  (datetime--test (list time
                        (+ time  0.5) (- time  0.5)     ; half a second
                        (+ time   30) (- time   30)     ; half a minute
                        (+ time 1800) (- time 1800)     ; half an hour
                        (+ time 3600) (- time 3600)     ; one hour
                        (+ time 7200) (- time 7200))))  ; two hours


(ert-deftest datetime-test-formatting-now ()
  (datetime--test-set-up 'UTC 'en "yyyy-MM-dd HH:mm:ss.SSS"
    (datetime--test (float-time))))

(ert-deftest datetime-test-formatting-now-standard-formats ()
  (let ((now (float-time)))
    (dolist (locale (datetime-list-locales t))
      (dolist (variant '(:short :medium :long :full))
        (let ((pattern (datetime-locale-date-time-pattern locale variant)))
          (unless (datetime-pattern-includes-timezone-p 'java pattern)
            (datetime--test-set-up 'UTC locale pattern
              (datetime--test now))))))))

(ert-deftest datetime-test-formatting-various-timestamps-1 ()
  (datetime--test-set-up 'UTC 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-test-formatting-various-timestamps-with-fixed-offset-timezone-1 ()
  (datetime--test-set-up 'Etc/GMT+1 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-test-formatting-various-timestamps-with-shifting-timezone-1 ()
  (datetime--test-set-up 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-test-formatting-various-timestamps-with-shifting-timezone-2 ()
  (datetime--test-set-up 'America/Anchorage 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-test-formatting-various-timestamps-with-shifting-timezone-3 ()
  (datetime--test-set-up 'Australia/Hobart 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Roughly from 400 AD till 3500 AD with 4 month step.
    (datetime--test (mapcar (lambda (k) (* k 10000000.123)) (number-sequence -5000 5000)))))

(ert-deftest datetime-test-formatting-text-1 ()
  (datetime--test-set-up 'UTC 'en "'on' EEEE 'the' d MMMM 'of' yyyy G, 'at' h:mm:ss a"
    ;; Roughly from 1200 BC till 5100 AD with 6 and a half year step.
    (datetime--test (mapcar (lambda (k) (* k 200000000.123)) (number-sequence -500 500)))))

(ert-deftest datetime-test-formatting-around-offset-transition-1 ()
  (datetime--test-set-up 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; First historical transition.
    (datetime--test-transition -2177452800)))

(ert-deftest datetime-test-formatting-around-offset-transition-2 ()
  (datetime--test-set-up 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2010-03-25.
    (datetime--test-transition 1269738000)))

(ert-deftest datetime-test-formatting-around-offset-transition-3 ()
  (datetime--test-set-up 'Europe/Madrid 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Future transition on 2480-10-27 (according to the rules as of 2018).
    (datetime--test-transition 16119997200)))

(ert-deftest datetime-test-formatting-around-offset-transition-4 ()
  (datetime--test-set-up 'America/Anchorage 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2009-03-08.
    (datetime--test-transition 1236510000)))

(ert-deftest datetime-test-formatting-around-offset-transition-5 ()
  (datetime--test-set-up 'Australia/Hobart 'en "yyyy-MM-dd HH:mm:ss.SSS"
    ;; Rule-based transition on 2014-10-05.
    (datetime--test-transition 1412438400)))
