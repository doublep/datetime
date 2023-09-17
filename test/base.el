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


(require 'datetime)
(require 'ert)


(defvar datetime--test-timezone nil)
(defvar datetime--test-locale   nil)
(defvar datetime--test-pattern  nil)

(defvar datetime--test-java-process nil)

(defvar datetime--test-directory (file-name-directory (or load-file-name (buffer-file-name))))

;; Spaces are included only for readability where needed.  They don't affect anything otherwise (or,
;; rather, should affect the library and the Java benchmark in the same way).
(defvar datetime--test-offset-format-specifiers
  '("Z" "ZZ" "ZZZ" " ZZZZ" "ZZZZZ"
    " O" " OOOO"
    "x" "xx" "xxx" "xxxx" "xxxxx"
    "X" "XX" "XXX" "XXXX" "XXXXX"))


(defmacro datetime--test-set-up (timezone locale pattern &rest body)
  (declare (debug (form form form body))
           (indent 3))
  `(let ((datetime--test-timezone  ,timezone)
         (datetime--test-locale    ,locale)
         (datetime--test-pattern   ,pattern))
     ,@body))

(defvar datetime--test-formatter nil)
(defvar datetime--test-parser    nil)
(defvar datetime--test-matcher   nil)

(defmacro datetime--test-set-up-formatter (timezone locale pattern &rest body)
  (declare (debug (form form form body))
           (indent 3))
  `(datetime--test-set-up ,timezone ,locale ,pattern
     (let ((datetime--test-formatter (datetime-float-formatter 'java datetime--test-pattern :timezone datetime--test-timezone :locale datetime--test-locale))
           ;; Currently, `datetime-matching-regexp' doesn't support timezone names.
           (datetime--test-matcher   (unless (datetime-pattern-includes-timezone-name-p 'java datetime--test-pattern)
                                       (datetime-matching-regexp 'java datetime--test-pattern :timezone datetime--test-timezone :locale datetime--test-locale))))
       ,@body)))

(defmacro datetime--test-set-up-parser (timezone locale pattern &rest body)
  (declare (debug (form form form body))
           (indent 3))
  `(datetime--test-set-up ,timezone ,locale ,pattern
     (let ((datetime--test-parser (datetime-parser-to-float 'java datetime--test-pattern :timezone datetime--test-timezone :locale datetime--test-locale)))
       ,@body)))

(defmacro datetime--test-set-up-formatter-and-parser (timezone locale pattern &rest body)
  (declare (debug (form form form body))
           (indent 3))
  `(datetime--test-set-up-formatter ,timezone ,locale ,pattern
     (datetime--test-set-up-parser datetime--test-timezone datetime--test-locale datetime--test-pattern
       ,@body)))

(defun datetime--test (command times)
  (unless (listp times)
    (setq times (list times)))
  (with-temp-buffer
    (let ((commands (current-buffer))
          (stderr   (get-buffer-create " java-benchmark/stderr")))
      (unless (process-live-p datetime--test-java-process)
        (let ((default-directory datetime--test-directory))
          (with-current-buffer stderr
            (erase-buffer))
          (setq datetime--test-java-process (make-process :name "java-benchmark" :buffer "java-benchmark" :stderr stderr
                                                          :command '("java" "ProcessTimestamp")))))
      (let* ((marker           (process-mark datetime--test-java-process))
             (position         (marker-position marker))
             (num-times        (length times))
             (num-result-lines 0)
             result
             successful)
        (unwind-protect
            (with-current-buffer (marker-buffer marker)
              ;; It is much faster to give "tasks" to the remote process in
              ;; batch, then fetch the results.
              (with-current-buffer commands
                (dolist (time times)
                  (insert (format "%s %s\n%s %s %s\n" command time datetime--test-timezone datetime--test-locale datetime--test-pattern))))
              (process-send-string datetime--test-java-process (with-current-buffer commands (buffer-string)))
              (while (< num-result-lines num-times)
                (while (or (= (marker-position marker) position) (/= (char-before marker) ?\n))
                  (accept-process-output datetime--test-java-process))
                (unless (process-live-p datetime--test-java-process)
                  (error "ProcessTimestamp process exited unexpectedly with code %d:\n%s"
                         (process-exit-status datetime--test-java-process) (with-current-buffer stderr (buffer-string))))
                (while (> (marker-position marker) position)
                  (goto-char position)
                  (end-of-line)
                  (let ((as-string (buffer-substring position (point))))
                    (push (if (eq command 'format) as-string (car (read-from-string as-string))) result))
                  (beginning-of-line 2)
                  (setq position         (point)
                        num-result-lines (1+ num-result-lines))))
              (setf successful t)
              (nreverse result))
          (unless successful
            (message "stderr of `java-benchmark':\n%s"
                     (condition-case error
                         (with-current-buffer stderr
                           (if (bobp) "[empty]" (buffer-string)))
                       (error (format "[failed to retrieve: %S]" error))))
            (message "command(s) to be sent or have been sent to `java-benchmark' last:\n%s"
                     (condition-case error
                         (with-current-buffer commands
                           (if (bobp) "[none]" (buffer-string)))
                       (error (format "[failed to retrieve: %S]" error))))))))))


(provide 'test/base)
