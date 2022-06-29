;;; -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022 Paul Pogonyshev

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


(defmacro datetime--test-set-up (timezone locale pattern &rest body)
  (declare (debug (form form form body))
           (indent 3))
  `(let ((datetime--test-timezone  ,timezone)
         (datetime--test-locale    ,locale)
         (datetime--test-pattern   ,pattern))
     ,@body))

(defun datetime--test (command times)
  (unless (listp times)
    (setq times (list times)))
  (unless (process-live-p datetime--test-java-process)
    (let ((default-directory datetime--test-directory)
          (stderr            (get-buffer-create "java-benchmark/stderr")))
      (with-current-buffer stderr
        (erase-buffer))
      (setq datetime--test-java-process (make-process :name "java-benchmark" :buffer "java-benchmark" :stderr stderr
                                                      :command '("java" "ProcessTimestamp")))))
  (let* ((marker           (process-mark datetime--test-java-process))
         (position         (marker-position marker))
         (num-times        (length times))
         (num-result-lines 0)
         result)
    (with-current-buffer (marker-buffer marker)
      ;; It is much faster to give "tasks" to the remote process in
      ;; batch, then fetch the results.
      (dolist (time times)
        (process-send-string datetime--test-java-process
                             (format "%s %s\n%s %s %s\n" command time datetime--test-timezone datetime--test-locale datetime--test-pattern)))
      (while (< num-result-lines num-times)
        (while (or (= (marker-position marker) position) (/= (char-before marker) ?\n))
          (accept-process-output datetime--test-java-process))
        (unless (process-live-p datetime--test-java-process)
          (error "ProcessTimestamp process exited unexpectedly with code %d:\n%s"
                 (process-exit-status datetime--test-java-process) (with-current-buffer "java-benchmark/stderr" (buffer-string ))))
        (while (> (marker-position marker) position)
          (goto-char position)
          (end-of-line)
          (let ((as-string (buffer-substring position (point))))
            (push (if (eq command 'format) as-string (car (read-from-string as-string))) result))
          (beginning-of-line 2)
          (setq position         (point)
                num-result-lines (1+ num-result-lines))))
      (nreverse result))))


(provide 'test/base)
