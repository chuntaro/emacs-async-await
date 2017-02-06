;;; async-await-examples.el --- Examples using `async-await.el'  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>
;; URL: https://github.com/chuntaro/emacs-async-await
;; Package-Requires: ((emacs "25") (promise "1.0"))
;; Version: 1.0
;; Keywords: async await convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is examples using `async-await.el'.
;;
;; To execute this, move the point after the last parenthesis of the following
;; Lisp code and press C-x C-e. (Launch the new Emacs and run (launcher))
;;
;; (start-process "emacs" nil (file-truename (expand-file-name invocation-name invocation-directory)) "-Q" "--execute" "(package-initialize)" "-L" (concat default-directory "../") "-L" default-directory "-l" (buffer-file-name) "--execute" "(launcher)")

;;; Code:

(require 'async-await)
(require 'promise-rejection-tracking)

(defun print (obj)
  "The `print' function for this example."
  (cl-fresh-line)
  (message "\n%S\n" obj))

(defun wait-sync (n)
  "Synchronous wait."
  (sleep-for n)
  n)

(defun example1 ()
  "Since it is synchronous, the process does not return until
all the execution is completed."
  (print (wait-sync 0.5))
  (message "---")

  (print (wait-sync 1.0))
  (message "---")

  (print (wait-sync 1.5))
  (message "---")

  (message "done"))

(defun wait-async (n)
  "Asynchronous wait."
  (promise-new (lambda (resolve _reject)
                 (run-at-time n
                              nil
                              (lambda ()
                                (funcall resolve n))))))

(async-defun example2 ()
  "As it is executed asynchronously, the process returns immediately."
  (print (await (wait-async 0.5)))
  (message "---")

  (print (await (wait-async 1.0)))
  (message "---")

  (print (await (wait-async 1.5)))
  (message "---")

  (message "await done"))

(defun example3 ()
  "The lambda version of example2."
  (funcall (async-lambda ()
             (print (await (wait-async 0.5)))
             (message "---")

             (print (await (wait-async 1.0)))
             (message "---")

             (print (await (wait-async 1.5)))
             (message "---")

             (message "await done"))))

(async-defun example4 ()
  "Any object can be an argument of await."
  (message "return value: %S" (await (example2)))

  (message "return value: %S" (await (example3)))

  (message "return value: %S" (await 3))

  (message "all await done"))

(defun example5 ()
  "Since Async Function is asynchronous,
the following are executed at the same time."
  (funcall (async-lambda ()
             (print (await (wait-async 0.7)))
             (message "---")

             (print (await (wait-async 0.7)))
             (message "---")

             (print (await (wait-async 0.7)))
             (message "---")

             (message "await done (0.7 * 3)")))
  (funcall (async-lambda ()
             (print (await (wait-async 1)))
             (message "---")

             (print (await (wait-async 1)))
             (message "---")

             (print (await (wait-async 1)))
             (message "---")

             (message "await done (1 * 3)"))))

(defun wait-to-reject (n)
  "Wait to reject when n is 2 or more.
Async/Await for Emacs causes error on reject."
  (promise-new (lambda (resolve reject)
                 (run-at-time n
                              nil
                              (lambda ()
                                (if (< n 2)
                                    (funcall resolve n)
                                  (funcall reject n)))))))

(async-defun example6 ()
  "Since errors are not caught, errors are not displayed at the end.
However, if rejection-tracking is enabled,
a warning is displayed in a separate window."
  (let ((n 0))
    (print (await (wait-to-reject n)))
    (cl-incf n)

    (print (await (wait-to-reject n)))
    (cl-incf n)

    (print (await (wait-to-reject n)))

    (message "await done")))

(async-defun example7 ()
  "Use `condition-case' to catch errors."
  (condition-case reason
      (let ((n 0))
        (print (await (wait-to-reject n)))
        (cl-incf n)

        (print (await (wait-to-reject n)))
        (cl-incf n)

        (print (await (wait-to-reject n)))

        (message "await done"))
    (error (message "catch the error: %s" reason))))

;;
;; Example using `url-retrieve'
;;

(require 'url-http)
(require 'xml)
(require 'dom)

(defun xml-retrieve (url)             ; Same as `promise:xml-retrieve'
  "Return `Promise' to resolve with XML object obtained by HTTP request."
  (promise-new
   (lambda (resolve reject)
     (url-retrieve url
                   (lambda (status)
                     ;; All errors are reliably captured and rejected with appropriate values.
                     (if (plist-get status :error)
                         (funcall reject (plist-get status :error))
                       (condition-case ex
                           (with-current-buffer (current-buffer)
                             (if (not (url-http-parse-headers))
                                 (funcall reject (buffer-string))
                               (search-forward-regexp "\n\\s-*\n" nil t)
                               (funcall resolve (xml-parse-region))))
                         (error (funcall reject ex)))))))))

(defun get-text-first-tag (xml tag)
  "Returns the first text that matches TAG in XML."
  (decode-coding-string (dom-text (cl-first (dom-by-tag xml tag)))
                        'utf-8))

(defun get-short-text-first-tag (xml tag)
  "Truncate the text obtained with `get-text-first-tag'."
  (concat (truncate-string-to-width (get-text-first-tag xml tag) 64)
          " ..."))

(defun wait-seconds (seconds fn &rest args) ; Same as `promise:run-at-time'
  "Return `Promise' to execute the function after the specified time."
  (promise-new (lambda (resolve _reject)
                 (run-at-time seconds
                              nil
                              (lambda ()
                                (funcall resolve (apply fn args)))))))

(async-defun example8 ()
  "Example using `xml-retrieve'."
  (condition-case reason
      (let* ((wikipedia-url (concat "https://en.wikipedia.org/w/api.php"
                                    "?format=xml&action=query&prop=extracts"
                                    "&exintro=&explaintext=&titles="))
             (xml-gnu (await (xml-retrieve (concat wikipedia-url "GNU"))))
             ;; Request after 2 seconds for load reduction.
             (xml-emacs (await (wait-seconds 2
                                             #'xml-retrieve
                                             (concat wikipedia-url "Emacs")))))
        (print (get-short-text-first-tag xml-gnu 'extract))
        (print (get-short-text-first-tag xml-emacs 'extract)))
    (error (message "error: %s" reason))))

;;
;; Asynchronous Processes
;;

(defun make-grep-process (&rest args)
  "Return Promise which invokes the process asynchronously
and resolves it in the output result."
  (promise-new
   (lambda (resolve reject)
     (make-process :name "grep"
                   :buffer "*grep-result*"
                   :command (cl-list* "grep" args)
                   :sentinel (lambda (_process event)
                               (if (string= event "finished\n")
                                   (with-current-buffer "*grep-result*"
                                     (funcall resolve (buffer-string)))
                                 (funcall reject event)))))))

(async-defun example9 ()
  "An example using `make-process'."
  (condition-case reason
      (message "grep result:\n%s" (await (make-grep-process "async" "async-await-examples.el")))
    (error (message "error: %s" reason))))

(async-defun example10 ()
  "Same result as `example9'."
  (condition-case reason
      (message "grep result:\n%s" (await (promise:make-process-string
                                          "grep" "async" "async-await-examples.el")))
    (error (message "error: %s" reason))))

;;
;; Launcher
;;

(defun launcher ()
  "A launcher that runs each example."
  (require 'ido)
  (switch-to-buffer "*Messages*")
  (setq inhibit-message t
        scroll-conservatively 10000)

  ;; Enable rejection-tracking to capture Unhandled Promise Rejection.
  ;; Please compare `example6' with `example7'.
  (promise-rejection-tracking-enable '((all-rejections . t)))

  (let (nums)
    (mapatoms
     (lambda (x)
       (when (fboundp x)
         (let ((name (symbol-name x)))
           (when (string-match "^example\\([0-9]+\\)$" name)
             (push (match-string 1 name) nums))))))
    (cl-callf cl-sort nums #'< :key #'string-to-number)
    (cl-loop
     (let* ((num (ido-completing-read "What number of examples do you run?: example"
                                      nums))
            (example (intern (concat "example" num))))
       (message "***** example%s *****" num)
       (funcall example)))))

;;; async-await-examples.el ends here
