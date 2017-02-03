Async/Await for Emacs
=====================

This is a simple implementation of Async/Await.  
Inspired by the Async/Await implementation of TypeScript.

This implementation uses generator.el included in Emacs 25 and [promise.el](https://github.com/chuntaro/emacs-promise).

For detailed tutorials on its use, see [What about Async/Await?](https://blogs.msdn.microsoft.com/typescript/2015/11/03/what-about-asyncawait/) (TypeScript)

Installation
------------

You can install from MELPA using package.el.  
The package name is **async-await**.

Usage
-----

See [async-await-examples.el](https://github.com/chuntaro/emacs-async-await/blob/master/async-await-examples.el) for details.


```emacs-lisp
(require 'async-await)

(defun wait-async (n)
  (promise-new (lambda (resolve _reject)
                 (run-at-time n
                              nil
                              (lambda ()
                                (funcall resolve n))))))

(async-defun example2 ()
  (print (await (wait-async 0.5)))
  (message "---")

  (print (await (wait-async 1.0)))
  (message "---")

  (print (await (wait-async 1.5)))
  (message "---")

  (message "await done"))

(example2) =>

 0.5

 ---

 1.0

 ---

 1.5

 ---
 await done

 The result of the execution is outputted from the top to the bottom
 like the order written in the code. However, asynchronously!

;;
;; Example using `url-retrieve'
;;

(require 'async-await)
(require 'url-http)
(require 'xml)
(require 'dom)

(defun xml-retrieve (url)
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

(defun wait-seconds (seconds fn &rest args)
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
```
