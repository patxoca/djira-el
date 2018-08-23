;;; -*- lexical-binding: t -*-
;;; tests-emacs-djira.el --- tests for emacs-djira

(require 'cl)
(require 'ert)

(require 'emacs-djira)


(defmacro with-buffer-fixture (content &rest body)
  `(let ((buffer (generate-new-buffer "*test-buffer*")))
     (unwind-protect
         (with-current-buffer buffer
           (insert ,content)
           ,@body)
       (kill-buffer buffer))))


(defmacro should-error-with (body type data)
  `(should (equal
            (cdr (should-error ,body :type ,type))
            ,data)))


(defmacro fpatch (varlist &rest body)
  ;; Macro that redefines functions for the length of the execution of
  ;; a block of code and restores them back when the block exits.
  ;;
  ;; Works by transforming:
  ;;
  ;;   (fpatch ((foo (lambda () 42))
  ;;            (bar (lambda () "hi")))
  ;;           (list (foo) (bar)))
  ;;
  ;; into:
  ;;
  ;;   (let ((g86 (symbol-function 'foo))
  ;;         (g87 (symbol-function 'bar))
  ;;         (g88))
  ;;     (fset 'foo (lambda nil 42))
  ;;     (fset 'bar (lambda nil "hi"))
  ;;     (unwind-protect
  ;;         (setq g88 (progn
  ;;                     (list
  ;;                      (foo)
  ;;                      (bar))))
  ;;       (fset 'foo g86)
  ;;       (fset 'bar g87)
  ;;       g88))
  ;;

  (let ((foo (mapcar (lambda (x) (cons (gensym) x)) varlist))
        (name-res (gensym)))
    `(let (,@(mapcar (lambda (x) `(,(car x) (symbol-function (quote ,(cadr x))))) foo)
           (,name-res))
       ,@(mapcar (lambda (x) `(fset (quote ,(cadr x)) ,(caddr x))) foo)
       (unwind-protect
           (setq ,name-res (progn ,@body))
         ,@(mapcar (lambda (x) `(fset (quote ,(cadr x)) ,(car x))) foo)
         ,name-res))))


(ert-deftest test-djira--make-url ()
  ""
  (cl-flet ((example (input expected)
                     (should (equal (apply 'djira--make-url input) expected))))
    (let ((djira-url "prefix"))
      (example '("a" "")   "prefix/a/")
      (example '("a/" "")  "prefix/a/")
      (example '("a" "c")  "prefix/a/?c")
      (example '("a/" "c") "prefix/a/?c"))
    (let ((djira-url "prefix/"))
      (example '("a" "")   "prefix/a/")
      (example '("a/" "")  "prefix/a/")
      (example '("a" "c")  "prefix/a/?c")
      (example '("a/" "c") "prefix/a/?c"))))


(ert-deftest test-djira--make-query-string ()
  "Tests query string generation."
  (cl-flet ((example (input expected)
                     (should (equal (apply 'djira--make-query-string input) expected))))
    (example '()            "")
    (example '(:foo "bar")  "foo=bar")
    (example '(:foo 3)      "foo=3")
    (example '(:foo 3.14)   "foo=3.14")
    (example '(:foo t)      "foo=true")
    (example '(:foo symbol) "foo=symbol")
    (example '(:foo (1 2))  "foo=1&foo=2")

    (example '(:foo 3 :bar "baz") "foo=3&bar=baz")
    (example '(:foo 3 :foo 4)     "foo=3&foo=4")

    (example '(:foo "R&R") "foo=R%26R")
    (example '(:foo "R?R") "foo=R%3FR")
    (example '(:foo "R=R") "foo=R%3DR")
    (example '(:foo "R R") "foo=R%20R")))


(ert-deftest test-djira--get-status-code ()
  "asdf"
  (cl-flet ((example (input expected)
                     (with-buffer-fixture
                      input
                      (should (equal (djira--get-status-code) expected)))))
    (example "HTTP/1.0 200 OK\nDate: Thu, 23 Aug 2018 12:31:55 GMT"  "200")
    (example "HTTP/1.0 400 OK\nDate: Thu, 23 Aug 2018 12:31:55 GMT"  "400")
    (example "HTTP/2.0 200 OK\nDate: Thu, 23 Aug 2018 12:31:55 GMT"  "200")
    (example "HTTP/20.0 200 OK\nDate: Thu, 23 Aug 2018 12:31:55 GMT" "200")))


(ert-deftest test-djira--get-content-type ()
  "asdf"
  (cl-flet ((example (input expected)
                     (with-buffer-fixture
                      input
                      (should (equal (djira--get-content-type) expected)))))
    (example
     "HTTP/1.0 200 OK
Date: Thu, 23 Aug 2018 12:31:55 GMT
Content-Type: application/json
Server: WSGIServer/0.2 CPython/3.6.4"
     "application/json")

    (example
     "HTTP/1.0 200 OK
Date: Thu, 23 Aug 2018 12:31:55 GMT
Content-Type: text/html
Server: WSGIServer/0.2 CPython/3.6.4"
     "text/html")

    (with-buffer-fixture
     "HTTP/1.0 200 OK
Date: Thu, 23 Aug 2018 12:31:55 GMT
Server: WSGIServer/0.2 CPython/3.6.4"
     (should-error (djira--get-content-type)))))


(ert-deftest test-djira--get-payload ()
  "asdf"
  (cl-flet ((example (input expected)
                     (with-buffer-fixture
                      input
                      (should (equal (djira--get-payload) expected)))))
    (example
     "first header
second header

simple payload"
     "simple payload")

    (example
     "first header
second header

multi
line
 payload"
     "multi\nline\n payload")

    (example
     "first header
second header
no blank line separating headers from payload
multi
line
 payload"
     "")))

(ert-deftest test-djira--do-magic ()
  ""

  (fpatch ((djira--get-status-code (lambda () "200"))
           (djira--get-content-type (lambda () "application/json"))
           (djira--get-payload (lambda () "42")))

          (should (equal (djira--do-magic) 42)))

  (fpatch ((djira--get-status-code (lambda () "200"))
           (djira--get-content-type (lambda () "text/html"))
           (djira--get-payload (lambda () "42")))

          (should-error-with (djira--do-magic) 'error '("Unexpected content-type: text/html")))

  (fpatch ((djira--get-status-code (lambda () "400"))
           (djira--get-content-type (lambda () "application/json"))
           (djira--get-payload (lambda () "42")))

          (should-error-with (djira--do-magic) 'error '("Bad request")))

  (fpatch ((djira--get-status-code (lambda () "404"))
           (djira--get-content-type (lambda () "application/json"))
           (djira--get-payload (lambda () "42")))

          (should-error-with (djira--do-magic) 'error '("Endpoint not found")))

  (fpatch ((djira--get-status-code (lambda () "500"))
           (djira--get-content-type (lambda () "application/json"))
           (djira--get-payload (lambda () "42")))

          (should-error-with (djira--do-magic) 'error '("Error calling endpoint")))

  (fpatch ((djira--get-status-code (lambda () "123"))
           (djira--get-content-type (lambda () "application/json"))
           (djira--get-payload (lambda () "42")))

          (should-error-with (djira--do-magic) 'error '("Unsupported status-code: 123")))
  )



;;;  tests-emacs-djira.el ends here
