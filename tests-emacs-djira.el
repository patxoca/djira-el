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


;;;      _  _ _                  _ _            _
;;;   __| |(_|_)_ __ __ _    ___| (_) ___ _ __ | |_
;;;  / _` || | | '__/ _` |  / __| | |/ _ \ '_ \| __|
;;; | (_| || | | | | (_| | | (__| | |  __/ | | | |_
;;;  \__,_|/ |_|_|  \__,_|  \___|_|_|\___|_| |_|\__|
;;;      |__/

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
                     (should (equal (djira--make-query-string input) expected))))
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

(ert-deftest test-djira--process-response-buffer ()
  ""

  (fpatch ((djira--get-status-code (lambda () "200"))
           (djira--get-content-type (lambda () "application/json"))
           (djira--get-payload (lambda () "42")))

          (should (equal (djira--process-response-buffer) 42))

          (fpatch ((djira--get-content-type (lambda () "text/html")))
                  (should-error-with (djira--process-response-buffer)
                                     'error '("Unexpected content-type: text/html")))

          (fpatch ((djira--get-status-code (lambda () "400")))
                  (should-error-with (djira--process-response-buffer)
                                     'error '("Bad request")))

          (fpatch ((djira--get-status-code (lambda () "404")))
                  (should-error-with (djira--process-response-buffer) 'error '("Endpoint not found")))

          (fpatch ((djira--get-status-code (lambda () "500")))
                  (should-error-with (djira--process-response-buffer) 'error '("Error calling endpoint")))

          (fpatch ((djira--get-status-code (lambda () "123")))
                  (should-error-with (djira--process-response-buffer) 'error '("Unsupported status-code: 123")))))


;;;                                 _                    _
;;;  _ __ ___  __ _ _   _  ___  ___| |_    ___ __ _  ___| |__   ___
;;; | '__/ _ \/ _` | | | |/ _ \/ __| __|  / __/ _` |/ __| '_ \ / _ \
;;; | | |  __/ (_| | |_| |  __/\__ \ |_  | (_| (_| | (__| | | |  __/
;;; |_|  \___|\__, |\__,_|\___||___/\__|  \___\__,_|\___|_| |_|\___|
;;;              |_|

(defmacro with-djira--request-cache (&rest body)
  `(let ((djira--request-cache (make-hash-table :test 'equal)))
     ,@body))

(ert-deftest test-with-djira--request-cache-creates-empty-hash-table ()
  "Just to make me more confident as I keep learning."
  (with-djira--request-cache
   (should (equal (type-of djira--request-cache) 'hash-table))
   (should (= (hash-table-count djira--request-cache) 0))))

(ert-deftest test-with-djira--request-cache-discards-changes ()
  "Just to make me more confident as I keep learning."
  (with-djira--request-cache
   (puthash "key" "value" djira--request-cache)
   (should (= (hash-table-count djira--request-cache) 1)))
  (should (= (hash-table-count djira--request-cache) 0)))


(ert-deftest test-djira--cache-invalidate ()
  "asdf"
  (with-djira--request-cache
   (puthash "key" "value" djira--request-cache)
   (should (= (hash-table-count djira--request-cache) 1)) ; just to be safe
   (djira--cache-invalidate)
   (should (= (hash-table-count djira--request-cache) 0))))


(ert-deftest test-djira--cache-put ()
  "asdf"
  (with-djira--request-cache
   (djira--cache-put "key" "first")
   (should (= (hash-table-count djira--request-cache) 1))
   (should (equal (gethash "key" djira--request-cache) "first"))
   (djira--cache-put "key" "second")
   (should (= (hash-table-count djira--request-cache) 1))
   (should (equal (gethash "key" djira--request-cache) "second"))))


(ert-deftest test-djira--cache-get ()
  "asdf"
  (with-djira--request-cache
   (should (null (djira--cache-get "key")))
   (djira--cache-put "key" "first")
   (should (equal (djira--cache-get "key") "first"))))


(ert-deftest test-djira--cache-contains ()
  "asdf"
  (with-djira--request-cache
   (should (null (djira--cache-contains "key")))
   (djira--cache-put "key" "first")
   (should (djira--cache-contains "key"))))


;;;  tests-emacs-djira.el ends here
