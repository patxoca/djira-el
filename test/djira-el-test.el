;;; djira-el-test.el --- Tests for djira-el
;;; -*- lexical-binding: t -*-

(require 'cl)
(require 'ert)

(require 'djira)

;;;  _          _
;;; | |__   ___| |_ __   ___ _ __ ___
;;; | '_ \ / _ \ | '_ \ / _ \ '__/ __|
;;; | | | |  __/ | |_) |  __/ |  \__ \
;;; |_| |_|\___|_| .__/ \___|_|  |___/
;;;              |_|

(ert-deftest test-djira--array-to-list ()
  "Just being paranoid."
  (should (listp (djira--array-to-list []))))


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


;;;      _  _ _                _    ____ ___
;;;   __| |(_|_)_ __ __ _     / \  |  _ \_ _|
;;;  / _` || | | '__/ _` |   / _ \ | |_) | |
;;; | (_| || | | | | (_| |  / ___ \|  __/| |
;;;  \__,_|/ |_|_|  \__,_| /_/   \_\_|  |___|
;;;      |__/

(ert-deftest test-djira-api-get-apps-details ()
  "Ensure that `djira-api-get-apps-details' collects info for
each app separately in order to improve cache hits."
  (let ((call-args))
    (fpatch
     ((djira-call (lambda (&rest args) (push args call-args) '("result"))))
     (djira-api-get-apps-details '("foo" "bar"))
     (should (equal (reverse call-args)
                    '(("get_apps_details" nil :labels "foo")
                      ("get_apps_details" nil :labels "bar")))))))


;;;      _  _ _             _        __
;;;   __| |(_|_)_ __ __ _  (_)_ __  / _| ___
;;;  / _` || | | '__/ _` | | | '_ \| |_ / _ \
;;; | (_| || | | | | (_| | | | | | |  _| (_) |
;;;  \__,_|/ |_|_|  \__,_| |_|_| |_|_|  \___/
;;;      |__/


(ert-deftest test-djira-info-get-project-root ()
  ""
  (let ((response '((django . "/some/path/lib/python3.6/site-packages/django")
                    (django_project_root . "/home/aroda/prog/djira/project")
                    (django_settings
                     (ABSOLUTE_URL_OVERRIDES)
                     (ADMINS . [])
                     (ALLOWED_HOSTS . [])
                     (APPEND_SLASH . t))
                    (django_settings_module . "project.settings")
                    (django_version . [1 11 15 "final" 0])
                    (python . "/some/path/bin/python")
                    (python_version . [3 6 4 "final" 0]))
                  ))
    (fpatch
     ((djira-api-get-system-info (lambda () response)))
     (should (equal (djira-info-get-project-root)
                    "/home/aroda/prog/djira/project")))))


(ert-deftest test-djira-info-get-app-root ()
  ""
  (let ((response '((admin
                     (label . "admin")
                     (models .
                             ["logentry"])
                     (name . "django.contrib.admin")
                     (path . "/some/path/django/contrib/admin")
                     (verbose_name . "Administration")))))
    (fpatch
     ((djira-api-get-apps-details (lambda (label) response)))
     (should (equal (djira-info-get-app-root "admin")
                    "/some/path/django/contrib/admin"))))

  (let ((response '((admin)))) ; read as '((admin . ())), no info available
    (fpatch
     ((djira-api-get-apps-details (lambda (label) response)))
     (should (null (djira-info-get-app-root "admin"))))))


(ert-deftest test-djira-info-get-all-apps-paths ()
  ""
  (let ((data '(("admin" . "/some/path/django/contrib/admin")
                ("foo" . "/some/path/django/contrib/foo")
                ("auth" . "/some/path/django/contrib/auth"))))
    (fpatch
     ((djira-info-get-all-apps-labels (lambda () '("admin" "auth")))
      (djira-info-get-app-root (lambda (label) (cdr (assoc label data)))))
     (should (equal (djira-info-get-all-apps-paths)
                    '(("admin" . "/some/path/django/contrib/admin")
                      ("auth" . "/some/path/django/contrib/auth")))))))


(ert-deftest test-djira-info-get-app-models ()
  ""
  (let ((response '((auth
                     (label . "auth")
                     (models . ["Group" "Permission" "User"])
                     (name . "django.contrib.auth")
                     (path . "/some/path/django/contrib/auth")
                     (verbose_name . "Authentication and Authorization")))))
    (fpatch
     ((djira-api-get-apps-details (lambda (x) response)))
     (should (equal (djira-info-get-app-models "auth")
                    '("auth.Group" "auth.Permission" "auth.User")))))

  (let ((response '((auth))))
    (fpatch
     ((djira-api-get-apps-details (lambda (x) response)))
     (should (null (djira-info-get-app-models "auth"))))))


(ert-deftest test-djira-info-get-all-apps-models ()
  ""
  (let ((data '(("foo" "foo.Model1" "foo.Model2" )
                ("bar" "bar.Model3")
                ("baz"))))
    (fpatch
     ((djira-info-get-all-apps-labels (lambda () (mapcar 'car data)))
      (djira-info-get-app-models (lambda (x) (cdr (assoc x data)))))
     (should (equal (djira-info-get-all-apps-models)
                    '("foo.Model1" "foo.Model2" "bar.Model3"))))))


(ert-deftest test-djira-info-get-settings-path ()
  ""
  (let ((data '((python_version . [2 7 14 "final" 0])
                (django_settings_module . "project.settings")
                (django_settings_path . "/some/path/project/settings.py")
                (python . "/some/path/pyvenv/hera_impl/bin/python")
                (django_project_root . "/some/path/prog/hera/django10/project")
                (django_version . [1 10 8 "final" 0])
                (django . "/some/path/pyvenv/hera_impl/lib/python2.7/site-packages/django"))))
    (fpatch
     ((djira-api-get-system-info (lambda () data)))
     (should (string= (djira-info-get-settings-path)
                      "/some/path/project/settings.py")))))


(ert-deftest test-djira-info-get-app-class-source ()
  ""
  (let ((data '((djira
                 (app_class_name . "DjiraAppConfig")
                 (app_class_source . "/some/path/site-packages/djira/app.py")
                 (name . "djira")
                 (app_class_line . 16)
                 (models . [])
                 (path . "/some/path/site-packages/djira")
                 (verbose_name . "djira")
                 (label . "djira")))))
    (fpatch
     ((djira-api-get-apps-details (lambda (x) data)))
     (should (equal (djira-info-get-app-class-source "djira")
                    '("/some/path/site-packages/djira/app.py" 16 "DjiraAppConfig"))))))


(ert-deftest test-djira-info-get-url-names ()
  ""
  (let ((data '((index
                 (url_name . "index")
                 (callback_name . "index")
                 (callback_path . "/some/path/site-packages/django/contrib/admin/sites.py")
                 (callback_lineno . 476))
                (login
                 (url_name . "login")
                 (callback_name . "login")
                 (callback_path . "/some/path/site-packages/django/contrib/admin/sites.py")
                 (callback_lineno . 361))
                (logout
                 (url_name . "logout")
                 (callback_name . "logout")
                 (callback_path . "/some/path/site-packages/django/contrib/admin/sites.py")
                 (callback_lineno . 339))
                (password_change
                 (url_name . "password_change")
                 (callback_name . "password_change")
                 (callback_path . "/some/path/site-packages/django/contrib/admin/sites.py")
                 (callback_lineno . 300)))))
    (fpatch
     ((djira-api-get-urls-details (lambda () data)))

     (should (equal (djira-info-get-url-names)
                    '("index" "login" "logout" "password_change")))

     (should (equal (djira-info-get-view-source "logout")
                    '("/some/path/site-packages/django/contrib/admin/sites.py"
                      339
                      "logout"))))))

;;;  _          _                                           _
;;; | |_ ___   | |__   ___   _ __   __ _ _ __ ___   ___  __| |
;;; | __/ _ \  | '_ \ / _ \ | '_ \ / _` | '_ ` _ \ / _ \/ _` |
;;; | || (_) | | |_) |  __/ | | | | (_| | | | | | |  __/ (_| |
;;;  \__\___/  |_.__/ \___| |_| |_|\__,_|_| |_| |_|\___|\__,_|


(ert-deftest test-djira-buffer-belongs-in-app-p ()
  ""
  (fpatch
   ((buffer-file-name (lambda (x) nil)))     ; non file buffer
   (should (null (djira-buffer-belongs-in-app-p "whatever"))))

  (let ((data '(("foo" . "/some/path/foo")
                ("bar" . "/other/path/bar"))))
    (fpatch
     ((buffer-file-name (lambda (x) "/some/path/foo/baz.txt"))
      (djira-info-get-all-apps-paths (lambda () data)))
     (should (equal (djira-buffer-belongs-in-app-p "whatever") "foo")))

    (fpatch
     ((buffer-file-name (lambda (x) "/third/path/baz.txt"))
      (djira-info-get-all-apps-paths (lambda () data)))
     (should (null (djira-buffer-belongs-in-app-p "whatever"))))))


;;;  djira-tests.el ends here

;;; djira-el-test.el ends here
