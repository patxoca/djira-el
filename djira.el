;;; djira.el --- djira client  -*- lexical-binding: t; -*-

;; $Id:$

;; Emacs List Archive Entry
;; Filename: djira.el
;; Version: 0.1.0
;; Keywords: convenience
;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Created: 2018-08-23
;; Description:
;; URL: https://github.com/patxoca/djira-el
;; Compatibility: Emacs24
;; Package-Requires: ((dash "2.12.0") (emacs "24") (s "1.12.0"))

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html

;;; Install:

;; Put this file on your Emacs-Lisp load path and add following into
;; emacs startup file.
;;
;;     (require 'djira)
;;

;;; Commentary:

;; Emacs client for djira. djira provides introspection for a running
;; django instance.
;;
;; Currently djira-el is just a library, see the package django-el
;; (https://github.com/patxoca/django-el) for an actual package
;; leveraging djira.

;;; History:
;;

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)

(require 'dash)
(require 's)


(defvar djira-url "http://localhost:8000/__djira__/"
  "Root URL for djira's API.")


;;;  _          _
;;; | |__   ___| |_ __   ___ _ __ ___
;;; | '_ \ / _ \ | '_ \ / _ \ '__/ __|
;;; | | | |  __/ | |_) |  __/ |  \__ \
;;; |_| |_|\___|_| .__/ \___|_|  |___/
;;;              |_|

(defsubst djira--array-to-list (s)
  "Convert the array S to a list."
  (mapcar (lambda (x) x) s))


;;;                                 _                    _
;;;  _ __ ___  __ _ _   _  ___  ___| |_    ___ __ _  ___| |__   ___
;;; | '__/ _ \/ _` | | | |/ _ \/ __| __|  / __/ _` |/ __| '_ \ / _ \
;;; | | |  __/ (_| | |_| |  __/\__ \ |_  | (_| (_| | (__| | | |  __/
;;; |_|  \___|\__, |\__,_|\___||___/\__|  \___\__,_|\___|_| |_|\___|
;;;              |_|

(defvar djira--request-cache (make-hash-table :test 'equal)
  "Cache table storing requests.")

(defun djira--cache-invalidate ()
  "Invalidate the contents of the cache."
  (clrhash djira--request-cache))

(defun djira--cache-put (url value)
  "Associate VALUE to URL in the cache."
  (puthash url value djira--request-cache))

(defun djira--cache-get (url)
  "Retrieve the value associated to URL from the cache."
  (gethash url djira--request-cache nil))

(defun djira--cache-contains (url)
  "Check whether URL is in the cache."
  (let ((marker '("marker")))
    (not (eq (gethash url djira--request-cache marker) marker))))


;;;      _  _ _                  _ _            _
;;;   __| |(_|_)_ __ __ _    ___| (_) ___ _ __ | |_
;;;  / _` || | | '__/ _` |  / __| | |/ _ \ '_ \| __|
;;; | (_| || | | | | (_| | | (__| | |  __/ | | | |_
;;;  \__,_|/ |_|_|  \__,_|  \___|_|_|\___|_| |_|\__|
;;;      |__/

;; The code in this section handles communication with the djira
;; service. The main entry point is `djira-call'.


(defun djira--make-url (endpoint query-string)
  "Make the url for ENDPOINT and QUERY-STRING."
  (concat
   (s-chop-suffix "/" djira-url) "/"
   (s-chop-suffix "/" endpoint) "/"
   (unless (string= query-string "")
     (concat "?" query-string))))


(defun djira--make-query-string (kwargs)
  "Make a query string from KWARGS.

It handles appropriately booleans and lists. Other types are
converted to strings using the `format' function.

The returned value is `url-hexified' so that it can be used
safely in an URL."
  (let ((res ()))
    ;; convert (:foo "bar" :baz 3) -> (("baz" 3) ("foo" "bar"))
    ;; handles booleans (:foo t) -> (("foo" "true"))
    ;; and lists (:foo (1 2)) -> (("foo" 2) ("foo" 1))
    (while kwargs
      (let ((kw (s-chop-prefix ":" (symbol-name (car kwargs))))
            (val (cadr kwargs)))
        (cond
         ((listp val)
          (mapc (lambda (x) (push (list kw x) res)) val))
         ((booleanp val)
          (push (list kw (if val "true" "false")) res))
         (t
          (push (list kw val) res))))
      (setq kwargs (cdr (cdr kwargs))))
    ;; convert (("baz" 3) ("foo" "bar")) -> "foo=bar&baz=3"
    (mapconcat
     (lambda (x) (format "%s=%s"
                    (car x)
                    (url-hexify-string (format "%s" (cadr x)))))
     (reverse res)
     "&")))


(defmacro djira--with-response-buffer (buffer &rest body)
  "Execute BODY in the context of the response buffer BUFFER.

When processing the response, guarantees an initial state and
preserves 'current-buffer', 'point', 'mark' and 'match-data'."
  `(with-current-buffer (or ,buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        ,@body))))


(defun djira--get-status-code (&optional buffer)
  "Get HTTP response status from BUFFER."
  (djira--with-response-buffer
   buffer
   (if (looking-at "HTTP/[[:digit:]]+\.[[:digit:]]+ \\([[:digit:]]\\{3\\}\\) .*$")
       (match-string 1)
     (error "Unsupported HTTP response format"))))


(defun djira--get-content-type (&optional buffer)
  "Get HTTP Content-Type from BUFFER."
  (djira--with-response-buffer
   buffer
   (if (search-forward-regexp "^Content-Type: \\([^ ]+\\)$" nil t)
       (match-string 1)
     (error "Unsupported HTTP Content-Type format"))))


(defun djira--get-payload (&optional buffer)
  "Get HTTP payload from BUFFER."
  (djira--with-response-buffer
   buffer
   (if (search-forward-regexp "^$" nil t)
       (buffer-substring-no-properties (1+ (point)) (point-max))
     "")))


(defun djira--parse-json (v)
  "Convert V to Emacs Lisp data."
  (json-read-from-string v))


(defun djira--process-response-buffer ()
  "Process the djira response from the current buffer.

Return the payload converted to Emacs Lisp types."
  (let ((status-code (djira--get-status-code))
        (content-type (djira--get-content-type))
        (payload (djira--get-payload)))
    (cond
     ((string= status-code "200")
      (if (not (string= content-type "application/json"))
          (error "Unexpected content-type: %s" content-type)
        (djira--parse-json payload)))
     ((string= status-code "400")
      ;; TODO: the payload carries details, in json, about the
      ;; error
      (error "Bad request"))
     ((string= status-code "404")
      (error "Endpoint not found"))
     ((string= status-code "500")
      ;; TODO: the payload carries details, in json, about the
      ;; error
      (error "Error calling endpoint"))
     (t
      (error "Unsupported status-code: %s" status-code)))))


(defun djira--call (url)
  "Call the endpoint at URL and return the parsed result."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("X-REQUESTED-WITH" . "XMLHttpRequest"))))
    (with-current-buffer (url-retrieve-synchronously url)
      (djira--process-response-buffer))))


(defun djira-call (endpoint skip-cache &rest kwargs)
  "Call the endpoint ENDPOINT and retuns the result.

If SKIP-CACHE is non nil the call goes directly to djira and the
result is not cached, otherwise the result is retrieved through
the cache.

The key used for indexing the cache ís the URL. The problem is
that two different URLs may be equivalent:

  http://foo/?bar=1&baz=2
  http://foo/?baz=2&bar=1

The simplest solution is creating a thin wrapper around
`djira-call' for each endpoint in order to provide a simpler
interface for client code and standardize the format of the
resulting URL.

Sorting KWARGS alphabetically seems a good idea but currently I
prefer not to inforce that in `djira--make-query-string' (list
arguments :foo 1 :foo 2)."
  (let ((url (djira--make-url endpoint (djira--make-query-string kwargs))))
    (if skip-cache
        (djira--call url)
      (unless (djira--cache-contains url)
        (djira--cache-put url (djira--call url)))
      (djira--cache-get url))))


;;;      _  _ _                _    ____ ___
;;;   __| |(_|_)_ __ __ _     / \  |  _ \_ _|
;;;  / _` || | | '__/ _` |   / _ \ | |_) | |
;;; | (_| || | | | | (_| |  / ___ \|  __/| |
;;;  \__,_|/ |_|_|  \__,_| /_/   \_\_|  |___|
;;;      |__/

;;; This section defines functions that hide the details of djira.
;;; Each function maps to exactly one endpoint.

(defun djira-api-ping ()
  "Call '__ping__' endpoint."
  (string= (djira-call "__ping__" t) "pong"))

(defun djira-api-get-apps-list ()
  "Call 'get_apps_list' endpoint."
  (djira--array-to-list (djira-call "get_apps_list" nil)))

(defun djira-api-get-apps-details (&optional labels)
  "Call 'get_apps_details' endpoint.

If provided LABELS must be a sequence of strings corresponding to
the labels of the apps we are interested in getting information
from. If omitted it will return information about all apps."
  ;; In order to play nicely with the cache we process each label
  ;; separately instead of leveragin that "get_apps_details" accepts a
  ;; list as argument.
  (mapcar
   (lambda (x) (car (djira-call "get_apps_details" nil :labels x)))
   (or labels (djira-api-get-apps-list))))

(defun djira-api-get-system-info ()
  "Call 'get_system_info' endpoint."
  (djira-call "get_system_info" nil))

(defun djira-api-get-urls-details ()
  "Call 'get_urls_details' endpoint."
  (djira-call "get_urls_details" nil))


;;;      _  _ _             _        __
;;;   __| |(_|_)_ __ __ _  (_)_ __  / _| ___
;;;  / _` || | | '__/ _` | | | '_ \| |_ / _ \
;;; | (_| || | | | | (_| | | | | | |  _| (_) |
;;;  \__,_|/ |_|_|  \__,_| |_|_| |_|_|  \___/
;;;      |__/

;;; Convenience functions built on top of djira-api-xxx. These
;;; functions simplify getting specific information out of djira data
;;; structures.

(defun djira-info-get-project-root ()
  "Return the root of the django project.

In this context 'project' refers to the directory containing the
'manage.py' command."
  (cdr (assoc 'django_project_root (djira-api-get-system-info))))

(defun djira-info-get-settings-path ()
  "Return the 'settings.py' path."
  (cdr (assoc 'django_settings_path (djira-api-get-system-info))))

(defun djira-info-get-all-apps-labels ()
  "Return a list containing the labels of all installed apps."
  (djira-api-get-apps-list))

(defun djira-info-get-app-root (label)
  "Return the root of a django app LABEL.

In this context the root of an app is the directory containing
'models.py' etc."
  (cdr (assoc 'path (cdar (djira-api-get-apps-details (list label))))))

(defun djira-info-get-app-class-source (label)
  "Return the location of the AppConfig class.

Return a list (full-path lineno class-name) identifying the
location of the 'AppConfig' class for the application LABEL. If
the app don't define an 'AppConfig' class returns nil."
  (let* ((app-info (cdar (djira-api-get-apps-details (list label))))
         (app-root (djira-info-get-app-root label))
         (class-path (cdr (assoc 'app_class_source app-info))))
    (when (s-starts-with-p app-root class-path)
      (list class-path
            (cdr (assoc 'app_class_line app-info))
            (cdr (assoc 'app_class_name app-info))))))

(defun djira-info-get-all-apps-paths ()
  "Return the root fo all django apps.

The returned value is an alist mapping app labels to app roots."
  (mapcar (lambda (x) (cons x (djira-info-get-app-root x)))
          (djira-info-get-all-apps-labels)))

(defun djira-info-get-app-models (label)
  "Return the names of the models defined in the app LABEL.

Names are qualified with the label of the app.

Django defines both 'the name' and 'the object name' of a model.
This function returns the object model, that's to say, the name
of the class."
  (mapcar
   (lambda (x) (concat label "." x))
   (cdr (assoc 'models (cdar (djira-api-get-apps-details (list label)))))))

(defun djira-info-get-all-apps-models ()
  "Return the names of the models defined in all the apps.

Names are qualified with the label of the app."
  (cl-reduce #'append (mapcar 'djira-info-get-app-models
                              (djira-info-get-all-apps-labels))))

(defun djira-info-get-url-names ()
  "Return the names of all the urls."
  (sort
   (-non-nil
    (mapcar (lambda (x) (cdr (assoc 'url_name (cdr x))))
            (djira-api-get-urls-details)))
   #'string-lessp))

(defun djira-info-get-url-pattern-name-alist ()
  "Return an alist (pattern . name) of all the urls, sorted py
pattern."
  (sort
   (-non-nil
    (mapcar (lambda (x) (cons (cdr (assoc 'pattern (cdr x)))
                         (cdr (assoc 'url_name (cdr x)))))
            (djira-api-get-urls-details)))
   (lambda (a b) (string-lessp (car a) (car b)))))

(defun djira-info-get-view-source (name)
  "Return the location of the view.

Return a list (full-path lineno view-name) identifying the
location of the view named NAME, an string. Returns nil if the
view don't exist."
  (let ((view-info (cdr (assoc (intern name) (djira-api-get-urls-details)))))
    (when view-info
      (list (cdr (assoc 'callback_path view-info))
            (cdr (assoc 'callback_lineno view-info))
            (cdr (assoc 'callback_name view-info))))))

;;;  _          _                                           _
;;; | |_ ___   | |__   ___   _ __   __ _ _ __ ___   ___  __| |
;;; | __/ _ \  | '_ \ / _ \ | '_ \ / _` | '_ ` _ \ / _ \/ _` |
;;; | || (_) | | |_) |  __/ | | | | (_| | | | | | |  __/ (_| |
;;;  \__\___/  |_.__/ \___| |_| |_|\__,_|_| |_| |_|\___|\__,_|

;;; This section defines helper functions and end user commands

(defun djira-buffer-belongs-in-app-p (buffer)
  "Check whether BUFFER belongs to any app.

Check whether BUFFER is visiting a buffer that belongs to any app
in the running django instance. Returns the app label or nil.

Note that 'setup.py' etc. are considered to be outside the app."
  (let ((filename (buffer-file-name buffer)))
    (when filename
      (car (-first
            (lambda (x) (s-starts-with-p (cdr x) filename))
            (djira-info-get-all-apps-paths))))))

(defalias 'djira-get-app-for-buffer 'djira-buffer-belongs-in-app-p)


(provide 'djira)

;;; djira.el ends here
