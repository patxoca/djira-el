;;; djira.el --- djira client for emacs

;; $Id:$

;; Emacs List Archive Entry
;; Filename: djira.el
;; Version: $Revision:$
;; Keywords:
;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Created: 2018-08-23
;; Description:
;; URL:
;; Compatibility: Emacs24

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
;; or use autoload:
;;
;;      (autoload 'djira-mode "djira" "" t)

;;; Commentary:
;;


;;; History:
;;

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)

(require 'dash)
(require 's)


(defgroup djira nil
  "Insert documentation here.")

(defcustom djira-url "http://localhost:8000/__djira__/"
  "djira API root URL."
  :group 'djira
  :type  'string
  :safe  'stringp)


;;;  _          _
;;; | |__   ___| |_ __   ___ _ __ ___
;;; | '_ \ / _ \ | '_ \ / _ \ '__/ __|
;;; | | | |  __/ | |_) |  __/ |  \__ \
;;; |_| |_|\___|_| .__/ \___|_|  |___/
;;;              |_|

(defsubst djira--array-to-list (s)
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
  (clrhash djira--request-cache))

(defun djira--cache-put (url value)
  (puthash url value djira--request-cache))

(defun djira--cache-get (url)
  (gethash url djira--request-cache nil))

(defun djira--cache-contains (url)
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
  (concat
   (s-chop-suffix "/" djira-url) "/"
   (s-chop-suffix "/" endpoint) "/"
   (unless (string= query-string "")
     (concat "?" query-string))))


(defun djira--make-query-string (kwargs)
  "Make a query string form a list of `:keyword value'.

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
  `(with-current-buffer (or ,buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        ,@body))))


(defun djira--get-status-code (&optional buffer)
  (djira--with-response-buffer
   buffer
   (if (looking-at "HTTP/[[:digit:]]+\.[[:digit:]]+ \\([[:digit:]]\\{3\\}\\) .*$")
       (match-string 1)
     (error "Unsupported HTTP response format"))))


(defun djira--get-content-type (&optional buffer)
  (djira--with-response-buffer
   buffer
   (if (search-forward-regexp "^Content-Type: \\([^ ]+\\)$" nil t)
       (match-string 1)
     (error "Unsupported HTTP Content-Type format"))))


(defun djira--get-payload (&optional buffer)
  (djira--with-response-buffer
   buffer
   (if (search-forward-regexp "^$" nil t)
       (buffer-substring-no-properties (1+ (point)) (point-max))
     "")))


(defun djira--parse-json (v)
  (json-read-from-string v))


(defun djira--process-response-buffer ()
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
  (with-current-buffer (url-retrieve-synchronously url)
    (djira--process-response-buffer)))


(defun djira-call (endpoint skip-cache &rest kwargs)
  "Call the endpoint and retuns the result.

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
  (string= (djira-call "__ping__" t) "pong"))

(defun djira-api-get-apps-list ()
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
  (djira-call "get_system_info" nil))


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
  "Return the root of the django project, that's to say, the path
of the directory containing the 'manage.py' command."
  (cdr (assoc 'django_project_root (djira-api-get-system-info))))

(defun djira-info-get-all-apps-labels ()
  "Return a list containing the labels of all installed apps."
  (djira-api-get-apps-list))

(defun djira-info-get-app-path (label)
  "Return the root of a django app, that's to say, the path of
the directory containing 'models.py' etc."
  (cdr (assoc 'path (cdar (djira-api-get-apps-details (list label))))))

(defun djira-info-get-all-apps-paths ()
  "Return the root fo all django apps. The returned value is an
alist mapping app labels to app roots."
  (mapcar (lambda (x) (cons x (djira-info-get-app-path x)))
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
  "Returns the names of the models defined in all the apps.
Names are qualified with the label of the app."
  (reduce 'append (mapcar 'djira-info-get-app-models
                          (djira-info-get-all-apps-labels))))


;;;  _          _                                           _
;;; | |_ ___   | |__   ___   _ __   __ _ _ __ ___   ___  __| |
;;; | __/ _ \  | '_ \ / _ \ | '_ \ / _` | '_ ` _ \ / _ \/ _` |
;;; | || (_) | | |_) |  __/ | | | | (_| | | | | | |  __/ (_| |
;;;  \__\___/  |_.__/ \___| |_| |_|\__,_|_| |_| |_|\___|\__,_|

;;; This section defines helper functions and end user commands

(defun djira-buffer-belongs-in-app-p (buffer)
  "Check whether BUFFER visits a buffer that belongs to any app
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
