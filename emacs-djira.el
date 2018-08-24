;;; emacs-djira.el --- djira client for emacs

;; $Id:$

;; Emacs List Archive Entry
;; Filename: emacs-djira.el
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
;;     (require 'emacs-djira)
;;
;; or use autoload:
;;
;;      (autoload 'emacs-djira-mode "emacs-djira" "" t)

;;; Commentary:
;;


;;; History:
;;

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)

(require 's)


(defgroup djira nil
  "Insert documentation here.")

(defcustom djira-url "http://localhost:8000/__djira__/"
  "djira API root URL."
  :group 'emacs-djira
  :type  'string
  :safe  'stringp)


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


(defun djira-call (endpoint &rest kwargs)
  "Call the endpoint and the retuns the result."
  (let ((url (djira--make-url endpoint (djira--make-query-string kwargs))))
    (with-current-buffer (url-retrieve-synchronously url)
      (djira--process-response-buffer))))


;;;      _  _ _                _    ____ ___
;;;   __| |(_|_)_ __ __ _     / \  |  _ \_ _|
;;;  / _` || | | '__/ _` |   / _ \ | |_) | |
;;; | (_| || | | | | (_| |  / ___ \|  __/| |
;;;  \__,_|/ |_|_|  \__,_| /_/   \_\_|  |___|
;;;      |__/

;;; This section defines functions that hide the details of djira.

(defun djira-api-ping ()
  (string= (djira-call "__ping__") "pong"))

(defun djira-api-get-apps-list ()
  (djira-call "get_apps_list"))

(defun djira-api-get-apps-details (&rest labels)
  (djira-call "get_apps_details" :labels labels))

(defun djira-api-get-system-info ()
  (djira-call "get_system_info"))


(provide 'emacs-djira)

;;; emacs-djira.el ends here
