;;; test-helper.el --- Helpers for djira-el-test.el

(require 'ert)

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

;;; test-helper.el ends here
