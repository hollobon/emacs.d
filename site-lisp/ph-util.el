;;; ph-util.el --- utility functions and macros specific to me

;; Copyright (C) 2006-2016 Pete Hollobon

;; Author: Pete Hollobon <emacs@hollobon.com>
;; Created: 7 Aug 2006
;; Keywords: utils

;;; Code:

(defmacro defdbcomint (name docstring instance-alist command product)
  "Macro to define an interactive function for creating a comint buffer with a shortcut"
  `(defun ,name ()
     ,docstring
     (interactive)

     (let
         ((instance-pair (assoc (completing-read "Instance: " ,instance-alist nil t)
                                ,instance-alist)))
       (when instance-pair
         (switch-to-buffer
          (apply #'make-comint (concat ,command "-" (car instance-pair))
                 ,command
                 nil
                 (if (listp (cdr instance-pair))
                     (cdr instance-pair)
                   (list (cdr instance-pair)))))
         (setq sql-interactive-product ,product)
         (setq sql-buffer (current-buffer))
         (sql-interactive-mode)))))

(defun create-dir-tree (tree s)
  "Flattens a directory tree list into a list of strings representing the directory at each node"
  (if tree
      (let ((path (if (equal "" s) "" (concat s "/"))))
        (cons (concat path (car tree))
              (if (listp (cadr tree)) ;; subdirectory
                  (append
                   (create-dir-tree (cadr tree) (concat path (car tree)))
                   (create-dir-tree (cddr tree) s))
                (create-dir-tree (cdr tree) s))))
    '()))

(defun location ()
  (if (string/starts-with (system-name) "REN")
      'work
    'home))

(defmacro when-at (&rest preds)
  "Macro for doing stuff depending on location" ; relies on variable capture, should be fixed.
  `(let ((work "example.*\\.com")
         (home "hollobon\.com"))
     (cond ,@(mapcar
              (lambda (pred) `((string-match ,(car pred) (system-name)) ,(cadr pred)))
              preds))))

(defmacro when-require (package &rest body)
  "Executes `body` if `package` is successfully loaded"

  `(when (require ,package nil t)
     ,@body))

(put 'when-require 'lisp-indent-function 1)

(defmacro on-windows (&rest body)
  `(when (eq system-type 'windows-nt)
     ,@body))

(defmacro on-mac (&rest body)
  `(when (eq system-type 'darwin)
     ,@body))

(defun copy-buffer-filename ()
  "copy the current buffer's filename to the kill ring"

  (interactive)
  (kill-new buffer-file-name))

(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end)))

(defun pretty-print-xml-line ()
  "Pretty print XML line"
  (interactive)
  (pretty-print-xml-region (line-beginning-position) (line-end-position)))

(defun explore ()
  "Show file in Windows Explorer"

  (interactive)
  (w32-shell-execute nil "c:\\users\\phollobon_ren\\bin\\explore_file.cmd"
		     (replace-regexp-in-string "/" "\\\\" (buffer-file-name))
		     0))

(defun toggle-identifier-naming-style ()
  "toggles the symbol at point between c-style naming,
e.g. `hello_world_string', and camel case,
e.g. `helloworldstring'."
  (interactive)
  (let* ((symbol-pos (bounds-of-thing-at-point 'symbol))
         case-fold-search symbol-at-point cstyle regexp func)
    (unless symbol-pos
      (error "no symbol at point"))
    (save-excursion
      (narrow-to-region (car symbol-pos) (cdr symbol-pos))
      (setq cstyle (string-match-p "_" (buffer-string))
            regexp (if cstyle "\\(?:\\_<\\|_\\)\\(\\w\\)" "\\([A-Z]\\)")
            func (if cstyle
                     'capitalize
                   (lambda (s)
                     (concat (if (= (match-beginning 1)
                                    (car symbol-pos))
                                 ""
                               "_")
                             (downcase s)))))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (replace-match (funcall func (match-string 1))
                       t nil))
      (widen))))

(defmacro string-case (keyform &rest clauses)
  "Version of case that matches strings using `equal`. Each clause is of the form '(string body)'"

  (let ((keyform-eval-sym (gensym)))
    `(let ((,keyform-eval-sym ,keyform))
	(cond ,@(mapcar #'(lambda (clause)
			    `((equal ,keyform-eval-sym ,(car clause)) ,(cadr clause)))
			clauses)))))

(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"

  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))

(defun visible-buffers (&optional first-window window results)
  "Get a list of all buffers visible"
  (let ((w (next-window window nil t)))
    (if (equal first-window w)
        results
      (visible-buffers (if (null first-window) w first-window)
                       w (cons (window-buffer w) results)))))

(defun find-file-or-switch-and-focus (filename)
  "If `filename` is already open, focus buffer's frame and window, otherwise open and focus"
  (let ((visible-buffer (memq (find-buffer-visiting filename) (visible-buffers))))
    (if visible-buffer
        (select-window (get-buffer-window (car visible-buffer)))
      (find-file filename)))
  (select-frame-set-input-focus (selected-frame)))

;; Org stuff

(defun org-paste-irc ()
  "insert clipboard contents in current buffer as an example, deleting status change lines"
  (insert "<e")
  (org-try-structure-completion)
  (push-mark) 
  (yank)
  (exchange-point-and-mark)
  (org-indent-block)
  (exchange-point-and-mark)
  (flush-lines " \\*\\*\\* " (mark) (point))
  (pop-mark))

(provide 'ph-util)
;;; ph-util.el ends here
