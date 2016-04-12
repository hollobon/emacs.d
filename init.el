;;; init.el -- GNU Emacs initialisation file.

;; Copyright (C) 2006-2016 Peter Hollobon
;;
;; Author: Pete Hollobon <emacs@hollobon.com>
;; Maintainer: Pete Hollobon <emacs@hollobon.com>
;; Created: March, 2006
;;

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(require 'ph-util)

;;;; Package Management
(require 'package)
(package-initialize)

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(add-to-list 'package-archives
	     '("marmalade" . "https://marmalade-repo.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(require 'pete-packages)

;;;; Customised settings

(load-file "~/.emacs.d/pete-custom.el")

;;;; Abbrevs

(setq-default abbrev-mode t)

;;;; Tabs

(setq-default indent-tabs-mode nil)

;;;; UI

(load-theme 'deeper-blue)
(load-theme 'pete-override)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq truncate-partial-width-windows nil)

(transient-mark-mode 0)

(set-mouse-color "white")
(set-cursor-color "white")

;;;; Scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;;;; Mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;;;; Global key bindings

(global-set-key [f5] #'ielm)
(global-set-key [f6] #'(lambda () (interactive) (switch-to-buffer "*Python*")))
(global-set-key [f7] #'(lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key [f8] #'(lambda () (interactive) (switch-to-buffer "*sql*")))
(global-set-key [f12] 'other-frame)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\C-x<" 'decrease-left-margin)
(global-set-key "\C-x>" 'increase-left-margin)

(global-unset-key "\C-z")  ; annoying binding to iconify-or-deiconify-frame.

(global-set-key "\M-\"" 'insert-pair)
(global-set-key "\M-{" 'insert-pair)
(global-set-key "\M-[" 'insert-pair)

;; Binary search up/down point movement.
(when-require 'chop
  (global-set-key "\M-p" 'chop-move-up)
  (global-set-key "\M-n" 'chop-move-down))

;;;; Aliases

(defalias 'dp 'delete-pair)
(defalias 'rr 'replace-regexp)
(defalias 'rs 'replace-string)
(defalias 'tins 'toggle-identifier-naming-style)
(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'dnm 'delete-non-matching-lines)

;;;; Misc functions

(defun db ()
  "Run diff-buffer-with-file on the current buffer"
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(defun lines-in-region ()
  "Show the number of lines in the region"
  (interactive)
  (message "%s"
           (- (line-number-at-pos (region-end))
              (line-number-at-pos (region-beginning)))))

;;;; File handling

(auto-compression-mode 1)
(auto-image-file-mode 1)

;;;; Server

(server-start)

;;;; initial frame size

(setq initial-frame-alist `((top . 1)
                            (left . 1)
                            (width . 203)
                                      (height . ,(- (/ (display-pixel-height)
                                                       (frame-char-height))
                                                    50))))

;;;; Windows / Frames

(require 'window-sizing)

(windmove-default-keybindings)

(defun ph-windmove-frame (direction)
  (if (null (windmove-find-other-window direction))
      (other-frame 1)
    (windmove-do-window-select direction)))

(defun ph-windmove-right ()
  (interactive)
  (ph-windmove-frame 'right))

(defun ph-windmove-left ()
  (interactive)
  (ph-windmove-frame 'left))

(dolist (pair '(("\M-L" . ph-windmove-right)
                ("\M-J" . windmove-down)
                ("\M-H" . ph-windmove-left)
                ("\M-K" . windmove-up)))
  (global-set-key (car pair) (cdr pair)))

(require 'popwin)
(popwin-mode 1)

;;;; point movement

;; Binary search up/down point movement.
(when-require 'chop
  (global-set-key "\M-p" 'chop-move-up)
  (global-set-key "\M-n" 'chop-move-down))

;;;; saving sessions

(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 50)
                tags-file-name
                register-alist)))

;; Save the desktop every minute or so
(run-with-idle-timer 63 t #'desktop-save-in-desktop-dir)

;;;; Python

(require 'subword)
(add-hook 'python-mode-hook #'(lambda () (subword-mode)))

(setq auto-mode-alist
      (cons '("\\.p\\(yx\\|xd\\)" . python-mode)
            auto-mode-alist))

(setq
 python-shell-interpreter "python"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;;;; Go

(add-hook 'go-mode-hook #'(lambda () 
                            (subword-mode)
                            (setq tab-width 4)))
(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)

;;;; Powershell

(when-require 'powershell-mode
  (setq auto-mode-alist
        (append (mapcar #'(lambda (x) `(,x . powershell-mode))
                        '("\\.psm1" "\\.psd1" "\\.ps1"))
                auto-mode-alist)))

(setq powershell-indent 4)

;;;; org-mode

(setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)")
                                (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "OPEN(o@)" "|"
                                          "CANCELLED(c@/!)"))))

(setq org-todo-keyword-faces (quote (("TODO" :foreground "#ff6666" :weight bold)
                                     ("STARTED" :foreground "#7788ff" :weight bold)
                                     ("DONE" :foreground "#66ff99" :weight bold)
                                     ("WAITING" :foreground "orange" :weight bold)
                                     ("SOMEDAY" :foreground "magenta" :weight bold)
                                     ("CANCELLED" :foreground "forest green" :weight bold)
                                     ("OPEN" :foreground "blue" :weight bold))))

(setf org-priority-faces '((65 . (:foreground "black" :background "yellow"))
                           (66 . (:foreground "black" :background "orange"))
                           (67 . (:foreground "yellow"))
                           (68 . (:foreground "cyan"))))

(setq org-directory "Dropbox/org")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-postgres-keywords)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sql . t)
   (python .t)
   (emacs-lisp . t)))

(setq org-src-fontify-natively t)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(defun today ()
  "Show Org-mode agenda log for today, including archives"
  (interactive)
  (progn (org-agenda-list 1) (org-agenda-log-mode) (org-agenda-archives-mode t)))

;;;; Snippets
(require 'yasnippet)
(yas-global-mode 1)

;; Add directory name to Python buffers, to make it easier to disambiguate __init__.py etc.
(defun python-buffer-name-find-file-hook ()
    (if (buffer-file-name)
        (rename-buffer (format "%s [%s]" (file-name-nondirectory buffer-file-name)
                               (car (last (split-string buffer-file-name "/") 2)))
                       t)))


(add-hook 'find-file-hook #'python-buffer-name-find-file-hook)

;;;; ediff

(setq ediff-split-window-function 'split-window-horizontally)


(load-library "mpc")
(setq mpc-status-buffer-format
  '("%-5{Time} / %{Duration} %2{Disc--}%4{Track}" "%{Title}" "%{Album}" "%{Artist}"))

;;;; css-mode

(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

;;;; Javascript

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'(lambda () (subword-mode)))

;;;; Calc window handling

(defun find-leftmost-window (window)
  (let ((left (windmove-find-other-window 'left nil window)))
    (if (null left)
        window
      (find-leftmost-window left))))

(defun display-calc-window ()
  "Hook function to always display the calc buffer on leftmost window"
  (let* ((buffer (current-buffer))
         (window (find-leftmost-window (selected-window)))
         (new-window (split-window window (- (window-height) 12) 'below)))
    (select-window new-window)
    (switch-to-buffer buffer)
    (set-window-dedicated-p new-window t)
    (set-window-buffer new-window (current-buffer))))

(add-hook 'calc-window-hook #'display-calc-window)

;; Time on modeline
(display-time-mode 1)

(defface pete-display-time
  '((((type x w32 mac))
     ;; #060525 is the background colour of my default face.
     (:foreground "#FFFA00" :inherit bold))
    (((type tty))
     (:foreground "blue")))
  "Face used to display the time in the mode line.")

;; This causes the current time in the mode line to be displayed in
;; `pete-display-time-face' to make it stand out visually.
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " ")
                    'face 'pete-display-time)))

;; mode-line-frame
(require 'mode-line-frame)
(mode-line-frame-create)

;; dired
(require 'dired+)
(require 'dired-x)

;; auto-dim-other-buffers
(add-hook 'after-init-hook (lambda ()
                             (when (fboundp 'auto-dim-other-buffers-mode)
                               (auto-dim-other-buffers-mode t))))

;; helm

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
(global-set-key (kbd "C-c h o") 'helm-occur)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(helm-mode 1)

;; helm-gtags

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; function-args

(require 'function-args)
(fa-config-default)
(define-key c-mode-map  [(control tab)] 'moo-complete)
(define-key c++-mode-map  [(control tab)] 'moo-complete)
(define-key c-mode-map (kbd "M-o")  'fa-show)
(define-key c++-mode-map (kbd "M-o")  'fa-show)

;; enable commands

(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; init.el ends here
