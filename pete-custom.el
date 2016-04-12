(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "tomato2" "SpringGreen2" "gold" "deep sky blue" "violet" "cyan" "white"])
 '(auto-dim-other-buffers-dim-on-focus-out nil)
 '(backup-directory-alist (quote ((".*" . "~/emacs_backups"))))
 '(c-basic-offset 4)
 '(comint-prompt-read-only t)
 '(custom-enabled-themes (quote (pete-override)))
 '(custom-file "~/.emacs.d/pete-custom.el")
 '(custom-safe-themes
   (quote
    ("34ba29daaf375bb18e1b87fdc59c9ba7cf115c78dea494c747031ff23f221808" "6734848f4b7dd51763d65cf902fc4bb8322242977fd573799202eba0c7b1385e" "854a23983c24f8870fa1a68a3e475b211b932f183563cf5c63f9bb3e181e415c" "02981abbf7a0720af511b6c639e36d93e00eea5f3f60b22424349a484cc2a020" default)))
 '(desktop-path (quote ("~/.emacs.desktop")))
 '(desktop-restore-eager 25)
 '(desktop-save-mode t)
 '(display-time-mode t)
 '(emulate-mac-british-keyboard-mode t)
 '(enable-recursive-minibuffers t)
 '(eval-expression-print-length 100)
 '(even-window-heights nil)
 '(fill-column 98)
 '(gnus-init-file "~/.emacs.d/gnus")
 '(gnus-startup-file "~/.emacs.d/newsrc")
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(ido-max-directory-size 80000)
 '(inhibit-startup-screen t)
 '(lua-indent-level 2)
 '(lua-prefix-key "C-c")
 '(minibuffer-depth-indicate-mode t)
 '(minibuffer-prompt-properties
   (quote
    (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification " " mode-line-position
     (vc-mode vc-mode)
     " " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(mode-line-frame-format (quote ("" org-mode-line-string "
" rcirc-activity-string)))
 '(mouse-wheel-mode t)
 '(mweb-tags
   (quote
    (\(\(php-mode\ \"<\\\\\?php\\|<\\\\\?\ \\|<\\\\\?=\"\ \"\\\\\?>\"\)\
     \ \ \ \ \(js-mode\ \"<script\[^>\]*>\"\ \"</script>\"\)\
     \ \ \ \ \(css-mode\ \"<style\[^>\]*>\"\ \"</style>\"\)\))) t)
 '(next-screen-context-lines 10)
 '(ns-alternate-modifier (quote super))
 '(ns-command-modifier (quote meta))
 '(org-agenda-window-setup (quote current-window))
 '(org-babel-load-languages (quote ((sql . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-enforce-todo-dependencies t)
 '(org-export-select-tags (quote ("MOBILE")))
 '(org-icalendar-include-todo nil)
 '(org-icalendar-store-UID t)
 '(org-indirect-buffer-display (quote current-window))
 '(org-log-done (quote time))
 '(org-lowest-priority 68)
 '(org-priority-faces
   (quote
    ((65 . "(:foreground \"red\")")
     (66 . "")
     (67 . "")
     (68 . ""))))
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(org-src-fontify-natively t)
 '(org-use-fast-todo-selection t)
 '(org-yank-adjusted-subtrees t)
 '(parens-require-spaces nil)
 '(savehist-mode t)
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(speedbar-frame-parameters
   (quote
    ((minibuffer)
     (width . 20)
     (border-width . 0)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (unsplittable . t)
     (set-background-color "black"))))
 '(sql-connection-alist
   (quote
    (("dev"
      (sql-product
       (quote postgres))
      (sql-server "exampleserver")
      (sql-database "exampledb")
      (sql-port 5433)))))
 '(sql-postgres-program "psql")
 '(transient-mark-mode nil)
 '(w32-system-shells
   (quote
    ("cmd" "cmd.exe" "command" "command.com" "4nt" "4nt.exe" "4dos" "4dos.exe" "tcc" "tcc.exe" "ndos" "ndos.exe" "powershell.exe")))
 '(which-func-modes
   (quote
    (emacs-lisp-mode c-mode c++-mode perl-mode cperl-mode makefile-mode sh-mode fortran-mode python-mode)))
 '(which-function-mode t)
 '(winner-mode t nil (winner))
 '(woman-use-own-frame nil))

 (unless (fboundp 'auto-detect-longlines) (defun auto-detect-longlines () t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#0C0d13"))))
 '(rcirc-server ((t (:foreground "gray42")))))
