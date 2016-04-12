(require 'cl)

(defvar pete-packages '(deft expand-region gist haskell-mode
                         markdown-mode paredit projectile python
                         sass-mode rainbow-mode scss-mode volatile-highlights yaml-mode
                         yasnippet dired+ powershell-mode lua-mode markdown-mode mpc
                         popwin js2-mode auto-dim-other-buffers helm helm-gtags function-args
                         go-mode)
                                        ; magit magithub  mode-line-frame diredx
  "A list of packages to ensure are installed at launch.")

(require 'cl)

(defun pete-packages-installed-p ()
  (loop for p in pete-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (pete-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p pete-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'pete-packages)
;;; pete-packages.el ends here
