(deftheme pete-override
  "Created 2013-06-27.")

(custom-theme-set-faces
 'pete-override
 '(mode-line ((t (:box (:line-width 1 :color "DarkOrange3" :style nil) :foreground "black" :background "gray35"))))
 '(mode-line-buffer-id ((t (:weight bold :foreground "chocolate1"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style nil)))))
 '(mode-line-inactive ((t (:weight light :box (:line-width 1 :color "gray40" :style nil) :foreground "gray60" :background "gray15" :inherit (mode-line)))))
 '(which-func ((t (:foreground "chartreuse2"))))
 '(font-lock-comment-face ((t (:foreground "gray60"))))
 '(fringe ((t (:background "grey10"))))
 '(default ((t (:inherit nil :stipple nil :background "#181a26" :foreground "gray80" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 83 :width normal :foundry "outline" :family "Consolas"))))
 '(vertical-border ((((type w32 tty)) (:foreground "gray30" :inherit (mode-line-inactive))))))

(provide-theme 'pete-override)
