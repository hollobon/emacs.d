(deftheme pete
  "Pete's custom theme")

(custom-theme-set-faces
 'pete
 '(default ((t (:inherit nil :stipple nil :background "#181a26" :foreground "gray80" :inverse-video nil :box nil))))
 '(mode-line ((t (:background "#995500" :foreground "#"))))
 '(error ((t (:background "red4" :foreground "Pink" :weight bold))))
 '(highlight ((t (:background "gray16"))))
 '(link ((t (:foreground "cyan3" :underline t))))
 '(mode-line ((t (:background "gray75" :foreground "black"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "grey30" :foreground "grey80" :weight light))))
 '(mode-line-highlight ((t nil)))
 '(org-date ((t (:foreground "dark orange" :underline nil))))
 '(rst-level-1-face ((t (:background "grey20"))) t))

(provide-theme 'pete)
