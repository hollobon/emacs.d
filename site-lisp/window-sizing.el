;;; window-sizing.el -- Window sizing and splitting.

;; Copyright (C) 2013 Peter Hollobon
;;
;; Author: Pete Hollobon <emacs@hollobon.com>
;; Maintainer: Pete Hollobon <emacs@hollobon.com>
;; Created: January, 2013
;;

(defun frame-height-full (n)
  (- (/ (display-pixel-height)
	(frame-char-height))
     20
     n))

(defun set-frame-height-full (n)
  (set-frame-height (selected-frame) (frame-height-full n)))

(defun psize ()
  "Size for two screen-height 100 column windows side-by-side with taskbar on left (Win 7)"

  (interactive)
  (set-frame-width (selected-frame) 203)
  (set-frame-height-full 0)

  (ignore-errors
    (while 1
      (delete-window)))

  (split-window-horizontally)
  (balance-windows)
  (set-frame-position (selected-frame) 60 0))

(defun vsize+1 ()
  (interactive)

  (set-frame-height (selected-frame) (+ (frame-height) 1)))

(defun vsize-1 ()
  (interactive)

  (set-frame-height (selected-frame) (- (frame-height) 1)))

(global-set-key [M-S-down] 'vsize+1)
(global-set-key [M-S-up] 'vsize-1)

(defun psize3-master (n)
  (set-frame-height-full -17)
  (set-frame-width (selected-frame) 306)

  (ignore-errors
    (while 1
      (delete-window)))

  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows)
  (set-frame-position (selected-frame) n 0))

(defun psize3 ()
  "Size for three screen-height 100 column windows side-by-side on the current monitor"
  (interactive)
  (if (not (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))
      (toggle-frame-fullscreen))
  (psize3-master (cadr (assoc 'workarea (frame-monitor-attributes)))))

(defun size100 ()
  "Make current window 100 wide"

  (interactive)
  (enlarge-window-horizontally (- 100 (window-width))))

(defun psize1 ()
  "One 100 column window"

  (interactive)
  (set-frame-width (selected-frame) 102)
  (set-frame-position (selected-frame) (frame-parameter (selected-frame) 'left) 0)
  (set-frame-height-full 2))

(defun psize1r ()
  "One 100 column window on right of left monitor"

  (interactive)
  (set-frame-width (selected-frame) 102)
  (set-frame-position (selected-frame) (- (display-pixel-width)
                                          (frame-pixel-width)
                                          8) 0)
  (set-frame-height-full 0))

(defun left-monitor ()
  "Move window to left monitor"
  (interactive)
  (set-frame-position (selected-frame) 56 0))

(defun right-monitor ()
  "Move window to right monitor"
  (interactive)
  (set-frame-position (selected-frame) (display-pixel-width) 0))

(defun move-to-monitor ()
  "Move window to selected monitor"
  
  (interactive)
  (let* ((display-names (mapcar (lambda (x)
                                  (cdr (assoc 'name x)))
                                (display-monitor-attributes-list)))
         (display (completing-read "Display:" display-names))
         (selected-display-attributes (car (cl-remove-if-not
                                            (lambda (attr) (equal (cdr (assoc 'name attr))
                                                                  display))
                                            (display-monitor-attributes-list)))))
    (destructuring-bind (_ left top right bottom) (assoc 'workarea selected-display-attributes)
      (set-frame-position (selected-frame) left top))))

(provide 'window-sizing)

;;; window-sizing.el ends here
