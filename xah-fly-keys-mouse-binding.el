;; -*- coding: utf-8 -*-
;; mouse settings.
;; Xah Lee
;; created: 2015-06-24

;; Emacs: How to Set Mouse Buttons ＆ Wheel
;; http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html


;; mouse

;; set the “forward button” (5th button) to close.
(cond
 ((string-equal system-type "gnu/linux")
  (progn
    (global-set-key (kbd "<mouse-3>") 'xah-mouse-click-to-search) ; right button

    (global-set-key (kbd "<S-mouse-4>") 'xah-cursor-up-25-lines)
    (global-set-key (kbd "<S-mouse-5>") 'xah-cursor-down-25-lines)

    (global-set-key (kbd "<C-mouse-4>") 'xah-backward-block)
    (global-set-key (kbd "<C-mouse-5>") 'xah-forward-block)))
 ((string-equal system-type "windows-nt") ; Windows
  (progn
    ))
 ((string-equal system-type "darwin") ; Mac
  (progn )))
