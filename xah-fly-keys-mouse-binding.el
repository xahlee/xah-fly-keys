;; -*- coding: utf-8 -*-
;; mouse settings.
;; Xah Lee
;; created: 2015-06-24

;; Emacs: How to Set Mouse Buttons ＆ Wheel
;; http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html


;; mouse

;; set the “forward button” (5th button) to close.
(cond
 ((string-equal system-type "windows-nt") ; Windows
  nil
  )

 ((string-equal system-type "gnu/linux")

  (global-set-key (kbd "<mouse-3>") 'xah-click-to-search) ; right button

  ;; (global-set-key (kbd "<mouse-4>") 'mwheel-scroll) ; wheel up
  ;; (global-set-key (kbd "<mouse-5>") 'mwheel-scroll) ; wheel down

  (global-set-key (kbd "<S-mouse-4>") 'xah-previous-user-buffer)
  (global-set-key (kbd "<S-mouse-5>") 'xah-next-user-buffer)

  (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease))

 ((string-equal system-type "darwin") ; Mac
  nil
  ))

(defun xah-fly-set-mouse-wheel-normal ()
  "Set mouse wheel to `mwheel-scroll'"
  (interactive)
  (progn
    (global-set-key (kbd "<mouse-4>") 'mwheel-scroll)
    (global-set-key (kbd "<mouse-5>") 'mwheel-scroll)))

(defun xah-set-mouse-wheel-mode ()
  "Set mouse wheel to move by word, line, block or other.
This command will prompt you.
When emacs is idle for 5 seconds, the normal wheel behavior will be restored."
  (interactive)
  (let (ξmode
        (ξmouse-wheel-modes
         '(
           "3 normal"
           "4 block"
           "1 line"
           "2 word"
           "5 char"
           )))
    (setq ξmode (ido-completing-read "set wheel mode to:" ξmouse-wheel-modes))
    (cond
     ((string-equal ξmode "3 normal") 'xah-fly-set-mouse-wheel-normal)

     ((string-equal ξmode "4 block")
      (progn
        (global-set-key (kbd "<mouse-4>") 'xah-beginning-of-line-or-block)
        (global-set-key (kbd "<mouse-5>") 'xah-end-of-line-or-block)))

     ((string-equal ξmode "1 line")
      (progn
        (global-set-key (kbd "<mouse-4>") 'xah-cursor-up-10-lines)
        (global-set-key (kbd "<mouse-5>") 'xah-cursor-down-10-lines)))

     ((string-equal ξmode "2 word")
      (progn
        (global-set-key (kbd "<mouse-4>") 'xah-backward-n-words)
        (global-set-key (kbd "<mouse-5>") 'xah-forward-n-words)))

     ((string-equal ξmode "2 char")
      (progn
        (global-set-key (kbd "<mouse-4>") 'xah-backward-n-chars)
        (global-set-key (kbd "<mouse-5>") 'xah-forward-n-chars)))

     (t (error "%s" "program logic error. No choice found")))

    (setq xah-fly-mouse-mode-timer-id (run-with-idle-timer 5 t 'xah-fly-set-mouse-wheel-normal))))

