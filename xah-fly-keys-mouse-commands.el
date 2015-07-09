;; -*- coding: utf-8 -*-

(defun xah-mouse-click-to-search (φclick)
  "Mouse click to start `isearch-forward-symbol-at-point' (emacs 24.4) at clicked point.
URL `http://ergoemacs.org/emacs/emacs_mouse_click_highlight_word.html'
Version 2015-04-22"
  (interactive "e")
  (let ((p1 (posn-point (event-start φclick))))
    (goto-char p1)
    (isearch-forward-symbol-at-point)))

(defun xah-click-describe-char (φclick)
  "Mouse click to `describe-char' at clicked point.
URL `http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html'
Version 2015-04-22"
  (interactive "e")
  (let ((p1 (posn-point (event-start φclick))))
    (goto-char p1)
    (describe-char p1)))

(defun xah-set-mouse-wheel-normal ()
  "Set mouse wheel to `mwheel-scroll'"
  (interactive)
  (cond
   ((string-equal system-type "gnu/linux")
    (progn
      (global-set-key (kbd "<mouse-4>") 'mwheel-scroll)
      (global-set-key (kbd "<mouse-5>") 'mwheel-scroll)))
   ((string-equal system-type "windows-nt") ; Windows
    (progn
      (global-set-key (kbd "<wheel-up>") 'mwheel-scroll)
      (global-set-key (kbd "<wheel-down>") 'mwheel-scroll)))
   ((string-equal system-type "darwin") ; Mac
    (progn
      (global-set-key (kbd "<wheel-up>") 'mwheel-scroll)
      (global-set-key (kbd "<wheel-down>") 'mwheel-scroll))))
  (message "Mouse wheel set to normal"))

(defun xah-set-mouse-scroll-by-50-line ()
  "Set mouse wheel to move cursor by n lines.
Version 2015-07-06"
  (interactive)
  (cond
   ((string-equal system-type "gnu/linux")
    (progn
      (global-set-key (kbd "<mouse-4>") 'xah-cursor-up-50-lines)
      (global-set-key (kbd "<mouse-5>") 'xah-cursor-down-50-lines)))
   ((string-equal system-type "windows-nt") ; Windows
    (progn
      (global-set-key (kbd "<wheel-up>") 'xah-cursor-up-50-lines)
      (global-set-key (kbd "<wheel-down>") 'xah-cursor-down-50-lines)))
   ((string-equal system-type "darwin") ; Mac
    (progn
      (global-set-key (kbd "<wheel-up>") 'xah-cursor-up-50-lines)
      (global-set-key (kbd "<wheel-down>") 'xah-cursor-down-50-lines))))
  (message "Mouse wheel set to move by 50 lines."))

(defun xah-set-mouse-scroll-by-block ()
  "Set mouse wheel to scroll by text block.
Version 2015-07-06"
  (interactive)
  (cond
   ((string-equal system-type "gnu/linux")
    (progn
      (global-set-key (kbd "<mouse-4>") 'xah-backward-block)
      (global-set-key (kbd "<mouse-5>") 'xah-forward-block)))
   ((string-equal system-type "windows-nt") ; Windows
    (progn
      (global-set-key (kbd "<wheel-up>") 'xah-backward-block)
      (global-set-key (kbd "<wheel-down>") 'xah-forward-block)))
   ((string-equal system-type "darwin") ; Mac
    (progn
      (global-set-key (kbd "<wheel-up>") 'xah-backward-block)
      (global-set-key (kbd "<wheel-down>") 'xah-forward-block))))
  (message "Mouse wheel set to move by block."))

(defun xah-set-mouse-wheel-mode ()
  "Set mouse wheel to move by line, block or other.
This command will prompt you.
When emacs is idle for 10 seconds, the normal wheel behavior will be restored."
  (interactive)
  (let (ξmode
        (ξmouse-wheel-modes
         '(
           "50 lines"
           "block"
           "normal"
           )))
    (setq ξmode (ido-completing-read "set wheel mode to:" ξmouse-wheel-modes))
    (cond
     ((string-equal ξmode "normal") (xah-set-mouse-wheel-normal))
     ((string-equal ξmode "block") (xah-set-mouse-scroll-by-block))
     ((string-equal ξmode "50 lines") (xah-set-mouse-scroll-by-50-line))
     (t (error "%s" "program logic error. No choice found")))

    (message "Wheel behavior will revert back to normal 10 seconds after idle.")

    (setq xah-fly-mouse-mode-timer-id (run-with-idle-timer 10 nil 'xah-set-mouse-wheel-normal))))
