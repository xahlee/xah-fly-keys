(defun xah-click-to-search (φclick)
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
