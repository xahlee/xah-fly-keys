;; -*- coding: utf-8 -*-

;; Emacs Keybinding Layout, Dvorak http://ergoemacs.org/emacs/emacs_kb_shortcuts_dv.html

;; Emacs's Keybinding Layout Diagram http://ergoemacs.org/emacs/emacs_kb_shortcuts.html

(progn
  ;; haven't decided what goes here

  (global-set-key (kbd "C-1") nil)
  (global-set-key (kbd "C-2") nil)
  (global-set-key (kbd "C-3") nil)
  (global-set-key (kbd "C-4") nil)
  (global-set-key (kbd "C-5") nil)
  (global-set-key (kbd "C-6") nil)
  (global-set-key (kbd "C-7") 'scroll-down)
  (global-set-key (kbd "C-8") 'scroll-up)
  (global-set-key (kbd "C-9") nil)
  (global-set-key (kbd "C-0") nil)
  )

(progn                                            ; standard keys
  (global-set-key (kbd "C-a") nil) ; was move-beginning-of-line

  (global-set-key (kbd "C-b") nil) ; was backward-char
  ;; (global-set-key (kbd "C-c") nil) ; was mode specific
  (global-set-key (kbd "C-d") nil) ; was delete-char
  (global-set-key (kbd "C-e") nil) ; was move-end-of-line
  (global-set-key (kbd "C-f") nil) ; was forward-char
  ;; C-g cancel
  ;; C-h help prefix
  ;; C-i tab
  ;; C-j newline
  (global-set-key (kbd "C-k") nil) ; was kill-line
  (global-set-key (kbd "C-l") nil) ; was recenter-top-bottom
  ;; C-m ; was newline
  (global-set-key (kbd "C-n") 'xah-new-empty-buffer) ; was next-line
  (global-set-key (kbd "C-S-n") 'make-frame-command) ; was nil
  (global-set-key (kbd "C-o") 'ido-find-file)            ; was open-line
  (global-set-key (kbd "C-p") nil) ; was previous-line
  ;; C-q quoted-insert
  (global-set-key (kbd "C-r") nil)           ; was isearch-backward
  (global-set-key (kbd "C-s") 'save-buffer)  ; was isearch-forward
  (global-set-key (kbd "C-S-s") 'write-file) ; was nil
  (global-set-key (kbd "C-S-t") 'xah-open-last-closed) ; was nil
  (global-set-key (kbd "C-t") nil) ; was transpose-chars
  (global-set-key (kbd "C-u") 'universal-argument) ; was universal-argument 2015-06-19 possibly some command won't recognize universal-argument if it's not C-u

  (global-set-key (kbd "C-v") 'yank) ; was scroll-up-command
  (global-set-key (kbd "C-w") 'xah-close-current-buffer) ; was kill-region
  (global-set-key (kbd "C-x") 'xah-cut-line-or-region) ; was ctl-x-map
  (global-set-key (kbd "C-y") 'redo) ; was yank
  (global-set-key (kbd "C-z") 'undo) ; was suspend-frame
  )

(progn
  (global-set-key (kbd "<C-next>") 'xah-next-user-buffer)
  (global-set-key (kbd "<C-prior>") 'xah-previous-user-buffer))
