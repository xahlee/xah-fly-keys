;; -*- coding: utf-8 -*-

;; Emacs Keybinding Layout, Dvorak
;; http://ergoemacs.org/emacs/emacs_kb_shortcuts_dv.html
;; Emacs's Keybinding Layout Diagram
;; http://ergoemacs.org/emacs/emacs_kb_shortcuts.html

(progn
  ;; haven't decided what goes here
  (global-set-key (kbd "<C-f1>") 'xah-cycle-font-previous)
  (global-set-key (kbd "<C-f2>") 'xah-cycle-font-next)
  (global-set-key (kbd "<C-f3>") nil)
  (global-set-key (kbd "<C-f4>") nil)
  (global-set-key (kbd "<C-f5>") nil)
  (global-set-key (kbd "<C-f6>") nil)
  (global-set-key (kbd "<C-f7>") nil)
  (global-set-key (kbd "<C-f8>") nil)
  (global-set-key (kbd "<C-f9>") nil)

  (global-set-key (kbd "C-1") nil )
  (global-set-key (kbd "C-2") nil )
  (global-set-key (kbd "C-3") nil )
  (global-set-key (kbd "C-4") 'xah-cycle-hyphen-underscore-space )
  (global-set-key (kbd "C-5") 'xah-cycle-font-2)
  (global-set-key (kbd "C-6") 'xah-cycle-camel-style-case)
  (global-set-key (kbd "C-7") nil)
  (global-set-key (kbd "C-8") nil)
  (global-set-key (kbd "C-9") 'hippie-expand)
  (global-set-key (kbd "C-0") nil)
  )



;; (progn

;;   ;; note: some of them have complex connections. you don't want to set to nil. ⁖ C-g , C-i, C-m. Set to nil cause problems elsewhere

;;   (global-set-key (kbd "C-a") 'mark-whole-buffer) ; was move-beginning-of-line
;;   (global-set-key (kbd "C-b") nil)                ; backward-char
;;   ;; (global-set-key (kbd "C-c") nil) ; major mode specific prefix
;;   (global-set-key (kbd "C-d") nil) ; delete-char
;;   (global-set-key (kbd "C-e") nil) ; move-end-of-line
;;   (global-set-key (kbd "C-f") nil) ; forward-char
;;   ;; (global-set-key (kbd "C-g") nil) ; was keyboard-quit
;;   ;; (global-set-key (kbd "C-h") nil) ; was help-map
;;   ;; (global-set-key (kbd "C-i") nil) ; indent-for-tab-command
;;   ;; (global-set-key (kbd "C-j") nil) ; electric-newline-and-maybe-indent
;;   (global-set-key (kbd "C-k") nil) ; kill-line
;;   (global-set-key (kbd "C-l") nil) ; recenter-top-bottom
;;   ;; (global-set-key (kbd "C-m") nil) ; newline
;;   (global-set-key (kbd "C-n") 'xah-new-empty-buffer) ; was next-line
;;   (global-set-key (kbd "C-S-n") 'make-frame-command)   ; was nil
;;   (global-set-key (kbd "C-o") 'find-file)            ; was open-line
;;   (global-set-key (kbd "C-p") nil)                   ; previous-line
;;   ;; (global-set-key (kbd "C-q") 'quoted-insert) ; was quoted-insert
;;   (global-set-key (kbd "C-r") nil) ; isearch-backward
;;   (global-set-key (kbd "C-s") 'save-buffer) ; was isearch-forward
;;   (global-set-key (kbd "C-S-s") 'write-file)           ; was nil
;;   (global-set-key (kbd "C-t") nil)          ; was transpose-chars
;;   (global-set-key (kbd "C-S-t") 'xah-open-last-closed) ; was nil
;;   ;; (global-set-key (kbd "C-u") nil) ; universal-argument
;;   ;; (global-set-key (kbd "C-v") nil) ; scroll-up-command
;;   (global-set-key (kbd "C-w") 'xah-close-current-buffer) ; was kill-region
;;   ;; (global-set-key (kbd "C-x") 'xah-cut-line-or-region) ; was ctl-x-map
;;   (global-set-key (kbd "C-y") 'redo)                   ; was yank
;;   (global-set-key (kbd "C-z") 'undo) ; was suspend-frame

;;   )

(progn 
  (global-set-key (kbd "<C-next>") 'xah-next-user-buffer)
  (global-set-key (kbd "<C-prior>") 'xah-previous-user-buffer))