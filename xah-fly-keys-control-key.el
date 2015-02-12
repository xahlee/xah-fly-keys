;; -*- coding: utf-8 -*-

;; Emacs Keybinding Layout, Dvorak
;; http://ergoemacs.org/emacs/emacs_kb_shortcuts_dv.html
;; Emacs's Keybinding Layout Diagram
;; http://ergoemacs.org/emacs/emacs_kb_shortcuts.html

(progn
  ;; haven't decided what goes here
  (global-set-key (kbd "<C-f1>") 'xah-cycle-font-previous)
  (global-set-key (kbd "<C-f2>") 'xah-cycle-font-next)

  (global-set-key (kbd "C-1") 'xah-cycle-font-2)
  (global-set-key (kbd "C-3") 'other-window )
  (global-set-key (kbd "C-7") 'xah-cycle-camel-style-case)
  (global-set-key (kbd "C-8") 'xah-cycle-hyphen-underscore-space)
  (global-set-key (kbd "C-9") 'hippie-expand)
  )

;; (progn                                            ; standard keys
;;   (global-set-key (kbd "C-a") 'mark-whole-buffer) ; was move-beginning-of-line
;;   (global-set-key (kbd "C-n") 'xah-new-empty-buffer) ; was next-line
;;   (global-set-key (kbd "C-S-n") 'make-frame-command) ; was nil
;;   (global-set-key (kbd "C-o") 'find-file)            ; was open-line
;;   (global-set-key (kbd "C-s") 'save-buffer)  ; was isearch-forward
;;   (global-set-key (kbd "C-S-s") 'write-file) ; was nil
;;   (global-set-key (kbd "C-S-t") 'xah-open-last-closed) ; was nil
  (global-set-key (kbd "C-w") 'xah-close-current-buffer) ; was kill-region
;;   (global-set-key (kbd "C-y") 'redo)                     ; was yank
;;   (global-set-key (kbd "C-z") 'undo) ; was suspend-frame
;;   )

(progn
  (global-set-key (kbd "<C-next>") 'xah-next-user-buffer)
  (global-set-key (kbd "<C-prior>") 'xah-previous-user-buffer))