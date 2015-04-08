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
  (global-set-key (kbd "C-7") nil)
  (global-set-key (kbd "C-8") nil)
  (global-set-key (kbd "C-9") nil)
  (global-set-key (kbd "C-0") nil)
  )

(progn                                            ; standard keys
  (global-set-key (kbd "C-a") 'mark-whole-buffer) ; was move-beginning-of-line
  (global-set-key (kbd "C-n") 'xah-new-empty-buffer) ; was next-line
  (global-set-key (kbd "C-S-n") 'make-frame-command) ; was nil
  (global-set-key (kbd "C-o") 'find-file)            ; was open-line
  (global-set-key (kbd "C-s") 'save-buffer)  ; was isearch-forward
  (global-set-key (kbd "C-S-s") 'write-file) ; was nil
  (global-set-key (kbd "C-S-t") 'xah-open-last-closed) ; was nil
  (global-set-key (kbd "C-w") 'xah-close-current-buffer) ; was kill-region
  (global-set-key (kbd "C-y") 'redo)                     ; was yank
  (global-set-key (kbd "C-z") 'undo) ; was suspend-frame
  )

(progn
  (global-set-key (kbd "<C-next>") 'xah-next-user-buffer)
  (global-set-key (kbd "<C-prior>") 'xah-previous-user-buffer))

;; (progn
;;   (global-set-key (kbd "C-t") ctl-x-map)

;;   ;; (define-key key-translation-map (kbd "M-t") (kbd "C-c"))
;;   ;; (define-key key-translation-map (kbd "C-c") (kbd "M-t"))
;;   ;; (define-key key-translation-map (kbd "C-t") (kbd "C-x"))
;;   ;; (define-key key-translation-map (kbd "C-x") (kbd "C-t"))
;;   ;; (global-set-key (kbd "C-t") 'xah-cut-line-or-region)
;;   ;; (global-set-key (kbd "M-t") 'xah-copy-line-or-region)

;;   (global-set-key (kbd "C-x") 'xah-cut-line-or-region)
;;   (global-set-key (kbd "C-v") 'yank)
;;   )