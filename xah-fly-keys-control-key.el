;; -*- coding: utf-8 -*-

;; Emacs Keybinding Layout, Dvorak
;; http://ergoemacs.org/emacs/emacs_kb_shortcuts_dv.html
;; Emacs's Keybinding Layout Diagram
;; http://ergoemacs.org/emacs/emacs_kb_shortcuts.html

(progn
  ;; haven't decided what goes here
  (global-set-key (kbd "C-1") 'xah-cycle-font-previous)
  (global-set-key (kbd "C-2") 'xah-cycle-font-next)
  (global-set-key (kbd "C-3") 'xah-toggle-letter-case )
  (global-set-key (kbd "C-4") 'xah-cycle-hyphen-underscore-space )
  (global-set-key (kbd "C-5") 'xah-cycle-font-2)
  (global-set-key (kbd "C-6") 'xah-cycle-camel-style-case)
  (global-set-key (kbd "C-7") 'xah-backward-quote)
  (global-set-key (kbd "C-8") 'xah-forward-quote)
  (global-set-key (kbd "C-9") 'hippie-expand)
  (global-set-key (kbd "C-0") nil)
  )



(progn

  ;; note: some of them have complex connections. you don't want to set to nil. ⁖ C-g , C-i, C-m. Set to nil cause problems elsewhere

  (global-set-key (kbd "C-a") 'mark-whole-buffer) ; was move-beginning-of-line
  (global-set-key (kbd "C-b") nil) ;
  ;; (global-set-key (kbd "C-c") nil) ;
  (global-set-key (kbd "C-d") nil) ;
  (global-set-key (kbd "C-e") nil) ;
  (global-set-key (kbd "C-f") nil) ;
  ;; (global-set-key (kbd "C-g") nil) ;
  ;; (global-set-key (kbd "C-h") nil) ;
  (global-set-key (kbd "C-i") nil) ;
  (global-set-key (kbd "C-j") nil) ;
  (global-set-key (kbd "C-k") nil) ;
  (global-set-key (kbd "C-l") nil) ;
  ;; (global-set-key (kbd "C-m") nil) ;
  (global-set-key (kbd "C-n") 'xah-new-empty-buffer) ; was next-line
  (global-set-key (kbd "C-o") 'find-file)         ; was open-line
  (global-set-key (kbd "C-p") nil) ;
  ;; (global-set-key (kbd "C-q") nil) ;
  (global-set-key (kbd "C-r") nil) ;
  (global-set-key (kbd "C-s") 'save-buffer) ; was isearch-forward
  (global-set-key (kbd "C-t") nil) ;
  (global-set-key (kbd "C-u") nil) ;
  (global-set-key (kbd "C-v") nil) ;
  (global-set-key (kbd "C-w") 'xah-close-current-buffer) ; was kill-region
  ;; (global-set-key (kbd "C-x") nil) ;
  (global-set-key (kbd "C-y") nil) ;
  (global-set-key (kbd "C-z") nil) ; was suspend-frame

  (global-set-key (kbd "C-S-n") 'make-frame-command)
  (global-set-key (kbd "C-S-s") 'write-file)
  (global-set-key (kbd "C-S-t") 'xah-open-last-closed) ; was transpose-chars

  (global-set-key (kbd "<C-next>") 'xah-next-user-buffer)
  (global-set-key (kbd "<C-prior>") 'xah-previous-user-buffer)

  (progn
    ;; uncomment if you want.
    ;; • you lose C-c for mode specific,
    ;; • helps you lose the habit of pressing emacs's C-x

    ;; (global-set-key (kbd "C-x") xah-cut-line-or-region) ; was C-x prefix
    ;; (global-set-key (kbd "C-c") xah-copy-line-or-region) ; was C-c prefix (for major modes)
    ;; (global-set-key (kbd "C-v") yank) ; scroll-up-command
    ;; (global-set-key (kbd "C-z") xah-cut-line-or-region) ; was suspend frame
    )
)

