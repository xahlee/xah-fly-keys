;; -*- coding: utf-8 -*-
;; 2014-01-29
;; emacs keybinding for special keys
;; Xah Lee

;; 〈Mac OS X: Keyboard Layout, Keymapping, Keybinding, Software ⌨〉
;; http://xahlee.info/kbd/Mac_OS_X_keymapping_keybinding_tools.html

(define-key key-translation-map (kbd "<f17>") (kbd "C-g"))



;; (define-key key-translation-map (kbd "<henkan>") (kbd "<delete>")) ; henkan is the 変換 key on Japanese keyboard for “do convert”



(global-set-key (kbd "<f2>") 'xah-cut-line-or-region)
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region)
(global-set-key (kbd "<f4>") 'yank)
(global-set-key (kbd "<C-f4>") 'yank-pop)
(global-set-key (kbd "<f5>") 'undo)
(global-set-key (kbd "<C-f5>") 'redo)

(global-set-key (kbd "<f11>") 'xah-previous-user-buffer)
(global-set-key (kbd "<f12>") 'xah-next-user-buffer)

(global-set-key (kbd "<S-f11>") 'xah-previous-emacs-buffer)
(global-set-key (kbd "<S-f12>") 'xah-next-emacs-buffer)



(global-set-key (kbd "<C-prior>") 'xah-previous-user-buffer) ; page up
(global-set-key (kbd "<C-next>") 'xah-next-user-buffer) ; page down

(global-set-key (kbd "<S-prior>") 'scroll-down) ; page up
(global-set-key (kbd "<S-next>") 'scroll-up) ; page down

;; keys for moving to prev/next code section (form feed; ^L)
(global-set-key (kbd "<C-M-prior>") 'backward-page) ; Ctrl+Alt+PageUp
(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown

(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown



(global-set-key (kbd "<insert>") 'other-frame)



(global-set-key (kbd "<f14>") 'xah-close-current-buffer) ;truely ergo keyboard, upper right key

(global-set-key (kbd "<XF86Launch5>") 'xah-close-current-buffer) ; F14 upper right corner

(global-set-key (kbd "<XF86Cut>") 'xah-cut-line-or-region)
(global-set-key (kbd "<XF86Copy>") 'xah-copy-line-or-region)
(global-set-key (kbd "<XF86Paste>") 'yank)
(global-set-key (kbd "C-<XF86Paste>") 'yank-pop)
(global-set-key (kbd "<XF86Close>") 'xah-close-current-buffer)

