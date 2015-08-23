;; -*- coding: utf-8 -*-

(when (string-equal system-type "windows-nt")
  (define-key key-translation-map (kbd "<apps>") (kbd "<menu>")))

(global-set-key (kbd "<menu>") 'xah-fly-leader-key-map)
(global-set-key (kbd "<home>") 'xah-fly-command-mode-activate)



(global-set-key (kbd "<f11>") 'xah-previous-user-buffer)
(global-set-key (kbd "<f12>") 'xah-next-user-buffer)
(global-set-key (kbd "<C-f11>") 'xah-previous-emacs-buffer)
(global-set-key (kbd "<C-f12>") 'xah-next-emacs-buffer)



(global-set-key (kbd "<C-prior>") 'xah-previous-user-buffer)
(global-set-key (kbd "<C-next>") 'xah-next-user-buffer)
