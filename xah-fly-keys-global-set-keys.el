;; -*- coding: utf-8 -*-

(when (string-equal system-type "windows-nt")
  (define-key key-translation-map (kbd "<apps>") (kbd "<menu>")))

(when (string-equal system-type "darwin")
  ;; on Mac OS X, pc keyboard's menu key sends C-p
  (define-key key-translation-map (kbd "C-p") (kbd "<menu>")))

(global-set-key (kbd "<menu>") 'xah-fly-leader-key-map)
(global-set-key (kbd "<end>") 'xah-user-keymap)

(global-set-key (kbd "<home>") 'xah-fly-command-mode-activate)

