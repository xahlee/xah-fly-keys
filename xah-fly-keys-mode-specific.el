;;-*- coding: utf-8 -*-

(progn
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward) ; single key, useful
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward) ; single key, useful
  (define-key isearch-mode-map (kbd "M-v") 'isearch-yank-pop)
 )

(progn
  (define-key minibuffer-local-map (kbd "M-7") 'previous-history-element)
  (define-key minibuffer-local-map (kbd "M-8") 'next-history-element)
  (define-key minibuffer-local-map (kbd "M-S-7") 'previous-matching-history-element)
  (define-key minibuffer-local-map (kbd "M-S-8") 'next-matching-history-element)
  )

(progn
  (require 'dired )

  ;; (define-key dired-mode-map (kbd "M-$") nil) ; was dired-up-directory
  ;; (local-set-key (kbd "6") 'dired-up-directory)
  ;; (define-key dired-mode-map (kbd "M-g") 'backward-word)
  ;; (define-key dired-mode-map (kbd "M-c") 'previous-line)
  ;; (define-key dired-mode-map (kbd "C-o") 'ido-find-file)
  (define-key dired-mode-map (kbd "o") 'other-window) ; was dired-find-file-other-window
  (define-key dired-mode-map (kbd "1") 'xah-previous-user-buffer)
  (define-key dired-mode-map (kbd "2") 'delete-window)
  (define-key dired-mode-map (kbd "3") 'delete-other-windows)
  (define-key dired-mode-map (kbd "4") 'split-window-vertically)
  (define-key dired-mode-map (kbd "6") 'dired-up-directory)
  (define-key dired-mode-map (kbd "C-o") 'find-file) ; was dired-display-file

  (when (>= emacs-major-version 23)
    ;;    (define-key dired-mode-map (kbd "M-s") 'isearch-forward)
    ;;    (define-key dired-mode-map (kbd "M-S") 'isearch-backward)
    ;; (define-key dired-mode-map (kbd "<tab>") (make-keymap))
    (define-key dired-mode-map (kbd "<menu> e t") 'wdired-change-to-wdired-mode) ; emacs 23 or later only
    ))

(progn
  (defun xah-help-mode-setup ()
    "for `help-mode'."
    (local-set-key (kbd "g") 'backward-word)
    (local-unset-key (kbd "r"))
    (local-unset-key (kbd "8"))
    (local-unset-key (kbd "2"))
    (local-unset-key (kbd "3"))
    (local-unset-key (kbd "4")))
  (add-hook 'help-mode-hook 'xah-help-mode-setup))

(progn
  (require 'ibuffer)
  (define-key ibuffer-mode-map (kbd "o") 'other-window) ; 'ibuffer-visit-buffer-other-window
  )

(defun xah-eww-mode-setup ()
  "for `eww-mode'."
  (local-set-key (kbd "1") nil)
  (local-set-key (kbd "2") nil)
  (local-set-key (kbd "3") 'delete-other-windows)
  (local-set-key (kbd "4") 'split-window-vertically)
  (local-set-key (kbd "5") nil)
  (local-set-key (kbd "6") nil)
  (local-set-key (kbd "7") 'eww-browse-with-external-browser)
  (local-set-key (kbd "8") nil)
  (local-set-key (kbd "9") nil)
  (local-set-key (kbd "0") nil)

(local-set-key (kbd "SPC") nil) ; 'scroll-up-command
(local-set-key (kbd "DEL") nil) ; 'scroll-down-command
(local-set-key (kbd "S-SPC") nil) ; 'scroll-down-command
(local-set-key (kbd "<delete>") nil) ; 'scroll-down-command

(local-set-key (kbd "b") nil)
(local-set-key (kbd "d") nil)
(local-set-key (kbd "g") nil)
(local-set-key (kbd "l") nil)
(local-set-key (kbd "r") nil)
(local-set-key (kbd "n") nil) ; 'eww-next-url
(local-set-key (kbd "p") nil) ; 'eww-previous-url
(local-set-key (kbd "t") nil) ; 'eww-top-url
(local-set-key (kbd "u") nil) ; 'eww-up-url

  ;; default keys
(local-set-key (kbd "TAB") 'shr-next-link)
(local-set-key (kbd "C-M-i") 'shr-previous-link)

(local-set-key (kbd "&") nil) ; 'eww-browse-with-external-browser
(local-set-key (kbd "-") nil) ; 'negative-argument
(local-set-key (kbd "<menu> e B") 'eww-list-bookmarks)
(local-set-key (kbd "<menu> e C") 'url-cookie-list)
(local-set-key (kbd "<menu> e H") 'eww-list-histories)

(local-set-key (kbd "<menu> e b") 'eww-add-bookmark)
(local-set-key (kbd "<menu> e d") 'eww-download)
(local-set-key (kbd "<menu> e g") 'eww-reload)
(local-set-key (kbd "<menu> e l") 'eww-back-url)
(local-set-key (kbd "<menu> e q") 'quit-window)
(local-set-key (kbd "<menu> e r") 'eww-forward-url)

(local-set-key (kbd "<menu> e v") 'eww-view-source)
(local-set-key (kbd "<menu> e w") 'eww-copy-page-url)
(local-set-key (kbd "M-n") 'eww-next-bookmark)
(local-set-key (kbd "M-p") 'eww-previous-bookmark)

)
(add-hook 'eww-mode-hook 'xah-eww-mode-setup)

(defun xah-Man-mode-keys ()
  "keys for `Man-mode'."
  (local-set-key (kbd "1") 'xah-previous-user-buffer)
  (local-set-key (kbd "2") 'delete-window)
  (local-set-key (kbd "3") 'delete-other-windows)
  (local-set-key (kbd "4") 'split-window-vertically)
  (local-set-key (kbd "6") 'xah-select-current-block)
  (local-set-key (kbd "8") 'xah-extend-selection)
  )
(add-hook 'Man-mode-hook 'xah-Man-mode-keys)

;; (when (fboundp 'cider-repl-mode)
;;   (defun xah-cider-repl-mode-keys ()
;;     "Modify keys for `cider-repl-mode'."
;;     (local-set-key (kbd "<home>") 'xfk-command-mode-activate))
;;   (add-hook 'cider-repl-mode-hook 'xah-cider-repl-mode-keys))

(defun xah-calc-mode-keys ()
  "keys for `Man-mode'."
  (local-set-key (kbd "o") 'other-window)
  )
(add-hook 'calc-mode-hook 'xah-calc-mode-keys)

(progn
  ;; used by modes {C, C++, java}
  (require 'cc-mode)
  ;; (setq c-electric-flag nil)
  (define-key c-mode-base-map "," nil) ; was c-electric-semi&comma
  )

(defun xah-occur-mode-keys ()
  "keys for `occur-mode'."
  (define-key occur-mode-map (kbd "o") 'other-window)
  )
(add-hook 'occur-mode-hook 'xah-occur-mode-keys)

;; used by message buffer. override it
(setq special-mode-map
      (let ((myMap (make-sparse-keymap)))
        (suppress-keymap myMap)
        (define-key myMap "q" 'quit-window)
        (define-key myMap " " 'scroll-up-command)
        (define-key myMap [?\S-\ ] 'scroll-down-command)
        (define-key myMap "\C-?" 'scroll-down-command)
        myMap))

