;;-*- coding: utf-8 -*-

(progn
  (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "<prior>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<next>") 'isearch-repeat-forward)

  (define-key isearch-mode-map (kbd "<f4>") 'isearch-yank-pop)

)

(progn
  (define-key minibuffer-local-map (kbd "<f11>") 'previous-history-element)
  (define-key minibuffer-local-map (kbd "<f12>") 'next-history-element)
  (define-key minibuffer-local-map (kbd "S-<f11>") 'previous-matching-history-element)
  (define-key minibuffer-local-map (kbd "S-<f12>") 'next-matching-history-element)
  )

(progn
  (require 'dired )

  ;; (define-key dired-mode-map (kbd "M-$") nil) ; was dired-up-directory
  ;; (local-set-key (kbd "6") 'dired-up-directory)
  ;; (define-key dired-mode-map (kbd "M-g") 'backward-word)
  ;; (define-key dired-mode-map (kbd "M-c") 'previous-line)
  ;; (define-key dired-mode-map (kbd "C-o") 'ido-find-file)
  (define-key dired-mode-map (kbd "o") 'other-window)
  (define-key dired-mode-map (kbd "1") 'xah-previous-user-buffer)
  (define-key dired-mode-map (kbd "2") 'delete-window)
  (define-key dired-mode-map (kbd "3") 'delete-other-windows)
  (define-key dired-mode-map (kbd "4") 'split-window-vertically)
  (define-key dired-mode-map (kbd "C-o") 'find-file)

  (when (>= emacs-major-version 23)
    ;;    (define-key dired-mode-map (kbd "M-s") 'isearch-forward)
    ;;    (define-key dired-mode-map (kbd "M-S") 'isearch-backward)
    ;; (define-key dired-mode-map (kbd "<tab>") (make-keymap))
    (define-key dired-mode-map (kbd "<delete> t") 'wdired-change-to-wdired-mode) ; emacs 23 or later only
    ))

(defun xah-help-mode-setup ()
  "for `help-mode'."
  (local-set-key (kbd "g") 'backward-word)
  (local-unset-key (kbd "r"))
  (local-unset-key (kbd "8"))
  (local-unset-key (kbd "2"))
  (local-unset-key (kbd "3"))
  (local-unset-key (kbd "4")))
(add-hook 'help-mode-hook 'xah-help-mode-setup)

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

;;   ;; default keys
;; (local-set-key (kbd "TAB") 'shr-next-link)
;; (local-set-key (kbd "SPC") 'scroll-up-command)
;; (local-set-key (kbd "&") 'eww-browse-with-external-browser)
;; (local-set-key (kbd "-") 'negative-argument)
;; (local-set-key (kbd "B") 'eww-list-bookmarks)
;; (local-set-key (kbd "C") 'url-cookie-list)
;; (local-set-key (kbd "H") 'eww-list-histories)
;; (local-set-key (kbd "b") 'eww-add-bookmark)
;; (local-set-key (kbd "d") 'eww-download)
;; (local-set-key (kbd "g") 'eww-reload)
;; (local-set-key (kbd "l") 'eww-back-url)
;; (local-set-key (kbd "n") 'eww-next-url)
;; (local-set-key (kbd "p") 'eww-previous-url)
;; (local-set-key (kbd "q") 'quit-window)
;; (local-set-key (kbd "r") 'eww-forward-url)
;; (local-set-key (kbd "t") 'eww-top-url)
;; (local-set-key (kbd "u") 'eww-up-url)
;; (local-set-key (kbd "v") 'eww-view-source)
;; (local-set-key (kbd "w") 'eww-copy-page-url)
;; (local-set-key (kbd "DEL") 'scroll-down-command)
;; (local-set-key (kbd "S-SPC") 'scroll-down-command)
;; (local-set-key (kbd "<delete>") 'scroll-down-command)
;; (local-set-key (kbd "M-n") 'eww-next-bookmark)
;; (local-set-key (kbd "M-p") 'eww-previous-bookmark)
;; (local-set-key (kbd "C-M-i") 'shr-previous-link)

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

