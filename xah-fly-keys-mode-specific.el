;;-*- coding: utf-8 -*-

(progn
  (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "<next>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<prior>") 'isearch-repeat-backward)
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

