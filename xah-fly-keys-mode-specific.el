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
    (define-key dired-mode-map (kbd "<delete> t") 'wdired-change-to-wdired-mode) ; emacs 23 or later only
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
  (local-set-key (kbd "1") nil)
  (local-set-key (kbd "2") nil)
  (local-set-key (kbd "3") nil)
  (local-set-key (kbd "4") nil)
  (local-set-key (kbd "6") nil)
  (local-set-key (kbd "8") nil)
  (local-set-key (kbd "s") nil)
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

(setq special-mode-map
      (let ((myMap (make-sparse-keymap)))
        (suppress-keymap myMap)
        (define-key myMap "2" 'delete-window)
        (define-key myMap "3" 'delete-other-windows)
        (define-key myMap "q" 'quit-window)
        (define-key myMap " " 'scroll-up-command)
        (define-key myMap [?\S-\ ] 'scroll-down-command)
        (define-key myMap "\C-?" 'scroll-down-command)
        myMap))

;; used by message buffer. override it
(progn
  (define-key messages-buffer-mode-map "2" nil)
  (define-key messages-buffer-mode-map "3" nil))

(defun xah-magit-mode-keys ()
  "keys for `magit-mode'."
  (define-key magit-mode-map (kbd "1") nil)
  (define-key magit-mode-map (kbd "2") nil)
  (define-key magit-mode-map (kbd "3") nil)
  (define-key magit-mode-map (kbd "4") nil)
  (define-key magit-mode-map (kbd "o") nil)

  (define-key magit-mode-map (kbd "<menu> e o") 'magit-key-mode-popup-submodule)
  (define-key magit-mode-map (kbd "<menu> e 1") 'magit-show-level-1)
  (define-key magit-mode-map (kbd "<menu> e 2") 'magit-show-level-2)
  (define-key magit-mode-map (kbd "<menu> e 3") 'magit-show-level-3)
  (define-key magit-mode-map (kbd "<menu> e 4") 'magit-show-level-4))
(add-hook 'magit-mode-hook 'xah-magit-mode-keys)

(defun xah-Info-mode-keys ()
  "Modify keybindings for `Info-mode'.
For `Info-mode-hook'."
  (local-set-key (kbd "<menu> e g") 'xah-view-emacs-manual-in-browser)

  ;; (let ((ξkmap (make-keymap)))
  ;;         (define-key ξkmap (kbd "<menu> e") nil)
  ;;         (define-key ξkmap (kbd "<menu> e .") 'beginning-of-buffer)
  ;;         (define-key ξkmap (kbd "<menu> e  ") 'Info-scroll-up)
  ;;         (define-key ξkmap [?\S-\ ] 'Info-scroll-down)
  ;;         (define-key ξkmap "\C-m" 'Info-follow-nearest-node)
  ;;         (define-key ξkmap "\t" 'Info-next-reference)
  ;;         (define-key ξkmap "\e\t" 'Info-prev-reference)
  ;;         (define-key ξkmap [backtab] 'Info-prev-reference)
  ;;         (define-key ξkmap (kbd "<menu> e 1") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 2") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 3") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 4") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 5") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 6") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 7") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 8") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 9") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 0") 'undefined)
  ;;         (define-key ξkmap (kbd "<menu> e ?") 'Info-summary)
  ;;         (define-key ξkmap (kbd "<menu> e ]") 'Info-forward-node)
  ;;         (define-key ξkmap (kbd "<menu> e [") 'Info-backward-node)
  ;;         (define-key ξkmap (kbd "<menu> e <") 'Info-top-node)
  ;;         (define-key ξkmap (kbd "<menu> e >") 'Info-final-node)
  ;;         (define-key ξkmap (kbd "<menu> e b") 'beginning-of-buffer)
  ;;         (put 'beginning-of-buffer :advertised-binding "b")
  ;;         (define-key ξkmap (kbd "<menu> e d") 'Info-directory)
  ;;         (define-key ξkmap (kbd "<menu> e e") 'end-of-buffer)
  ;;         (define-key ξkmap (kbd "<menu> e f") 'Info-follow-reference)
  ;;         (define-key ξkmap (kbd "<menu> e g") 'Info-goto-node)
  ;;         (define-key ξkmap (kbd "<menu> e h") 'Info-help)
  ;;         ;; This is for compatibility with standalone info (>~ version 5.2).
  ;;         ;; Though for some time, standalone info had H and h reversed.
  ;;         ;; See <http://debbugs.gnu.org/16455>.
  ;;         (define-key ξkmap (kbd "<menu> e H") 'describe-mode)
  ;;         (define-key ξkmap (kbd "<menu> e i") 'Info-index)
  ;;         (define-key ξkmap (kbd "<menu> e I") 'Info-virtual-index)
  ;;         (define-key ξkmap (kbd "<menu> e l") 'Info-history-back)
  ;;         (define-key ξkmap (kbd "<menu> e L") 'Info-history)
  ;;         (define-key ξkmap (kbd "<menu> e m") 'Info-menu)
  ;;         (define-key ξkmap (kbd "<menu> e n") 'Info-next)
  ;;         (define-key ξkmap (kbd "<menu> e p") 'Info-prev)
  ;;         (define-key ξkmap (kbd "<menu> e q") 'Info-exit)
  ;;         (define-key ξkmap (kbd "<menu> e r") 'Info-history-forward)
  ;;         (define-key ξkmap (kbd "<menu> e s") 'Info-search)
  ;;         (define-key ξkmap (kbd "<menu> e S") 'Info-search-case-sensitively)
  ;;         (define-key ξkmap "\M-n" 'clone-buffer)
  ;;         (define-key ξkmap (kbd "<menu> e t") 'Info-top-node)
  ;;         (define-key ξkmap (kbd "<menu> e T") 'Info-toc)
  ;;         (define-key ξkmap (kbd "<menu> e u") 'Info-up)
  ;;         ;; `w' for consistency with `dired-copy-filename-as-kill'.
  ;;         (define-key ξkmap (kbd "<menu> e w") 'Info-copy-current-node-name)
  ;;         (define-key ξkmap (kbd "<menu> e c") 'Info-copy-current-node-name)
  ;;         ;; `^' for consistency with `dired-up-directory'.
  ;;         (define-key ξkmap (kbd "<menu> e ^") 'Info-up)
  ;;         (define-key ξkmap (kbd "<menu> e ,") 'Info-index-next)
  ;;         (define-key ξkmap "\177" 'Info-scroll-down)
  ;;         (define-key ξkmap [mouse-2] 'Info-mouse-follow-nearest-node)
  ;;         (define-key ξkmap [down-mouse-2] 'ignore) ;Override potential global binding.
  ;;         (define-key ξkmap [follow-link] 'mouse-face)
  ;;         (define-key ξkmap [XF86Back] 'Info-history-back)
  ;;         (define-key ξkmap [XF86Forward] 'Info-history-forward)
  ;;         ξkmap)

  )
(add-hook 'Info-mode-hook 'xah-Info-mode-keys)
 