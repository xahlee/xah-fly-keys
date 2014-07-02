;; -*- coding: utf-8 -*-

;; ergoemacs-xah (eex).
;; a vi-like modal keybinding for emacs.
;; the keys are based on ergoemacs-mode and dvorak layout and for The truly Ergonomic Keyboard.
;; created: 2013-09-10
;; Xah Lee

;; some notes:
;; • This is not a vi emulation mode. other than being a modal input system, this design doesn't follow vi or vim's traditions. For example: there's no command such as “dd”. and there's no typing a digit followed by a command to repeat n times.
;; • the keymap is largely compatible with ergoemacs-mode. It's based on mapping the most frequetly used command to the most easy-to-press key positions.
;; • created this around 2013-08. Used it daily since.
;; • you'll also need xah_emacs_keybinding_functions.el

;; home page 〈ergoemacs-xah Mode〉 http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; TODO
;; • make it a proper minor mode.
;; • fine-tune lots keys in command mode. (introduce key sequence there. Consider whether {open, close, save} should be there. and some other commands such as {dired-jump, query-replace-regexp, xah-toggle-letter-case}.)
;; • support different keyboard layouts.
;; • reconsider some keybinding so it's more friendly for normal PC keyboard or Microsoft 4000.

;; i wrote this mode for myself. License is open source. Feel free to copy but please link to http://ergoemacs.org/emacs/emacs.html or a specific relevant page on that domain.
;; if you like to see this mode go further, buy my tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html and let me know. Thanks.

;; sample keys to activate command/insert mode. They should be on home row or thumb key. On standard PC keyboard, good positions are capslock, tab, space, key next to space, return. But you should use Maltron, Kinesis, Truly Ergonomic keyboard etc. see http://ergoemacs.org/emacs/emacs_best_keyboard.html http://xahlee.info/kbd/keyboarding.html
;; (global-set-key (kbd "<f7>") 'eex-command-mode-activate)
;; (global-set-key (kbd "<escape>") 'eex-command-mode-activate)
;; (global-set-key (kbd "<home>") 'eex-command-mode-activate)
;; (global-set-key (kbd "<f19>") 'eex-command-mode-activate)
;; (global-set-key (kbd "<f8>") 'eex-insert-mode-activate)

;; (global-set-key (kbd "<f8>") 'eex-insert-mode-activate)
;; (global-set-key (kbd "<end>") 'eex-insert-mode-activate)
;; (global-set-key (kbd "<return>") 'eex-insert-mode-activate)



(defun xah-fullpath-relative-to-caller (φfile-relative-path)
  "Return the full path of ΦFILE-RELATIVE-PATH, relative to caller's file location.

Example: If you have this line
 (xah-fullpath-relative-to-caller \"../xyz.el\")
in the file at
 /home/mary/emacs/emacs_lib.el
then the return value is
 /home/mary/xyz.el
Regardless how or where emacs_lib.el is called.

This function solves 2 problems.

① If you have file A, that calls the `load' on a file at B, and B calls `load' on file C using a relative path, then Emacs will complain about unable to find C. Because, emacs does not switch current directory with `load'.

To solve this problem, when your code only knows the relative path of another file C, you can use the variable `load-file-name' to get the current file's full path, then use that with the relative path to get a full path of the file you are interested.

② To know the current file's full path, emacs has 2 ways: `load-file-name' and `buffer-file-name'. If the file is loaded by `load', then `load-file-name' works but `buffer-file-name' doesn't. If the file is called by `eval-buffer', then `load-file-name' is nil. You want to be able to get the current file's full path regardless the file is run by `load' or interactively by `eval-buffer'."

  (concat (file-name-directory (or load-file-name buffer-file-name)) φfile-relative-path)
)

(require 'xeu_elisp_util)

(load (xah-fullpath-relative-to-caller "xah-fly-keys-cursor-movement"))
(load (xah-fullpath-relative-to-caller "xah-fly-keys-editing-commands"))
(load (xah-fullpath-relative-to-caller "xah-fly-keys-text-selection"))



(defvar eex-insert-state-q t "Boolean value. true means insertion mode is on.")
(setq eex-insert-state-q t)

(defun eex-insert-mode-init ()
  "set insertion mode keys"
  (interactive)
  ;; TODO use a proper keymap
  (progn
    (global-set-key (kbd ";") 'self-insert-command)
    (global-set-key (kbd "=") 'self-insert-command)
    (global-set-key (kbd "[") 'self-insert-command)
    (global-set-key (kbd "\\") 'self-insert-command)
    (global-set-key (kbd ".") 'self-insert-command)
    (global-set-key (kbd "'") 'self-insert-command)
    (global-set-key (kbd ",") 'self-insert-command)
    (global-set-key (kbd "-") 'self-insert-command)
    (global-set-key (kbd "/") 'self-insert-command)
    (global-set-key (kbd "0") 'self-insert-command)
    (global-set-key (kbd "1") 'self-insert-command)
    (global-set-key (kbd "2") 'self-insert-command)
    (global-set-key (kbd "3") 'self-insert-command)
    (global-set-key (kbd "4") 'self-insert-command)
    (global-set-key (kbd "5") 'self-insert-command)
    (global-set-key (kbd "6") 'self-insert-command)
    (global-set-key (kbd "7") 'self-insert-command)
    (global-set-key (kbd "8") 'self-insert-command)
    (global-set-key (kbd "9") 'self-insert-command)
    (global-set-key (kbd "a") 'self-insert-command)
    (global-set-key (kbd "b") 'self-insert-command)
    (global-set-key (kbd "c") 'self-insert-command)
    (global-set-key (kbd "d") 'self-insert-command)
    (global-set-key (kbd "e") 'self-insert-command)
    (global-set-key (kbd "f") 'self-insert-command)
    (global-set-key (kbd "g") 'self-insert-command)
    (global-set-key (kbd "h") 'self-insert-command)
    (global-set-key (kbd "i") 'self-insert-command)
    (global-set-key (kbd "j") 'self-insert-command)
    (global-set-key (kbd "k") 'self-insert-command)
    (global-set-key (kbd "l") 'self-insert-command)
    (global-set-key (kbd "m") 'self-insert-command)
    (global-set-key (kbd "n") 'self-insert-command)
    (global-set-key (kbd "o") 'self-insert-command)
    (global-set-key (kbd "p") 'self-insert-command)
    (global-set-key (kbd "q") 'self-insert-command)
    (global-set-key (kbd "r") 'self-insert-command)
    (global-set-key (kbd "s") 'self-insert-command)
    (global-set-key (kbd "t") 'self-insert-command)
    (global-set-key (kbd "u") 'self-insert-command)
    (global-set-key (kbd "v") 'self-insert-command)
    (global-set-key (kbd "w") 'self-insert-command)
    (global-set-key (kbd "x") 'self-insert-command)
    (global-set-key (kbd "y") 'self-insert-command)
    (global-set-key (kbd "z") 'self-insert-command)))

(defun eex-command-mode-init ()
  "set command mode keys"
  (interactive)
  (progn
    (global-set-key (kbd ";") nil)
    (global-set-key (kbd ".") 'backward-kill-word)
    (global-set-key (kbd "'") 'xah-compact-uncompact-block)
    (global-set-key (kbd ",") 'xah-shrink-whitespaces)
    (global-set-key (kbd "1") nil)
    (global-set-key (kbd "2") 'delete-window)
    (global-set-key (kbd "3") 'delete-other-windows)
    (global-set-key (kbd "4") 'split-window-vertically)
    (global-set-key (kbd "5") 'redo)
    (global-set-key (kbd "6") 'xah-select-current-block)
    (global-set-key (kbd "7") 'xah-select-current-line)
    (global-set-key (kbd "8") 'xah-extend-selection)
    (global-set-key (kbd "9") 'xah-select-text-in-bracket-or-quote)
    (global-set-key (kbd "0") 'xah-forward-quote)

    (global-set-key (kbd "a") 'open-line)
    (global-set-key (kbd "b") 'xah-toggle-letter-case)
    (global-set-key (kbd "c") 'previous-line)
    (global-set-key (kbd "d") 'xah-beginning-of-line-or-block)
    (global-set-key (kbd "e") 'delete-backward-char)
    (global-set-key (kbd "f") 'undo)
    (global-set-key (kbd "g") 'backward-word)
    (global-set-key (kbd "h") 'backward-char)
    (global-set-key (kbd "i") 'kill-line)
    (global-set-key (kbd "j") 'xah-copy-line-or-region)
    (global-set-key (kbd "k") 'yank)
    (global-set-key (kbd "l") 'recenter-top-bottom)
    (global-set-key (kbd "m") 'xah-backward-left-bracket)
    (global-set-key (kbd "n") 'forward-char)
    (global-set-key (kbd "o") 'other-window)
    (global-set-key (kbd "p") 'kill-word)
    (global-set-key (kbd "q") 'xah-cut-line-or-region)
    (global-set-key (kbd "r") 'forward-word)
    (global-set-key (kbd "s") 'xah-end-of-line-or-block)
    (global-set-key (kbd "t") 'next-line)
    (global-set-key (kbd "u") 'delete-char)
    (global-set-key (kbd "v") 'xah-forward-right-bracket)
    (global-set-key (kbd "w") 'eex-insert-mode-activate)
    (global-set-key (kbd "x") 'xah-cycle-hyphen-underscore-space)
    (global-set-key (kbd "y") 'set-mark-command)
    (global-set-key (kbd "z") 'comment-dwim)))

(defun eex-mode-toggle ()
  "Switch between {insertion, command} modes."
  (interactive)
  (if eex-insert-state-q
      (eex-command-mode-activate)
    (eex-insert-mode-activate)))

(defun eex-command-mode-activate ()
  "Activate command mode."
  (interactive)
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (setq eex-insert-state-q nil )
  (eex-command-mode-init))

(defun eex-insert-mode-activate ()
  "Activate insertion mode."
  (interactive)
  (modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
  (setq eex-insert-state-q t )
  (eex-insert-mode-init))

;; when in going into minibuffer, switch to insertion mode.
(add-hook 'minibuffer-setup-hook 'eex-insert-mode-activate)
(add-hook 'minibuffer-exit-hook 'eex-command-mode-activate)

;; TODO when in shell mode, switch to insertion mode.
(add-hook 'shell-mode-hook 'eex-insert-mode-activate)



;; 2014-06-20
;; 6562    4.43%  backward-word
;; 5856    3.95%  forward-word

;;  974    0.66%  undo-tree-undo
;;  140    0.09%  undo-tree-redo

;; 1500    1.01%  backward-kill-word
;; 1019    0.69%  kill-word

;; 1712    1.16%  xah-cut-line-or-region
;;  885    0.60%  xah-copy-line-or-region

;; 2249    1.52%  delete-backward-char
;;  770    0.52%  set-mark-command
;;  451    0.30%  delete-char

;; 1128    0.76%  xah-shrink-whitespaces

;;  911    0.61%  other-window

;; 766    0.52%  delete-other-windows
;; 266    0.18%  split-window-vertically

;;  276    0.19%  kill-line

;;  414    0.28%  xah-toggle-letter-case
;;   61    0.04%  xah-cycle-hyphen-underscore-space

;; 1598    1.08%  xah-backward-left-bracket
;;  805    0.54%  xah-forward-right-bracket

;;  415    0.28%  xah-select-current-block
;;  515    0.35%  xah-select-current-line
;;  908    0.61%  xah-extend-selection
;;  329    0.22%  xah-select-text-in-quote

;;  333    0.22%  ffap
;; 161    0.11%  smex



;; For all major modes:

;; 32455   21.91%  self-insert-command
;; 10679    7.21%  next-line
;; 10267    6.93%  xah-beginning-of-line-or-block
;; 7658    5.17%  mwheel-scroll
;; 7054    4.76%  previous-line
;; 7039    4.75%  xah-end-of-line-or-block
;; 6562    4.43%  backward-word
;; 5856    3.95%  forward-word
;; 3856    2.60%  eex-command-mode-activate
;; 3744    2.53%  isearch-printing-char
;; 3439    2.32%  eex-insert-mode-activate
;; 2418    1.63%  yank
;; 2249    1.52%  delete-backward-char
;; 2036    1.37%  save-buffer
;; 2019    1.36%  newline
;; 1712    1.16%  xah-cut-line-or-region
;; 1672    1.13%  isearch-repeat-forward
;; 1598    1.08%  xah-backward-left-bracket
;; 1593    1.08%  org-self-insert-command
;; 1506    1.02%  xah-close-current-buffer
;; 1500    1.01%  backward-kill-word
;; 1128    0.76%  xah-shrink-whitespaces
;; 1080    0.73%  backward-char
;; 1019    0.69%  kill-word
;; 999    0.67%  handle-switch-frame
;; 974    0.66%  undo-tree-undo
;; 969    0.65%  isearch-forward
;; 927    0.63%  isearch-exit
;; 911    0.61%  other-window
;; 908    0.61%  xah-extend-selection
;; 885    0.60%  xah-copy-line-or-region
;; 805    0.54%  xah-forward-right-bracket
;; 770    0.52%  set-mark-command
;; 766    0.52%  delete-other-windows
;; 671    0.45%  forward-char
;; 544    0.37%  xah-open-file-fast
;; 515    0.35%  xah-select-current-line
;; 451    0.30%  delete-char
;; 423    0.29%  xah-next-user-buffer
;; 415    0.28%  xah-select-current-block
;; 414    0.28%  xah-toggle-letter-case
;; 361    0.24%  recenter-top-bottom
;; 352    0.24%  keyboard-quit
;; 333    0.22%  ffap
;; 329    0.22%  xah-select-text-in-quote
;; 325    0.22%  xhm-wrap-html-tag
;; 314    0.21%  exit-minibuffer
;; 300    0.20%  xah-browse-url-of-buffer
;; 294    0.20%  previous-history-element
;; 291    0.20%  minibuffer-keyboard-quit
;; 291    0.20%  ido-exit-minibuffer
;; 286    0.19%  describe-function
;; 286    0.19%  query-replace
;; 276    0.19%  kill-line
;; 267    0.18%  open-line
;; 266    0.18%  split-window-vertically
;; 258    0.17%  describe-key
;; 257    0.17%  scroll-up-command
;; 249    0.17%  xah-run-current-file
;; 245    0.17%  xah-compact-uncompact-block
;; 227    0.15%  xah-previous-user-buffer
;; 227    0.15%  left-char
;; 226    0.15%  scroll-down-command
;; 221    0.15%  yas/expand
;; 217    0.15%  xhm-wrap-p-tag
;; 217    0.15%  dired-find-file
;; 209    0.14%  dired-previous-line
;; 204    0.14%  isearch-forward-symbol-at-point
;; 201    0.14%  comment-dwim
;; 197    0.13%  xah-forward-quote
;; 196    0.13%  dired-next-line
;; 192    0.13%  xah-all-linkify
;; 190    0.13%  org-return
;; 175    0.12%  isearch-repeat-backward
;; 173    0.12%  xah-shell-commands
;; 173    0.12%  xah-clean-whitespace
;; 168    0.11%  magit-toggle-section
;; 161    0.11%  smex
;; 156    0.11%  xah-copy-file-path
;; 142    0.10%  forward-button
;; 140    0.09%  undo-tree-redo
;; 139    0.09%  push-button
;; 122    0.08%  dired-jump
;; 122    0.08%  mouse-drag-region
;; 118    0.08%  org-delete-backward-char
;; 115    0.08%  beginning-of-buffer
;; 112    0.08%  xah-insert-paren
;; 112    0.08%  xah-new-empty-buffer
;; 109    0.07%  mouse-set-point
;; 107    0.07%  describe-prefix-bindings
;; 106    0.07%  xah-insert-double-curly-quote“”
;; 103    0.07%  ido-find-file
;; 96    0.06%  xem-compact-parens
;; 89    0.06%  delete-window
;; 87    0.06%  ido-next-match
;; 82    0.06%  xah-open-file-from-clipboard
;; 80    0.05%  eshell
;; 79    0.05%  xah-find-text
;; 77    0.05%  xem-complete-or-indent
;; 75    0.05%  ido-delete-backward-updir
;; 75    0.05%  isearch-abort
;; 74    0.05%  magit-status
;; 73    0.05%  xah-open-last-closed
;; 72    0.05%  xah-delete-current-file
;; 69    0.05%  xhm-toggle-syntax-coloring-markup
;; 69    0.05%  hippie-expand
;; 66    0.04%  xhm-mark-unicode
;; 65    0.04%  magit-stage-item
;; 64    0.04%  list-matching-lines
;; 63    0.04%  minibuffer-complete-and-exit
;; 62    0.04%  end-of-buffer
;; 61    0.04%  xah-cite
;; 61    0.04%  xah-cycle-hyphen-underscore-space
;; 58    0.04%  revert-buffer
;; 58    0.04%  xah-cut-all
;; 57    0.04%  eval-last-sexp
;; 57    0.04%  xah-insert-ascii-double-quote
;; 57    0.04%  xah-html-insert-date-tag
;; 57    0.04%  xah-find-replace-text
;; 54    0.04%  xmsi-change-to-symbol
;; 53    0.04%  xah-make-atom-entry
;; 52    0.04%  xah-make-backup
;; 49    0.03%  xhm-lines-to-html-list
;; 47    0.03%  xhm-htmlize-keyboard-shortcut-notation
;; 46    0.03%  describe-variable
;; 45    0.03%  xhm-htmlize-or-de-precode
;; 45    0.03%  magit-key-mode-popup-committing
;; 45    0.03%  git-commit-commit
;; 44    0.03%  quoted-insert
;; 44    0.03%  ac-complete
;; 43    0.03%  xah-brackets-to-html
;; 41    0.03%  xhm-update-title
;; 41    0.03%  xahsite-update-article-timestamp
;; 41    0.03%  title-case-string-region-or-line
;; 40    0.03%  xah-copy-all
;; 37    0.02%  xah-insert-brace
;; 37    0.02%  dired-copy-filename-as-kill
;; 35    0.02%  xhm-get-precode-make-new-file
;; 34    0.02%  recentf-open-files
;; 34    0.02%  delete-forward-char
;; 34    0.02%  xhm-skip-tag-forward
;; 33    0.02%  insert-date
;; 33    0.02%  delete-matching-lines
;; 33    0.02%  xhm-pre-source-code
;; 32    0.02%  xah-insert-black-lenticular-bracket【】
;; 30    0.02%  ds
;; 30    0.02%  magit-key-mode-popup-pushing
;; 29    0.02%  wdired-change-to-wdired-mode
;; 29    0.02%  wdired-finish-edit
;; 29    0.02%  xhm-htmlize-elisp-keywords
;; 29    0.02%  xah-insert-corner-bracket「」
;; 29    0.02%  ido-switch-buffer
;; 28    0.02%  right-char
;; 27    0.02%  isearch-yank-word-or-char
;; 26    0.02%  quit-window
;; 25    0.02%  widen
;; 24    0.02%  indent-for-tab-command
;; 24    0.02%  eshell-send-input
;; 23    0.02%  ibuffer
;; 23    0.02%  ispell-word
;; 22    0.01%  minibuffer-complete
;; 22    0.01%  magit-stage-all
;; 22    0.01%  dired-do-flagged-delete
;; 21    0.01%  ido-magic-delete-char
;; 20    0.01%  xhm-redo-syntax-coloring-buffer
;; 20    0.01%  whitespace-mode
;; 20    0.01%  isearch-forward-word
;; 19    0.01%  xhm-remove-html-tags
;; 19    0.01%  xah-insert-tortoise-shell-bracket〔〕
;; 19    0.01%  magit-mode-quit-window
;; 18    0.01%  ido-prev-match-dir
;; 18    0.01%  narrow-to-defun
;; 18    0.01%  xah-insert-random-number
;; 18    0.01%  mark-whole-buffer
;; 18    0.01%  xah-js-mode
;; 17    0.01%  repeat-complex-command
;; 17    0.01%  elisp-index-search
;; 17    0.01%  ace-jump-mode
;; 17    0.01%  flyspell-buffer
;; 16    0.01%  xhm-extract-url
;; 16    0.01%  dired-do-copy
;; 16    0.01%  dired-mark
;; 16    0.01%  lookup-google
;; 16    0.01%  xah-forward-punct
;; 16    0.01%  view-echo-area-messages
;; 15    0.01%  shell
;; 15    0.01%  org-delete-char
;; 15    0.01%  ibuffer-visit-buffer
;; 14    0.01%  occur-mode-mouse-goto
;; 14    0.01%  dired-flag-file-deletion
;; 14    0.01%  xah-describe-major-mode
;; 13    0.01%  dired-flag-backup-files
;; 13    0.01%  comint-send-input
;; 13    0.01%  xah-insert-greater-less
;; 13    0.01%  magit-visit-item
;; 13    0.01%  xah-elisp-mode
;; 12    0.01%  calcDigit-key
;; 12    0.01%  dired-do-shell-command
;; 12    0.01%  replace-rectangle
;; 11    0.01%  xhm-skip-tag-backward
;; 11    0.01%  dired-do-rename
;; 11    0.01%  org-meta-return
;; 11    0.01%  widget-button-press
;; 11    0.01%  js2
;; 10    0.01%  xah-html-mode
;; 10    0.01%  Info-next-reference
;; 10    0.01%  ace-jump-move
;; 10    0.01%  undo
;; 10    0.01%  js2-mode-show-node
;; 10    0.01%  xah-html-image-linkify
;; 10    0.01%  top-level
;; 10    0.01%  ac-expand
;; 10    0.01%  toggle-case-fold-search
;; 9    0.01%  xah-cycle-camel-style-case
;; 9    0.01%  xah-find-count
;; 9    0.01%  js-mode
;; 9    0.01%  kill-rectangle
;; 9    0.01%  xah-insert-bracket
;; 9    0.01%  make-frame-command
;; 8    0.01%  xah-redo-syntax-coloring-html-buffer
;; 8    0.01%  ibuffer-mark-for-delete
;; 8    0.01%  narrow-to-region
;; 8    0.01%  xah-next-emacs-buffer
;; 8    0.01%  jsm
;; 7    0.00%  info
;; 7    0.00%  copy-to-register
;; 7    0.00%  occur
;; 7    0.00%  sort-lines
;; 7    0.00%  ido-magic-forward-char
;; 7    0.00%  xah-cursor-up-10-lines
;; 7    0.00%  text-scale-increase
;; 7    0.00%  xah-click-describe-char
;; 7    0.00%  linum-mode
;; 6    0.00%  calcDigit-start
;; 6    0.00%  calcDigit-nondigit
;; 6    0.00%  magit-unstage-item
;; 6    0.00%  xhm-insert-br-tag
;; 6    0.00%  xah-process-paypal-order
;; 6    0.00%  mouse-yank-primary
;; 6    0.00%  next-history-element
;; 6    0.00%  desktop-save
;; 6    0.00%  insert-register
;; 6    0.00%  lookup-wikipedia
;; 6    0.00%  xah-insert-single-angle-quote‹›
;; 6    0.00%  org-open-line
;; 6    0.00%  xah-previous-emacs-buffer
;; 6    0.00%  ido-write-file
;; 6    0.00%  xah-insert-unicode
;; 6    0.00%  yas/minor-mode
;; 5    0.00%  magit-key-mode-popup-submodule
;; 5    0.00%  Info-follow-nearest-node
;; 5    0.00%  comint-previous-input
;; 5    0.00%  ac-next
;; 5    0.00%  redo
;; 5    0.00%  dired-unmark-all-marks
;; 5    0.00%  ido-next-match-dir
;; 5    0.00%  backward-sexp
;; 5    0.00%  xah-backward-punct
;; 5    0.00%  shell-command
;; 5    0.00%  eshell-pcomplete
;; 5    0.00%  man
;; 5    0.00%  Info-copy-current-node-name
;; 5    0.00%  turn-on-eldoc-mode
;; 5    0.00%  dired-up-directory
;; 5    0.00%  toggle-debug-on-error
;; 5    0.00%  acm
;; 5    0.00%  isearch-delete-char
;; 4    0.00%  minibuffer-complete-word
;; 4    0.00%  view-lossage
;; 4    0.00%  dired-toggle-marks
;; 4    0.00%  ido-prev-match
;; 4    0.00%  Info-up
;; 4    0.00%  xah-insert-emacs-quote
;; 4    0.00%  ido-complete
;; 4    0.00%  xahsite-generate-sitemap
;; 4    0.00%  xah-toggle-read-novel-mode
;; 4    0.00%  toggle-input-method
;; 4    0.00%  forward-sexp
;; 4    0.00%  recentf-open-most-recent-file-3
;; 4    0.00%  js2-mode
;; 4    0.00%  eshell-previous-input
;; 4    0.00%  xah-insert-curly-single-quote‘’
;; 4    0.00%  save-buffers-kill-terminal
;; 4    0.00%  org-beginning-of-line
;; 4    0.00%  xah-copy-url-current-file
;; 3    0.00%  calc
;; 3    0.00%  xah-open-in-desktop
;; 3    0.00%  xhm-insert-hr-tag
;; 3    0.00%  xah-ref-span-tag
;; 3    0.00%  hi-lock-write-interactive-patterns
;; 3    0.00%  eval-defun
;; 3    0.00%  delete-rectangle
;; 3    0.00%  calendar-other-month
;; 3    0.00%  isearch-yank-kill
;; 3    0.00%  recentf-open-most-recent-file-1
;; 3    0.00%  debug-help-follow
;; 3    0.00%  delete-non-matching-lines
;; 3    0.00%  bookmark-bmenu-list
;; 3    0.00%  abort-recursive-edit
;; 3    0.00%  xah-escape-quotes
;; 3    0.00%  goto-char
;; 3    0.00%  lookup-word-definition
;; 3    0.00%  magit-key-mode-popup-merging
;; 3    0.00%  occur-mode-goto-occurrence-other-window
;; 3    0.00%  elm
;; 3    0.00%  xjs-complete-symbol
;; 3    0.00%  calc-quit
;; 2    0.00%  keyfreq-show
;; 2    0.00%  calc-times
;; 2    0.00%  eshell-previous-matching-input-from-input
;; 2    0.00%  dired-sort-toggle-or-edit
;; 2    0.00%  insert-char
;; 2    0.00%  Info-history-back
;; 2    0.00%  xah-sync-css
;; 2    0.00%  backward-delete-char-untabify
;; 2    0.00%  describe-char
;; 2    0.00%  rot13-region
;; 2    0.00%  icomplete-mode
;; 2    0.00%  mouse-set-region
;; 2    0.00%  describe-language-environment
;; 2    0.00%  where-is
;; 2    0.00%  list-packages
;; 2    0.00%  describe-key-briefly
;; 2    0.00%  compile-goto-error
;; 2    0.00%  point-to-register
;; 2    0.00%  find-function
;; 2    0.00%  magit-log
;; 2    0.00%  org-time-stamp
;; 2    0.00%  xc-comment-smart
;; 2    0.00%  upcase-initials
;; 2    0.00%  ibuffer-visit-buffer-other-window-noselect
;; 2    0.00%  ido-delete-backward-word-updir
;; 2    0.00%  highlight-phrase
;; 2    0.00%  dired-create-directory
;; 2    0.00%  sort-numeric-fields
;; 2    0.00%  isearch-ring-retreat
;; 2    0.00%  dts
;; 2    0.00%  exchange-point-and-mark
;; 2    0.00%  cm
;; 2    0.00%  Info-goto-emacs-key-command-node
;; 2    0.00%  exit-recursive-edit
;; 2    0.00%  xah-color-me-pinky
;; 2    0.00%  dired-do-byte-compile
;; 2    0.00%  xah-redo-syntax-coloring-html
;; 2    0.00%  xhm-remove-span-tag-region
;; 2    0.00%  yank-pop
;; 2    0.00%  undefined
;; 2    0.00%  delete-duplicate-lines
;; 2    0.00%  scroll-bar-toolkit-scroll
;; 2    0.00%  describe-syntax
;; 2    0.00%  view-emacs-news
;; 2    0.00%  magit-refresh
;; 2    0.00%  recentf-open-most-recent-file-2
;; 2    0.00%  xhm-replace-html-&<>-to-entities
;; 2    0.00%  magit-diff-working-tree
;; 2    0.00%  calc-pop
;; 2    0.00%  calendar
;; 1    0.00%  recentf-open-most-recent-file-5
;; 1    0.00%  yas/reload-all
;; 1    0.00%  indent-rigidly
;; 1    0.00%  indent-rigidly-right
;; 1    0.00%  xhm-wikipedia-linkify
;; 1    0.00%  ac-previous
;; 1    0.00%  dired-unmark
;; 1    0.00%  recentf-open-most-recent-file-0
;; 1    0.00%  ido-up-directory
;; 1    0.00%  recentf-open-most-recent-file-4
;; 1    0.00%  xah-insert-random-hex
;; 1    0.00%  help-follow
;; 1    0.00%  magit-dired-jump
;; 1    0.00%  indent-region
;; 1    0.00%  completion-at-point
;; 1    0.00%  flyspell-correct-word
;; 1    0.00%  ido-magic-backward-char
;; 1    0.00%  ido-enter-find-file
;; 1    0.00%  xem-complete-symbol
;; 1    0.00%  ido-toggle-case
;; 1    0.00%  magit-jump-to-diffstats
;; 1    0.00%  clone-buffer
;; 1    0.00%  occur-mode-goto-occurrence
;; 1    0.00%  frame-configuration-to-register
;; 1    0.00%  recentf-open-most-recent-file-7
;; 1    0.00%  xah-insert-ascii-single-quote
;; 1    0.00%  diredp-rename-this-file
;; 1    0.00%  ediff
;; 1    0.00%  finder-by-keyword
;; 1    0.00%  finder-select
;; 1    0.00%  compilation-next-error
;; 1    0.00%  jump-to-register
;; 1    0.00%  markdown-enter-key
;; 1    0.00%  markdown-exdent-or-delete
;; 1    0.00%  smex-major-mode-commands
;; 1    0.00%  org-shiftright
;; 1    0.00%  org-metaright
;; 1    0.00%  xah-insert-double-angle-quote«»
;; 1    0.00%  xah-copy-to-register-1
;; 1    0.00%  query-replace-regexp
;; 1    0.00%  isearch-forward-symbol
;; 1    0.00%  find-file
;; 1    0.00%  fundamental-mode
;; 1    0.00%  ruby-mode
;; 1    0.00%  ace-jump-done
;; 1    0.00%  upcase-word
;; 1    0.00%  describe-mode
;; 1    0.00%  ibuffer-do-kill-on-deletion-marks
;; 1    0.00%  ibuffer-update
;; 1    0.00%  pop-global-mark
;; 1    0.00%  perl-mode
;; 1    0.00%  shell-command-on-region
;; 1    0.00%  isearch-backward
;; 1    0.00%  toggle-word-wrap
;; 1    0.00%  xah-dired-2zip
;; 1    0.00%  recentf-cancel-dialog
;; 1    0.00%  mouse-minor-mode-menu
;; 1    0.00%  ido-list-directory
;; 1    0.00%  rectangle-mark-mode
;; 1    0.00%  xah-remove-square-brackets
;; 1    0.00%  ignore
;; 1    0.00%  recentf-open-most-recent-file-6
;; 1    0.00%  ping
;; 1    0.00%  desktop-save-mode
;; 1    0.00%  xah-replace-BOM-mark-etc
;; 1    0.00%  Info-mouse-follow-nearest-node
;; 1    0.00%  secrets-show-secrets
;; 1    0.00%  end-of-buffer-other-window
;; 1    0.00%  wdired-previous-line
;; 1    0.00%  ido-select-text
;; 1    0.00%  help-for-help
;; 1    0.00%  ido-completion-help
;; 1    0.00%  xah-cursor-down-10-lines
;; 1    0.00%  text-scale-normal-size
;; 1    0.00%  ibuffer-visit-buffer-other-window
;; 1    0.00%  backward-up-list
;; 1    0.00%  text-mode
;; 1    0.00%  xah-insert-alphabets-az
;; 1    0.00%  help-go-back
;; 1    0.00%  buffer-enable-undo
;; 1    0.00%  eshell-interrupt-process
;; 1    0.00%  xah-replace-straight-quotes
;; 1    0.00%  xah-convert-english-chinese-punctuation
;; 1    0.00%  calendar-exit

(provide 'xah-fly-keys)