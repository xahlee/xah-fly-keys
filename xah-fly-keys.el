;; -*- coding: utf-8 -*-

;; ergoemacs-xah (eex).
;; a vi-like modal keybinding for emacs.
;; created: 2013-09-10
;; Xah Lee

;; home page
;; http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;;; INSTALL

;; put the xah-fly-keys directory in ~/.emacs.d/lisp/

;; put the following in your emacs init at ~/.emacs.d/init.el

;; (add-to-list 'load-path "~/.emacs.d/lisp/xah-fly-keys/")
;; (require 'xah-fly-keys)

;;; LICENSE
;; buy my tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html

;;; TODO
;; • make it a proper minor mode.
;; • make it support diff Keyboard layouts
;; • fine-tune keys in command mode. (introduce key sequence there. Consider whether {open, close, save} should be there. and some other commands such as {dired-jump, query-replace-regexp, xah-toggle-letter-case}.)
;; • support different keyboard layouts.



(global-set-key (kbd "<home>") 'xfk-command-mode-activate)

(defun xah-get-fullpath (φfile-relative-path)
  "Return the full path of ΦFILE-RELATIVE-PATH, relative to caller's file location.

Example: If you have this line
 (xah-get-fullpath \"../xyz.el\")
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

(load (xah-get-fullpath "xah-fly-keys-cursor-movement"))
(load (xah-get-fullpath "xah-fly-keys-editing-commands"))
(load (xah-get-fullpath "xah-fly-keys-text-selection"))



(defvar xfk-insert-state-q t "Boolean value. true means insertion mode is on.")
(setq xfk-insert-state-q t)

(defun xfk-insert-mode-init ()
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

    (global-set-key (kbd "SPC") 'self-insert-command)

    (global-set-key (kbd "1") 'self-insert-command)
    (global-set-key (kbd "2") 'self-insert-command)
    (global-set-key (kbd "3") 'self-insert-command)
    (global-set-key (kbd "4") 'self-insert-command)
    (global-set-key (kbd "5") 'self-insert-command)
    (global-set-key (kbd "6") 'self-insert-command)
    (global-set-key (kbd "7") 'self-insert-command)
    (global-set-key (kbd "8") 'self-insert-command)
    (global-set-key (kbd "9") 'self-insert-command)
    (global-set-key (kbd "0") 'self-insert-command)

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

(defun xfk-command-mode-init ()
  "set command mode keys"
  (interactive)
  (progn
    (global-set-key (kbd ";") nil)
    (global-set-key (kbd ".") 'backward-kill-word)
    (global-set-key (kbd "'") 'xah-compact-uncompact-block)
    (global-set-key (kbd ",") 'xah-shrink-whitespaces)

    (global-set-key (kbd "SPC") 'xfk-insert-mode-activate)

    (global-set-key (kbd "1") 'xfk-insert-mode-activate)
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
    (global-set-key (kbd "w") nil)
    (global-set-key (kbd "x") 'xah-cycle-hyphen-underscore-space)
    (global-set-key (kbd "y") 'set-mark-command)
    (global-set-key (kbd "z") 'comment-dwim)))

(defun xfk-mode-toggle ()
  "Switch between {insertion, command} modes."
  (interactive)
  (if xfk-insert-state-q
      (xfk-command-mode-activate)
    (xfk-insert-mode-activate)))

(defun xfk-command-mode-activate ()
  "Activate command mode."
  (interactive)
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (setq xfk-insert-state-q nil )
  (xfk-command-mode-init))

(defun xfk-insert-mode-activate ()
  "Activate insertion mode."
  (interactive)
  (modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
  (setq xfk-insert-state-q t )
  (xfk-insert-mode-init))



;; when in going into minibuffer, switch to insertion mode.
(add-hook 'minibuffer-setup-hook 'xfk-insert-mode-activate)
(add-hook 'minibuffer-exit-hook 'xfk-command-mode-activate)

;; TODO when in shell mode, switch to insertion mode.
(add-hook 'shell-mode-hook 'xfk-insert-mode-activate)



(provide 'xah-fly-keys)