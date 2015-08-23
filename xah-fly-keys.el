;; -*- coding: utf-8 -*-

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
;; • make it support diff Keyboard layouts
;; • fine-tune keys in command mode. (introduce key sequence there. Consider whether {open, close, save} should be there. and some other commands such as {dired-jump, query-replace-regexp, xah-toggle-letter-case}.)

(require 'dired) ; in emacs
(require 'dired-x) ; in emacs



(defvar xah-fly-key-map nil "Keybinding for `xah-fly-keys' minor mode.")
(progn
  (setq xah-fly-key-map (make-sparse-keymap))

  (define-key xah-fly-key-map (kbd "'") 'self-insert-command)

  (define-key xah-fly-key-map (kbd "M-[") 'xah-cycle-font-previous)
  (define-key xah-fly-key-map (kbd "M-]") 'xah-cycle-font-next)

  (define-key xah-fly-key-map (kbd "M-8") 'xah-cycle-font-2)
  (define-key xah-fly-key-map (kbd "M-7") nil)
  (define-key xah-fly-key-map (kbd "M-3") 'other-frame)
  (define-key xah-fly-key-map (kbd "M-4") nil)
  (define-key xah-fly-key-map (kbd "M-5") nil)
  (define-key xah-fly-key-map (kbd "M-6") 'yank-pop)
  (define-key xah-fly-key-map (kbd "M-2") 'xah-previous-user-buffer)
  (define-key xah-fly-key-map (kbd "M-1") 'xah-next-user-buffer)
  (define-key xah-fly-key-map (kbd "M-9") 'xah-previous-emacs-buffer)
  (define-key xah-fly-key-map (kbd "M-0") 'xah-next-emacs-buffer)

  (define-key xah-fly-key-map (kbd "M-t") 'xah-toggle-letter-case)
  (define-key xah-fly-key-map (kbd "M-c") 'xah-cycle-hyphen-underscore-space)
  (define-key xah-fly-key-map (kbd "M-r") 'hippie-expand)

  (progn
    ;; haven't decided what goes here

    (define-key xah-fly-key-map (kbd "C-1") nil)
    (define-key xah-fly-key-map (kbd "C-2") nil)
    (define-key xah-fly-key-map (kbd "C-3") nil)
    (define-key xah-fly-key-map (kbd "C-4") nil)
    (define-key xah-fly-key-map (kbd "C-5") nil)
    (define-key xah-fly-key-map (kbd "C-6") nil)
    (define-key xah-fly-key-map (kbd "C-7") nil)
    (define-key xah-fly-key-map (kbd "C-8") nil)
    (define-key xah-fly-key-map (kbd "C-9") nil)
    (define-key xah-fly-key-map (kbd "C-0") nil)

    (define-key xah-fly-key-map (kbd "<C-next>") 'xah-next-user-buffer)
    (define-key xah-fly-key-map (kbd "<C-prior>") 'xah-previous-user-buffer)))

(defvar xah-fly-major-mode-lead-key nil "Lead key for all major mode's key sequence. By default, it's (kbd \"<menu> e\"). Only supported by xah's modes.")
(setq xah-fly-major-mode-lead-key (kbd "<menu> e"))

(defvar xah-fly-use-xah-keys-p nil "If true, use xah lee's personal keys, that may not be suitable for other keyboard or workflow.")
(setq xah-fly-use-xah-keys-p t)



(defun xah-fly--get-fullpath (φfile-relative-path)
  "Return the full path of ΦFILE-RELATIVE-PATH, relative to caller's file location.

Example: If you have this line
 (xah-fly--get-fullpath \"../xyz.el\")
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

(load (xah-fly--get-fullpath "xah-fly-keys-cursor-movement"))
(load (xah-fly--get-fullpath "xah-fly-keys-editing-commands"))
(load (xah-fly--get-fullpath "xah-fly-keys-text-selection"))
(load (xah-fly--get-fullpath "xah-fly-keys-insert-commands"))
(load (xah-fly--get-fullpath "xah-fly-keys-dired-commands"))
(load (xah-fly--get-fullpath "xah-fly-keys-misc-commands"))
(load (xah-fly--get-fullpath "xah-fly-keys-insertion-keymap"))
(load (xah-fly--get-fullpath "xah-fly-keys-mode-specific"))
(load (xah-fly--get-fullpath "xah-fly-keys-user-keymap"))
(load (xah-fly--get-fullpath "xah-fly-keys-emacs-1k-keymap"))
(load (xah-fly--get-fullpath "xah-fly-keys-global-set-keys"))



(defvar xah-fly-insert-state-q t "Boolean value. true means insertion mode is on.")
(setq xah-fly-insert-state-q t)

(defun xah-fly-insert-mode-init ()
  "set insertion mode keys"
  (interactive)
  (progn

    (define-key xah-fly-key-map (kbd "'") 'self-insert-command)
    (define-key xah-fly-key-map (kbd ",") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "-") 'self-insert-command)
    (define-key xah-fly-key-map (kbd ".") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "/") 'self-insert-command)
    (define-key xah-fly-key-map (kbd ";") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "=") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "[") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "\\") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "]") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "`") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "SPC") 'self-insert-command)

    (define-key xah-fly-key-map (kbd "1") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "2") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "3") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "4") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "5") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "6") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "7") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "8") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "9") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "0") 'self-insert-command)

    (define-key xah-fly-key-map (kbd "a") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "b") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "c") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "d") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "e") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "f") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "g") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "h") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "i") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "j") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "k") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "l") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "m") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "n") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "o") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "p") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "q") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "r") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "s") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "t") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "u") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "v") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "w") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "x") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "y") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "z") 'self-insert-command)

    (define-key xah-fly-key-map (kbd "A") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "B") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "C") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "D") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "E") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "F") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "G") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "H") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "I") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "J") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "K") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "L") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "M") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "N") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "O") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "P") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "Q") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "R") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "S") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "T") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "U") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "V") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "W") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "X") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "Y") 'self-insert-command)
    (define-key xah-fly-key-map (kbd "Z") 'self-insert-command)
))

(defun xah-fly-command-mode-init ()
  "set command mode keys"
  (interactive)
  (progn

    (define-key xah-fly-key-map (kbd "'") 'xah-compact-uncompact-block)
    (define-key xah-fly-key-map (kbd ",") 'xah-shrink-whitespaces)
    (define-key xah-fly-key-map (kbd "-") nil)
    (define-key xah-fly-key-map (kbd ".") 'backward-kill-word)
    (define-key xah-fly-key-map (kbd ";") nil)
    (define-key xah-fly-key-map (kbd "/") 'xah-backward-equal-sign)
    (define-key xah-fly-key-map (kbd "\\") nil)
    (define-key xah-fly-key-map (kbd "=") 'xah-forward-equal-sign)
    (define-key xah-fly-key-map (kbd "[") 'xah-backward-quote )
    (define-key xah-fly-key-map (kbd "]") 'xah-forward-quote)
    (define-key xah-fly-key-map (kbd "`") nil)
    (define-key xah-fly-key-map (kbd "SPC") 'xah-fly-insert-mode-activate)

    ;; note:
    ;; keys 1 and 8 are swapped
    ;; keys 2 and 7 are swapped
    (define-key xah-fly-key-map (kbd "8") 'xah-fly-insert-mode-activate)
    (define-key xah-fly-key-map (kbd "7") 'delete-window)
    (define-key xah-fly-key-map (kbd "3") 'delete-other-windows)
    (define-key xah-fly-key-map (kbd "4") 'split-window-below)
    (define-key xah-fly-key-map (kbd "5") 'redo)

    (define-key xah-fly-key-map (kbd "6") 'xah-select-current-block)
    (define-key xah-fly-key-map (kbd "2") 'xah-select-current-line)
    (define-key xah-fly-key-map (kbd "1") 'xah-extend-selection)
    (define-key xah-fly-key-map (kbd "9") 'xah-select-text-in-quote)
    (define-key xah-fly-key-map (kbd "0") 'xah-backward-punct)

    (define-key xah-fly-key-map (kbd "a") 'open-line)
    (define-key xah-fly-key-map (kbd "b") 'save-buffer)
    (define-key xah-fly-key-map (kbd "c") 'previous-line)
    (define-key xah-fly-key-map (kbd "d") 'xah-beginning-of-line-or-block)
    (define-key xah-fly-key-map (kbd "e") 'delete-backward-char)
    (define-key xah-fly-key-map (kbd "f") 'undo)
    (define-key xah-fly-key-map (kbd "g") 'backward-word)
    (define-key xah-fly-key-map (kbd "h") 'backward-char)
    (define-key xah-fly-key-map (kbd "i") 'kill-line)
    (define-key xah-fly-key-map (kbd "j") 'xah-cut-line-or-region)
    (define-key xah-fly-key-map (kbd "k") 'yank)
    (define-key xah-fly-key-map (kbd "l") 'xah-forward-punct)
    (define-key xah-fly-key-map (kbd "m") 'xah-backward-left-bracket)
    (define-key xah-fly-key-map (kbd "n") 'forward-char)
    (define-key xah-fly-key-map (kbd "o") 'xah-insert-space-after)
    (define-key xah-fly-key-map (kbd "p") 'kill-word)
    (define-key xah-fly-key-map (kbd "q") 'xah-copy-line-or-region)
    (define-key xah-fly-key-map (kbd "r") 'forward-word)
    (define-key xah-fly-key-map (kbd "s") 'xah-end-of-line-or-block)
    (define-key xah-fly-key-map (kbd "t") 'next-line)
    (define-key xah-fly-key-map (kbd "u") 'delete-char)
    (define-key xah-fly-key-map (kbd "v") 'xah-forward-right-bracket)
    (define-key xah-fly-key-map (kbd "w") 'other-window)
    (define-key xah-fly-key-map (kbd "x") 'xah-fly-leader-key-map)
    (define-key xah-fly-key-map (kbd "y") 'set-mark-command)
    (define-key xah-fly-key-map (kbd "z") 'comment-dwim)

    (define-key xah-fly-key-map (kbd "A") nil)
    (define-key xah-fly-key-map (kbd "B") nil)
    (define-key xah-fly-key-map (kbd "C") nil)
    (define-key xah-fly-key-map (kbd "D") nil)
    (define-key xah-fly-key-map (kbd "E") nil)
    (define-key xah-fly-key-map (kbd "F") nil)
    (define-key xah-fly-key-map (kbd "G") nil)
    (define-key xah-fly-key-map (kbd "H") nil)
    (define-key xah-fly-key-map (kbd "I") nil)
    (define-key xah-fly-key-map (kbd "J") nil)
    (define-key xah-fly-key-map (kbd "K") nil)
    (define-key xah-fly-key-map (kbd "L") nil)
    (define-key xah-fly-key-map (kbd "M") nil)
    (define-key xah-fly-key-map (kbd "N") nil)
    (define-key xah-fly-key-map (kbd "O") nil)
    (define-key xah-fly-key-map (kbd "P") nil)
    (define-key xah-fly-key-map (kbd "Q") nil)
    (define-key xah-fly-key-map (kbd "R") nil)
    (define-key xah-fly-key-map (kbd "S") nil)
    (define-key xah-fly-key-map (kbd "T") nil)
    (define-key xah-fly-key-map (kbd "U") nil)
    (define-key xah-fly-key-map (kbd "V") nil)
    (define-key xah-fly-key-map (kbd "W") nil)
    (define-key xah-fly-key-map (kbd "X") nil)
    (define-key xah-fly-key-map (kbd "Y") nil)
    (define-key xah-fly-key-map (kbd "Z") nil)

))

(defun xah-fly-mode-toggle ()
  "Switch between {insertion, command} modes."
  (interactive)
  (if xah-fly-insert-state-q
      (xah-fly-command-mode-activate)
    (xah-fly-insert-mode-activate)))

(defun xah-fly-command-mode-activate ()
  "Activate command mode."
  (interactive)
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (setq xah-fly-insert-state-q nil )
  (xah-fly-command-mode-init))

(defun xah-fly-insert-mode-activate ()
  "Activate insertion mode."
  (interactive)
  (modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
  (setq xah-fly-insert-state-q t )
  (xah-fly-insert-mode-init))



;; when in going into minibuffer, switch to insertion mode.
(add-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)
(add-hook 'minibuffer-exit-hook 'xah-fly-command-mode-activate)

;; when in shell mode, switch to insertion mode.
(add-hook 'shell-mode-hook 'xah-fly-insert-mode-activate)



;; experimental. auto switch back to command mode after some sec of idle time
;; (setq xah-fly-timer-id (run-with-idle-timer 20 t 'xah-fly-command-mode-activate))
;; (cancel-timer xah-fly-timer-id)

(define-minor-mode xah-fly-keys
  "A modal keybinding set, like vim, but based on ergonomic principles, like Dvorak layout."
  1 "ξxfk" xah-fly-key-map
  (xah-fly-command-mode-activate))

(provide 'xah-fly-keys)
