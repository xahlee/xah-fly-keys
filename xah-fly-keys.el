;;; xah-fly-keys.el --- ergonomic modal keybinding minor mode. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2013-2024 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Maintainer: Xah Lee <xah@xahlee.org>
;; Version: 24.19.20240112095825
;; Created: 2013-09-10
;; Package-Requires: ((emacs "27"))
;; Keywords: convenience, vi, vim, ergoemacs, keybinding
;; License: GPL v3.
;; Homepage: http://xahlee.info/emacs/misc/xah-fly-keys.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; xah-fly-keys is a efficient keybinding for emacs. It is modal like
;; vi, but key choices are based on statistics of command call
;; frequency.

;;; Usage:

;; M-x xah-fly-keys to toggle the mode on/off.

;; Important command/insert mode switch keys:

;; `xah-fly-command-mode-activate' (press <escape> or <home> or F8 or Alt+Space or Ctrl+Space or menu key. Note: escape only works when in GUI mode, by design.)

;; `xah-fly-insert-mode-activate' (when in command mode, press qwerty letter key f.)

;; When in command mode:

;; "f" calls `xah-fly-insert-mode-activate'.

;; Space is a leader key. For example, "SPC r" calls `query-replace'.
;; Press "SPC C-h" to see the full list.

;; "SPC SPC" also activates insertion mode.

;; "SPC RET" calls `execute-extended-command'.

;; "a" calls `execute-extended-command'.

;; The leader key sequence basically supplant ALL emacs commands that
;; starts with C-x key.

;; When using xah-fly-keys, you don't need to press Control or Meta,
;; with the following exceptions:

;; "C-c" for major mode commands.
;; "C-g" for cancel.
;; "C-q" for quoted-insert.
;; "C-h" for getting a list of keys following a prefix/leader key.

;; Leader key

;; You NEVER need to press "C-x"

;; Any emacs command that has a keybinding starting with C-x, has also
;; a key sequence binding in xah-fly-keys. For example,

;; "C-x b" for `switch-to-buffer' is "SPC f"
;; "C-x C-f" for `find-file' is "SPC i e"
;; "C-x n n" for `narrow-to-region' is "SPC l l"

;; The first key we call it leader key. In the above examples, the SPC
;; is the leader key.

;; When in command mode, the "SPC" is a leader key.

;; the following standard keys with Control are supported:

;; "C-TAB" `xah-next-user-buffer'
;; "C-S-TAB" `xah-previous-user-buffer'
;; "C-v" paste
;; "C-w" close
;; "C-z" undo
;; "C-n" new
;; "C-o" open
;; "C-s" save
;; "C-S-s" save as
;; "C-S-t" open last closed
;; "C-+" `text-scale-increase'
;; "C--" `text-scale-decrease'

;; To disable both Control and Meta shortcut keys, add the following
;; lines to you init.el BEFORE loading xah-fly-keys:

;; (setq xah-fly-use-control-key nil)
;; (setq xah-fly-use-meta-key nil)

;; If you have a bug, post on github.

;; For detail about design and other info, see home page at
;; http://xahlee.info/emacs/misc/xah-fly-keys.html

;; If you like this project, Buy Xah Emacs Tutorial
;; http://xahlee.info/emacs/emacs/buy_xah_emacs_tutorial.html or make
;; a donation. Thanks.

;;; Installation:
;; here's how to manual install
;;
;; put the file xah-fly-keys.el in ~/.emacs.d/lisp/
;; create the dir if doesn't exist.
;;
;; put the following in your emacs init file:
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'xah-fly-keys)
;; (xah-fly-keys-set-layout "qwerty") ; required
;; (xah-fly-keys 1)
;;
;; possible layout values:
;; adnw
;; azerty
;; azerty-be
;; beopy
;; bepo
;; carpalx-qfmlwy
;; carpalx-qgmlwb
;; carpalx-qgmlwy
;; colemak
;; colemak-dhm
;; colemak-dhm-angle
;; colemak-dhk
;; dvorak
;; koy
;; neo2
;; norman
;; programer-dvorak
;; pt-nativo
;; qwerty
;; qwerty-abnt
;; qwerty-no (qwerty Norwegian)
;; qwertz
;; workman
;;
;; supported layouts are stored in the variable xah-fly-layouts


;;; Code:

(require 'dired) ; in emacs
(require 'dired-x) ; in emacs



(defgroup xah-fly-keys nil
  "Ergonomic modal keybinding minor mode."
  :group 'keyboard)

(defvar xah-fly-command-mode-activate-hook nil "Hook for `xah-fly-command-mode-activate'")
(defvar xah-fly-insert-mode-activate-hook nil "Hook for `xah-fly-insert-mode-activate'")

(defvar xah-fly-command-mode-indicator "c"
  "Character in mode line indicating command mode is active.")
(defvar xah-fly-insert-mode-indicator "i"
  "Character in mode line indicating insert mode is active.")

(defcustom xah-fly-use-control-key t
  "If nil, do not bind any control key. When t, standard keys for open, close, copy, paste etc, are bound."
  :type 'boolean)

(defcustom xah-fly-use-meta-key t
  "If nil, do not bind any meta key."
  :type 'boolean)

(defcustom xah-fly-use-isearch-arrows t
  "If nil, no change to any key in isearch (`isearch-forward'). Otherwise, arrow keys are for moving between occurrences, and C-v is paste."
  :type 'boolean)

(when (not (boundp 'xah-repeat-key))
  (defvar xah-repeat-key nil "A key that some xah command use as a key to repeat the command, pressed right after command call. Value should be the same format that `kbd' returns. e.g. (kbd \"m\")")
  (if xah-repeat-key nil (setq xah-repeat-key (kbd "m"))))

(defun xah-get-bounds-of-block ()
  "Return the boundary (START . END) of current block.

URL `http://xahlee.info/emacs/emacs/elisp_get-selection-or-unit.html'
Version: 2021-08-12"
  (let (xp1 xp2 (xblankRegex "\n[ \t]*\n"))
    (save-excursion
      (setq xp1 (if (re-search-backward xblankRegex nil 1)
                    (goto-char (match-end 0))
                  (point)))
      (setq xp2 (if (re-search-forward xblankRegex nil 1)
                    (match-beginning 0)
                  (point))))
    (cons xp1 xp2)))

(defun xah-get-bounds-of-block-or-region ()
  "If region is active, return its boundary, else same as `xah-get-bounds-of-block'.

URL `http://xahlee.info/emacs/emacs/elisp_get-selection-or-unit.html'
Version: 2021-08-12"
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (xah-get-bounds-of-block)))


;; cursor movement

(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Repeat call cycles all positions in `mark-ring'.

URL `http://xahlee.info/emacs/emacs/emacs_cycle_local_mark_ring.html'
Version: 2016-04-04 2023-09-03"
  (interactive)
  (set-mark-command t))

(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of indent or line, end of previous block, in that order.

If `visual-line-mode' is on, beginning of line means visual line.

URL `http://xahlee.info/emacs/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version: 2018-06-04 2022-07-03 2022-07-06 2023-10-04"
  (interactive)
  (let ((xp (point)))
    (if (or (eq (point) (line-beginning-position))
            (eq last-command this-command))
        (when (re-search-backward "\n[\t\n ]*\n+" nil :move)
          (skip-chars-backward "\n\t ")
          ;; (forward-char)
          )
      (if visual-line-mode
          (beginning-of-visual-line)
        (if (eq major-mode 'eshell-mode)
            (progn
              (declare-function eshell-bol "esh-mode.el" ())
              (eshell-bol))
          (back-to-indentation)
          (when (eq xp (point))
            (beginning-of-line)))))))

(defun xah-end-of-line-or-block ()
  "Move cursor to end of line or next block.

• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.
• if `visual-line-mode' is on, end of line means visual line.

URL `http://xahlee.info/emacs/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version: 2018-06-04 2022-03-05 2023-10-04"
  (interactive)
  (if (or (eq (point) (line-end-position))
          (eq last-command this-command))
      (re-search-forward "\n[\t\n ]*\n+" nil :move)
    (if visual-line-mode
        (end-of-visual-line)
      (end-of-line))))

(defvar xah-brackets '("\"\""  "“”" "()" "[]" "{}" "<>" "＜＞" "（）" "［］" "｛｝" "⦅⦆" "〚〛" "⦃⦄" "‹›" "«»" "「」" "〈〉" "《》" "【】" "〔〕" "⦗⦘" "『』" "〖〗" "〘〙" "｢｣" "⟦⟧" "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "⌈⌉" "⌊⌋" "⦇⦈" "⦉⦊" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬❭" "❮❯" "❰❱" "❲❳" "〈〉" "⦑⦒" "⧼⧽" "﹙﹚" "﹛﹜" "﹝﹞" "⁽⁾" "₍₎" "⦋⦌" "⦍⦎" "⦏⦐" "⁅⁆" "⸢⸣" "⸤⸥" "⟅⟆" "⦓⦔" "⦕⦖" "⸦⸧" "⸨⸩" "｟｠")
 "A list of strings, each element is a string of 2 chars, the left bracket and a matching right bracket.
Used by `xah-select-text-in-quote' and others.
Version: 2024-01-01")

(defconst xah-left-brackets
  (mapcar (lambda (x) (substring x 0 1)) xah-brackets)
  "List of left bracket chars. Each element is a string.")

(defconst xah-right-brackets
  (mapcar (lambda (x) (substring x 1 2)) xah-brackets)
  "List of right bracket chars. Each element is a string.")

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2016-11-22 2023-07-22 2023-08-02"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp))
     ((looking-at (regexp-opt xah-left-brackets))
      (forward-sexp))
     ((if (eq (point-min) (point))
          nil
        (prog2
            (backward-char)
            (looking-at (regexp-opt xah-right-brackets))
          (forward-char)))
      ;; (prog2 (backward-char) (looking-at (regexp-opt xah-right-brackets)) (forward-char))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

(defun xah-forward-quote ()
  "Move cursor to the next occurrence of \".
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2016-07-23"
  (interactive)
  (if (re-search-forward "\\\"+" nil t)
      t
    (progn
      (message "No more quotes after cursor..")
      nil)))

(defun xah-forward-quote-twice ()
  "Call `xah-forward-quote' twice.
Returns `t' if found, else `nil'.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2016-07-23"
  (interactive)
  (when (xah-forward-quote)
    (xah-forward-quote)))

(defun xah-forward-quote-smart ()
  "Move cursor to the current or next string quote.
Place cursor at the position after the left quote.
Repeated call will find the next string.

URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
Version: 2016-11-22"
  (interactive)
  (let ((xpos (point)))
    (if (nth 3 (syntax-ppss))
        (progn
          (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
          (forward-sexp)
          (re-search-forward "\\\"" nil t))
      (progn (re-search-forward "\\\"" nil t)))
    (when (<= (point) xpos)
      (progn (re-search-forward "\\\"" nil t)))))

(defun xah-sort-lines ()
  "Like `sort-lines' but if no region, do the current block.
Version: 2022-01-22 2022-01-23"
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (sort-lines current-prefix-arg xp1 xp2)))

(defun xah-narrow-to-region ()
  "Same as `narrow-to-region', but if no selection, narrow to the current block.
Version: 2022-01-22"
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (narrow-to-region xp1 xp2)))


;; editing commands

(defun xah-copy-line-or-region ()
  "Copy current line or selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://xahlee.info/emacs/emacs/emacs_copy_cut_current_line.html'
Version: 2010-05-21 2022-10-03"
  (interactive)
  (let ((inhibit-field-text-motion nil))
    (if current-prefix-arg
        (progn
          (copy-region-as-kill (point-min) (point-max)))
      (if (region-active-p)
          (progn
            (copy-region-as-kill (region-beginning) (region-end)))
        (if (eq last-command this-command)
            (if (eobp)
                (progn )
              (progn
                (kill-append "\n" nil)
                (kill-append
                 (buffer-substring (line-beginning-position) (line-end-position))
                 nil)
                (progn
                  (end-of-line)
                  (forward-char))))
          (if (eobp)
              (if (eq (char-before) 10 )
                  (progn )
                (progn
                  (copy-region-as-kill (line-beginning-position) (line-end-position))
                  (end-of-line)))
            (progn
              (copy-region-as-kill (line-beginning-position) (line-end-position))
              (end-of-line)
              (forward-char))))))))

(defun xah-cut-line-or-region ()
  "Cut current line or selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://xahlee.info/emacs/emacs/emacs_copy_cut_current_line.html'
Version: 2010-05-21 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (region-active-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-all-or-region ()
  "Copy buffer or selection content to `kill-ring'.
Respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_cut_all_or_region.html'
Version: 2015-08-22"
  (interactive)
  (if (region-active-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (message "Text selection copied."))
    (progn
      (kill-new (buffer-string))
      (message "Buffer content copied."))))

(defun xah-cut-all-or-region ()
  "Cut buffer or selection content to `kill-ring'.
Respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_cut_all_or_region.html'
Version: 2015-08-22"
  (interactive)
  (if (region-active-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
    (progn
      (kill-new (buffer-string))
      (delete-region (point-min) (point-max)))))

(defun xah-copy-all ()
  "Put the whole buffer content into the `kill-ring'.
(respects `narrow-to-region')
Version: 2016-10-06"
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copied."))

(defun xah-cut-all ()
  "Cut the whole buffer content into the `kill-ring'.
Respects `narrow-to-region'.
Version: 2017-01-03"
  (interactive)
  (kill-new (buffer-string))
  (delete-region (point-min) (point-max)))

(defun xah-paste-or-paste-previous ()
  "Paste. When called repeatedly, paste previous.
This command calls `yank', and if repeated, call `yank-pop'.

When `universal-argument' is called first with a number arg, paste that many times.

URL `http://xahlee.info/emacs/emacs/emacs_paste_or_paste_previous.html'
Version: 2017-07-25 2020-09-08"
  (interactive)
  (progn
    (when (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if current-prefix-arg
        (progn
          (dotimes (_ (prefix-numeric-value current-prefix-arg))
            (yank)))
      (if (eq real-last-command this-command)
          (yank-pop 1)
        (yank)))))

(defconst xah-show-kill-ring-separator "\n\nSfR2h________________________________________________________________________\n\n"
  "A line divider for `xah-show-kill-ring'.")

(defun xah-show-kill-ring ()
  "Insert all `kill-ring' content in a new buffer named *copy history*.

URL `http://xahlee.info/emacs/emacs/emacs_show_kill_ring.html'
Version: 2019-12-02 2021-07-03"
  (interactive)
  (let ((xbuf (generate-new-buffer "*copy history*"))
        (inhibit-read-only t))
    (progn
      (switch-to-buffer xbuf)
      (funcall 'fundamental-mode)
      (mapc
       (lambda (x)
         (insert x xah-show-kill-ring-separator ))
       kill-ring))
    (goto-char (point-min))))

(defun xah-move-block-up ()
  "Swap the current text block with the previous.
After this command is called, press <up> or <down> to move. Any other key to exit.
Version: 2022-03-04"
  (interactive)
  (let ((xp0 (point))
        xc1 ; current block begin
        xc2 ; current Block End
        xp1 ; prev Block Begin
        xp2 ; prev Block end
        )
    (if (re-search-forward "\n[ \t]*\n+" nil "move")
        (setq xc2 (match-beginning 0))
      (setq xc2 (point)))
    (goto-char xp0)
    (if (re-search-backward "\n[ \t]*\n+" nil "move")
        (progn
          (skip-chars-backward "\n \t")
          (setq xp2 (point))
          (skip-chars-forward "\n \t")
          (setq xc1 (point)))
      (error "No previous block."))
    (goto-char xp2)
    (if (re-search-backward "\n[ \t]*\n+" nil "move")
        (progn
          (setq xp1 (match-end 0)))
      (setq xp1 (point)))
    (transpose-regions xp1 xp2 xc1 xc2)
    (goto-char xp1)
    (set-transient-map
     (let ((xkmap (make-sparse-keymap)))
       (define-key xkmap (kbd "<up>") #'xah-move-block-up)
       (define-key xkmap (kbd "<down>") #'xah-move-block-down)
       xkmap))))

(defun xah-move-block-down ()
  "Swap the current text block with the next.
After this command is called, press <up> or <down> to move. Any other key to exit.
Version: 2022-03-04"
  (interactive)
  (let ((xp0 (point))
        xc1 ; current block begin
        xc2 ; current Block End
        xn1 ; next Block Begin
        xn2 ; next Block end
        )
    (if (eq (point-min) (point))
        (setq xc1 (point))
      (if (re-search-backward "\n\n+" nil "move")
          (progn
            (setq xc1 (match-end 0)))
        (setq xc1 (point))))
    (goto-char xp0)
    (if (re-search-forward "\n[ \t]*\n+" nil "move")
        (progn
          (setq xc2 (match-beginning 0))
          (setq xn1 (match-end 0)))
      (error "No next block."))
    (if (re-search-forward "\n[ \t]*\n+" nil "move")
        (progn
          (setq xn2 (match-beginning 0)))
      (setq xn2 (point)))
    (transpose-regions xc1 xc2 xn1 xn2)
    (goto-char xn2))
  (set-transient-map
   (let ((xkmap (make-sparse-keymap)))
     (define-key xkmap (kbd "<up>") #'xah-move-block-up)
     (define-key xkmap (kbd "<down>") #'xah-move-block-down)
     xkmap)))

(defun xah-delete-left-char-or-selection ()
  "Delete backward 1 character, or selection.
Version: 2022-01-22"
  (interactive)
  (if (region-active-p)
      (progn (delete-region (region-beginning) (region-end)))
    (delete-char -1)))

(defun xah-delete-backward-char ()
  "Delete one char backward.
Version: 2023-07-22"
  (interactive)
  (delete-char -1))

(defun xah-delete-forward-bracket-pairs (&optional DeleteInnerTextQ)
  "Delete the matching bracket/quote text to the right of cursor.
e.g. ▮(a b c)

In lisp code, if DeleteInnerTextQ is true, also delete the inner text.

After the command, mark is set at the left matching bracket position, so you can `exchange-point-and-mark' to select it.

This command assumes the char to the right of point is a left bracket or quote, and have a matching one after.

What char is considered bracket or quote is determined by current syntax table.

URL `http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version: 2017-07-02 2023-07-30"
  (interactive (list t))
  (if DeleteInnerTextQ
      (progn
        (mark-sexp)
        (kill-region (region-beginning) (region-end)))
    (let ((xpt (point)))
      (forward-sexp)
      (delete-char -1)
      (push-mark (point) t)
      (goto-char xpt)
      (delete-char 1))))

(defun xah-delete-string-backward ()
  "Delete string to the left of cursor.
e.g. 「\"▮some\"▮」

Version: 2023-11-12"
  (interactive)
  (when (prog2 (backward-char) (looking-at "\\s\"") (forward-char))
    (let ((xp0 (point)) xp1 xp2)
      ;; xp1 xp2 are the begin and end pos of the string
      (if (nth 3 (syntax-ppss))
          (setq xp1 (1- xp0)
                xp2
                (progn
                  (backward-char)
                  (forward-sexp)
                  (point)))
        (setq xp2 (point)
              xp1
              (progn (forward-sexp -1) (point))))
      (if current-prefix-arg
          (progn (goto-char xp2)
                 (delete-char -1)
                 (goto-char xp1)
                 (delete-char -1))
        (kill-region xp1 xp2)))))

(defun xah-delete-backward-bracket-text ()
  "Delete the matching bracket/quote text to the left of cursor.
e.g. (a b c)▮

This command assumes the left of cursor is a right bracket, and there is a matching one before it.

What char is considered bracket or quote is determined by current syntax table.

URL `http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version: 2017-09-21 2023-07-30"
  (interactive)
  (progn
    (forward-sexp -1)
    (mark-sexp)
    (kill-region (region-beginning) (region-end))))

(defun xah-delete-backward-bracket-pair ()
  "Delete the matching brackets/quotes to the left of cursor.
After call, mark is set at the matching bracket position, so you can `exchange-point-and-mark' to select it.

This command assumes the left of point is a right bracket, and there is a matching one before it.

What char is considered bracket or quote is determined by current syntax table.

URL `http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version: 2017-07-02"
  (interactive)
  (let ((xp0 (point)) xp1)
    (forward-sexp -1)
    (setq xp1 (point))
    (goto-char xp0)
    (delete-char -1)
    (goto-char xp1)
    (delete-char 1)
    (push-mark (point) t)
    (goto-char (- xp0 2))))

(defun xah-delete-bracket-text-backward ()
  "Delete bracket pair and inner text to the left of cursor.
e.g.  「(▮some)▮」
The bracket can be paren, square bracket, curly bracket, or any matching pair in syntax table.

The deleted text can be pasted later.

What char is considered bracket is determined by current syntax table.

If cursor left is not a bracket, nothing is done.

If `universal-argument' is called first, do not delete inner text.

URL `http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version: 2017-07-02 2023-07-22 2023-07-30"
  (interactive)
  (cond
   ((prog2 (backward-char) (looking-at "\\s)") (forward-char))
    (if current-prefix-arg
        (xah-delete-backward-bracket-pair)
      (xah-delete-backward-bracket-text)))
   ((prog2 (backward-char) (looking-at "\\s(") (forward-char))
    (let ((xp0 (point)))
      (progn
        (goto-char (1- xp0))
        (forward-sexp)
        (if current-prefix-arg
            (progn
              (delete-char -1)
              (goto-char xp0)
              (delete-char -1))
          (kill-region (1- xp0) (point))))))
   ))

(defun xah-delete-blank-lines ()
  "Delete all newline around cursor.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version: 2018-04-02"
  (interactive)
  (let (xp3 xp4)
          (skip-chars-backward "\n")
          (setq xp3 (point))
          (skip-chars-forward "\n")
          (setq xp4 (point))
          (delete-region xp3 xp4)))

(defun xah-fly-delete-spaces ()
  "Delete space, tab, IDEOGRAPHIC SPACE (U+3000) around cursor.
Version: 2019-06-13"
  (interactive)
  (let (xp1 xp2)
    (skip-chars-forward " \t　")
    (setq xp2 (point))
    (skip-chars-backward " \t　")
    (setq xp1 (point))
    (delete-region xp1 xp2)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor .

Shrink neighboring spaces, then newlines, then spaces again, leaving one space or newline at each step, till no more white space.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version: 2014-10-21 2021-11-26 2021-11-30 2023-07-12"
  (interactive)
  (let ((xeol-count 0)
        (xp0 (point))
        xp1  ; whitespace begin
        xp2  ; whitespace end
        (xcharBefore (char-before))
        (xcharAfter (char-after))
        xspace-neighbor-p)

    (setq xspace-neighbor-p (or (eq xcharBefore 32) (eq xcharBefore 9) (eq xcharAfter 32) (eq xcharAfter 9)))

    (skip-chars-backward " \n\t　")
    (setq xp1 (point))
    (goto-char xp0)
    (skip-chars-forward " \n\t　")
    (setq xp2 (point))
    (goto-char xp1)
    (while (search-forward "\n" xp2 t)
      (setq xeol-count (1+ xeol-count)))
    (goto-char xp0)
    (cond
     ((eq xeol-count 0)
      (if (> (- xp2 xp1) 1)
          (progn
            (delete-horizontal-space) (insert " "))
        (progn (delete-horizontal-space))))
     ((eq xeol-count 1)
      (if xspace-neighbor-p
          (xah-fly-delete-spaces)
        (progn (xah-delete-blank-lines) (insert " "))))
     ((eq xeol-count 2)
      (if xspace-neighbor-p
          (xah-fly-delete-spaces)
        (progn
          (xah-delete-blank-lines)
          (insert "\n"))))
     ((> xeol-count 2)
      (if xspace-neighbor-p
          (xah-fly-delete-spaces)
        (progn
          (goto-char xp2)
          (search-backward "\n")
          (delete-region xp1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here"))))))

;; (defun xah-shrink-whitespaces ()
;;   "Remove whitespaces around cursor.

;; Shrink neighboring whitespace.
;; First shrink space or tab, then newlines.
;; Repeated calls eventually results in no whitespace around cursor.

;; URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
;; Version: 2014-10-21 2023-07-26 2023-08-02"
;;   (interactive)
;;   (cond
;;    ((if (eq (point-min) (point))
;;         nil
;;       (prog2 (backward-char) (looking-at "[ \t]") (forward-char)))
;;     (progn
;;       ;; (print (format "space on left"))
;;       (delete-char (- (skip-chars-backward " \t")))))
;;    ((looking-at "[ \t]")
;;     (progn
;;       ;; (print (format "space on right"))
;;       (delete-char (- (skip-chars-forward " \t")))))
;;    ((or
;;      (and (eq (char-before) 10) (eq (char-after) 10))
;;      (looking-at "\n\n")
;;      (and (eq (char-before (point)) 10) (eq (char-before (1- (point))) 10)))
;;     (progn
;;       ;; (print (format "2 newlines on left or right, or one each"))
;;       (delete-char (- (skip-chars-backward "\n")))
;;       (delete-char (- (skip-chars-forward "\n")))
;;       (insert "\n")))
;;    (t
;;     (progn
;;       ;; (print (format "catch all"))
;;       (delete-char (- (skip-chars-backward " \n")))
;;       (delete-char (- (skip-chars-forward " \n")))))))

(defvar xah-smart-delete-dispatch
  nil
  "Used by `xah-smart-delete'.
This makes that function behavior `major-mode' dependent.
Value is Alist of pairs, each is of the form
(‹major-mode-name› . ‹function-name›)
If the major mode name match current buffer, the paired function is called.
If nothing match, `xah-smart-delete' default behavior is used.
Version: 2023-11-12")

(setq xah-smart-delete-dispatch
      '((xah-wolfram-mode . xah-wolfram-smart-delete-backward)
        (xah-html-mode . xah-html-smart-delete-backward)))

(defun xah-smart-delete ()
  "Smart backward delete.
Typically, delete to the left 1 char or entire bracketed text.
Behavior depends on what's left char, and current `major-mode'.
This command never delete text to the right of cursor.

If region active, delete region.
If cursor left is space tab linefeed, delete continuous sequence of them.
If `xah-smart-delete-dispatch' match, call the matched function.
If cursor left is string quote, delete the string.
If cursor left is bracket, delete the bracketed text.
Else just delete one char to the left.

Version: 2023-07-22 2023-08-10 2023-08-23 2023-11-12"
  (interactive)
  (let (xfun)
    (cond
     ((region-active-p) (delete-region (region-beginning) (region-end)))
     ;; 32 is space, 9 is tab, 10 is linefeed
     ((eq (char-before) 32) (while (eq (char-before) 32) (delete-char -1)))
     ((eq (char-before) 9) (while (eq (char-before) 9) (delete-char -1)))
     ((eq (char-before) 10) (while (eq (char-before) 10) (delete-char -1)))
     ((setq xfun (assq major-mode xah-smart-delete-dispatch))
      (message "calling cdr of %s" xfun)
      (funcall (cdr xfun)))
     ((prog2 (backward-char) (looking-at "\\s(\\|\\s)") (forward-char))
      (message "calling xah-delete-bracket-text-backward")
      (xah-delete-bracket-text-backward))
     ((prog2 (backward-char) (looking-at "\\s\"") (forward-char))
      (message "calling xah-delete-string-backward")
      (xah-delete-string-backward))
     (t (delete-char -1)))))

(defun xah-change-bracket-pairs (FromChars ToChars)
  "Change bracket pairs to another type or none.
For example, change all parenthesis () to square brackets [].
Works on current block or selection.

In lisp code, FromChars is a string with at least 2 spaces.
e.g.
paren ( )
french angle ‹ ›
double bracket [[ ]]
etc.
It is split by space, and last 2 items are taken as left and right brackets.

ToChars is similar, with a special value of
none
followed by 2 spaces.
,it means replace by empty string.

URL `http://xahlee.info/emacs/emacs/elisp_change_brackets.html'
Version: 2020-11-01 2023-03-31 2023-08-25 2023-09-29"
  (interactive
   (let ((xbrackets
          '(
            "square [ ]"
            "brace { }"
            "paren ( )"
            "greater < >"
            "double quote \" \""
            "single quote ' '"
            "emacs ` '"
            "markdown grave accent ` `"
            "double square [[ ]]"
            "tilde ~ ~"
            "equal = ="
            "curly double quote “ ”"
            "curly single quote ‘ ’"
            "french angle ‹ ›"
            "french double angle « »"
            "corner 「 」"
            "white corner 『 』"
            "lenticular 【 】"
            "white lenticular 〖 〗"
            "angle 〈 〉"
            "double angle 《 》"
            "tortoise 〔 〕"
            "white tortoise 〘 〙"
            "white square 〚 〛"
            "white paren ⦅ ⦆"
            "white curly bracket ⦃ ⦄"
            "pointing angle 〈 〉"
            "angle with dot ⦑ ⦒"
            "curved angle ⧼ ⧽"
            "math square ⟦ ⟧"
            "math angle ⟨ ⟩"
            "math double angle ⟪ ⟫"
            "math flattened parenthesis ⟮ ⟯"
            "math white tortoise shell ⟬ ⟭"
            "heavy single quotation mark ornament ❛ ❜"
            "heavy double turned comma quotation mark ornament ❝ ❞"
            "medium parenthesis ornament ❨ ❩"
            "medium flattened parenthesis ornament ❪ ❫"
            "medium curly ornament ❴ ❵"
            "medium pointing angle ornament ❬ ❭"
            "heavy pointing angle quotation mark ornament ❮ ❯"
            "heavy pointing angle ornament ❰ ❱"
            "none  "
            )))
     (let ((completion-ignore-case t))
       (list
        (completing-read "Replace this:" xbrackets nil t nil nil (car xbrackets))
        (completing-read "To:" xbrackets nil t nil nil (car (last xbrackets)))))))
  (let (xp1 xp2 xleft xright xtoL xtoR)
    (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (let ((xsFrom (last (split-string FromChars " ") 2))
          (xsTo (last (split-string ToChars " ") 2)))

      ;; (when (< (length xsFrom) 3)
      ;; (error "cannot find input brackets %s" xsFrom))

      ;; (when (< (length xsTo) 3)
      ;;   (message "replace blacket is empty string")
      ;;   (setq xsTo (list "" "" "")))

      (setq xleft (car xsFrom)  xright (car (cdr xsFrom))
            xtoL (car xsTo) xtoR (car (cdr xsTo)))

      (save-excursion
        (save-restriction
          (narrow-to-region xp1 xp2)
          (let ((case-fold-search nil))
            (if (string-equal xleft xright)
                (let ((xx (regexp-quote xleft)))
                  (goto-char (point-min))
                  (while
                      (re-search-forward
                       (format "%s\\([^%s]+?\\)%s" xx xx xx)
                       nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match (concat xtoL "\\1" xtoR) t)))
              (progn
                (progn
                  (goto-char (point-min))
                  (while (search-forward xleft nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match xtoL t t)))
                (progn
                  (goto-char (point-min))
                  (while (search-forward xright nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match xtoR t t)))))))))))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://xahlee.info/emacs/emacs/emacs_toggle_letter_case.html'
Version: 2020-06-26 2023-11-14"
  (interactive)
  (let ( (deactivate-mark nil) xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq xp1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq xp2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region xp1 xp2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region xp1 xp2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region xp1 xp2)
      (put this-command 'state 0)))))

;; test case
;; test_case some
;; test-case
;; tes▮t-case

(defun xah-toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor.

URL `http://xahlee.info/emacs/emacs/emacs_toggle_letter_case.html'
Version: 2015-12-22 2023-11-14"
  (interactive)
  (let ((case-fold-search nil))
    (left-char 1)
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
    (right-char)))

(defun xah-upcase-sentence ()
  "Upcase first letters of sentences of current block or selection.

URL `http://xahlee.info/emacs/emacs/emacs_upcase_sentence.html'
Version: 2020-12-08 2020-12-24 2021-08-13 2022-05-16 2022-08-27"
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        ;; after period or question mark or exclamation
        (goto-char (point-min))
        (while (re-search-forward "\\(\\.\\|\\?\\|!\\)[ \n]+ *\\([a-z]\\)" nil :move)
          (upcase-region (match-beginning 2) (match-end 2))
          (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))
        ;; after a blank line, after a bullet, or beginning of buffer
        (goto-char (point-min))
        (while (re-search-forward "\\(\\`\\|• \\|\n\n\\)\\([a-z]\\)" nil :move)
          (upcase-region (match-beginning 2) (match-end 2))
          (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))
        ;; for HTML. first letter after tag
        (when
            (or
             (eq major-mode 'xah-html-mode)
             (eq major-mode 'html-mode)
             (eq major-mode 'sgml-mode)
             (eq major-mode 'nxml-mode)
             (eq major-mode 'xml-mode)
             (eq major-mode 'mhtml-mode))
          (goto-char (point-min))
          (while
              (re-search-forward "\\(<title>[ \n]?\\|<h[1-6]>[ \n]?\\|<p>[ \n]?\\|<li>[ \n]?\\|<dd>[ \n]?\\|<td>[ \n]?\\|<br ?/?>[ \n]?\\|<figcaption>[ \n]?\\)\\([a-z]\\)" nil :move)
            (upcase-region (match-beginning 2) (match-end 2))
            (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))))
      (goto-char (point-max)))
    (skip-chars-forward " \n\t")))

(defun xah-title-case-region-or-line (&optional Begin End)
  "Title case text between nearest brackets, or current line or selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, Begin End are region boundaries.

URL `http://xahlee.info/emacs/emacs/elisp_title_case_text.html'
Version: 2017-01-11 2021-03-30 2021-09-19"
  (interactive)
  (let* ((xskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕")
         (xp0 (point))
         (xp1 (if Begin
                  Begin
                (if (region-active-p)
                    (region-beginning)
                  (progn
                    (skip-chars-backward xskipChars (line-beginning-position)) (point)))))
         (xp2 (if End
                  End
                (if (region-active-p)
                    (region-end)
                  (progn (goto-char xp0)
                         (skip-chars-forward xskipChars (line-end-position)) (point)))))
         (xstrPairs [
                     [" A " " a "]
                     [" An " " an "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ["'T " "'t "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (xx)
             (goto-char (point-min))
             (while
                 (search-forward (aref xx 0) nil t)
               (replace-match (aref xx 1) t t)))
           xstrPairs))))))

(defun xah-add-space-after-comma ()
  "Add a space after comma of current block or selection.
and highlight changes made.
Version: 2022-01-20"
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (goto-char (point-min))
      (while
          (re-search-forward ",\\b" nil t)
        (replace-match ", ")
        (overlay-put
         (make-overlay
          (match-beginning 0)
          (match-end 0)) 'face 'highlight)))))

(defun xah-toggle-read-novel-mode ()
  "Setup current frame to be suitable for reading long novel/article text.
• Set frame width to 70
• Line wrap at word boundaries.
• Line spacing is increased.
• Proportional width font is used.
Call again to toggle back.

URL `http://xahlee.info/emacs/emacs/emacs_novel_reading_mode.html'
Version: 2019-01-30 2021-01-16"
  (interactive)
  (if (eq (frame-parameter (selected-frame) 'width) 70)
      (progn
        (set-frame-parameter (selected-frame) 'width 106)
        (variable-pitch-mode 0)
        (setq line-spacing nil)
        (setq word-wrap nil))
    (progn
      (set-frame-parameter (selected-frame) 'width 70)
      (variable-pitch-mode 1)
      (setq line-spacing 0.5)
      (setq word-wrap t)))
  (redraw-frame (selected-frame)))

(defun xah-fill-or-unfill ()
  "Reformat current block or selection to short/long line.
First call will break into multiple short lines. Repeated call toggles between short and long lines.
This commands calls `fill-region' to do its work. Set `fill-column' for short line length.

URL `http://xahlee.info/emacs/emacs/modernization_fill-paragraph.html'
Version: 2020-11-22 2021-08-13"
  (interactive)
  ;; This command symbol has a property “'longline-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( (xisLongline (if (eq last-command this-command) (get this-command 'longline-p) t))
         (deactivate-mark nil)
         xp1 xp2 )
    (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (if xisLongline
        (fill-region xp1 xp2)
      (let ((fill-column 99999 ))
        (fill-region xp1 xp2)))
    (put this-command 'longline-p (not xisLongline))))

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://xahlee.info/emacs/emacs/emacs_unfill-paragraph.html'
Version: 2010-05-12 2022-05-20"
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph)))

(defun xah-unfill-region (Begin End)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://xahlee.info/emacs/emacs/emacs_unfill-paragraph.html'
Version: 2010-05-12 2022-05-20"
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region Begin End)))

(defun xah-change-newline-chars-to-one (Begin End)
  "Replace newline char sequence by just one.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Version: 2021-07-06"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (re-search-forward "\n\n+" nil :move) (replace-match "\n")))))

(defun xah-reformat-whitespaces-to-one-space (Begin End)
  "Replace whitespaces by one space.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Version: 2017-01-11 2022-01-08"
  (interactive "r")
  (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (search-forward "\n" nil :move) (replace-match " "))
      (goto-char (point-min))
      (while (search-forward "\t" nil :move) (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward " +" nil :move) (replace-match " "))
      (goto-char (point-max))))

(defun xah-reformat-to-multi-lines ( &optional Begin End MinLength)
  "Replace spaces by a newline at ~70 chars, on current block or selection.
If `universal-argument' is called first, ask user for max width.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Version: 2018-12-16 2021-07-06 2021-08-12"
  (interactive)
  (let ( xp1 xp2 xminlen )
    (setq xminlen (if MinLength MinLength (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column)))
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds))))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil :move)
          (when (> (- (point) (line-beginning-position)) xminlen)
            (replace-match "\n" )))))))

(defun xah-reformat-lines (&optional Width)
  "Reformat current block or selection into short lines or 1 long line.
When called for the first time, change to one line. Second call change it to multi-lines. Repeated call toggles.
If `universal-argument' is called first, ask user to type max length of line. By default, it is 66.

Note: this command is different from emacs `fill-region' or `fill-paragraph'.
This command never adds or delete non-whitespace chars. It only exchange whitespace sequence.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Created 2016 or before.
Version: 2021-07-05 2021-08-13 2022-03-12 2022-05-16 2022-12-24"
  (interactive)
  ;; This symbol has a property 'is-long-p, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let (xisLong xwidth xp1 xp2)
    (setq xwidth (if Width Width (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 66)))
    (setq xisLong (if (eq last-command this-command) (get this-command 'is-long-p) nil))
    (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (if current-prefix-arg
        (xah-reformat-to-multi-lines xp1 xp2 xwidth)
      (if xisLong
          (xah-reformat-to-multi-lines xp1 xp2 xwidth)
        (progn
          (xah-reformat-whitespaces-to-one-space xp1 xp2))))
    (put this-command 'is-long-p (not xisLong))))

(defun xah-reformat-to-sentence-lines ()
  "Reformat current block or selection into multiple lines by ending period.
Move cursor to the beginning of next text block.
After this command is called, press `xah-repeat-key' to repeat it.

URL `http://xahlee.info/emacs/emacs/elisp_reformat_to_sentence_lines.html'
Version: 2020-12-02 2023-05-25 2023-11-09"
  (interactive)
  (let (xp1 xp2)
    (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (goto-char (point-min)) (while (search-forward "。" nil t) (replace-match "。\n"))
      ;; (goto-char (point-min)) (while (search-forward " <a " nil t) (replace-match "\n<a "))
      ;; (goto-char (point-min)) (while (search-forward "</a> " nil t) (replace-match "</a>\n"))
      (goto-char (point-min))
      (while (re-search-forward "\\([A-Za-z0-9]+\\)[ \t]*\n[ \t]*\\([A-Za-z0-9]+\\)" nil t)
        (replace-match "\\1 \\2"))
      (goto-char (point-min))
      (while (re-search-forward "\\([,]\\)[ \t]*\n[ \t]*\\([A-Za-z0-9]+\\)" nil t)
        (replace-match "\\1 \\2"))
      (goto-char (point-min))
      (while (re-search-forward "  +" nil t) (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "\\([.?!]\\) +\\([(0-9A-Za-z]+\\)" nil t) (replace-match "\\1\n\\2"))
      (goto-char (point-max))
      (while (eq (char-before) 32) (delete-char -1))))
  (re-search-forward "\n+" nil :move)
  (set-transient-map (let ((xkmap (make-sparse-keymap))) (define-key xkmap (or xah-repeat-key (kbd "DEL")) this-command) xkmap))
  (set-transient-map (let ((xkmap (make-sparse-keymap))) (define-key xkmap (kbd "DEL") this-command) xkmap)))

(defun xah-space-to-newline ()
  "Replace space sequence to a newline char in current block or selection.

URL `http://xahlee.info/emacs/emacs/emacs_space_to_newline.html'
Version: 2017-08-19 2021-11-28"
  (interactive)
  (let* ((xbds (xah-get-bounds-of-block-or-region))
         (xp1 (car xbds))
         (xp2 (cdr xbds)))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (goto-char (point-min))
      (while (re-search-forward " +" nil t)
        (replace-match "\n")))))

(defun xah-slash-to-backslash (&optional Begin End)
  "Replace slash by backslash on current line or region.
Version: 2021-07-14 2021-09-12"
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (setq xp1 (line-beginning-position) xp2 (line-end-position))))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "/" nil t)
          (replace-match "\\\\"))))))

(defun xah-backslash-to-slash (&optional Begin End)
  "Replace backslash by slash on current line or region.
Version: 2021-09-11"
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (setq xp1 (line-beginning-position) xp2 (line-end-position))))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\" nil t)
          (replace-match "/"))))))

(defun xah-double-backslash (&optional Begin End)
  "Replace backslash by two backslash on current line or region.
Version: 2021-11-09"
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (setq xp1 (line-beginning-position) xp2 (line-end-position))))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\" nil t)
          (replace-match "\\\\\\\\"))))))

(defun xah-double-backslash-to-single (&optional Begin End)
  "Replace double backslash by single backslash on current line or region.
Version: 2021-11-09"
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (setq xp1 (line-beginning-position) xp2 (line-end-position))))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\\\"  nil t)
          (replace-match "\\\\"))))))

(defun xah-slash-to-double-backslash (&optional Begin End)
  "Replace slash by double backslash on current line or region.
Version: 2021-07-14"
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (setq xp1 (line-beginning-position) xp2 (line-end-position))))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "/" nil t)
          (replace-match "\\\\\\\\"))))))

(defun xah-double-backslash-to-slash (&optional Begin End)
  "Replace double backslash by slash on current line or region.
Version: 2021-07-14"
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (setq xp1 (line-beginning-position) xp2 (line-end-position))))
    (save-restriction
      (narrow-to-region xp1 xp2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\\\" nil t)
          (replace-match "/"))))))

(defun xah-comment-dwim ()
  "Toggle comment in programing language code.

Like `comment-dwim', but toggle comment if cursor is not at end of line.
If cursor is at end of line, either add comment at the line end or move cursor to start of line end comment. call again to comment out whole line.

URL `http://xahlee.info/emacs/emacs/emacs_toggle_comment_by_line.html'
Version: 2016-10-25 2023-07-10"
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let ((xbegin (line-beginning-position))
          (xend (line-end-position)))
      (if (eq xbegin xend)
          (progn
            (comment-dwim nil))
        (if (eq (point) xend)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region xbegin xend)
            (forward-line )))))))

(defun xah-quote-lines (QuoteL QuoteR Sep)
  "Add quotes/brackets and separator (comma) to lines.
Act on current block or selection.

For example,

 cat
 dog
 cow

becomes

 \"cat\",
 \"dog\",
 \"cow\",

or

 (cat)
 (dog)
 (cow)

In lisp code, QuoteL QuoteR Sep are strings.

URL `http://xahlee.info/emacs/emacs/emacs_quote_lines.html'
Version: 2020-06-26 2023-09-19 2023-10-29"
  (interactive
   (let ((xbrackets
          '(
            "\"double quote\""
            "'single quote'"
            "(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "“curly double”"
            "‘curly single’"
            "‹french angle›"
            "«french double angle»"
            "「corner」"
            "none"
            "other"
            ))
         (xcomma '("comma ," "semicolon ;" "none" "other"))
         xbktChoice xsep xsepChoice xquoteL xquoteR)
     (let ((completion-ignore-case t))
       (setq xbktChoice (completing-read "Quote to use:" xbrackets nil t nil nil (car xbrackets)))
       (setq xsepChoice (completing-read "line separator:" xcomma nil t nil nil (car xcomma))))
     (cond
      ((string-equal xbktChoice "none")
       (setq xquoteL "" xquoteR ""))
      ((string-equal xbktChoice "other")
       (let ((xx (read-string "Enter 2 chars, for begin/end quote:")))
         (setq xquoteL (substring xx 0 1)
               xquoteR (substring xx 1 2))))
      (t (setq xquoteL (substring xbktChoice 0 1)
               xquoteR (substring xbktChoice -1))))
     (setq xsep
           (cond
            ((string-equal xsepChoice "comma ,") ",")
            ((string-equal xsepChoice "semicolon ;") ";")
            ((string-equal xsepChoice "none") "")
            ((string-equal xsepChoice "other") (read-string "Enter separator:"))
            (t xsepChoice)))
     (list xquoteL xquoteR xsep)))
  (let (xp1 xp2 (xquoteL QuoteL) (xquoteR QuoteR) (xsep Sep))
    (let ((xbds (xah-get-bounds-of-block-or-region)))
      (setq xp1 (car xbds) xp2 (cdr xbds)))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (goto-char (point-min))
        (catch 'EndReached
          (while t
            (skip-chars-forward "\t ")
            (insert xquoteL)
            (end-of-line)
            (insert xquoteR xsep)
            (if (eq (point) (point-max))
                (throw 'EndReached t)
              (forward-char))))))))

(defun xah-escape-quotes (Begin End)
  "Add slash before double quote in current line or selection.
Double quote is codepoint 34.
See also: `xah-unescape-quotes'
URL `http://xahlee.info/emacs/emacs/elisp_escape_quotes.html'
Version: 2017-01-11"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
      (save-restriction
        (narrow-to-region Begin End)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" t t)))))

(defun xah-unescape-quotes (&optional Begin End)
  "Replace  「\\\"」 by 「\"」 in current line or selection.
See also: `xah-escape-quotes'

URL `http://xahlee.info/emacs/emacs/elisp_escape_quotes.html'
Version: 2017-01-11 2023-11-02"
  (interactive)
  (let (xp1 xp2)
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (setq xp1 (line-beginning-position) xp2 (line-end-position))))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (goto-char (point-min))
        (while (search-forward "\\\"" nil t)
          (replace-match "\"" t t))))))

(defun xah-cycle-hyphen-lowline-space (&optional Begin End)
  "Cycle {hyphen lowline space} chars.

The region to work on is by this order:
 1. if there is a selection, use that.
 2. If cursor is in a string quote or any type of bracket, and is within current line, work on that region.
 3. else, work on current line.

After this command is called, press `xah-repeat-key' to repeat it.

URL `http://xahlee.info/emacs/emacs/elisp_change_space-hyphen_underscore.html'
Version: 2019-02-12 2023-07-16 2024-01-04"
  (interactive)
  ;; this function sets a property 'state. Possible values are 0 to length of xcharArray.
  (let (xp1 xp2 xlen
            (xcharArray ["-" "_" " "])
            (xregionWasActive-p (region-active-p))
            (xnowState (if (eq last-command this-command) (get 'xah-cycle-hyphen-lowline-space 'state) 0))
            xchangeTo)
    (setq
     xlen (length xcharArray)
     xchangeTo (elt xcharArray xnowState))
    (if (and Begin End)
        (setq xp1 Begin xp2 End)
      (if (region-active-p)
          (setq xp1 (region-beginning) xp2 (region-end))
        (let ((xskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
          (skip-chars-backward xskipChars (line-beginning-position))
          (setq xp1 (point))
          (skip-chars-forward xskipChars (line-end-position))
          (setq xp2 (point))
          (push-mark xp1))))
    (save-excursion
      (save-restriction
        (narrow-to-region xp1 xp2)
        (goto-char (point-min))
        (while (re-search-forward (elt xcharArray (% (+ xnowState 2) xlen)) (point-max) 1)
          (replace-match xchangeTo t t))))
    (when (or (string-equal xchangeTo " ") xregionWasActive-p)
      (goto-char xp2)
      (push-mark xp1)
      (setq deactivate-mark nil))
    (put 'xah-cycle-hyphen-lowline-space 'state (% (+ xnowState 1) xlen)))
  (set-transient-map (let ((xkmap (make-sparse-keymap))) (define-key xkmap (or xah-repeat-key (kbd "DEL")) this-command) xkmap)))

(defun xah-copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the current or marked files.

If a buffer is not file and not dired, copy value of `default-directory'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_file_path.html'
Version: 2018-06-18 2021-09-30"
  (interactive "P")
  (let ((xfpath
         (if (eq major-mode 'dired-mode)
             (progn
               (let ((xresult (mapconcat #'identity
                                         (dired-get-marked-files) "\n")))
                 (if (equal (length xresult) 0)
                     (progn default-directory )
                   (progn xresult))))
           (if buffer-file-name
               buffer-file-name
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "Directory copied: %s" (file-name-directory xfpath))
           (file-name-directory xfpath))
       (progn
         (message "File path copied: %s" xfpath)
         xfpath )))))

(defun xah-delete-current-text-block ()
  "Delete the current text block plus blank lines, or selection, and copy to `kill-ring'.

If cursor is between blank lines, delete following blank lines.

URL `http://xahlee.info/emacs/emacs/emacs_delete_block.html'
Version: 2017-07-09 2023-06-07 2023-10-09"
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (progn
        (if (re-search-backward "\n[ \t]*\n+" nil :move)
            (setq xp1 (goto-char (match-end 0)))
          (setq xp1 (point)))
        (if (re-search-forward "\n[ \t]*\n+" nil :move)
            (setq xp2 (match-end 0))
          (setq xp2 (point-max)))))
    (kill-region xp1 xp2)))

(defun xah-copy-to-register-1 ()
  "Copy current line or selection to register 1.

See also:
`xah-copy-to-register-1'
`xah-append-to-register-1'
`xah-paste-from-register-1'
`xah-clear-register-1'

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version: 2012-07-17 2023-04-07 2023-08-05"
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
         (setq xp1 (region-beginning) xp2 (region-end))
      (setq xp1 (line-beginning-position) xp2 (line-end-position)))
    (copy-to-register ?1 xp1 xp2)
    (message "Copied to register 1: [%s]." (buffer-substring xp1 xp2))))

(defun xah-append-to-register-1 ()
  "Append current line or selection to register 1.
When no selection, append current line, with newline char.

See also:
`xah-copy-to-register-1'
`xah-append-to-register-1'
`xah-paste-from-register-1'
`xah-clear-register-1'

URL `http://xahlee.info/emacs/emacs/emacs_copy_append.html'
Version: 2015-12-08 2023-04-07 2023-08-05"
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
         (setq xp1 (region-beginning) xp2 (region-end))
      (setq xp1 (line-beginning-position) xp2 (line-end-position)))
    (append-to-register ?1 xp1 xp2)
    (with-temp-buffer (insert "\n")
                      (append-to-register ?1 (point-min) (point-max)))
    (message "Appended to register 1: [%s]." (buffer-substring xp1 xp2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.

See also:
`xah-copy-to-register-1'
`xah-append-to-register-1'
`xah-paste-from-register-1'
`xah-clear-register-1'

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version: 2015-12-08 2023-04-07"
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun xah-clear-register-1 ()
  "Clear register 1.

See also:
`xah-copy-to-register-1'
`xah-append-to-register-1'
`xah-paste-from-register-1'
`xah-clear-register-1'

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version: 2015-12-08 2023-04-07"
  (interactive)
  (progn
    (copy-to-register ?1 (point-min) (point-min))
    (message "Cleared register 1.")))


;; insertion commands

(defun xah-insert-date ()
  "Insert current date time.
Insert date in this format: yyyy-mm-dd.
If `universal-argument' is called first, prompt for a format to use.
If there is selection, delete it first.

URL `http://xahlee.info/emacs/emacs/elisp_insert-date-time.html'
Version: 2013-05-10 2023-09-30 2023-10-01"
  (interactive)
  (let (xmenu xstyle)
    (setq
     xmenu
     '(("ISO date • 2018-04-12" . (format-time-string "%Y-%m-%d"))
       ("all digits datetime • 20180412224611" . (format-time-string "%Y%m%d%H%M%S"))
       ("date _ time digits • 2018-04-12_224611" . (format-time-string "%Y-%m-%d_%H%M%S"))
       ("ISO datetime full • 2018-04-12T22:46:11-07:00" .
        (concat
         (format-time-string "%Y-%m-%dT%T")
         ((lambda (xx) (format "%s:%s" (substring xx 0 3) (substring xx 3 5)))
          (format-time-string "%z"))))
       ("ISO datetime w space • 2018-04-12 22:46:11-07:00" .
        (concat
         (format-time-string "%Y-%m-%d %T")
         ((lambda (xx) (format "%s:%s" (substring xx 0 3) (substring xx 3 5)))
          (format-time-string "%z"))))
       ("ISO date + weekday • 2018-04-12 Thursday" . (format-time-string "%Y-%m-%d %A"))
       ("USA date + weekday • Thursday, April 12, 2018" . (format-time-string "%A, %B %d, %Y"))
       ("USA date + weekday abbrev • Thu, Apr 12, 2018" . (format-time-string "%a, %b %d, %Y"))
       ("USA date • April 12, 2018" . (format-time-string "%B %d, %Y"))
       ("USA date abbrev • Apr 12, 2018" . (format-time-string "%b %d, %Y")))

     xstyle
     (if current-prefix-arg
         (let ((completion-ignore-case t))
           (completing-read "Style:" xmenu nil t nil nil (caar xmenu)))
       (caar xmenu)))
    (when (region-active-p) (delete-region (region-beginning) (region-end)))
    (insert (eval (cdr (assoc xstyle xmenu))))))

(defun xah-insert-bracket-pair (LBracket RBracket &optional WrapMethod)
  "Insert brackets around selection, word, at point, and maybe move cursor in between.

 LBracket and RBracket are strings. WrapMethod must be either `line' or `block'. `block' means between empty lines.

• If there is a active region, wrap around region.
Else
• If WrapMethod is `line', wrap around line.
• If WrapMethod is `block', wrap around block.
Else
• If cursor is at beginning of line and its not empty line and contain at least 1 space, wrap around the line.
• If cursor is at end of a word or buffer, one of the following will happen:
 xyz▮ → xyz(▮)
 xyz▮ → (xyz▮)       if in one of the lisp modes.
• wrap brackets around word if any. e.g. xy▮z → (xyz▮). Or just (▮)

URL `http://xahlee.info/emacs/emacs/elisp_insert_brackets_by_pair.html'
Version: 2017-01-17 2021-08-12"
  (if (region-active-p)
      (progn
        (let ((xp1 (region-beginning)) (xp2 (region-end)))
          (goto-char xp2) (insert RBracket)
          (goto-char xp1) (insert LBracket)
          (goto-char (+ xp2 2))))
    (let (xp1 xp2)
      (cond
       ((eq WrapMethod 'line)
        (setq xp1 (line-beginning-position) xp2 (line-end-position))
        (goto-char xp2)
        (insert RBracket)
        (goto-char xp1)
        (insert LBracket)
        (goto-char (+ xp2 (length LBracket))))
       ((eq WrapMethod 'block)
        (save-excursion
          (let ((xbds (xah-get-bounds-of-block-or-region))) (setq xp1 (car xbds) xp2 (cdr xbds)))
          (goto-char xp2)
          (insert RBracket)
          (goto-char xp1)
          (insert LBracket)
          (goto-char (+ xp2 (length LBracket)))))
       ( ; do line. line must contain space
        (and
         (eq (point) (line-beginning-position))
         (not (eq (line-beginning-position) (line-end-position))))
        (insert LBracket)
        (end-of-line)
        (insert  RBracket))
       ((and
         (or ; cursor is at end of word or buffer. i.e. xyz▮
          (looking-at "[^-_[:alnum:]]")
          (eq (point) (point-max)))
         (not (or
               (eq major-mode 'xah-elisp-mode)
               (eq major-mode 'emacs-lisp-mode)
               (eq major-mode 'lisp-mode)
               (eq major-mode 'lisp-interaction-mode)
               (eq major-mode 'common-lisp-mode)
               (eq major-mode 'clojure-mode)
               (eq major-mode 'xah-clojure-mode)
               (eq major-mode 'scheme-mode))))
        (progn
          (setq xp1 (point) xp2 (point))
          (insert LBracket RBracket)
          (search-backward RBracket)))
       (t (progn
            ;; wrap around “word”. basically, want all alphanumeric, plus hyphen and underscore, but don't want space or punctuations. Also want chinese chars
            ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
            (skip-chars-backward "-_[:alnum:]")
            (setq xp1 (point))
            (skip-chars-forward "-_[:alnum:]")
            (setq xp2 (point))
            (goto-char xp2)
            (insert RBracket)
            (goto-char xp1)
            (insert LBracket)
            (goto-char (+ xp2 (length LBracket)))))))))

(defun xah-insert-paren () (interactive) (xah-insert-bracket-pair "(" ")") )
(defun xah-insert-square-bracket () (interactive) (xah-insert-bracket-pair "[" "]") )
(defun xah-insert-brace () (interactive) (xah-insert-bracket-pair "{" "}") )

(defun xah-insert-markdown-quote () (interactive) (xah-insert-bracket-pair "`" "`") )
(defun xah-insert-markdown-triple-quote () (interactive) (xah-insert-bracket-pair "```\n" "\n```"))

(defun xah-insert-double-curly-quote () (interactive) (xah-insert-bracket-pair "“" "”") )
(defun xah-insert-curly-single-quote () (interactive) (xah-insert-bracket-pair "‘" "’") )
(defun xah-insert-single-angle-quote () (interactive) (xah-insert-bracket-pair "‹" "›") )
(defun xah-insert-double-angle-quote () (interactive) (xah-insert-bracket-pair "«" "»") )
(defun xah-insert-ascii-double-quote () (interactive) (xah-insert-bracket-pair "\"" "\"") )
(defun xah-insert-ascii-single-quote () (interactive) (xah-insert-bracket-pair "'" "'") )
(defun xah-insert-emacs-quote () (interactive) (xah-insert-bracket-pair "`" "'") )
(defun xah-insert-corner-bracket () (interactive) (xah-insert-bracket-pair "「" "」" ) )
(defun xah-insert-white-corner-bracket () (interactive) (xah-insert-bracket-pair "『" "』") )
(defun xah-insert-angle-bracket () (interactive) (xah-insert-bracket-pair "〈" "〉") )
(defun xah-insert-double-angle-bracket () (interactive) (xah-insert-bracket-pair "《" "》") )
(defun xah-insert-white-lenticular-bracket () (interactive) (xah-insert-bracket-pair "〖" "〗") )
(defun xah-insert-black-lenticular-bracket () (interactive) (xah-insert-bracket-pair "【" "】") )
(defun xah-insert-tortoise-shell-bracket () (interactive) (xah-insert-bracket-pair "〔" "〕" ) )

(defun xah-insert-hyphen ()
  "Insert a HYPHEN-MINUS character."
  (interactive)
  (insert "-"))

(defun xah-insert-low-line ()
  "Insert a LOW LINE character."
  (interactive)
  (insert "_"))

(defun xah-insert-string-assignment ()
  "Insert =\"\""
  (interactive)
  (progn (insert "=\"\"")
         (left-char)))

(defun xah-insert-space-before ()
  "Insert space before cursor."
  (interactive)
  (insert " "))

(defun xah-insert-space-after ()
  "Insert space after cursor"
  (interactive)
  (insert " ")
  (left-char))

(defun xah-insert-formfeed ()
  "Insert a form feed char (codepoint 12)"
  (interactive)
  (insert "\n\u000c\n"))

(defun xah-show-formfeed-as-line ()
  "Display the formfeed ^L char as line.
URL `http://xahlee.info/emacs/emacs/emacs_show_form_feed_as_line.html'
Version: 2018-08-30 2023-07-29"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))

(defun xah-insert-column-az ()
  "Insert letters A to Z vertically, similar to `rectangle-number-lines'.
The commpand will prompt for a start char, and number of chars to insert.
The start char can be any char in Unicode.

URL `http://xahlee.info/emacs/emacs/emacs_insert-alphabets.html'
Version: 2013-06-12 2019-03-07"
  (interactive)
  (let (
        (xstartChar (string-to-char (read-string "Start char: " "a")))
        (xhowmany (string-to-number (read-string "How many: " "26")))
        (xcolpos (- (point) (line-beginning-position))))
    (dotimes (xi xhowmany )
      (progn
        (insert-char (+ xi xstartChar))
        (forward-line)
        (beginning-of-line)
        (forward-char xcolpos)))))

(defvar xah-unicode-list nil
 "A alist.
Each item is (prompStr . xString). Used by `xah-insert-unicode'.
prompStr is used for prompt.
xString is used for insert a unicode.
xString can be any string, needs not be a char or emoji.
")

(setq
 xah-unicode-list
 '(
   ;;
   ("smile beaming 😊" . "😊")
   ("tears of joy" . "😂")
   ("hug 🤗" . "🤗")
   ("heart eyes 😍" . "😍")
   ("heart face 🥰" . "🥰")
   ("angry 😠" . "😠")
   ("vomit 🤮" . "🤮")
   ("thumb up 👍" . "👍")
   ("thumb down 👎" . "👎")
   ("checkmark ✅" . "✅")
   ("new 🆕" . "🆕")
   ("glowing star 🌟" . "🌟")
   ("star ⭐" . "⭐")
   ("sparkles ✨" . "✨")
   ("rocket 🚀" . "🚀")
   ("sun 🌞" . "🌞")
   ("heart 🧡" . "🧡")
   ("clown 🤡" . "🤡")
   ("large circle" . "⭕")
   ("cross ❌" . "❌")
   ("red triangle 🔺" . "🔺")
   ("diamond 💠" . "💠")
   ("square" . "⬛")
   ("cursor ▮" . "▮")

   ("double angle bracket" . "《》")
   ("black lenticular bracket" . "【】")
   ("corner-bracket" . "「」")
   ("tortoise shell bracket" . "〔〕")
   ("angle bracket" . "〈〉")
   ("double angle quote" . "«»")

   ("bullet •" . "•")
   ("diamond ◆" . "◆")
   ("...ellipsis …" . "…")
   ("nbsp non breaking space" . " ")
   ("chinese comma 、" . "、")
   ("emdash —" . "—")
   ("fullwidth ampersand ＆" . "＆")
   ("left arrow ←" . "←")
   ("right arrow →" . "→")
   ("up arrow ↑" . "↑")
   ("down arrow ↓" . "↓")
   ;;
   ))

(defun xah-insert-unicode ()
  "Insert a unicode from a custom list `xah-unicode-list'.
URL `http://xahlee.info/emacs/emacs/emacs_insert_unicode.html'
Version: 2021-01-05 2023-08-25 2023-08-31 2023-09-19"
  (interactive)
  (let ((xkey
         (let ((completion-ignore-case t))
           (completing-read "Insert:" xah-unicode-list nil t))))
    (insert (cdr (assoc xkey xah-unicode-list)))))


;; text selection

(defun xah-select-block ()
  "Select the current/next block plus 1 blankline.
If region is active, extend selection downward by block.

URL `http://xahlee.info/emacs/emacs/emacs_select_text_block.html'
Version: 2019-12-26 2021-08-13 2023-11-14"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil :move)
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil :move)
        (goto-char (match-end 0)))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil :move))))

(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
If `visual-line-mode' is on, consider line as visual line.

URL `http://xahlee.info/emacs/emacs/emacs_select_line.html'
Version: 2017-11-01 2023-07-16 2023-11-14"
  (interactive)
  (if (region-active-p)
      (if visual-line-mode
          (let ((xp1 (point)))
            (end-of-visual-line 1)
            (when (eq xp1 (point))
              (end-of-visual-line 2)))
        (progn
          (forward-line 1)
          (end-of-line)))
    (if visual-line-mode
        (progn (beginning-of-visual-line)
               (push-mark (point) t t)
               (end-of-visual-line))
      (progn
        (push-mark (line-beginning-position) t t)
        (end-of-line)))))

(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.

when there is no selection,
• If cursor is on any type of bracket (including parenthesis, quotation mark), select whole bracketed thing including bracket
• else, select current word.

when there is a selection, the selection extension behavior is still experimental. But when cursor is on a any type of bracket (parenthesis, quote), it extends selection to outer bracket.

URL `http://xahlee.info/emacs/emacs/emacs_extend_selection.html'
Version: 2020-02-04 2023-08-24 2023-11-14"
  (interactive)

  (cond
   ((region-active-p)
    (let ((xp1 (region-beginning)) (xp2 (region-end)))
      (goto-char xp1)
      (cond
       ((looking-at "\\s(")
        (if (eq (nth 0 (syntax-ppss)) 0)
            (progn
              ;; (message "debug: left bracket, depth 0.")
              (end-of-line) ; select current line
              (push-mark (line-beginning-position) t t))
          (progn
            ;; (message "debug: left bracket, depth not 0")
            (up-list -1 t t)
            (mark-sexp))))
       ((eq xp1 (line-beginning-position))
        (progn
          (goto-char xp1)
          (let ((xfirstLineEndPos (line-end-position)))
            (cond
             ((eq xp2 xfirstLineEndPos)
              (progn
                ;; (message "debug: exactly 1 line. extend to next whole line." )
                (forward-line 1)
                (end-of-line)))
             ((< xp2 xfirstLineEndPos)
              (progn
                ;; (message "debug: less than 1 line. complete the line." )
                (end-of-line)))
             ((> xp2 xfirstLineEndPos)
              (progn
                ;; (message "debug: beginning of line, but end is greater than 1st end of line" )
                (goto-char xp2)
                (if (eq (point) (line-end-position))
                    (progn
                      ;; (message "debug: exactly multiple lines" )
                      (forward-line 1)
                      (end-of-line))
                  (progn
                    ;; (message "debug: multiple lines but end is not eol. make it so" )
                    (goto-char xp2)
                    (end-of-line)))))
             (t (error "%s: logic error 42946" real-this-command))))))
       ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
        (progn
          ;; (message "debug: less than 1 line" )
          (end-of-line) ; select current line
          (push-mark (line-beginning-position) t t)))
       (t
        ;; (message "debug: last resort" )
        nil))))

   ((looking-at "\\s(")
    ;; (message "debug: left bracket")
    (mark-sexp))

   ((looking-at "\\s)")
    ;; (message "debug: right bracket")
    (backward-up-list) (mark-sexp))

   ((looking-at "\\s\"")
    ;; (message "debug: string quote")
    (mark-sexp))

   ((looking-at "[ \t\n]")
    ;; (message "debug: is white space")
    (skip-chars-backward " \t\n")
    (push-mark)
    (skip-chars-forward " \t\n")
    (setq mark-active t))

   ((looking-at "[-_a-zA-Z0-9]")
    ;; (message "debug: left is word or symbol")
    (skip-chars-backward "-_a-zA-Z0-9")
    (push-mark)
    (skip-chars-forward "-_a-zA-Z0-9")
    (setq mark-active t))

   ((and (looking-at "[:blank:]")
         (prog2 (backward-char) (looking-at "[:blank:]") (forward-char)))
    ;; (message "debug: left and right both space" )
    (skip-chars-backward "[:blank:]") (push-mark (point) t t)
    (skip-chars-forward "[:blank:]"))

   ((and (looking-at "\n")
         (eq (char-before) 10))
    ;; (message "debug: left and right both newline")
    (skip-chars-forward "\n")
    (push-mark (point)  t t)
    (re-search-forward "\n[ \t]*\n"))

   (t
    ;; (message "debug: just mark sexp" )
    (mark-sexp)
    (exchange-point-and-mark))))

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes QUOTATION MARK, GRAVE ACCENT, and anything in `xah-brackets'.
This command ignores nesting. For example, if text is
「(a(b)c▮)」
the selected char is 「c」, not 「a(b)c」.

URL `http://xahlee.info/emacs/emacs/emacs_select_quote_text.html'
Version: 2020-11-24 2023-07-23 2023-11-14"
  (interactive)
  (let ((xskipChars (concat "^\"`" (mapconcat #'identity xah-brackets ""))))
    (skip-chars-backward xskipChars)
    (push-mark (point) t t)
    (skip-chars-forward xskipChars)))

(defun xah-cut-text-in-quote ()
  "Cut text between the nearest left and right delimiters.
See `xah-select-text-in-quote'

Version: 2023-07-23 2023-11-14"
  (interactive)
  (let ((xskipChars (concat "^\"`" (mapconcat #'identity xah-brackets ""))))
    (skip-chars-backward xskipChars)
    (push-mark (point) t t)
    (skip-chars-forward xskipChars)
    (kill-region nil nil t)))


;; misc

(defun xah-user-buffer-p ()
  "Return t if current buffer is a user buffer, else nil.
A user buffer has buffer name NOT starts with * or space.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
Version: 2016-06-18 2022-05-19 2023-10-18"
  (interactive)
  (cond
   ((string-match "^\*" (buffer-name)) nil)
   ((eq major-mode 'dired-mode) nil)
   ((eq major-mode 'eww-mode) nil)
   ((eq major-mode 'help-mode) nil)
   (t t)))

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-p'.

URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version: 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 30)
      (if (not (xah-user-buffer-p))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-p'.

URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version: 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-p))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
“emacs buffer” here is buffer whose name starts with *.

URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version: 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
“emacs buffer” here is buffer whose name starts with *.

URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version: 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))

(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
Returns the buffer object.
New buffer is named untitled, untitled<2>, etc.

Warning: new buffer is not prompted for save when killed, see `kill-buffer'.
Or manually `save-buffer'

URL `http://xahlee.info/emacs/emacs/emacs_new_empty_buffer.html'
Version: 2017-11-01 2022-04-05"
  (interactive)
  (let ((xbuf (generate-new-buffer "untitled")))
    (switch-to-buffer xbuf)
    (funcall initial-major-mode)
    xbuf
    ))

(declare-function minibuffer-keyboard-quit "delsel" ())
(declare-function org-edit-src-save "org-src" ())

(defcustom xah-recently-closed-buffers-max 40 "The maximum length for `xah-recently-closed-buffers'."
  :type 'integer)

(defvar xah-recently-closed-buffers nil "A Alist of recently closed buffers.
Each element is (bufferName . filePath).
The max number to track is controlled by the variable `xah-recently-closed-buffers-max'.")

(defun xah-add-to-recently-closed (&optional BufferName BufferFileName)
  "Add to `xah-recently-closed-buffers'.
Version: 2023-03-02"
  (let ((xbn (if BufferName BufferName (buffer-name)))
        (xbfn (if BufferFileName BufferFileName buffer-file-name)))
    (setq xah-recently-closed-buffers (cons (cons xbn xbfn) xah-recently-closed-buffers)))
  (when (> (length xah-recently-closed-buffers) xah-recently-closed-buffers-max)
    (setq xah-recently-closed-buffers (butlast xah-recently-closed-buffers 1))))

(defvar xah-temp-dir-path nil "Path to temp dir used by xah commands.
by default, the value is dir named temp at `user-emacs-directory'.
Version: 2023-03-21")

(setq xah-temp-dir-path
      (if xah-temp-dir-path
          xah-temp-dir-path
        (concat user-emacs-directory "temp/")))

(defun xah-close-current-buffer ()
  "Close the current buffer with possible backup of modified file.

• If the buffer is a file and not modified, kill it. If is modified, do nothing. Print a message.
• If the buffer is not a file, first save it to `xah-temp-dir-path' named untitled_‹datetime›_‹randomhex›.txt.

If `universal-argument' is called first, call `kill-buffer'.
(this is useful when a file is modified, and then it is is changed
by some app outside emacs, and `auto-revert-mode' is on, then, emacs
goes into a loop asking to revert or save.)

If the buffer is a file, add the path to the list `xah-recently-closed-buffers'.

URL `http://xahlee.info/emacs/emacs/elisp_close_buffer_open_last_closed.html'
Version: 2016-06-19 2023-09-27 2023-10-25"
  (interactive)
  (widen)
  (cond
   (current-prefix-arg (kill-buffer))
   ;; ((eq major-mode 'minibuffer-inactive-mode) (minibuffer-keyboard-quit))
   ;; ((active-minibuffer-window) (minibuffer-keyboard-quit))
   ((minibufferp (current-buffer)) (minibuffer-keyboard-quit))
   ((and buffer-file-name (not (buffer-modified-p)))
    (xah-add-to-recently-closed (buffer-name) buffer-file-name)
    (kill-buffer))
   ((and buffer-file-name (buffer-modified-p))
    (message "buffer file modified. Save it first.\n%s" buffer-file-name)
    ;; (let ((xnewName
    ;;            (format "%s~%s~"
    ;;                    buffer-file-name
    ;;                    (format-time-string "%Y-%m-%d_%H%M%S"))))
    ;;       (write-region (point-min) (point-max) xnewName)
    ;;       (print (format "The modified version is saved at
    ;; %s
    ;; call xah-open-last-closed twice to open." xnewName))
    ;;       (xah-add-to-recently-closed (buffer-name) xnewName)
    ;;       (xah-add-to-recently-closed (buffer-name) buffer-file-name)
    ;;       (kill-buffer))
    )
   ((and (not buffer-file-name) (xah-user-buffer-p) (not (eq (point-max) 1)))
    (let ((xnewName (format "%suntitled_%s_%x.txt"
                            xah-temp-dir-path
                            (format-time-string "%Y%m%d_%H%M%S")
                            (random #xfffff))))
      (when (not (file-exists-p xah-temp-dir-path)) (make-directory xah-temp-dir-path))
      (write-region (point-min) (point-max) xnewName)
      (xah-add-to-recently-closed (buffer-name) xnewName)
      (kill-buffer)))
   (t (kill-buffer))))

(defun xah-open-last-closed ()
  "Open the last closed file.
URL `http://xahlee.info/emacs/emacs/elisp_close_buffer_open_last_closed.html'
Version: 2016-06-19 2022-03-22"
  (interactive)
  (if (> (length xah-recently-closed-buffers) 0)
      (find-file (cdr (pop xah-recently-closed-buffers)))
    (progn (message "No recently close buffer in this session."))))

(defun xah-open-recently-closed ()
  "Open recently closed file.
Prompt for a choice.

URL `http://xahlee.info/emacs/emacs/elisp_close_buffer_open_last_closed.html'
Version: 2016-06-19 2023-08-25 2023-09-19"
  (interactive)
  (find-file
   (let ((completion-ignore-case t))
     (completing-read
      "Open:"
      (mapcar (lambda (f) (cdr f)) xah-recently-closed-buffers)
      nil t
      ))))

(defun xah-list-recently-closed ()
  "List recently closed file.

URL `http://xahlee.info/emacs/emacs/elisp_close_buffer_open_last_closed.html'
Version: 2016-06-19"
  (interactive)
  (let ((xbuf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer xbuf)
    (mapc (lambda (xf) (insert (cdr xf) "\n"))
          xah-recently-closed-buffers)))

(defvar xah-open-file-at-cursor-pre-hook nil "Hook for `xah-open-file-at-cursor'.
Functions in the hook will be called in order, each given the path as arg.
The first return non-nil, its value is given to `xah-open-file-at-cursor' as input.
This is useful for transforming certain url into file path (your website url), so instead of opening in browser, it opens in emacs as file.")

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.

• If there is selection, use it for path.
• Path can be {relative, full path, URL}.
• If the path starts with 「https*://」, open the URL in browser.
• Path may have a trailing 「:‹n›」 that indicates line number, or 「:‹n›:‹m›」 with line and column number. If so, jump to that line number.

If path does not have a file extension, automatically try with .el for elisp files.

See also `xah-open-file-at-cursor-pre-hook'.

This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://xahlee.info/emacs/emacs/emacs_open_file_path_fast.html'
Version: 2020-10-17 2023-03-22 2023-09-29"
  (interactive)
  (let (xinput xinput2 xpath)
    (setq
     xinput
     (if (region-active-p)
         (buffer-substring-no-properties (region-beginning) (region-end))
       (let ((xp0 (point)) xp1 xp2
             (xpathStops "^  \t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
         (skip-chars-backward xpathStops)
         (setq xp1 (point))
         (goto-char xp0)
         (skip-chars-forward xpathStops)
         (setq xp2 (point))
         (goto-char xp0)
         (buffer-substring-no-properties xp1 xp2)))
     xinput2
     (if (> (length xah-open-file-at-cursor-pre-hook) 0)
         (let ((xprehook (run-hook-with-args-until-success 'xah-open-file-at-cursor-pre-hook xinput)))
           (if xprehook xprehook xinput))
       xinput)
     xpath
     (replace-regexp-in-string "^/C:/" "/" (replace-regexp-in-string "^file://" "" (replace-regexp-in-string ":\\'" "" xinput2))))

    (if (string-match-p "\\`https?://" xpath)
        (browse-url xpath)
      (let ((xpathNoQ
             (let ((xHasQuery (string-match "\?[a-z]+=" xpath)))
               (if xHasQuery
                   (substring xpath 0 xHasQuery)
                 xpath))))
        (cond
         ((string-match "#" xpathNoQ)
          (let ((xfpath (substring xpathNoQ 0 (match-beginning 0)))
                (xfractPart (substring xpathNoQ (1+ (match-beginning 0)))))
            (if (file-exists-p xfpath)
                (progn
                  (find-file xfpath)
                  (goto-char (point-min))
                  (search-forward xfractPart))
              (progn
                (message "File does not exist. Created at\n%s" xfpath)
                (find-file xfpath)))))
         ((string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" xpathNoQ)
          (let ((xfpath (match-string-no-properties 1 xpathNoQ))
                (xlineNum (string-to-number (match-string-no-properties 2 xpathNoQ))))
            (if (file-exists-p xfpath)
                (progn
                  (find-file xfpath)
                  (goto-char (point-min))
                  (forward-line (1- xlineNum)))
              (progn
                (message "File does not exist. Created at\n%s" xfpath)
                (find-file xfpath)))))
         ((file-exists-p xpathNoQ)
          (progn ; open f.ts instead of f.js
            (let ((xext (file-name-extension xpathNoQ))
                  (xfnamecore (file-name-sans-extension xpathNoQ)))
              (if (and (string-equal xext "js")
                       (file-exists-p (concat xfnamecore ".ts")))
                  (find-file (concat xfnamecore ".ts"))
                (find-file xpathNoQ)))))
         ((file-exists-p (concat xpathNoQ ".el"))
          (find-file (concat xpathNoQ ".el")))
         (t (progn
              (message "File does not exist. Created at\n%s" xpathNoQ)
              (find-file xpathNoQ))))))))



(defvar xah-run-current-file-before-hook nil "Hook for `xah-run-current-file'. Before the file is run.")

(defvar xah-run-current-file-after-hook nil "Hook for `xah-run-current-file'. After the file is run.")

(defun xah-run-current-go-file ()
  "Run or build current golang file.
To build, call `universal-argument' first.
Version: 2018-10-12 2023-09-29 2024-01-01"
  (interactive)
  (when (not buffer-file-name) (user-error "Buffer is not file. Save it first."))
  (when (buffer-modified-p) (save-buffer))
  (let (xoutputb xfname xprogName xcmdStr)
    (setq
     xoutputb (get-buffer-create "*xah-run*" t)
     xfname buffer-file-name
     xprogName "go"
     xcmdStr (format (if current-prefix-arg
                         "%s build \"%s\" "
                       "%s run \"%s\" &")
                     xprogName xfname))
    (message "running %s" xfname)
    (message "%s" xcmdStr)
    (shell-command xcmdStr xoutputb)))

(defvar xah-run-current-file-map
  '(("clj" . "clj")
    ("go" . "go run")
    ("hs" . "runhaskell")
    ("java" . "javac")
    ("js" . "deno run")
    ("latex" . "pdflatex")
    ("m" . "wolframscript -file")
    ("mjs" . "node --experimental-modules ")
    ("ml" . "ocaml")
    ("php" . "php")
    ("pl" . "perl")
    ("ps1" . "pwsh")
    ("py" . "python")
    ("py2" . "python2")
    ("py3" . "python3")
    ("rb" . "ruby")
    ("rkt" . "racket")
    ("sh" . "bash")
    ("tex" . "pdflatex")
    ("ts" . "deno run")
    ("tsx" . "tsc")
    ("vbs" . "cscript")
    ("wl" . "wolframscript -file")
    ("wls" . "wolframscript -file")
    ("pov" . "povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640"))
 "A association list that maps file extension to program name, used by `xah-run-current-file'.
Each item is (EXT . PROGRAM), both strings.
EXT is file suffix (without the dot prefix), PROGRAM is program name or path, with possibly command options.
You can customize this alist.")

(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call python x.py in a shell.
Output is printed to buffer “*xah-run output*”.
File suffix is used to determine which program to run, set in the variable `xah-run-current-file-map'.

When `universal-argument' is called first, prompt user to enter command line options.

If the file is modified or not saved, save it automatically before run.

URL `http://xahlee.info/emacs/emacs/elisp_run_current_file.html'
Version: 2020-09-24 2023-12-31 2024-01-06"
  (interactive)
  ;; (setenv "NO_COLOR" "1") ; 2022-09-10 for deno. default color has yellow parts, hard to see
  (when (not buffer-file-name) (user-error "Buffer is not file. Save it first."))
  (when (buffer-modified-p) (save-buffer))
  (let (xoutBuffer xextAppMap xfname xfExt xappCmdStr xcmdStr)
    (setq
     xoutBuffer (get-buffer-create "*xah-run output*" t)
     xextAppMap xah-run-current-file-map
     xfname buffer-file-name
     xfExt (file-name-extension buffer-file-name)
     xappCmdStr (cdr (assoc xfExt xextAppMap))
     xcmdStr
     (when xappCmdStr
       (format "%s %s &"
               xappCmdStr
               (shell-quote-argument xfname))))

    ;; FIXME: Rather than `shell-command' with an `&', better use
    ;; `make-process' or `start-process' since we're not using the shell at all
    ;; (worse, we need to use `shell-quote-argument' to circumvent the shell).

    (run-hooks 'xah-run-current-file-before-hook)
    (cond
     ((string-equal xfExt "el")
      (load xfname))
     ((string-equal xfExt "go")
      (xah-run-current-go-file))
     ((string-match "\\.\\(ws?l\\|m\\|nb\\)\\'" xfExt)
      (if (fboundp 'xah-wolfram-run-script)
          (progn
            (xah-wolfram-run-script nil current-prefix-arg))
        (if xappCmdStr
            (progn
              (message "Running")
              (shell-command xcmdStr xoutBuffer))
          (error "%s: Unknown file extension: %s" real-this-command xfExt))))
     ((string-equal xfExt "java")
      (progn
        ;; FIXME: Better use `call-process', or else at least use
        ;; `shell-quote-argument'.
        (shell-command (format "javac %s" xfname) xoutBuffer)
        (shell-command (format "java %s" (file-name-sans-extension
                                          (file-name-nondirectory xfname)))
                       xoutBuffer)))
     (t
      (if xappCmdStr
          (progn
            (if current-prefix-arg
                (let ((xuserCmd (read-string "run with command:" xcmdStr)))
                  (message "Running 「%s」" xuserCmd)
                  (shell-command xuserCmd xoutBuffer))
              (progn
                (message "Running 「%s」" xcmdStr)
                (shell-command xcmdStr xoutBuffer))))
        (error "%s: Unknown file extension: %s" real-this-command xfExt))))
    (run-hooks 'xah-run-current-file-after-hook))
  ;; (setenv "NO_COLOR")
  )

(defun xah-clean-empty-lines ()
  "Replace repeated blank lines to just 1, in whole buffer or selection.
Respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/elisp_compact_empty_lines.html'
Version: 2017-09-22 2020-09-08"
  (interactive)
  (let (xbegin xend)
    (if (region-active-p)
        (setq xbegin (region-beginning) xend (region-end))
      (setq xbegin (point-min) xend (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region xbegin xend)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil :move)
            (replace-match "\n\n")))))))

(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or selection, respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/elisp_compact_empty_lines.html'
Version: 2017-09-22 2021-08-27 2022-08-06"
  (interactive)
  (let (xbegin xend)
    (if (region-active-p)
        (setq xbegin (region-beginning) xend (region-end))
      (setq xbegin (point-min) xend (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region xbegin xend)
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+\n" nil :move) (replace-match "\n"))
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil :move) (replace-match "\n\n"))
        (goto-char (point-max))
        (while (eq (char-before) 32) (delete-char -1)))))
  (message "%s done" real-this-command))

(defun xah-make-backup ()
  "Make a backup copy of current file or dired marked files.
If in dired, backup current file or marked files.
The backup file name is in this format
 x.html~2018-05-15_133429~
 The last part is hour, minutes, seconds.
in the same dir. If such a file already exist, it is overwritten.
If the current buffer is not associated with a file, nothing's done.

URL `http://xahlee.info/emacs/emacs/elisp_make-backup.html'
Version: 2018-06-06 2020-12-18 2022-06-13"
  (interactive)
  (let ((xfname buffer-file-name)
        (xdateTimeFormat "%Y-%m-%d_%H%M%S"))
    (if xfname
        (let ((xbackupName
               (concat xfname "~" (format-time-string xdateTimeFormat) "~")))
          (copy-file xfname xbackupName t)
          (message (concat "\nBackup saved at: " xbackupName)))
      (if (eq major-mode 'dired-mode)
          (progn
            (mapc (lambda (xx)
                    (let ((xbackupName
                           (concat xx "~" (format-time-string xdateTimeFormat) "~")))
                      (copy-file xx xbackupName t)))
                  (dired-get-marked-files))
            (revert-buffer))
        (user-error "%s: buffer not file nor dired" real-this-command)))))

(defun xah-make-backup-and-save ()
  "Backup of current file and save, or backup dired marked files.
For detail, see `xah-make-backup'.
If the current buffer is not associated with a file nor dired, nothing's done.

URL `http://xahlee.info/emacs/emacs/elisp_make-backup.html'
Version: 2015-10-14"
  (interactive)
  (if buffer-file-name
      (progn
        (xah-make-backup)
        (when (buffer-modified-p)
          (save-buffer)))
    (progn
      (xah-make-backup))))

(defun xah-delete-current-file-make-backup ()
  "Delete current file, makes a backup~, close the buffer.
If buffer is not a file, copy content to `kill-ring', delete buffer.
If buffer is a file, the file's directory is shown with cursor at the next file.

Backup filename is “‹name›~‹dateTimeStamp›~”. Existing file of the same name is overwritten. If buffer is not a file, the backup file name starts with “xx_”.

Call `xah-open-last-closed' to open the backup file.

URL `http://xahlee.info/emacs/emacs/elisp_delete-current-file.html'
Version: 2018-05-15 2023-08-11 2023-10-28"
  (interactive)
  (when (eq major-mode 'dired-mode)
    (user-error "%s: In dired. Nothing is done." real-this-command))
  (let ((xfname buffer-file-name)
        (xbuffname (buffer-name))
        xbackupPath)
    (setq xbackupPath
          (concat (if xfname xfname (format "%sxx" default-directory))
                  (format "~%s~" (format-time-string "%Y-%m-%d_%H%M%S"))))
    (if xfname
        (progn
          (save-buffer xfname)
          (rename-file xfname xbackupPath t)
          (kill-buffer xbuffname)
          ;; (dired-jump nil xbackupPath)
          ;; (revert-buffer t t t)
          ;; (dired-goto-file xbackupPath)
          ;; (dired-next-line 1)
          (message "File deleted.
Backup at
%s
Call `xah-open-last-closed' to open." xbackupPath)
          (when (boundp 'xah-recently-closed-buffers)
            (push (cons nil xbackupPath) xah-recently-closed-buffers)))
      (progn
        (widen)
        (kill-new (buffer-string))
        (kill-buffer xbuffname)
        (message "non-file buffer killed. buffer text copied to `kill-ring'."))))
  (when (eq major-mode 'dired-mode) (revert-buffer)))



(defun xah-search-current-word ()
  "Call `isearch' on current word or selection.
“word” here is A to Z, a to z, and hyphen [-] and lowline [_], independent of syntax table.

URL `http://xahlee.info/emacs/emacs/modernization_isearch.html'
Version: 2015-04-09"
  (interactive)
  (let (xp1 xp2)
    (if (region-active-p)
        (setq xp1 (region-beginning) xp2 (region-end))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq xp1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq xp2 (point))))
    (setq mark-active nil)
    (when (< xp1 (point))
      (goto-char xp1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties xp1 xp2))))

(declare-function w32-shell-execute "w32fns.c" (operation document &optional parameters show-flag)) ; (w32-shell-execute "open" default-directory)

(defun xah-show-in-desktop ()
  "Show current file in desktop.
 (Mac Finder, Microsoft Windows File Explorer, Linux file manager)
This command can be called when in a file buffer or in `dired'.

URL `http://xahlee.info/emacs/emacs/emacs_show_in_desktop.html'
Version: 2020-11-20 2022-08-19 2023-06-26 2023-09-09"
  (interactive)
  (let ((xpath (if (eq major-mode 'dired-mode)
                   (if (eq nil (dired-get-marked-files))
                       default-directory
                     (car (dired-get-marked-files)))
                 (if buffer-file-name buffer-file-name default-directory))))
    (cond
     ((eq system-type 'windows-nt)
      (shell-command (format "PowerShell -Command invoke-item '%s'" (expand-file-name default-directory )))
      ;; (let ((xcmd (format "Explorer /select,%s"
      ;;                     (replace-regexp-in-string "/" "\\" xpath t t)
      ;;                     ;; (shell-quote-argument (replace-regexp-in-string "/" "\\" xpath t t ))
      ;;                     )))
      ;;   (shell-command xcmd))
      )
     ((eq system-type 'darwin)
      (shell-command
       (concat "open -R " (shell-quote-argument xpath))))
     ((eq system-type 'gnu/linux)
      (call-process shell-file-name nil 0 nil
                    shell-command-switch
                    (format "%s %s"
                            "xdg-open"
                            (file-name-directory xpath)))
      ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
      ))))

(defun xah-open-in-vscode ()
  "Open current file or dir in vscode.
URL `http://xahlee.info/emacs/emacs/emacs_open_in_vscode.html'

Version: 2020-02-13 2021-01-18 2022-08-04 2023-06-26"
  (interactive)
  (let ((xpath (if buffer-file-name buffer-file-name (expand-file-name default-directory))))
    (message "path is %s" xpath)
    (cond
     ((eq system-type 'darwin)
      (shell-command (format "open -a Visual\\ Studio\\ Code.app %s" (shell-quote-argument xpath))))
     ((eq system-type 'windows-nt)
      (shell-command (format "code.cmd %s" (shell-quote-argument xpath))))
     ((eq system-type 'gnu/linux)
      (shell-command (format "code %s" (shell-quote-argument xpath)))))))

(defun xah-open-in-external-app (&optional Fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if Fname is given, open that.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2019-11-04 2023-04-05 2023-06-26"
  (interactive)
  (let (xfileList xdoIt)
    (setq xfileList
          (if Fname
              (list Fname)
            (if (eq major-mode 'dired-mode)
                (dired-get-marked-files)
              (list buffer-file-name))))
    (setq xdoIt (if (<= (length xfileList) 10) t (y-or-n-p "Open more than 10 files? ")))
    (when xdoIt
      (cond
       ((eq system-type 'windows-nt)
        (let ((xoutBuf (get-buffer-create "*xah open in external app*"))
              (xcmdlist (list "PowerShell" "-Command" "Invoke-Item" "-LiteralPath")))
          (mapc
           (lambda (x)
             (message "%s" x)
             (apply 'start-process (append (list "xah open in external app" xoutBuf) xcmdlist (list (format "'%s'" (if (string-match "'" x) (replace-match "`'" t t x) x))) nil)))
           xfileList)
          ;; (switch-to-buffer-other-window xoutBuf)
          )
        ;; old code. calling shell. also have a bug if filename contain apostrophe
        ;; (mapc (lambda (xfpath) (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name xfpath)) "'"))) xfileList)
        )
       ((eq system-type 'darwin)
        (mapc (lambda (xfpath) (shell-command (concat "open " (shell-quote-argument xfpath)))) xfileList))
       ((eq system-type 'gnu/linux)
        (mapc (lambda (xfpath)
                (call-process shell-file-name nil 0 nil
                              shell-command-switch
                              (format "%s %s"
                                      "xdg-open"
                                      (shell-quote-argument xfpath))))
              xfileList))
       ((eq system-type 'berkeley-unix)
        (mapc (lambda (xfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" xfpath))) xfileList))))))

(defvar xah-fly-mswin-terminal
  "wt"
  "A string. Value should be one of: wt (for Windows Terminal) or pwsh (for PowerShell Core (cross-platform)) or powershell (for Microsoft PowerShell).")

(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
On Microsoft Windows, which terminal it starts depends on `xah-fly-mswin-terminal'.

URL `http://xahlee.info/emacs/emacs/emacs_open_in_terminal.html'
Version: 2020-11-21 2022-08-04 2023-03-01 2023-06-26"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (cond
     ((string-equal xah-fly-mswin-terminal "wt")
      (shell-command (format "wt -d \"%s\"" default-directory)))
     ((string-equal xah-fly-mswin-terminal "pwsh")
      (shell-command
       (format "pwsh -Command Start-Process pwsh -WorkingDirectory '%s'" (shell-quote-argument default-directory))))
     ((string-equal xah-fly-mswin-terminal "powershell")
      (shell-command
       (format "powershell -Command Start-Process powershell -WorkingDirectory '%s'" (shell-quote-argument default-directory))))
     (t (error "Error 702919: value of `xah-fly-mswin-terminal' is not expected. Its value is %s" xah-fly-mswin-terminal))))
   ((eq system-type 'darwin)
    (shell-command (concat "open -a terminal " (shell-quote-argument (expand-file-name default-directory)))))
   ((eq system-type 'gnu/linux)
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))
   ((eq system-type 'berkeley-unix)
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))))

(defun xah-next-window-or-frame ()
  "Switch to next window or frame.
If current frame has only one window, switch to next frame.
If `universal-argument' is called first, do switch frame.
Version: 2017-01-27"
  (interactive)
  (if current-prefix-arg
      (other-frame 1)
    (if (one-window-p)
        (other-frame 1)
      (other-window 1))))

(defun xah-unsplit-window-or-next-frame ()
  "Unsplit window. If current frame has only one window, switch to next frame.
Version: 2017-01-29"
  (interactive)
  (if (one-window-p)
      (other-frame 1)
    (delete-other-windows)))


;; layout lookup tables for key conversion

(defvar xah-fly-layouts nil "A alist.
Key is layout name, string type.
Value is a alist, each element is of the form (\"e\" . \"d\").
First char is Dvorak, second is corresponding char of the destination layout.
When a char is not in this alist, they are assumed to be the same. ")

(push '("azerty" . (("." . "e") ("," . "z") ("'" . "a") (";" . "w") ("/" . "^") ("[" . ")") ("]" . "=") ("=" . "$") ("-" . "ù") ("a" . "q") ("b" . "n") ("c" . "i") ("d" . "h") ("e" . "d") ("f" . "y") ("g" . "u") ("h" . "j") ("i" . "g") ("j" . "c") ("k" . "v") ("l" . "p") ("m" . ",") ("n" . "l") ("o" . "s") ("p" . "r") ("q" . "x") ("r" . "o") ("s" . "m") ("t" . "k") ("u" . "f") ("v" . ":") ("w" . ";") ("x" . "b") ("y" . "t") ("z" . "!") ("1" . "&") ("2" . "é") ("3" . "\"") ("4" . "'") ("5" . "(") ("6" . "-") ("7" . "è") ("8" . "_") ("9" . "ç") ("0" . "à") ("\\" . "*") ("`" . "²"))) xah-fly-layouts)

(push '("azerty-be" . (("." . "e") ("," . "z") ("'" . "a") (";" . "w") ("/" . "^") ("[" . ")") ("]" . "-") ("=" . "$") ("-" . "ù") ("a" . "q") ("b" . "n") ("c" . "i") ("d" . "h") ("e" . "d") ("f" . "y") ("g" . "u") ("h" . "j") ("i" . "g") ("j" . "c") ("k" . "v") ("l" . "p") ("m" . ",") ("n" . "l") ("o" . "s") ("p" . "r") ("q" . "x") ("r" . "o") ("s" . "m") ("t" . "k") ("u" . "f") ("v" . ":") ("w" . ";") ("x" . "b") ("y" . "t") ("z" . "=") ("1" . "&") ("2" . "é") ("3" . "\"") ("4" . "'") ("5" . "(") ("6" . "§") ("7" . "è") ("8" . "!") ("9" . "ç") ("0" . "à") ("\\" . "µ") ("`" . "²"))) xah-fly-layouts)

(push '("colemak" . (("'" . "q") ("," . "w") ("." . "f") ("y" . "g") ("f" . "j") ("g" . "l") ("c" . "u") ("r" . "y") ("l" . ";") ("o" . "r") ("e" . "s") ("u" . "t") ("i" . "d") ("d" . "h") ("h" . "n") ("t" . "e") ("n" . "i") ("s" . "o") (";" . "z") ("q" . "x") ("j" . "c") ("k" . "v") ("x" . "b") ("b" . "k") ("w" . ",") ("v" . ".") ("z" . "/"))) xah-fly-layouts)

(push '("colemak-dhm" . (("'" . "q") ("," . "w") ("." . "f") (";" . "z") ("b" . "k") ("c" . "u") ("d" . "m") ("e" . "s") ("f" . "j") ("g" . "l") ("h" . "n") ("i" . "g") ("j" . "c") ("k" . "d") ("l" . ";") ("m" . "h") ("n" . "i") ("o" . "r") ("q" . "x") ("r" . "y") ("s" . "o") ("t" . "e") ("u" . "t") ("v" . ".") ("w" . ",") ("x" . "v") ("y" . "b") ("z" . "/"))) xah-fly-layouts)

(push '("colemak-dhm-angle" . (("'" . "q") ("," . "w") ("." . "f") (";" . "x") ("b" . "k") ("c" . "u") ("d" . "m") ("e" . "s") ("f" . "j") ("g" . "l") ("h" . "n") ("i" . "g") ("j" . "d") ("k" . "v") ("l" . ";") ("m" . "h") ("n" . "i") ("o" . "r") ("q" . "c") ("r" . "y") ("s" . "o") ("t" . "e") ("u" . "t") ("v" . ".") ("w" . ",") ("x" . "\\") ("y" . "b") ("z" . "/"))) xah-fly-layouts)

(push '("colemak-dhk" . (("'" . "q") ("," . "w") ("." . "f") (";" . "z") ("b" . "m") ("c" . "u") ("d" . "k") ("e" . "s") ("f" . "j") ("g" . "l") ("h" . "n") ("i" . "g") ("j" . "c") ("k" . "d") ("l" . ";") ("m" . "h") ("n" . "i") ("o" . "r") ("q" . "x") ("r" . "y") ("s" . "o") ("t" . "e") ("u" . "t") ("v" . ".") ("w" . ",") ("x" . "v") ("y" . "b") ("z" . "/"))) xah-fly-layouts)

(push '("dvorak" . nil) xah-fly-layouts)

(push '("optimot" . (("-" . "^") ("'" . "à") ("," . "j") ("." . "o") (";" . "k") ("/" . "x") ("[" . "#") ("]" . "@") ("=" . "ç") ("a" . "a") ("b" . "g") ("c" . "l") ("d" . "p") ("e" . "e") ("f" . "f") ("g" . "d") ("h" . "t") ("i" . ",") ("j" . "è") ("k" . ".") ("l" . "q") ("m" . "c") ("n" . "r") ("o" . "i") ("p" . "é") ("q" . "y") ("r" . "'") ("s" . "n") ("t" . "s") ("u" . "u") ("v" . "h") ("w" . "m") ("x" . "w") ("y" . "b") ("z" . "v") ("1" . "«") ("2" . "»") ("3" . "\"") ("4" . "-") ("5" . "+") ("6" . "*") ("7" . "/") ("8" . "=") ("9" . "(") ("0" . ")") ("\\" . "ç") ("`" . "$"))) xah-fly-layouts)

(push '("programer-dvorak" . ( ("`" . "$") ("1" . "&") ("2" . "[") ("3" . "{") ("4" . "}") ("5" . "(") ("6" . "=") ("7" . "*") ("8" . ")") ("9" . "+") ("0" . "]") ("[" . "!") ("]" . "#") ("!" . "%") ("@" . "7") ("#" . "5") ("$" . "3") ("%" . "1") ("^" . "9") ("&" . "0") ("*" . "2") ("(" . "4") (")" . "6") ("{" . "8") ("}" . "`") ("'" . ";") ("\"" . ":") (";" . "'") (":" . "\"") ("=" . "@") ("+" . "^") )) xah-fly-layouts)

(push '("qwerty" . (("." . "e") ("," . "w") ("'" . "q") (";" . "z") ("/" . "[") ("[" . "-") ("]" . "=") ("=" . "]") ("-" . "'") ("a" . "a") ("b" . "n") ("c" . "i") ("d" . "h") ("e" . "d") ("f" . "y") ("g" . "u") ("h" . "j") ("i" . "g") ("j" . "c") ("k" . "v") ("l" . "p") ("n" . "l") ("o" . "s") ("p" . "r") ("q" . "x") ("r" . "o") ("s" . ";") ("t" . "k") ("u" . "f") ("v" . ".") ("w" . ",") ("x" . "b") ("y" . "t") ("z" . "/"))) xah-fly-layouts)

;; QWERTY Norwegian
(push '("qwerty-no" . (("." . "e") ("," . "w") ("'" . "q") (";" . "z") ("/" . "å") ("[" . "+") ("]" . "´") ("=" . "¨") ("-" . "æ") ("b" . "n") ("c" . "i") ("d" . "h") ("e" . "d") ("f" . "y") ("g" . "u") ("h" . "j") ("i" . "g") ("j" . "c") ("k" . "v") ("l" . "p") ("n" . "l") ("o" . "s") ("p" . "r") ("q" . "x") ("r" . "o") ("s" . "ø") ("t" . "k") ("u" . "f") ("v" . ".") ("w" . ",") ("x" . "b") ("y" . "t") ("z" . "-"))) xah-fly-layouts)

(push '("qwerty-abnt" . (("." . "e") ("," . "w") ("'" . "q") (";" . "z") ("/" . "'") ("[" . "-") ("]" . "=") ("=" . "[") ("-" . "~") ("b" . "n") ("c" . "i") ("d" . "h") ("e" . "d") ("f" . "y") ("g" . "u") ("h" . "j") ("i" . "g") ("j" . "c") ("k" . "v") ("l" . "p") ("n" . "l") ("o" . "s") ("p" . "r") ("q" . "x") ("r" . "o") ("s" . "ç") ("t" . "k") ("u" . "f") ("v" . ".") ("w" . ",") ("x" . "b") ("y" . "t") ("z" . ";"))) xah-fly-layouts)

(push '("qwertz" . (("." . "e") ("," . "w") ("'" . "q") (";" . "y") ("/" . "ü") ("[" . "ß") ("]" . "´") ("=" . "+") ("-" . "ä") ("b" . "n") ("c" . "i") ("d" . "h") ("e" . "d") ("f" . "z") ("g" . "u") ("h" . "j") ("i" . "g") ("j" . "c") ("k" . "v") ("l" . "p") ("n" . "l") ("o" . "s") ("p" . "r") ("q" . "x") ("r" . "o") ("s" . "ö") ("t" . "k") ("u" . "f") ("v" . ".") ("w" . ",") ("x" . "b") ("y" . "t") ("z" . "-"))) xah-fly-layouts)

(push '("workman" . (("[" . "-") ("]" . "=") ("'" . "q") ("," . "d") ("." . "r") ("p" . "w") ("y" . "b") ("f" . "j") ("g" . "f") ("c" . "u") ("r" . "p") ("l" . ";") ("/" . "[") ("=" . "]") ("o" . "s") ("e" . "h") ("u" . "t") ("i" . "g") ("d" . "y") ("h" . "n") ("t" . "e") ("n" . "o") ("s" . "i") ("-" . "'") (";" . "z") ("q" . "x") ("j" . "m") ("k" . "c") ("x" . "v") ("b" . "k") ("m" . "l") ("w" . ",") ("v" . ".") ("z" . "/"))) xah-fly-layouts)

(push '("norman" . (("'" . "q") ("," . "w") ("." . "d") ("p" . "f") ("y" . "k") ("f" . "j") ("g" . "u") ("c" . "r") ("r" . "l") ("l" . ";") ("o" . "s") ("u" . "t") ("i" . "g") ("d" . "y") ("h" . "n") ("t" . "i") ("n" . "o") ("s" . "h") (";" . "z") ("q" . "x") ("j" . "c") ("k" . "v") ("x" . "b") ("b" . "p") ("w" . ",") ("v" . ".") ("z" . "/"))) xah-fly-layouts)

(push '("neo2" . (("'" . "x") ("," . "v") ("." . "l") ("p" . "c") ("y" . "w") ("f" . "k") ("g" . "h") ("c" . "g") ("r" . "f") ("l" . "q") ("a" . "u") ("o" . "i") ("e" . "a") ("u" . "e") ("i" . "o") ("d" . "s") ("h" . "n") ("t" . "r") ("n" . "t") ("s" . "d") (";" . "ü") ("q" . "ö") ("j" . "ä") ("k" . "p") ("x" . "z") ("w" . ",") ("v" . ".") ("z" . "j") ("/" . "ß") ("[" . "-") ("-" . "y"))) xah-fly-layouts)

(push '("koy" . (("'" . "k") ("," . ".") ("." . "o") ("p" . ",") ("f" . "v") ("r" . "l") ("l" . "ß") ("a" . "h") ("o" . "a") ("u" . "i") ("i" . "u") ("h" . "t") ("t" . "r") (";" . "x") ("j" . "ä") ("k" . "ü") ("x" . "ö") ("m" . "p") ("v" . "m") ("z" . "j"))) xah-fly-layouts)

(push '("adnw" . (("'" . "k") ("," . "u") ("." . "ü") ("p" . ".") ("y" . "ä") ("f" . "v") ("r" . "l") ("l" . "j") ("/" . "f") ("a" . "h") ("o" . "i") ("u" . "a") ("i" . "o") ("h" . "t") ("t" . "r") ("-" . "ß") (";" . "x") ("q" . "y") ("j" . "ö") ("k" . ",") ("x" . "q") ("m" . "p") ("v" . "m"))) xah-fly-layouts)

(push '("pt-nativo" . ((";" . "«") ("/" . "~") ("[" . "º") ("]" . "<") ("=" . "-") ("-" . "´") ("a" . "i") ("b" . "q") ("c" . "t") ("d" . "m") ("e" . "a") ("f" . "w") ("g" . "l") ("h" . "d") ("i" . "u") ("k" . "b") ("l" . "p") ("m" . "v") ("n" . "r") ("o" . "e") ("p" . "h") ("q" . "ç") ("r" . "c") ("s" . "n") ("t" . "s") ("u" . "o") ("v" . "f") ("w" . "g") ("x" . "k") ("y" . "x"))) xah-fly-layouts)

(push '("carpalx-qgmlwy" . (("." . "m") ("," . "g") ("'" . "q") (";" . "z") ("/" . "[") ("[" . "-") ("]" . "=") ("=" . "]") ("-" . "'") ("a" . "d") ("b" . "k") ("c" . "u") ("d" . "i") ("e" . "t") ("f" . "y") ("g" . "f") ("h" . "a") ("i" . "r") ("j" . "c") ("k" . "v") ("l" . ";") ("m" . "p") ("n" . "o") ("o" . "s") ("p" . "l") ("q" . "x") ("r" . "b") ("s" . "h") ("t" . "e") ("u" . "n") ("v" . ".") ("w" . ",") ("x" . "j") ("y" . "w") ("z" . "/"))) xah-fly-layouts)

(push '("carpalx-qgmlwb" . (("." . "m") ("," . "g") ("'" . "q") (";" . "z") ("/" . "[") ("[" . "-") ("]" . "=") ("=" . "]") ("-" . "'") ("a" . "d") ("b" . "k") ("c" . "u") ("d" . "i") ("e" . "t") ("f" . "b") ("g" . "y") ("h" . "a") ("i" . "r") ("j" . "c") ("k" . "f") ("l" . ";") ("m" . "p") ("n" . "o") ("o" . "s") ("p" . "l") ("q" . "x") ("r" . "v") ("s" . "h") ("t" . "e") ("u" . "n") ("v" . ".") ("w" . ",") ("x" . "j") ("y" . "w") ("z" . "/"))) xah-fly-layouts)

(push '("carpalx-qfmlwy" . (("." . "m") ("," . "f") ("'" . "q") (";" . "z") ("/" . "[") ("[" . "-") ("]" . "=") ("=" . "]") ("-" . "'") ("a" . "d") ("b" . "p") ("c" . "o") ("d" . "i") ("e" . "t") ("f" . "y") ("g" . "u") ("h" . "a") ("i" . "r") ("j" . "g") ("k" . "c") ("l" . "j") ("m" . "k") ("n" . "h") ("o" . "s") ("p" . "l") ("q" . "v") ("r" . "b") ("s" . ";") ("t" . "e") ("u" . "n") ("v" . ".") ("w" . ",") ("y" . "w") ("z" . "/"))) xah-fly-layouts)

(push '("bepo" . (("'" . "b") ("," . "é") ("." . "p") ("p" . "o") ("y" . "è") ("f" . "^") ("g" . "v") ("c" . "d") ("r" . "l") ("l" . "j") ("o" . "u") ("e" . "i") ("u" . "e") ("i" . ",") ("d" . "c") ("h" . "t") ("t" . "s") ("n" . "r") ("s" . "n") (":" . "à") ("q" . "y") ("j" . "x") ("k" . ".") ("x" . "k") ("b" . "’") ("m" . "q") ("w" . "g") ("v" . "h") ("z" . "f") ("3" . "»") ("4" . "(") ("5" . ")") ("6" . "@") ("7" . "+") ("8" . "-") ("9" . "/"))) xah-fly-layouts)

(defvar xah-fly-key-current-layout nil
  "The current keyboard layout. Value is a key in `xah-fly-layouts'.
Do not set this variable manually. Use `xah-fly-keys-set-layout' to set it.
If the value is nil, it is automatically set to \"qwerty\".
When this variable changes, suitable change must also be done to `xah-fly--key-convert-table'.
Version: 2022-10-22")

(if xah-fly-key-current-layout nil (setq xah-fly-key-current-layout "qwerty"))

(defvar xah-fly--key-convert-table nil
  "A alist that's the conversion table from dvorak to current layout.
Value structure is one of the key's value of `xah-fly-layouts'.
Value is programtically set from value of `xah-fly-key-current-layout'.
Do not manually set this variable.
Version: 2019-02-12 2022-10-22" )

(setq xah-fly--key-convert-table
      (cdr (assoc xah-fly-key-current-layout xah-fly-layouts)))

(defun xah-fly--convert-kbd-str (Charstr)
  "Return the corresponding char Charstr according to
`xah-fly--key-convert-table'. Charstr must be a string that is the argument to `kbd'. e.g. \"a\" and \"a b c\"
Each space separated token is converted according to `xah-fly--key-convert-table'.
Version: 2022-10-25"
  (interactive)
  (mapconcat
   'identity
   (mapcar
    (lambda (x)
      (let ((xresult (assoc x xah-fly--key-convert-table)))
        (if xresult (cdr xresult) x)))
    (split-string Charstr " +"))
   " "))

(defun xah-fly--define-keys (KeymapName KeyCmdAlist &optional Direct-p)
  "Map `define-key' over a alist KeyCmdAlist, with key layout remap.
The key is remapped from Dvorak to the current keyboard layout by `xah-fly--convert-kbd-str'.
If Direct-p is t, do not remap key to current keyboard layout.
Example usage:
 (xah-fly--define-keys
  (define-prefix-command \\='xyz-map)
  \\='(
    (\"h\" . highlight-symbol-at-point)
    (\".\" . isearch-forward-symbol-at-point)
    (\"w\" . isearch-forward-word)))
Version: 2020-04-18 2022-10-25 2023-08-21"
  (mapcar
   (lambda (x)
     (define-key
      KeymapName
      (kbd (if Direct-p (car x) (xah-fly--convert-kbd-str (car x))))
      (cdr x)))
   KeyCmdAlist))


;; keymaps

(defvar xah-fly-key-map (make-sparse-keymap)
  "Backward-compatibility map for `xah-fly-keys' minor mode. If
`xah-fly-insert-state-p' is true, point to `xah-fly-insert-map', else,
point to points to `xah-fly-command-map'.")

(make-obsolete-variable
 'xah-fly-key-map
 "Put bindings for command mode in `xah-fly-command-map', bindings for
insert mode in `xah-fly-insert-map' and bindings that are common to both
command and insert modes in `xah-fly-shared-map'."
 "2020-04-16")

(defvar xah-fly-shared-map (make-sparse-keymap)
  "Parent keymap of `xah-fly-command-map' and `xah-fly-insert-map'.

Define keys that are available in both command and insert modes here, like
`xah-fly-mode-toggle'")

;; (cons 'keymap xah-fly-shared-map) makes a new keymap with `xah-fly-shared-map' as its parent. See info node (elisp)Inheritance and Keymaps.
(defvar xah-fly-command-map (cons 'keymap xah-fly-shared-map)
  "Keymap that takes precedence over all other keymaps in command mode.

Inherits bindings from `xah-fly-shared-map'.
In command mode, if no binding is found in this map `xah-fly-shared-map' is checked, then if there is still no binding, the other active keymaps are checked like normal.
However, if a key is explicitly bound to nil in this map, it will not be looked up in `xah-fly-shared-map' and lookup will skip directly to the normally active maps.
In this way, bindings in `xah-fly-shared-map' can be disabled by this map.

Effectively, this map takes precedence over all others when command mode
is enabled.")

(defvar xah-fly-insert-map (cons 'keymap xah-fly-shared-map)
  "Keymap for bindings that will be checked in insert mode. Active whenever
`xah-fly-keys' is non-nil.

Inherits bindings from `xah-fly-shared-map'. In insert mode, if no binding
is found in this map `xah-fly-shared-map' is checked, then if there is
still no binding, the other active keymaps are checked like normal. However,
if a key is explicitly bound to nil in this map, it will not be looked
up in `xah-fly-shared-map' and lookup will skip directly to the normally
active maps. In this way, bindings in `xah-fly-shared-map' can be disabled
by this map.

Keep in mind that this acts like a normal global minor mode map, so other
minor modes loaded later may override bindings in this map.")

(defvar xah-fly--deactivate-command-mode-func nil)


;; setting keys

(defun xah-fly-define-keys ()
  "Define the keys for xah-fly-keys.
Used by `xah-fly-keys-set-layout' for changing layout.
Version: 2022-10-31"
  (interactive)
  (let ()

    ;; Movement key integrations with built-in Emacs packages
    (xah-fly--define-keys
     indent-rigidly-map
     '(("h" . indent-rigidly-left)
       ("n" . indent-rigidly-right)))

    (xah-fly--define-keys
     xah-fly-shared-map
     '(("<home>" . xah-fly-command-mode-activate)
       ("<menu>" . xah-fly-command-mode-activate)
       ("<escape>" . xah-fly-command-mode-activate)
       ("<f8>" . xah-fly-command-mode-activate))
     :direct)

    (when xah-fly-use-isearch-arrows
      (xah-fly--define-keys
       isearch-mode-map
       '(("<up>" . isearch-ring-retreat)
         ("<down>" . isearch-ring-advance)
         ("<left>" . isearch-repeat-backward)
         ("<right>" . isearch-repeat-forward)
         ("C-v" . isearch-yank-kill))
       :direct)
      (xah-fly--define-keys
       minibuffer-local-isearch-map
       '(("<left>" . isearch-reverse-exit-minibuffer)
         ("<right>" . isearch-forward-exit-minibuffer))
       :direct))

    (xah-fly--define-keys
     (define-prefix-command 'xah-fly-leader-key-map)
     '(("SPC" . xah-fly-insert-mode-activate)
       ("RET" . execute-extended-command)

       ("TAB" . nil)

       ("TAB TAB" . indent-for-tab-command)

       ("TAB i" . complete-symbol)
       ("TAB g" . indent-rigidly)
       ("TAB r" . indent-region)
       ("TAB s" . indent-sexp)

       (". ." . highlight-symbol-at-point)
       (". g" . unhighlight-regexp)
       (". c" . highlight-lines-matching-regexp)
       (". h" . highlight-regexp)
       (". t" . highlight-phrase)
       (". e" . isearch-forward-symbol-at-point)
       (". u" . isearch-forward-symbol)
       (". p" . isearch-forward-word)

       ("'" . xah-fill-or-unfill)

       (", t" . xref-find-definitions)
       (", n" . xref-pop-marker-stack)

       ;; - / ; = [
       ("\\" . toggle-input-method)
       ;; `

       ("3" . delete-window)
       ("4" . split-window-right)
       ("5" . balance-windows)
       ("6" . xah-upcase-sentence)

       ("9" . ispell-word)

       ("a" . mark-whole-buffer)
       ("b" . end-of-buffer)

       ("c ," . xah-open-in-external-app)
       ("c ." . find-file)
       ("c c" . bookmark-bmenu-list)
       ("c e" . ibuffer)
       ("c f" . xah-open-recently-closed)
       ("c g" . xah-open-in-terminal)
       ("c h" . recentf-open-files)
       ("c j" . xah-copy-file-path)
       ("c l" . bookmark-set)
       ("c n" . xah-new-empty-buffer)
       ("c o" . xah-show-in-desktop)
       ("c p" . xah-open-last-closed)
       ("c r" . bookmark-jump)
       ("c s" . write-file)
       ("c u" . xah-open-file-at-cursor)
       ("c x" . set-buffer-file-coding-system)
       ("c y" . xah-list-recently-closed)
       ("c z" . revert-buffer-with-coding-system)

       ;; set-buffer-process-coding-system
       ;; set-file-name-coding-system
       ;; set-keyboard-coding-system
       ;; set-language-environment
       ;; set-next-selection-coding-system
       ;; set-selection-coding-system
       ;; set-terminal-coding-system
       ;; universal-coding-system-argument

       ("d" . beginning-of-buffer)

       ("e a" . xah-insert-double-angle-bracket)     ; 《》
       ("e b" . xah-insert-black-lenticular-bracket) ; 【】

       ("e c r" . expand-region-abbrevs)
       ("e c t" . edit-abbrevs)
       ("e c u" . expand-abbrev)

       ("e c g" . add-mode-abbrev)
       ("e c c" . add-global-abbrev)
       ("e c m" . inverse-add-mode-abbrev)
       ("e c w" . inverse-add-global-abbrev)

       ("e c f" . unexpand-abbrev)

       ("e c h" . expand-jump-to-previous-slot)
       ("e c n" . expand-jump-to-next-slot)
       ("e c y" . abbrev-prefix-mark)

       ("e d" . xah-insert-double-curly-quote) ; “”
       ("e e" . xah-insert-unicode)
       ("e f" . xah-insert-emacs-quote)        ; `'
       ("e g" . xah-insert-ascii-double-quote) ; ""

       ("e h" . xah-insert-brace)              ; {}
       ("e i" . xah-insert-curly-single-quote) ; ‘’
       ("e j" . insert-char)
       ("e k" . xah-insert-markdown-quote) ; ``
       ("e l" . xah-insert-formfeed)
       ("e m" . xah-insert-corner-bracket)     ; 「」
       ("e n" . xah-insert-square-bracket)     ; []
       ("e o" . xah-insert-ascii-single-quote) ; ''

       ("e p" . xah-insert-single-angle-quote)     ; ‹›
       ;; q
       ("e r" . xah-insert-tortoise-shell-bracket) ; 〔〕
       ;; s
       ("e t" . xah-insert-paren) ; ()
       ("e u" . xah-insert-date)
       ("e v" . xah-insert-markdown-triple-quote) ;
       ("e w" . xah-insert-angle-bracket)         ; 〈〉
       ;; x
       ("e y" . xah-insert-double-angle-quote) ; «»
       ;; z

       ("f" . xah-search-current-word)
       ("g" . xah-close-current-buffer)

       ("h a" . apropos-command)
       ("h b" . describe-bindings)
       ("h c" . describe-char)
       ("h d" . apropos-documentation)
       ("h e" . view-echo-area-messages)
       ("h f" . describe-face)
       ("h g" . info-lookup-symbol)
       ("h h" . describe-function)
       ("h i" . info)
       ("h j" . man)
       ("h k" . describe-key)
       ("h l" . view-lossage)
       ("h m" . describe-mode)
       ("h n" . describe-variable)
       ("h o" . describe-language-environment)
       ("h r" . apropos-variable)
       ("h s" . describe-syntax)
       ("h u" . elisp-index-search)
       ("h v" . apropos-value)
       ("h x" . describe-command) ; emacs 28
       ("h z" . describe-coding-system)

       ("i" . kill-line)
       ("j" . xah-copy-all-or-region)

       ("l" . recenter-top-bottom)

       ("m" . dired-jump)

       ;; ("m t" . dired-jump)
       ;; ("m e" . delete-other-windows)
       ;; ("m u" . split-window-below)
       ;; ("m w" . universal-argument)

       ;; commands here are “harmless”, they don't modify text etc. they turn on modes, change display, prompt, start shell, etc.
       ("n SPC" . whitespace-mode)
       ("n ," . abbrev-mode)
       ("n ." . toggle-frame-maximized)
       ("n 1" . set-input-method)
       ("n 2" . global-hl-line-mode)
       ("n 4" . global-display-line-numbers-mode)
       ("n 6" . calendar)
       ("n 7" . calc)
       ("n 9" . shell-command)
       ("n 0" . shell-command-on-region)

       ("n a" . text-scale-adjust)
       ("n b" . toggle-debug-on-error)
       ("n c" . toggle-case-fold-search)
       ("n d" . narrow-to-page)
       ("n e" . eshell)
       ;; f
       ("n g" . xah-toggle-read-novel-mode)
       ("n h" . widen)
       ("n i" . make-frame-command)
       ("n j" . flyspell-buffer)
       ("n k" . menu-bar-open)
       ("n l" . toggle-word-wrap)
       ("n m" . jump-to-register)
       ("n n" . xah-narrow-to-region)
       ("n o" . variable-pitch-mode)
       ("n p" . read-only-mode)
       ;; q
       ("n r" . count-words)
       ("n s" . count-matches)
       ("n t" . narrow-to-defun)
       ("n u" . shell)
       ("n v" . visual-line-mode)
       ("n w" . eww)
       ("n x" . save-some-buffers)
       ("n y" . toggle-truncate-lines)
       ("n z" . abort-recursive-edit)

       ("o" . exchange-point-and-mark)
       ("p" . query-replace)
       ("q" . xah-cut-all-or-region)

       ;; roughly text replacement related
       ("r SPC" . rectangle-mark-mode)
       ("r ," . apply-macro-to-region-lines)
       ("r ." . kmacro-start-macro)
       ("r 3" . number-to-register)
       ("r 4" . increment-register)

       ;; a
       ;; b
       ("r c" . replace-rectangle)
       ("r d" . delete-rectangle)
       ("r e" . call-last-kbd-macro)
       ;; f
       ("r g" . kill-rectangle)
       ("r h" . xah-change-bracket-pairs)
       ("r i" . xah-space-to-newline)
       ("r j" . copy-rectangle-to-register)
       ("r k" . xah-slash-to-double-backslash)
       ("r l" . clear-rectangle)

       ("r m" . xah-slash-to-backslash)
       ("r n" . rectangle-number-lines)
       ("r o" . open-rectangle)
       ("r p" . kmacro-end-macro)

       ;; q
       ("r r" . yank-rectangle)
       ;; s t
       ("r u" . xah-quote-lines)
       ;; v w
       ("r x" . xah-double-backslash-to-slash)
       ("r y" . delete-whitespace-rectangle)
       ;; z

       ("s" . save-buffer)

       ;; most frequently used
       ("t <up>"  . xah-move-block-up)
       ("t <down>"  . xah-move-block-down)

       ("t ," . sort-numeric-fields)
       ("t ." . xah-sort-lines)
       ("t 1" . xah-append-to-register-1)
       ("t 2" . xah-clear-register-1)
       ("t 3" . xah-copy-to-register-1)
       ("t 4" . xah-paste-from-register-1)
       ("t 7" . xah-append-to-register-1)
       ("t 8" . xah-clear-register-1)

       ("t a" . xah-reformat-to-sentence-lines)
       ;; c b
       ("t d" . mark-defun)
       ("t e" . list-matching-lines)
       ("t f" . move-to-column)
       ("t g" . goto-line)
       ("t h" . repeat-complex-command)
       ("t i" . delete-non-matching-lines)
       ("t j" . copy-to-register)
       ("t k" . insert-register)
       ("t l" . xah-escape-quotes)
       ("t m" . xah-make-backup-and-save)
       ("t n" . goto-char)
       ("t o" . xah-clean-whitespace)
       ("t p" . query-replace-regexp)
       ("t q" . xah-cut-text-in-quote)
       ;; r
       ;; s
       ("t t" . repeat)
       ("t u" . delete-matching-lines)

       ("t w" . xah-next-window-or-frame)
       ("t x" . xah-title-case-region-or-line)
       ("t y" . delete-duplicate-lines)

       ("u" . switch-to-buffer)
       ("v" . universal-argument)

       ;; dangerous map. run program, delete file, etc
       ("w DEL" . xah-delete-current-file-make-backup)
       ("w ." . eval-buffer)
       ("w e" . eval-defun)
       ("w m" . eval-last-sexp)
       ("w p" . eval-expression)
       ("w u" . eval-region)
       ("w q" . save-buffers-kill-terminal)
       ("w w" . delete-frame)
       ("w j" . xah-run-current-file)

       ("x" . xah-toggle-previous-letter-case)
       ("y" . xah-show-kill-ring)

       ;; vc command keys subject to change. need a frequency stat of the commands.

       ("z b" . vc-root-diff)   ; D
       ("z c" . vc-update)      ; git pull, +
       ("z d" . vc-annotate)    ; g
       ("z f" . vc-revert)      ; u
       ("z g" . vc-push)        ; git push, P
       ("z h" . vc-diff)        ; git diff, =
       ("z l" . vc-print-root-log) ; L
       ("z m" . vc-dir)         ; git status, C-x v d
       ("z n" . vc-print-log)   ; git log, l
       ("z r" . vc-merge)       ; m
       ("z t" . vc-register)    ; git add, i
       ("z z" . vc-next-action) ; v

       ("z 1" . vc-create-tag)            ; s
       ("z 2" . vc-insert-headers)        ; h
       ("z 4" . vc-retrieve-tag)          ; r
       ("z 5" . vc-revision-other-window) ; ~
       ("z 6" . vc-switch-backend)        ; b
       ("z 7" . vc-update-change-log)     ; a

       ;;
       ))

    (xah-fly--define-keys
     xah-fly-command-map
     '(("SPC" . xah-fly-leader-key-map)
       ("'" . xah-reformat-lines)
       ("," . xah-shrink-whitespaces)
       ("-" . delete-other-windows)
       ("." . backward-kill-word)
       ("/" . hippie-expand)
       (";" . xah-comment-dwim)
       ("[" . split-window-below)
       ("\\" . xah-cycle-hyphen-lowline-space)
       ("]" . split-window-right)
       ("`" . other-frame)

       ("1" . undefined)
       ("2" . undefined)
       ("3" . delete-other-windows)
       ("4" . split-window-below)
       ("5" . delete-char)
       ("6" . xah-select-block)
       ("7" . xah-select-line)
       ("8" . xah-extend-selection)
       ("9" . xah-select-text-in-quote)
       ("0" . xah-pop-local-mark-ring)

       ("a" . execute-extended-command)
       ("b" . isearch-forward)
       ("c" . previous-line)
       ("d" . xah-beginning-of-line-or-block)
       ("e" . xah-smart-delete)
       ("f" . undo)
       ("g" . backward-word)
       ("h" . backward-char)
       ("i" . xah-delete-current-text-block)
       ("j" . xah-copy-line-or-region)
       ("k" . xah-paste-or-paste-previous)
       ;; ("l" . xah-fly-insert-mode-activate-space-before)
       ("l" . xah-insert-space-before)
       ("m" . xah-backward-left-bracket)
       ("n" . forward-char)
       ("o" . open-line)
       ("p" . kill-word)
       ("q" . xah-cut-line-or-region)
       ("r" . forward-word)
       ("s" . xah-end-of-line-or-block)
       ("t" . next-line)
       ("u" . xah-fly-insert-mode-activate)
       ("v" . xah-forward-right-bracket)
       ("w" . xah-next-window-or-frame)
       ("x" . xah-toggle-letter-case)
       ("y" . set-mark-command)
       ("z" . xah-goto-matching-bracket)))

    ;;
    ))

(xah-fly-define-keys)


;; set control meta, etc keys

(defcustom xah-fly-unset-useless-key t
  "If true, unbind many obsolete or useless or redundant
  keybinding. e.g. <help>, <f1>."
  :type 'boolean)

(when xah-fly-unset-useless-key
  (global-set-key (kbd "<help>") nil)
  (global-set-key (kbd "<f1>") nil))

(when xah-fly-use-meta-key

  (global-set-key (kbd "M-<home>") nil) ; beginning-of-buffer-other-window
  (global-set-key (kbd "M-<end>") nil) ; end-of-buffer-other-window

  (global-set-key (kbd "M-SPC") #'xah-fly-command-mode-activate)
  (global-set-key (kbd "M-\\") nil) ; delete-horizontal-space
  (global-set-key (kbd "M-!") nil)  ; shell-command
  (global-set-key (kbd "M-$") nil)  ; ispell-word
  (global-set-key (kbd "M-%") nil)  ; query-replace
  (global-set-key (kbd "M-&") nil)  ; async-shell-command
  (global-set-key (kbd "M-'") nil)  ; abbrev-prefix-mark
  (global-set-key (kbd "M-(") nil)  ; insert-parentheses
  (global-set-key (kbd "M-)") nil)  ; move-past-close-and-reindent
  ;; (global-set-key (kbd "M-,") nil) ; xref-pop-marker-stack
  ;; (global-set-key (kbd "M-.") nil) ; xref-find-definitions
  (global-set-key (kbd "M-/") nil) ; dabbrev-expand
  (global-set-key (kbd "M-:") nil) ; eval-expression
  ;; (global-set-key (kbd "M-;") nil) ; comment-dwim
  (global-set-key (kbd "M-<") nil) ; beginning-of-buffer
  (global-set-key (kbd "M-=") nil) ; count-words-region
  (global-set-key (kbd "M->") nil) ; end-of-buffer
  ;; (global-set-key (kbd "M-?") nil) ; xref-find-references
  (global-set-key (kbd "M-@") nil) ; mark-word
  (global-set-key (kbd "M-^") nil) ; delete-indentation
  (global-set-key (kbd "M-`") nil) ; tmm-menubar
  (global-set-key (kbd "M-a") nil) ; backward-sentence
  (global-set-key (kbd "M-b") nil) ; backward-word
  (global-set-key (kbd "M-c") nil) ; capitalize-word
  (global-set-key (kbd "M-d") nil) ;  kill-word
  (global-set-key (kbd "M-e") nil) ; forward-sentence
  (global-set-key (kbd "M-f") nil) ; forward-word
  (global-set-key (kbd "M-g") nil) ; Prefix Command
  (global-set-key (kbd "M-h") nil) ; mark-paragraph
  (global-set-key (kbd "M-i") nil) ; tab-to-tab-stop
  (global-set-key (kbd "M-j") nil) ; default-indent-new-line
  (global-set-key (kbd "M-k") nil) ; kill-sentence
  (global-set-key (kbd "M-l") nil) ; downcase-word
  (global-set-key (kbd "M-m") nil) ; back-to-indentation
  (global-set-key (kbd "M-o") nil) ; facemenu-keymap
  (global-set-key (kbd "M-q") nil) ; fill-paragraph
  (global-set-key (kbd "M-r") nil) ; move-to-window-line-top-bottom
  (global-set-key (kbd "M-s") nil) ; Prefix Command
  (global-set-key (kbd "M-t") nil) ; transpose-words
  (global-set-key (kbd "M-u") nil) ; upcase-word
  (global-set-key (kbd "M-v") nil) ; scroll-down-command
  (global-set-key (kbd "M-w") nil) ; kill-ring-save
  ;; (global-set-key (kbd "M-x") nil) ; execute-extended-command
  ;; (global-set-key (kbd "M-y") nil) ; yank-pop
  (global-set-key (kbd "M-z") nil)   ; zap-to-char
  (global-set-key (kbd "M-{") nil)   ; backward-paragraph
  (global-set-key (kbd "M-|") nil)   ; shell-command-on-region
  (global-set-key (kbd "M-}") nil)   ; forward-paragraph
  (global-set-key (kbd "M-~") nil)   ; not-modified
  (global-set-key (kbd "M-DEL") nil) ; backward-kill-word
  )

(when xah-fly-use-control-key

  ;; 2021-08-07 was
  ;; (xah-fly--define-keys
  ;;  xah-fly-shared-map
  ;;  '(
  ;;    ("C-1" . cmd)
  ;;    ("C-2" . cmd)
  ;;    )
  ;;  :direct)

 ;; define control combo in xah-fly-shared-map may be a problem.
;; by setting them in xah-fly-shared-map, it becomes unchangeable, that is, if a major mode set a key for C-t, it will have no effect.
;; Current solution is just to use global-set-key.
;; The disadvantage is that these changes leak, that is, xah-fly-keys is turned off, these ctrl keys are still changed. Still, this is better, because xah fly keys is not really meant to be turned off temporarily.
;; Removing the tons of default emacs control and meta keys is desirable.
;; because there are hundreds of them, confusing, and mostly useless.

  (global-set-key (kbd "<C-S-prior>") #'xah-previous-emacs-buffer)
  (global-set-key (kbd "<C-S-next>") #'xah-next-emacs-buffer)

  (global-set-key (kbd "<C-tab>") #'xah-next-user-buffer)
  (global-set-key (kbd "<C-S-tab>") #'xah-previous-user-buffer)
  (global-set-key (kbd "<C-S-iso-lefttab>") #'xah-previous-user-buffer)

  (global-set-key (kbd "<C-prior>") #'xah-previous-user-buffer)
  (global-set-key (kbd "<C-next>") #'xah-next-user-buffer)

  (global-set-key (kbd "<f7>") 'xah-fly-leader-key-map)

  ;; (global-set-key (kbd "C-1") nil)
  (global-set-key (kbd "C-2") #'pop-global-mark)
  (global-set-key (kbd "C-3") #'previous-error)
  (global-set-key (kbd "C-4") #'next-error)
  (global-set-key (kbd "C-5") #'xah-previous-emacs-buffer)
  (global-set-key (kbd "C-6") #'xah-next-emacs-buffer)
  (global-set-key (kbd "C-7") #'xah-previous-user-buffer)
  (global-set-key (kbd "C-8") #'xah-next-user-buffer)
  (global-set-key (kbd "C-9") #'scroll-down-command)
  (global-set-key (kbd "C-0") #'scroll-up-command)

  (global-set-key (kbd "C--") #'text-scale-decrease)
  (global-set-key (kbd "C-=") #'text-scale-increase)

  (global-set-key (kbd "C-SPC") #'xah-fly-command-mode-activate)

  (global-set-key (kbd "C-S-n") #'make-frame-command)
  (global-set-key (kbd "C-S-s") #'write-file)
  (global-set-key (kbd "C-S-t") #'xah-open-last-closed)

  ;; (global-set-key (kbd "C-@") nil)

  (global-set-key (kbd "C-a") #'mark-whole-buffer)
  ;; (global-set-key (kbd "C-b") nil)
  ;; (global-set-key (kbd "C-c") nil)
  ;; (global-set-key (kbd "C-d") nil)
  ;; (global-set-key (kbd "C-e") nil)
  ;; (global-set-key (kbd "C-f") nil)
  ;; (global-set-key (kbd "C-g") nil) ; cancel
  ;; (global-set-key (kbd "C-h") nil) ; help
  ;; (global-set-key (kbd "C-i") nil) ; tab
  ;; (global-set-key (kbd "C-j") nil) ; newline
  ;; (global-set-key (kbd "C-k") nil)
  ;; (global-set-key (kbd "C-l") nil)
  ;; (global-set-key (kbd "C-m") nil)
  (global-set-key (kbd "C-n") #'xah-new-empty-buffer)
  (global-set-key (kbd "C-o") #'find-file)
  ;; (global-set-key (kbd "C-p") nil)
  ;; (global-set-key (kbd "C-q") nil)
  ;; (global-set-key (kbd "C-r") nil)
  (global-set-key (kbd "C-s") #'save-buffer)
  (global-set-key (kbd "C-t") #'hippie-expand)
  ;; (global-set-key (kbd "C-u") nil)
  (global-set-key (kbd "C-v") #'yank)
  (global-set-key (kbd "C-w") #'xah-close-current-buffer)
  ;; (global-set-key (kbd "C-x") nil)

  (when (>= emacs-major-version 28)
    (global-set-key (kbd "C-y") #'undo-redo))
  (global-set-key (kbd "C-z") #'undo)
  ;;
  )



(when (< emacs-major-version 28)
  (defalias 'execute-extended-command-for-buffer #'execute-extended-command))


;;;; misc

;; the following have keys in gnu emacs, but i decided not to give them a key, because either they are rarely used (say, 95% of emacs users use them less than once a month ), or there is a more efficient command/workflow with key in xah-fly-keys

;; C-x r w → window-configuration-to-register
;; C-x r f → frameset-to-register

;; C-x C-p   →   mark-page
;; C-x C-l   →   downcase-region
;; C-x C-u   →   upcase-region

;; C-x C-t   →   transpose-lines
;; C-x C-o   →   delete-blank-lines

;; C-x C-r   →   find-file-read-only
;; C-x C-v   →   find-alternate-file

;; C-x =   →   what-cursor-position, use describe-char instead
;; C-x <   →   scroll-left
;; C-x >   →   scroll-right
;; C-x [   →   backward-page
;; C-x ]   →   forward-page
;; C-x ^   →   enlarge-window

;; C-x {   →   shrink-window-horizontally
;; C-x }   →   enlarge-window-horizontally
;; C-x DEL   →   backward-kill-sentence

;; C-x C-z   →   suspend-frame

;; C-x k   →   kill-buffer , use xah-close-current-buffer
;; C-x l   →   count-lines-page
;; C-x m   →   compose-mail


;; undecided yet

;; C-x e   →   kmacro-end-and-call-macro
;; C-x q   →   kbd-macro-query
;; C-x C-k   →   kmacro-keymap

;; C-x C-d   →   list-directory
;; C-x C-n   →   set-goal-column
;; C-x ESC   →   Prefix Command
;; C-x $   →   set-selective-display
;; C-x *   →   calc-dispatch
;; C-x -   →   shrink-window-if-larger-than-buffer
;; C-x .   →   set-fill-prefix

;; C-x 4   →   ctl-x-4-prefix
;; C-x 5   →   ctl-x-5-prefix
;; C-x 6   →   2C-command
;; C-x ;   →   comment-set-column

;; C-x f   →   set-fill-column
;; C-x i   →   insert-file
;; C-x n   →   Prefix Command
;; C-x r   →   Prefix Command

;; C-x C-k C-a   →   kmacro-add-counter
;; C-x C-k C-c   →   kmacro-set-counter
;; C-x C-k C-d   →   kmacro-delete-ring-head
;; C-x C-k C-e   →   kmacro-edit-macro-repeat
;; C-x C-k C-f   →   kmacro-set-format
;; C-x C-k TAB   →   kmacro-insert-counter
;; C-x C-k C-k   →   kmacro-end-or-call-macro-repeat
;; C-x C-k C-l   →   kmacro-call-ring-2nd-repeat
;; C-x C-k RET   →   kmacro-edit-macro
;; C-x C-k C-n   →   kmacro-cycle-ring-next
;; C-x C-k C-p   →   kmacro-cycle-ring-previous
;; C-x C-k C-t   →   kmacro-swap-ring
;; C-x C-k C-v   →   kmacro-view-macro-repeat
;; C-x C-k SPC   →   kmacro-step-edit-macro
;; C-x C-k b   →   kmacro-bind-to-key
;; C-x C-k e   →   edit-kbd-macro
;; C-x C-k l   →   kmacro-edit-lossage
;; C-x C-k n   →   kmacro-name-last-macro
;; C-x C-k q   →   kbd-macro-query



;; C-x 4 C-f   →   find-file-other-window
;; C-x 4 C-o   →   display-buffer
;; C-x 4 .   →   find-tag-other-window
;; C-x 4 0   →   kill-buffer-and-window
;; C-x 4 a   →   add-change-log-entry-other-window
;; C-x 4 b   →   switch-to-buffer-other-window
;; C-x 4 c   →   clone-indirect-buffer-other-window
;; C-x 4 d   →   dired-other-window
;; C-x 4 f   →   find-file-other-window
;; C-x 4 m   →   compose-mail-other-window
;; C-x 4 r   →   find-file-read-only-other-window

;; C-x 6 2   →   2C-two-columns
;; C-x 6 b   →   2C-associate-buffer
;; C-x 6 s   →   2C-split

;; ctl-x-5-map

;; r C-f   →   find-file-other-frame
;; r C-o   →   display-buffer-other-frame
;; r .   →   find-tag-other-frame
;; r 0   →   delete-frame
;; r 1   →   delete-other-frames
;; r 2   →   make-frame-command
;; r b   →   switch-to-buffer-other-frame
;; r d   →   dired-other-frame
;; r f   →   find-file-other-frame
;; r m   →   compose-mail-other-frame
;; r o   →   other-frame
;; r r   →   find-file-read-only-other-frame



(defvar xah-fly-insert-state-p t "non-nil means insertion mode is on.")

(defun xah-fly--update-key-map ()
  (setq xah-fly-key-map (if xah-fly-insert-state-p
                            xah-fly-insert-map
                          xah-fly-command-map)))

(defun xah-fly-keys-set-layout (Layout)
  "Set a keyboard layout.
Argument must be one of the key name in `xah-fly-layouts'
Version: 2021-05-19 2022-09-11 2022-10-22 2022-10-31"
  (interactive "sType a layout: ")
  (let ((xnewlout
         (cond
          ((stringp Layout) Layout)
          ((symbolp Layout) (symbol-name Layout))
          (t (user-error "Layout %s must be a string." Layout))))
        (xoldlout xah-fly-key-current-layout))
    (setq xah-fly-key-current-layout xnewlout)
    (setq xah-fly--key-convert-table
          (cdr (assoc xah-fly-key-current-layout xah-fly-layouts)))
    (when (and (featurep 'xah-fly-keys)
               (not (string-equal xoldlout xnewlout)))
      (xah-fly-define-keys))))

(defun xah-fly-space-key ()
  "Switch to command mode if the char before cursor is a space.
experimental
Version: 2018-05-07"
  (interactive)
  (if (eq (char-before ) 32)
      (xah-fly-command-mode-activate)
    (insert " ")))

(defun xah-fly-command-mode-init ()
  "Set command mode keys.
Version: 2022-07-06"
  (interactive)
  (setq xah-fly-insert-state-p nil)
  (xah-fly--update-key-map)
  (when xah-fly--deactivate-command-mode-func
    (funcall xah-fly--deactivate-command-mode-func))
  (setq xah-fly--deactivate-command-mode-func
        (set-transient-map xah-fly-command-map (lambda () t)))
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  ;; (set-face-background 'cursor "red")
  (setq mode-line-front-space xah-fly-command-mode-indicator)
  (force-mode-line-update))

(defun xah-fly-insert-mode-init (&optional no-indication)
  "Enter insertion mode."
  (interactive)
  (setq xah-fly-insert-state-p t)
  (xah-fly--update-key-map)
  (funcall xah-fly--deactivate-command-mode-func)
  (unless no-indication
    (modify-all-frames-parameters '((cursor-type . bar)))
    ;; (set-face-background 'cursor "black")
    (setq mode-line-front-space xah-fly-insert-mode-indicator))
  (force-mode-line-update))

(defun xah-fly-mode-toggle ()
  "Switch between {insertion, command} modes."
  (interactive)
  (if xah-fly-insert-state-p
      (xah-fly-command-mode-activate)
    (xah-fly-insert-mode-activate)))

(defun xah-fly-save-buffer-if-file ()
  "Save current buffer if it is a file."
  (interactive)
  (when buffer-file-name
    (save-buffer)))

(defun xah-fly-command-mode-activate ()
  "Activate command mode and run `xah-fly-command-mode-activate-hook'
Version: 2017-07-07"
  (interactive)
  (xah-fly-command-mode-init)
  (run-hooks 'xah-fly-command-mode-activate-hook))

(defun xah-fly-command-mode-activate-no-hook ()
  "Activate command mode. Does not run `xah-fly-command-mode-activate-hook'
Version: 2017-07-07"
  (interactive)
  (xah-fly-command-mode-init))

(defun xah-fly-insert-mode-activate ()
  "Activate insertion mode.
Version: 2017-07-07"
  (interactive)
  (xah-fly-insert-mode-init)
  (run-hooks 'xah-fly-insert-mode-activate-hook))

(defun xah-fly-insert-mode-activate-newline ()
  "Activate insertion mode, insert newline below."
  (interactive)
  (xah-fly-insert-mode-activate)
  (open-line 1))

(defun xah-fly-insert-mode-activate-space-before ()
  "Insert a space, then activate insertion mode."
  (interactive)
  (insert " ")
  (xah-fly-insert-mode-activate))

(defun xah-fly-insert-mode-activate-space-after ()
  "Insert a space, then activate insertion mode."
  (interactive)
  (insert " ")
  (xah-fly-insert-mode-activate)
  (left-char))



;;;###autoload
(define-minor-mode xah-fly-keys
  "A modal keybinding set, like vim, but based on ergonomic
  principles, like Dvorak layout.

URL `http://xahlee.info/emacs/misc/xah-fly-keys.html'"
  :global t
  :lighter " ∑flykeys"
  :keymap xah-fly-insert-map
  (delete-selection-mode 1)
  (setq shift-select-mode nil)

  (if xah-fly-keys
      ;; Construction:
      (progn
        (add-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)
        (add-hook 'minibuffer-exit-hook 'xah-fly-command-mode-activate)
        (add-hook 'isearch-mode-end-hook 'xah-fly-command-mode-activate)
        (when (and (keymapp xah-fly-key-map)
                   (not (memq xah-fly-key-map (list xah-fly-command-map
                                                    xah-fly-insert-map))))
          (set-keymap-parent xah-fly-key-map xah-fly-shared-map)
          (setq xah-fly-shared-map xah-fly-key-map))
        (xah-fly-command-mode-activate))
    (progn
      ;; Teardown:
      (remove-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)
      (remove-hook 'minibuffer-exit-hook 'xah-fly-command-mode-activate)
      (remove-hook 'isearch-mode-end-hook 'xah-fly-command-mode-activate)
      (remove-hook 'eshell-mode-hook 'xah-fly-insert-mode-activate)
      (remove-hook 'shell-mode-hook 'xah-fly-insert-mode-activate)
      (xah-fly-insert-mode-init :no-indication)
      (setq mode-line-front-space '(:eval (if (display-graphic-p) " " "-")))

      ;;
      )))

(provide 'xah-fly-keys)



;; Local Variables:
;; byte-compile-docstring-max-column: 999
;; End:

;;; xah-fly-keys.el ends here
