;;; xah-fly-keys.el --- ergonomic modal keybinding minor mode. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2013-2022 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Maintainer: Xah Lee <xah@xahlee.org>
;; Version: 22.3.20221211131110
;; Created: 10 Sep 2013
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience, emulations, vim, ergoemacs
;; License: GPL v3.
;; Homepage: http://xahlee.info/emacs/misc/xah-fly-keys.html

;; This file is not part of GNU Emacs.

;;; Commentary:

;; xah-fly-keys is a efficient keybinding for emacs. It is modal like
;; vi, but key choices are based on statistics of command call
;; frequency.

;; HOW TO USE

;; M-x xah-fly-keys to toggle the mode on/off.

;; Important command/insert mode switch keys:

;; xah-fly-command-mode-activate (press <home> or F8 or Alt+Space or
;; Ctrl+Space or menu key)

;; xah-fly-insert-mode-activate (when in command mode, press qwerty
;; letter key f.)

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

;; put the file xah-fly-keys.el in ~/.emacs.d/lisp/
;; create the dir if doesn't exist.

;; put the following in your emacs init file:

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'xah-fly-keys)
;; (xah-fly-keys-set-layout "qwerty") ; required

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

;; supported layouts are stored as the keys in xah-fly-layouts

;; (xah-fly-keys 1)


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
Version: 2021-08-12"
  (let ( $p1 $p2 ($blankRegex "\n[ \t]*\n"))
    (save-excursion
      (setq $p1 (if (re-search-backward $blankRegex nil 1)
                    (goto-char (match-end 0))
                  (point)))
      (setq $p2 (if (re-search-forward $blankRegex nil 1)
                    (match-beginning 0)
                  (point))))
    (cons $p1 $p2 )))

(defun xah-get-bounds-of-block-or-region ()
  "If region is active, return its boundary, else same as `xah-get-bounds-of-block'.
Version: 2021-08-12"
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (xah-get-bounds-of-block)))


;; cursor movement

(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.

URL `http://xahlee.info/emacs/emacs/emacs_jump_to_previous_position.html'
Version: 2016-04-04"
  (interactive)
  (set-mark-command t))

(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of line or previous block.

• When called first time, move cursor to beginning of char in current line. (if already, move to beginning of line.)
• When called again, move cursor backward by jumping over any sequence of whitespaces containing 2 blank lines.
• if `visual-line-mode' is on, beginning of line means visual line.

URL `http://xahlee.info/emacs/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version: 2018-06-04 2021-03-16 2022-03-30 2022-07-03 2022-07-06"
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (eq last-command this-command))
        (when
            (re-search-backward "\n[\t\n ]*\n+" nil 1)
          (skip-chars-backward "\n\t ")
          (forward-char))
      (if visual-line-mode
          (beginning-of-visual-line)
        (if (eq major-mode 'eshell-mode)
            (progn
              (declare-function eshell-bol "esh-mode.el" ())
              (eshell-bol))
          (back-to-indentation)
          (when (eq $p (point))
            (beginning-of-line)))))))

(defun xah-end-of-line-or-block ()
  "Move cursor to end of line or next block.

• When called first time, move cursor to end of line.
• When called again, move cursor forward by jumping over any sequence of whitespaces containing 2 blank lines.
• if `visual-line-mode' is on, end of line means visual line.

URL `http://xahlee.info/emacs/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
Version: 2018-06-04 2021-03-16 2022-03-05"
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command))
      (re-search-forward "\n[\t\n ]*\n+" nil 1)
    (if visual-line-mode
        (end-of-visual-line)
      (end-of-line))))

(defvar xah-brackets '("“”" "()" "[]" "{}" "<>" "＜＞" "（）" "［］" "｛｝" "⦅⦆" "〚〛" "⦃⦄" "‹›" "«»" "「」" "〈〉" "《》" "【】" "〔〕" "⦗⦘" "『』" "〖〗" "〘〙" "｢｣" "⟦⟧" "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "⌈⌉" "⌊⌋" "⦇⦈" "⦉⦊" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬❭" "❮❯" "❰❱" "❲❳" "〈〉" "⦑⦒" "⧼⧽" "﹙﹚" "﹛﹜" "﹝﹞" "⁽⁾" "₍₎" "⦋⦌" "⦍⦎" "⦏⦐" "⁅⁆" "⸢⸣" "⸤⸥" "⟅⟆" "⦓⦔" "⦕⦖" "⸦⸧" "⸨⸩" "｟｠")
  "A list of strings, each element is a string of 2 chars, the left bracket and a matching right bracket. Used by `xah-select-text-in-quote' and others.")

(defconst xah-left-brackets
  (mapcar (lambda (x) (substring x 0 1)) xah-brackets)
  "List of left bracket chars. Each element is a string.")

(defconst xah-right-brackets
  (mapcar (lambda (x) (substring x 1 2)) xah-brackets)
  "List of right bracket chars. Each element is a string.")

(defconst xah-punctuation-regex "[!?\".,`'#$%&*+:;=@^|~]+"
  "A regex string for the purpose of moving cursor to a punctuation.")

(defun xah-forward-punct (&optional n)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `xah-punctuation-regex'

URL `http://xahlee.info/emacs/emacs/emacs_jump_to_punctuations.html'
Version: 2017-06-26"
  (interactive "p")
  (re-search-forward xah-punctuation-regex nil t n))

(defun xah-backward-punct (&optional n)
  "Move cursor to the previous occurrence of punctuation.
See `xah-forward-punct'
URL `http://xahlee.info/emacs/emacs/emacs_jump_to_punctuations.html'
Version: 2017-06-26"
  (interactive "p")
  (re-search-backward xah-punctuation-regex nil t n))

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
Version: 2016-11-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at (regexp-opt xah-left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt xah-right-brackets) (max (- (point) 1) 1))
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
  (let (($pos (point)))
    (if (nth 3 (syntax-ppss))
        (progn
          (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
          (forward-sexp)
          (re-search-forward "\\\"" nil t))
      (progn (re-search-forward "\\\"" nil t)))
    (when (<= (point) $pos)
      (progn (re-search-forward "\\\"" nil t)))))

(defun xah-sort-lines ()
  "Like `sort-lines' but if no region, do the current block.
Version 2022-01-22 2022-01-23"
  (interactive)
  (let ($p1 $p2)
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (sort-lines current-prefix-arg $p1 $p2)))

(defun xah-narrow-to-region ()
  "Same as `narrow-to-region', but if no selection, narrow to the current block.
Version 2022-01-22"
  (interactive)
  (let ($p1 $p2)
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (narrow-to-region $p1 $p2)))


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
                 (buffer-substring-no-properties (line-beginning-position) (line-end-position))
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
  (let (($buf (generate-new-buffer "*copy history*"))
        (inhibit-read-only t))
    (progn
      (switch-to-buffer $buf)
      (funcall 'fundamental-mode)
      (mapc
       (lambda (x)
         (insert x xah-show-kill-ring-separator ))
       kill-ring))
    (goto-char (point-min))))

(defun xah-move-block-up ()
  "Swap the current text block with the previous.
After this command is called, press <up> or <down> to move. Any other key to exit.
Version 2022-03-04"
  (interactive)
  (let (($p0 (point))
        $c1 ; current block begin
        $c2 ; current Block End
        $p1 ; prev Block Begin
        $p2 ; prev Block end
        )
    (if (re-search-forward "\n[ \t]*\n+" nil "move")
        (setq $c2 (match-beginning 0))
      (setq $c2 (point)))
    (goto-char $p0)
    (if (re-search-backward "\n[ \t]*\n+" nil "move")
        (progn
          (skip-chars-backward "\n \t")
          (setq $p2 (point))
          (skip-chars-forward "\n \t")
          (setq $c1 (point)))
      (error "No previous block."))
    (goto-char $p2)
    (if (re-search-backward "\n[ \t]*\n+" nil "move")
        (progn
          (setq $p1 (match-end 0)))
      (setq $p1 (point)))
    (transpose-regions $p1 $p2 $c1 $c2)
    (goto-char $p1)
    (set-transient-map
     (let (($kmap (make-sparse-keymap)))
       (define-key $kmap (kbd "<up>") #'xah-move-block-up)
       (define-key $kmap (kbd "<down>") #'xah-move-block-down)
       $kmap))))

(defun xah-move-block-down ()
  "Swap the current text block with the next.
After this command is called, press <up> or <down> to move. Any other key to exit.
Version 2022-03-04"
  (interactive)
  (let (($p0 (point))
        $c1 ; current block begin
        $c2 ; current Block End
        $n1 ; next Block Begin
        $n2 ; next Block end
        )
    (if (eq (point-min) (point))
        (setq $c1 (point))
      (if (re-search-backward "\n\n+" nil "move")
          (progn
            (setq $c1 (match-end 0)))
        (setq $c1 (point))))
    (goto-char $p0)
    (if (re-search-forward "\n[ \t]*\n+" nil "move")
        (progn
          (setq $c2 (match-beginning 0))
          (setq $n1 (match-end 0)))
      (error "No next block."))
    (if (re-search-forward "\n[ \t]*\n+" nil "move")
        (progn
          (setq $n2 (match-beginning 0)))
      (setq $n2 (point)))
    (transpose-regions $c1 $c2 $n1 $n2)
    (goto-char $n2))
  (set-transient-map
   (let (($kmap (make-sparse-keymap)))
     (define-key $kmap (kbd "<up>") #'xah-move-block-up)
     (define-key $kmap (kbd "<down>") #'xah-move-block-down)
     $kmap)))

(defun xah-delete-left-char-or-selection ()
  "Delete backward 1 character, or selection.
Version: 2022-01-22"
  (interactive)
  (if (region-active-p)
      (progn (delete-region (region-beginning) (region-end)))
    (delete-char -1)))

(defun xah-delete-backward-char-or-bracket-text ()
  "Delete 1 character or delete quote/bracket pair and inner text.
If the char to the left of cursor is a matching pair, delete it alone with inner text, push the deleted text to `kill-ring'.

What char is considered bracket or quote is determined by current syntax table.

If `universal-argument' is called first, do not delete inner text.

URL `http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version: 2017-07-02 2022-06-23"
  (interactive)
  (if (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end))
    (cond
     ((looking-back "\\s)" 1)
      (if (string-equal major-mode "xah-wolfram-mode")
          (let ($isComment ($p0 (point)))
            (backward-char)
            (setq $isComment (nth 4 (syntax-ppss)))
            (goto-char $p0)
            (if $isComment
                (if (forward-comment -1)
                    (kill-region (point) $p0)
                  (message "error GSNN2:parsing comment failed."))
              (if current-prefix-arg
                  (xah-delete-backward-bracket-pair)
                (xah-delete-backward-bracket-text))))
        (progn
          (if current-prefix-arg
              (xah-delete-backward-bracket-pair)
            (xah-delete-backward-bracket-text)))))
     ((looking-back "\\s(" 1)
      (message "left of cursor is opening bracket")
      (let ($pOpenBracketLeft
            ($pOpenBracketRight (point)) $isComment)
        (backward-char)
        (setq $pOpenBracketLeft (point))
        (goto-char $pOpenBracketRight)
        (forward-char)
        (setq $isComment (nth 4 (syntax-ppss)))
        (if $isComment
            (progn
              (message "cursor is in comment")
              (goto-char $pOpenBracketLeft)
              (if (forward-comment 1)
                  (kill-region (point) $pOpenBracketLeft)
                (message "error hSnRp: parsing comment failed.")))
          (progn
            (message "right 1 char of cursor is not in comment")
            (goto-char $pOpenBracketLeft)
            (forward-sexp)
            (if current-prefix-arg
                (xah-delete-backward-bracket-pair)
              (xah-delete-backward-bracket-text))))))
     ((looking-back "\\s\"" 1)
      (if (nth 3 (syntax-ppss))
          (progn
            (backward-char)
            (xah-delete-forward-bracket-pairs (not current-prefix-arg)))
        (if current-prefix-arg
            (xah-delete-backward-bracket-pair)
          (xah-delete-backward-bracket-text))))
     (t
      (delete-char -1)))))

(defun xah-delete-backward-bracket-text ()
  "Delete the matching brackets/quotes to the left of cursor, including the inner text.

This command assumes the left of cursor is a right bracket, and there is a matching one before it.

What char is considered bracket or quote is determined by current syntax table.

URL `http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version: 2017-09-21"
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
  (let (( $p0 (point)) $p1)
    (forward-sexp -1)
    (setq $p1 (point))
    (goto-char $p0)
    (delete-char -1)
    (goto-char $p1)
    (delete-char 1)
    (push-mark (point) t)
    (goto-char (- $p0 2))))

(defun xah-delete-forward-bracket-pairs ( &optional DeleteInnerTextQ)
  "Delete the matching brackets/quotes to the right of cursor.
If DeleteInnerTextQ is true, also delete the inner text.

After the command, mark is set at the left matching bracket position, so you can `exchange-point-and-mark' to select it.

This command assumes the char to the right of point is a left bracket or quote, and have a matching one after.

What char is considered bracket or quote is determined by current syntax table.

URL `http://xahlee.info/emacs/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version: 2017-07-02"
  (interactive)
  (if DeleteInnerTextQ
      (progn
        (mark-sexp)
        (kill-region (region-beginning) (region-end)))
    (let (($pt (point)))
      (forward-sexp)
      (delete-char -1)
      (push-mark (point) t)
      (goto-char $pt)
      (delete-char 1))))

(defun xah-change-bracket-pairs ( FromChars ToChars)
  "Change bracket pairs to another type or none.
For example, change all parenthesis () to square brackets [].
Works on current block or selection.

When called in lisp program, FromChars or ToChars is a string of bracket pair. eg \"(paren)\",  \"[bracket]\", etc.
The first and last characters are used. (the middle is for convenience in ido selection.)
If the string contains “,2”, then the first 2 chars and last 2 chars are used, for example  \"[[bracket,2]]\".
If ToChars is equal to string “none”, the brackets are deleted.

URL `http://xahlee.info/emacs/emacs/elisp_change_brackets.html'
Version: 2020-11-01 2021-08-15 2022-04-07 2022-07-05"
  (interactive
   (let (($brackets
          '("(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown GRAVE ACCENT`"
            "~tilde~"
            "=equal="
            "\"strait double quote\""
            "'single'"
            "[[double square,2]]"
            "“curly double quote”"
            "‘curly single quote’"
            "‹french angle›"
            "«french double angle»"
            "「corner」"
            "『white corner』"
            "【lenticular】"
            "〖white lenticular〗"
            "〈angle〉"
            "《double angle》"
            "〔tortoise〕"
            "〘white tortoise〙"
            "⦅white paren⦆"
            "〚white square〛"
            "⦃white curly⦄"
            "〈pointing angle〉"
            "⦑ANGLE WITH DOT⦒"
            "⧼CURVED ANGLE⧽"
            "⟦math square⟧"
            "⟨math angle⟩"
            "⟪math DOUBLE ANGLE⟫"
            "⟮math FLATTENED PARENTHESIS⟯"
            "⟬math WHITE TORTOISE SHELL⟭"
            "❛HEAVY SINGLE QUOTATION MARK ORNAMENT❜"
            "❝HEAVY DOUBLE TURNED COMMA QUOTATION MARK ORNAMENT❞"
            "❨MEDIUM LEFT PARENTHESIS ORNAMENT❩"
            "❪MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT❫"
            "❴MEDIUM LEFT CURLY ORNAMENT❵"
            "❬MEDIUM LEFT-POINTING ANGLE ORNAMENT❭"
            "❮HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT❯"
            "❰HEAVY LEFT-POINTING ANGLE ORNAMENT❱"
            "none"
            )))
     (list
      (completing-read "Replace this:" $brackets )
      (completing-read "To:" $brackets ))))
  (let ( $p1 $p2 )
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ( (case-fold-search nil) $fromLeft $fromRight $toLeft $toRight)
          (cond
           ((string-match ",2" FromChars  )
            (progn
              (setq $fromLeft (substring FromChars 0 2))
              (setq $fromRight (substring FromChars -2))))
           (t
            (progn
              (setq $fromLeft (substring FromChars 0 1))
              (setq $fromRight (substring FromChars -1)))))
          (cond
           ((string-match ",2" ToChars)
            (progn
              (setq $toLeft (substring ToChars 0 2))
              (setq $toRight (substring ToChars -2))))
           ((string-match "none" ToChars)
            (progn
              (setq $toLeft "")
              (setq $toRight "")))
           (t
            (progn
              (setq $toLeft (substring ToChars 0 1))
              (setq $toRight (substring ToChars -1)))))
          (cond
           ((string-match "markdown" FromChars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "`\\([^`]+?\\)`" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) t ))))
           ((string-match "tilde" FromChars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "~\\([^~]+?\\)~" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) t ))))
           ((string-match "ascii quote" FromChars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "\"\\([^\"]+?\\)\"" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) t ))))
           ((string-match "equal" FromChars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "=\\([^=]+?\\)=" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) t ))))
           (t (progn
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromLeft nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toLeft t t)))
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromRight nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toRight t t)))))))))))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://xahlee.info/emacs/emacs/modernization_upcase-word.html'
Version: 2020-06-26"
  (interactive)
  (let ( (deactivate-mark nil) $p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq $p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))

;; test case
;; test_case some
;; test-case
;; tes▮t-case

(defun xah-toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor.

URL `http://xahlee.info/emacs/emacs/modernization_upcase-word.html'
Version: 2015-12-22"
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
  (let ($p1 $p2)
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        ;; after period or question mark or exclamation
        (goto-char (point-min))
        (while (re-search-forward "\\(\\.\\|\\?\\|!\\)[ \n]+ *\\([a-z]\\)" nil 1)
          (upcase-region (match-beginning 2) (match-end 2))
          (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))
        ;; after a blank line, after a bullet, or beginning of buffer
        (goto-char (point-min))
        (while (re-search-forward "\\(\\`\\|• \\|\n\n\\)\\([a-z]\\)" nil 1)
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
              (re-search-forward "\\(<title>[ \n]?\\|<h[1-6]>[ \n]?\\|<p>[ \n]?\\|<li>[ \n]?\\|<dd>[ \n]?\\|<td>[ \n]?\\|<br ?/?>[ \n]?\\|<figcaption>[ \n]?\\)\\([a-z]\\)" nil 1)
            (upcase-region (match-beginning 2) (match-end 2))
            (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))))
      (goto-char (point-max)))
    (skip-chars-forward " \n\t")))

(defun xah-title-case-region-or-line (&optional Begin End)
  "Title case text between nearest brackets, or current line or selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, Begin End are region boundaries.

URL `http://xahlee.info/emacs/emacs/elisp_title_case_text.html'
Version: 2017-01-11 2021-03-30 2021-09-19"
  (interactive)
  (let* (($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕")
         ($p0 (point))
         ($p1 (if Begin
                  Begin
                (if (region-active-p)
                    (region-beginning)
                  (progn
                    (skip-chars-backward $skipChars (line-beginning-position)) (point)))))
         ($p2 (if End
                  End
                (if (region-active-p)
                    (region-end)
                  (progn (goto-char $p0)
                         (skip-chars-forward $skipChars (line-end-position)) (point)))))
         ($strPairs [
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
        (narrow-to-region $p1 $p2)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while
                 (search-forward (aref $x 0) nil t)
               (replace-match (aref $x 1) t t)))
           $strPairs))))))

(defun xah-add-space-after-comma ()
  "Add a space after comma of current block or selection.
and highlight changes made.
Version 2022-01-20"
  (interactive)
  (let ($p1 $p2)
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (while
          (re-search-forward ",\\b" nil t)
        (replace-match ", ")
        (overlay-put
         (make-overlay
          (match-beginning 0)
          (match-end 0)) 'face 'highlight)))))

(defun xah-delete-blank-lines ()
  "Delete all newline around cursor.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version: 2018-04-02"
  (interactive)
  (let ($p3 $p4)
          (skip-chars-backward "\n")
          (setq $p3 (point))
          (skip-chars-forward "\n")
          (setq $p4 (point))
          (delete-region $p3 $p4)))

(defun xah-fly-delete-spaces ()
  "Delete space, tab, IDEOGRAPHIC SPACE (U+3000) around cursor.
Version: 2019-06-13"
  (interactive)
  (let (p1 p2)
    (skip-chars-forward " \t　")
    (setq p2 (point))
    (skip-chars-backward " \t　")
    (setq p1 (point))
    (delete-region p1 p2)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor .

Shrink neighboring spaces, then newlines, then spaces again, leaving one space or newline at each step, till no more white space.

URL `http://xahlee.info/emacs/emacs/emacs_shrink_whitespace.html'
Version: 2014-10-21 2021-11-26 2021-11-30"
  (interactive)
  (let* (($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9))))
    (skip-chars-backward " \n\t　")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t　")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t)
      (setq $eol-count (1+ $eol-count)))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if (> (- $p2 $p1) 1)
          (progn
            (delete-horizontal-space) (insert " "))
        (progn (delete-horizontal-space))))
     ((eq $eol-count 1)
      (if $space-neighbor-p
          (xah-fly-delete-spaces)
        (progn (xah-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
          (xah-fly-delete-spaces)
        (progn
          (xah-delete-blank-lines)
          (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
          (xah-fly-delete-spaces)
        (progn
          (goto-char $p2)
          (search-backward "\n")
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here"))))))

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
  (let ( ($isLongline (if (eq last-command this-command) (get this-command 'longline-p) t))
         (deactivate-mark nil)
         $p1 $p2 )
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (if $isLongline
        (fill-region $p1 $p2)
      (let ((fill-column 99999 ))
        (fill-region $p1 $p2)))
    (put this-command 'longline-p (not $isLongline))))

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
      (while (re-search-forward "\n\n+" nil 1) (replace-match "\n")))))

(defun xah-reformat-whitespaces-to-one-space (Begin End)
  "Replace whitespaces by one space.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Version: 2017-01-11 2022-01-08"
  (interactive "r")
  (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (search-forward "\n" nil 1) (replace-match " "))
      (goto-char (point-min))
      (while (search-forward "\t" nil 1) (replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward " +" nil 1) (replace-match " "))
      (goto-char (point-max))))

(defun xah-reformat-to-multi-lines ( &optional Begin End MinLength)
  "Replace spaces by a newline at ~70 chars, on current block or selection.
If `universal-argument' is called first, ask user for max width.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Version: 2018-12-16 2021-07-06 2021-08-12"
  (interactive)
  (let ( $p1 $p2 $minlen )
    (setq $minlen (if MinLength MinLength (if current-prefix-arg (prefix-numeric-value current-prefix-arg) fill-column)))
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward " +" nil 1)
          (when (> (- (point) (line-beginning-position)) $minlen)
            (replace-match "\n" )))))))

(defun xah-reformat-lines (&optional Width)
  "Reformat current block or selection into short lines or 1 long line.
When called for the first time, change to short lines. Second call change it to one long line. Repeated call toggles.
If `universal-argument' is called first, ask user to type max length of line. By default, it is 66.

Note: this command is different from emacs `fill-region' or `fill-paragraph'.
This command never adds or delete non-whitespace chars. It only exchange whitespace sequence.

URL `http://xahlee.info/emacs/emacs/emacs_reformat_lines.html'
Created 2016 or before.
Version: 2021-07-05 2021-08-13 2022-03-12 2022-05-16"
  (interactive)
  ;; This symbol has a property 'is-long-p, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ($isLong $width $p1 $p2)
    (setq $width (if Width Width (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 66)))
    (setq $isLong (if (eq last-command this-command) (get this-command 'is-long-p) t))
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (if current-prefix-arg
        (xah-reformat-to-multi-lines $p1 $p2 $width)
      (if $isLong
          (xah-reformat-to-multi-lines $p1 $p2 $width)
        (progn
          (xah-reformat-whitespaces-to-one-space $p1 $p2))))
    (put this-command 'is-long-p (not $isLong))))

(defun xah-reformat-to-sentence-lines ()
  "Reformat current block or selection into multiple lines by ending period.
Move cursor to the beginning of next text block.
After this command is called, press `xah-repeat-key' to repeat it.

URL `http://xahlee.info/emacs/emacs/elisp_reformat_to_sentence_lines.html'
Version: 2020-12-02 2022-03-22 2022-12-11"
  (interactive)
  (let ($p1 $p2)
    (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
    (save-restriction
      (narrow-to-region $p1 $p2)
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
  (re-search-forward "\n+" nil 1)
  (set-transient-map (let (($kmap (make-sparse-keymap))) (define-key $kmap (or xah-repeat-key (kbd "DEL")) this-command) $kmap))
  (set-transient-map (let (($kmap (make-sparse-keymap))) (define-key $kmap (kbd "DEL") this-command) $kmap)))

(defun xah-space-to-newline ()
  "Replace space sequence to a newline char in current block or selection.

URL `http://xahlee.info/emacs/emacs/emacs_space_to_newline.html'
Version: 2017-08-19 2021-11-28"
  (interactive)
  (let* (($bds (xah-get-bounds-of-block-or-region))
         ($p1 (car $bds))
         ($p2 (cdr $bds)))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (while (re-search-forward " +" nil t)
        (replace-match "\n")))))

(defun xah-slash-to-backslash (&optional Begin End)
  "Replace slash by backslash on current line or region.
Version: 2021-07-14 2021-09-12"
  (interactive)
  (let ($p1 $p2)
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "/" nil t)
          (replace-match "\\\\"))))))

(defun xah-backslash-to-slash (&optional Begin End)
  "Replace backslash by slash on current line or region.
Version: 2021-09-11"
  (interactive)
  (let ($p1 $p2)
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\" nil t)
          (replace-match "/"))))))

(defun xah-double-backslash (&optional Begin End)
  "Replace backslash by two backslash on current line or region.
Version: 2021-11-09"
  (interactive)
  (let ($p1 $p2)
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\" nil t)
          (replace-match "\\\\\\\\"))))))

(defun xah-double-backslash-to-single (&optional Begin End)
  "Replace double backslash by single backslash on current line or region.
Version: 2021-11-09"
  (interactive)
  (let ($p1 $p2)
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\\\"  nil t)
          (replace-match "\\\\"))))))

(defun xah-slash-to-double-backslash (&optional Begin End)
  "Replace slash by double backslash on current line or region.
Version: 2021-07-14"
  (interactive)
  (let ($p1 $p2)
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "/" nil t)
          (replace-match "\\\\\\\\"))))))

(defun xah-double-backslash-to-slash (&optional Begin End)
  "Replace double backslash by slash on current line or region.
Version: 2021-07-14"
  (interactive)
  (let ($p1 $p2)
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (setq $p1 (line-beginning-position) $p2 (line-end-position))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (let ((case-fold-search nil))
        (goto-char (point-min))
        (while (search-forward "\\\\" nil t)
          (replace-match "/"))))))

(defun xah-comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line.

URL `http://xahlee.info/emacs/emacs/emacs_toggle_comment_by_line.html'
Version: 2016-10-25"
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            (forward-line )))))))

(defun xah-quote-lines (Begin End QuoteL QuoteR Sep)
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
Version: 2020-06-26 2021-09-15 2022-04-07 2022-04-13"
  (interactive
   (let* (($bds (xah-get-bounds-of-block-or-region))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($brackets
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
            )) $bktChoice $sep $sepChoice $quoteL $quoteR)
     (setq $bktChoice (completing-read "Quote to use:" $brackets))
     (setq $sepChoice (completing-read "line separator:" '("," ";" "none" "other")))
     (cond
      ((string-equal $bktChoice "none")
       (setq $quoteL "" $quoteR ""))
      ((string-equal $bktChoice "other")
       (let (($x (read-string "Enter 2 chars, for begin/end quote:")))
         (setq $quoteL (substring-no-properties $x 0 1)
               $quoteR (substring-no-properties $x 1 2))))
      (t (setq $quoteL (substring-no-properties $bktChoice 0 1)
               $quoteR (substring-no-properties $bktChoice -1))))
     (setq $sep
           (cond
            ((string-equal $sepChoice "none") "")
            ((string-equal $sepChoice "other") (read-string "Enter separator:"))
            (t $sepChoice)))
     (list $p1 $p2 $quoteL $quoteR $sep)))
  (let (($p1 Begin) ($p2 End) ($quoteL QuoteL) ($quoteR QuoteR) ($sep Sep))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (catch 'EndReached
          (while t
            (skip-chars-forward "\t ")
            (insert $quoteL)
            (end-of-line)
            (insert $quoteR $sep)
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

(defun xah-unescape-quotes (Begin End)
  "Replace  「\\\"」 by 「\"」 in current line or selection.
See also: `xah-escape-quotes'

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
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" t t)))))

(defun xah-dired-rename-space-to-underscore ()
  "In dired, rename current or marked files by replacing space to lowline _.
If not in `dired', do nothing.

URL `http://xahlee.info/emacs/emacs/elisp_dired_rename_space_to_underscore.html'
Version: 2016-10-04 2020-03-03 2022-05-16"
  (interactive)
  (require 'dired-aux)
  (if (eq major-mode 'dired-mode)
      (let ((markedFiles (dired-get-marked-files )))
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              markedFiles)
        ;; (dired-next-line 1)
        (revert-buffer)
        )
    (user-error "%s: Not in dired" real-this-command)))

(defun xah-dired-rename-space-to-hyphen ()
  "In dired, rename current or marked files by replacing space to hyphen -.
If not in `dired', do nothing.

URL `http://xahlee.info/emacs/emacs/elisp_dired_rename_space_to_underscore.html'
Version: 2016-10-04 2019-11-24 2022-05-16"
  (interactive)
  (require 'dired-aux)
  (if (eq major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "-" x) nil)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "%s: Not in dired" real-this-command)))

(defun xah-cycle-hyphen-lowline-space (&optional Begin End)
  "Cycle hyphen/lowline/space chars in selection or inside quote/bracket or line, in that order.
After this command is called, press `xah-repeat-key' to repeat it.
The region to work on is by this order:
 1. if there is a selection, use that.
 2. If cursor is string quote or any type of bracket, and is within current line, work on that region.
 3. else, work on current line.

URL `http://xahlee.info/emacs/emacs/elisp_change_space-hyphen_underscore.html'
Version: 2019-02-12 2021-08-20 2022-03-22 2022-10-20"
  (interactive)
  ;; this function sets a property 'state. Possible values are 0 to length of $charArray.
  (let* ($p1
         $p2
         ($charArray ["-" "_" " "])
         ($n (length $charArray))
         ($regionWasActive-p (region-active-p))
         ($nowState (if (eq last-command this-command) (get 'xah-cycle-hyphen-lowline-space 'state) 0))
         ($changeTo (elt $charArray $nowState)))
    (if (and Begin End)
        (setq $p1 Begin $p2 End)
      (if (region-active-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (let (($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
          (skip-chars-backward $skipChars (line-beginning-position))
          (setq $p1 (point))
          (skip-chars-forward $skipChars (line-end-position))
          (setq $p2 (point))
          (set-mark $p1))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (goto-char (point-min))
        (while (re-search-forward (elt $charArray (% (+ $nowState 2) $n)) (point-max) 1)
          (replace-match $changeTo t t))))
    (when (or (string-equal $changeTo " ") $regionWasActive-p)
      (goto-char $p2)
      (set-mark $p1)
      (setq deactivate-mark nil))
    (put 'xah-cycle-hyphen-lowline-space 'state (% (+ $nowState 1) $n)))
  (set-transient-map (let (($kmap (make-sparse-keymap))) (define-key $kmap (or xah-repeat-key (kbd "DEL")) this-command) $kmap)))

(defun xah-copy-file-path (&optional DirPathOnlyQ)
  "Copy current buffer file path or dired path.
Result is full path.
If `universal-argument' is called first, copy only the dir path.

If in dired, copy the current or marked files.

If a buffer is not file and not dired, copy value of `default-directory'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_file_path.html'
Version: 2018-06-18 2021-09-30"
  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat #'identity
                                         (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if DirPathOnlyQ
         (progn
           (message "Directory copied: %s" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: %s" $fpath)
         $fpath )))))

(defun xah-delete-current-text-block ()
  "Delete the current text block plus blank lines, or selection, and copy to `kill-ring'.

URL `http://xahlee.info/emacs/emacs/emacs_delete_block.html'
Version: 2017-07-09 2021-08-14 2022-07-31"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (progn
        (if (re-search-backward "\n[ \t]*\n+" nil 1)
            (setq $p1 (goto-char (match-end 0)))
          (setq $p1 (point)))
        (if (re-search-forward "\n\n" nil 1)
            (setq $p2 (match-end 0))
          (setq $p2 (point-max)))))
    (kill-region $p1 $p2)))

(defun xah-clear-register-1 ()
  "Clear register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version: 2015-12-08"
  (interactive)
  (progn
    (copy-to-register ?1 (point-min) (point-min))
    (message "Cleared register 1.")))

(defun xah-copy-to-register-1 ()
  "Copy current line or selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version: 2012-07-17 2022-10-03"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (copy-to-register ?1 $p1 $p2)
    (message "Copied to register 1: [%s]." (buffer-substring-no-properties $p1 $p2))))

(defun xah-append-to-register-1 ()
  "Append current line or selection to register 1.
When no selection, append current line, with newline char.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_append.html'
Version: 2015-12-08 2020-09-08"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (append-to-register ?1 $p1 $p2)
    (with-temp-buffer (insert "\n")
                      (append-to-register ?1 (point-min) (point-max)))
    (message "Appended to register 1: [%s]." (buffer-substring-no-properties $p1 $p2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'.

URL `http://xahlee.info/emacs/emacs/elisp_copy-paste_register_1.html'
Version: 2015-12-08"
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun xah-copy-rectangle-to-kill-ring (Begin End)
  "Copy region as column (rectangle region) to `kill-ring'
See also: `kill-rectangle', `copy-to-register'.

URL `http://xahlee.info/emacs/emacs/emacs_copy_rectangle_text_to_clipboard.html'
version 2016-07-17"
  ;; extract-rectangle suggested by YoungFrog, 2012-07-25
  (interactive "r")
  (require 'rect)
  (kill-new (mapconcat #'identity (extract-rectangle Begin End) "\n")))


;; insertion commands

(defun xah-insert-date ()
  "Insert current date time.
Insert date in this format: yyyy-mm-dd.
If `universal-argument' is called first, prompt for a format to use.
If there is selection, delete it first.

URL `http://xahlee.info/emacs/emacs/elisp_insert-date-time.html'
Version 2013-05-10 2021-11-07 2022-04-07"
  (interactive)
  (let (($style
         (if current-prefix-arg
             (string-to-number
              (substring
               (completing-read
                "Style:"
                '(
                  "1 → 20180412224611"
                  "2 → 2018-04-12_224611"
                  "3 → 2018-04-12T22:46:11-07:00"
                  "4 → 2018-04-12 22:46:11-07:00"
                  "5 → 2018-04-12 Thursday"
                  "6 → Thursday, April 12, 2018"
                  "7 → Thu, Apr 12, 2018"
                  "8 → April 12, 2018"
                  "9 → Apr 12, 2018"
                  )) 0 1))
           0
           )))
    (when (region-active-p) (delete-region (region-beginning) (region-end)))
    (insert
     (cond
      ((= $style 0)
       ;; "2016-10-10"
       (format-time-string "%Y-%m-%d"))
      ((= $style 1)
       ;; "1 → 20180412224611"
       (replace-regexp-in-string ":" "" (format-time-string "%Y%m%d%T")))
      ((= $style 2)
       ;; "2 → 2018-04-12_224611"
       (replace-regexp-in-string ":" "" (format-time-string "%Y-%m-%d_%T")))
      ((= $style 3)
       ;; "3 → 2018-04-12T22:46:11-07:00"
       (concat
        (format-time-string "%Y-%m-%dT%T")
        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z"))))
      ((= $style 4)
       ;; "4 → 2018-04-12 22:46:11-07:00"
       (concat
        (format-time-string "%Y-%m-%d %T")
        (funcall (lambda ($x) (format "%s:%s" (substring $x 0 3) (substring $x 3 5))) (format-time-string "%z"))))
      ((= $style 5)
       ;; "5 → 2018-04-12 Thursday"
       (format-time-string "%Y-%m-%d %A"))
      ((= $style 6)
       ;; "6 → Thursday, April 12, 2018"
       (format-time-string "%A, %B %d, %Y"))
      ((= $style 7)
       ;; "7 → Thu, Apr 12, 2018"
       (format-time-string "%a, %b %d, %Y"))
      ((= $style 8)
       ;; "8 → April 12, 2018"
       (format-time-string "%B %d, %Y"))
      ((= $style 9)
       ;; "9 → Apr 12, 2018"
       (format-time-string "%b %d, %Y"))
      (t
       (format-time-string "%Y-%m-%d"))))))

(defun xah-insert-bracket-pair (LBracket RBracket &optional WrapMethod)
  "Insert brackets around selection, word, at point, and maybe move cursor in between.

 LBracket and RBracket are strings. WrapMethod must be either `line' or `block'. `block' means between empty lines.

• If there is a an active region, add brackets around region.
• If WrapMethod is `line', wrap around line.
• If WrapMethod is `block', wrap around block.
• if cursor is at beginning of line and its not empty line and contain at least 1 space, wrap around the line.
• If cursor is at end of a word or buffer, one of the following will happen:
 xyz▮ → xyz(▮)
 xyz▮ → (xyz▮)       if in one of the lisp modes.
• wrap brackets around word if any. e.g. xy▮z → (xyz▮). Or just (▮)

URL `http://xahlee.info/emacs/emacs/elisp_insert_brackets_by_pair.html'
Version: 2017-01-17 2021-08-12"
  (if (region-active-p)
      (progn
        (let ( ($p1 (region-beginning)) ($p2 (region-end)))
          (goto-char $p2) (insert RBracket)
          (goto-char $p1) (insert LBracket)
          (goto-char (+ $p2 2))))
    (let ($p1 $p2)
      (cond
       ((eq WrapMethod 'line)
        (setq $p1 (line-beginning-position) $p2 (line-end-position))
        (goto-char $p2)
        (insert RBracket)
        (goto-char $p1)
        (insert LBracket)
        (goto-char (+ $p2 (length LBracket))))
       ((eq WrapMethod 'block)
        (save-excursion
          (let (($bds (xah-get-bounds-of-block-or-region))) (setq $p1 (car $bds) $p2 (cdr $bds)))
          (goto-char $p2)
          (insert RBracket)
          (goto-char $p1)
          (insert LBracket)
          (goto-char (+ $p2 (length LBracket)))))
       ( ;  do line. line must contain space
        (and
         (eq (point) (line-beginning-position))
         ;; (string-match " " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         (not (eq (line-beginning-position) (line-end-position))))
        (insert LBracket )
        (end-of-line)
        (insert  RBracket))
       ((and
         (or ; cursor is at end of word or buffer. i.e. xyz▮
          (looking-at "[^-_[:alnum:]]")
          (eq (point) (point-max)))
         (not (or
               (string-equal major-mode "xah-elisp-mode")
               (string-equal major-mode "emacs-lisp-mode")
               (string-equal major-mode "lisp-mode")
               (string-equal major-mode "lisp-interaction-mode")
               (string-equal major-mode "common-lisp-mode")
               (string-equal major-mode "clojure-mode")
               (string-equal major-mode "xah-clojure-mode")
               (string-equal major-mode "scheme-mode"))))
        (progn
          (setq $p1 (point) $p2 (point))
          (insert LBracket RBracket)
          (search-backward RBracket )))
       (t (progn
            ;; wrap around “word”. basically, want all alphanumeric, plus hyphen and underscore, but don't want space or punctuations. Also want chinese chars
            ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
            (skip-chars-backward "-_[:alnum:]")
            (setq $p1 (point))
            (skip-chars-forward "-_[:alnum:]")
            (setq $p2 (point))
            (goto-char $p2)
            (insert RBracket)
            (goto-char $p1)
            (insert LBracket)
            (goto-char (+ $p2 (length LBracket)))))))))

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

URL `http://xahlee.info/emacs/emacs/emacs_form_feed_section_paging.html'
Version: 2018-08-30"
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
        ($startChar (string-to-char (read-string "Start char: " "a")))
        ($howmany (string-to-number (read-string "How many: " "26")))
        ($colpos (- (point) (line-beginning-position))))
    (dotimes ($i $howmany )
      (progn
        (insert-char (+ $i $startChar))
        (forward-line)
        (beginning-of-line)
        (forward-char $colpos)))))

(defvar xah-unicode-list
  '(
    ;;
    ("smile beaming 😊")
    ("tears of joy 😂")
    ("hug 🤗")
    ("heart eyes 😍")
    ("heart face 🥰")

    ("angry 😠")
    ("vomit 🤮")

    ("thumb up 👍")
    ("thumb down 👎")

    ("checkmark ✅")
    ("new 🆕")
    ("star glowing 🌟")
    ("star ⭐")
    ("sparkles ✨")
    ("rocket 🚀")

    ("sun 🌞")
    ("red heart ❤")

    ("clown 🤡")

    ("large circle ⭕")
    ("cross ❌")

    ("red triangle 🔺")
    ("diamond 💠")
    ("square ⬛")

    ("bullet •")
    ("diamond ◆")
    ("...ellipsis …")
    ("nbsp  ")
    ("chinese comma 、")
    ("-emdash —")
    ("fullwidth ampersand ＆")
    ("left arrow ←")
    ("right arrow →")
    ("up arrow ↑")
    ("down arrow ↓")
    ;;
    )

  "A list of strings used by `xah-insert-unicode'.
Each item is a string.
The first part of string before last space, is used as name of a unicode char, the last part before last space, is the unicode Unicode character to insert. (can be more than 1 char).")

(defun xah-insert-unicode ()
  "Insert a unicode from a custom list `xah-unicode-list'.
URL `http://xahlee.info/emacs/emacs/emacs_insert_unicode.html'
Version: 2021-01-05 2022-04-07 2022-10-30"
  (interactive)
  (let (($str
         (completing-read
          "Insert:" xah-unicode-list)))
    (insert (car (last (split-string $str " " t))))))


;; text selection

(defun xah-select-block ()
  "Select the current/next block plus 1 blankline.
If region is active, extend selection downward by block.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2019-12-26 2021-04-04 2021-08-13"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil 1)
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil 1)
        (goto-char (match-end 0)))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil 1))))

(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
If `visual-line-mode' is on, consider line as visual line.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2017-11-01 2021-03-19"
  (interactive)
  (if (region-active-p)
      (if visual-line-mode
          (let (($p1 (point)))
                (end-of-visual-line 1)
                (when (eq $p1 (point))
                  (end-of-visual-line 2)))
        (progn
          (forward-line 1)
          (end-of-line)))
    (if visual-line-mode
        (progn (beginning-of-visual-line)
               (set-mark (point))
               (end-of-visual-line))
      (progn
        (end-of-line)
        (set-mark (line-beginning-position))))))

(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.

when there is no selection,
• if cursor is on a any type of bracket (including parenthesis, quotation mark), select whole bracketed thing including bracket
• else, select current word.

when there is a selection, the selection extension behavior is still experimental. But when cursor is on a any type of bracket (parenthesis, quote), it extends selection to outer bracket.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2020-02-04 2022-05-16"
  (interactive)
  (if (region-active-p)
      (progn
        (let (($rb (region-beginning)) ($re (region-end)))
          (goto-char $rb)
          (cond
           ((looking-at "\\s(")
            (if (eq (nth 0 (syntax-ppss)) 0)
                (progn
                  ;; (message "left bracket, depth 0.")
                  (end-of-line) ; select current line
                  (set-mark (line-beginning-position)))
              (progn
                ;; (message "left bracket, depth not 0")
                (up-list -1 t t)
                (mark-sexp))))
           ((eq $rb (line-beginning-position))
            (progn
              (goto-char $rb)
              (let (($firstLineEndPos (line-end-position)))
                (cond
                 ((eq $re $firstLineEndPos)
                  (progn
                    ;; (message "exactly 1 line. extend to next whole line." )
                    (forward-line 1)
                    (end-of-line)))
                 ((< $re $firstLineEndPos)
                  (progn
                    ;; (message "less than 1 line. complete the line." )
                    (end-of-line)))
                 ((> $re $firstLineEndPos)
                  (progn
                    ;; (message "beginning of line, but end is greater than 1st end of line" )
                    (goto-char $re)
                    (if (eq (point) (line-end-position))
                        (progn
                          ;; (message "exactly multiple lines" )
                          (forward-line 1)
                          (end-of-line))
                      (progn
                        ;; (message "multiple lines but end is not eol. make it so" )
                        (goto-char $re)
                        (end-of-line)))))
                 (t (error "%s: logic error 42946" real-this-command ))))))
           ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
            (progn
              ;; (message "less than 1 line" )
              (end-of-line) ; select current line
              (set-mark (line-beginning-position))))
           (t
            ;; (message "last resort" )
            nil))))
    (progn
      (cond
       ((looking-at "\\s(")
        ;; (message "left bracket")
        (mark-sexp)) ; left bracket
       ((looking-at "\\s)")
        ;; (message "right bracket")
        (backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
        ;; (message "string quote")
        (mark-sexp)) ; string quote
       ;; ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
       ;;  (message "beginning of line and not empty")
       ;;  (end-of-line)
       ;;  (set-mark (line-beginning-position)))
       ((or (looking-back "\\s_" 1) (looking-back "\\sw" 1))
        ;; (message "left is word or symbol")
        (skip-syntax-backward "_w" )
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (push-mark)
        (skip-syntax-forward "_w")
        (setq mark-active t)
        ;; (exchange-point-and-mark)
        )
       ((and (looking-at "\\s ") (looking-back "\\s " 1))
        ;; (message "left and right both space" )
        (skip-chars-backward "\\s " ) (set-mark (point))
        (skip-chars-forward "\\s "))
       ((and (looking-at "\n") (looking-back "\n" 1))
        ;; (message "left and right both newline")
        (skip-chars-forward "\n")
        (set-mark (point))
        (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next block
       (t
        ;; (message "just mark sexp" )
        (mark-sexp)
        (exchange-point-and-mark))
       ;;
       ))))

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \" ` and anything in `xah-brackets'.
This command ignores nesting. For example, if text is
    (a(b)c▮)
the selected char is “c”, not “a(b)c”.

URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
Version: 2020-11-24 2021-07-11 2021-12-21 2022-03-26"
  (interactive)
  (let (($skipChars (concat "^\"`" (mapconcat #'identity xah-brackets ""))))
    (skip-chars-backward $skipChars)
    (set-mark (point))
    (skip-chars-forward $skipChars)))


;; misc

(defun xah-user-buffer-p ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it is not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
Version: 2016-06-18 2022-05-19"
  (interactive)
  (cond
   ((string-equal "*" (substring (buffer-name) 0 1)) nil)
   ((string-equal major-mode "dired-mode") nil)
   ((string-equal major-mode "eww-mode") nil)
   ((string-equal major-mode "help-mode") nil)
   (t t)))

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-p'.

URL `http://xahlee.info/emacs/emacs/elisp_next_prev_user_buffer.html'
Version: 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
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
New buffer is named untitled, untitled<2>, etc.

On emacs quit, if you want emacs to prompt for save, set `buffer-offer-save' to t.

It returns the buffer.

URL `http://xahlee.info/emacs/emacs/emacs_new_empty_buffer.html'
Version: 2017-11-01 2022-04-05"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    $buf
    ))

(defvar xah-recently-closed-buffers nil "a Alist of recently closed buffers. Each element is (bufferName . filePath). The max number to track is controlled by the variable `xah-recently-closed-buffers-max'.")

(defcustom xah-recently-closed-buffers-max 40 "The maximum length for `xah-recently-closed-buffers'."
  :type 'integer)

(declare-function minibuffer-keyboard-quit "delsel" ())
(declare-function org-edit-src-save "org-src" ())

(defun xah-close-current-buffer ()
  "Close the current buffer.

Similar to `kill-buffer', with the following addition:

• Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• If the buffer is editing a source code file in an `org-mode' file, prompt the user to save before closing.
• If the buffer is a file, add the path to the list `xah-recently-closed-buffers'.

URL `http://xahlee.info/emacs/emacs/elisp_close_buffer_open_last_closed.html'
Version: 2016-06-19 2022-05-13 2022-10-18"
  (interactive)
  (let (($isOrgModeSourceFile (string-match "^*Org Src" (buffer-name))))
    (if (active-minibuffer-window) ; if the buffer is minibuffer
        ;; (string-equal major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit)
      (progn
        ;; Offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (Because `kill-buffer' does not offer to save buffers that are not associated with files.)
        (when (and (buffer-modified-p)
                   (xah-user-buffer-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        (when (and (buffer-modified-p)
                   $isOrgModeSourceFile)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))
        ;; save to a list of closed buffer
        (when (buffer-file-name)
          (setq xah-recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) xah-recently-closed-buffers))
          (when (> (length xah-recently-closed-buffers) xah-recently-closed-buffers-max)
            (setq xah-recently-closed-buffers (butlast xah-recently-closed-buffers 1))))
        (kill-buffer (current-buffer))))))

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
Version: 2016-06-19 2021-10-27 2022-04-07"
  (interactive)
  (find-file (completing-read "Open:" (mapcar (lambda (f) (cdr f)) xah-recently-closed-buffers))))

(defun xah-list-recently-closed ()
  "List recently closed file.

URL `http://xahlee.info/emacs/emacs/elisp_close_buffer_open_last_closed.html'
Version: 2016-06-19"
  (interactive)
  (let (($buf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer $buf)
    (mapc (lambda ($f) (insert (cdr $f) "\n"))
          xah-recently-closed-buffers)))

(defvar xah-open-file-at-cursor-pre-hook nil "Hook for `xah-open-file-at-cursor'. Functions in the hook will be called in order, each given the path as arg. The first return non-nil, its value is given to `xah-open-file-at-cursor' as input. This is useful for transforming certain url into file path (your website url), so instead of opening in browser, it opens in emacs as file.")

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is selection, use it for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number, or “:‹n›:‹m›” with line and column number. If so, jump to that line number.
If path does not have a file extension, automatically try with “.el” for elisp files.

See also `xah-open-file-at-cursor-pre-hook'.

This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://xahlee.info/emacs/emacs/emacs_open_file_path_fast.html'
Version: 2020-10-17 2021-10-16"
  (interactive)
  (let* (($input
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (let (($p0 (point)) $p1 $p2
                  ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
              (skip-chars-backward $pathStops)
              (setq $p1 (point))
              (goto-char $p0)
              (skip-chars-forward $pathStops)
              (setq $p2 (point))
              (goto-char $p0)
              (buffer-substring-no-properties $p1 $p2))))
         $input2 $path
         )
    (setq $input2
          (if (> (length xah-open-file-at-cursor-pre-hook) 0)
              (let (($x (run-hook-with-args-until-success 'xah-open-file-at-cursor-pre-hook $input)))
                (if $x $x $input))
            $input))
    (setq $path (replace-regexp-in-string "^/C:/" "/" (replace-regexp-in-string "^file://" "" (replace-regexp-in-string ":\\'" "" $input2))))
    (if (string-match-p "\\`https?://" $path)
        (browse-url $path)
      (progn ; not starting “http://”
        (if (string-match "#" $path)
            (let (($fpath (substring $path 0 (match-beginning 0)))
                  ($fractPart (substring $path (1+ (match-beginning 0)))))
              (if (file-exists-p $fpath)
                  (progn
                    (find-file $fpath)
                    (goto-char (point-min))
                    (search-forward $fractPart))
                (when (y-or-n-p (format "file does not exist: [%s]. Create?" $fpath))
                  (find-file $fpath))))
          (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" $path)
              (let (($fpath (match-string-no-properties 1 $path))
                    ($lineNum (string-to-number (match-string-no-properties 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char (point-min))
                      (forward-line (1- $lineNum)))
                  (when (y-or-n-p (format "file does not exist: [%s]. Create?" $fpath))
                    (find-file $fpath))))
            (if (file-exists-p $path)
                (progn ; open f.ts instead of f.js
                  (let (($ext (file-name-extension $path))
                        ($fnamecore (file-name-sans-extension $path)))
                    (if (and (string-equal $ext "js")
                             (file-exists-p (concat $fnamecore ".ts")))
                        (find-file (concat $fnamecore ".ts"))
                      (find-file $path))))
              (if (file-exists-p (concat $path ".el"))
                  (find-file (concat $path ".el"))
                (when (y-or-n-p (format "file does not exist: [%s]. Create?" $path))
                  (find-file $path))))))))))

(defalias 'xah-display-line-numbers-mode
  (if (fboundp 'global-display-line-numbers-mode)
      #'global-display-line-numbers-mode
    #'linum-mode))



(defvar xah-run-current-file-before-hook nil "Hook for `xah-run-current-file'. Before the file is run.")

(defvar xah-run-current-file-after-hook nil "Hook for `xah-run-current-file'. After the file is run.")

(defun xah-run-current-go-file ()
  "Run or build current golang file.
To build, call `universal-argument' first.
Version: 2018-10-12"
  (interactive)
  (when (not buffer-file-name) (save-buffer))
  (when (buffer-modified-p) (save-buffer))
  (let* (
         ($outputb "*xah-run output*")
         ;; (resize-mini-windows nil)
         ($fname buffer-file-name)
         ;; ($fSuffix (file-name-extension $fname))
         ($progName "go")
         ($cmdStr
          (concat $progName " \""   $fname "\" &")))
    (setq $cmdStr (format (if current-prefix-arg
                              "%s build \"%s\" "
                            "%s run \"%s\" &")
                          $progName $fname))
    (progn
      (message "running %s" $fname)
      (message "%s" $cmdStr)
      (shell-command $cmdStr $outputb )
      ;;
      )))

(defconst xah-run-current-file-map
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
  "A association list that maps file extension to program name, used by `xah-run-current-file'. Each item is (EXT . PROGRAM), both strings. EXT is file suffix (without the dot prefix), PROGRAM is program name or path, with possibly command options. You can customize this alist.")

(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call [python x.py] in a shell.
Output is printed to buffer “*xah-run output*”.
File suffix is used to determine which program to run, set in the variable `xah-run-current-file-map'.

If the file is modified or not saved, save it automatically before run.

URL `http://xahlee.info/emacs/emacs/elisp_run_current_file.html'
Version: 2020-09-24 2022-08-12 2022-09-16 2022-09-18"
  (interactive)
  (setenv "NO_COLOR" "1") ; 2022-09-10 for deno. default color has yellow parts, hard to see
  (when (not buffer-file-name) (save-buffer))
  (let* (($outBuffer "*xah-run output*")
         ;; (resize-mini-windows nil)
         ($extAppMap xah-run-current-file-map)
         ($fname buffer-file-name)
         ($fExt (file-name-extension $fname))
         ($appCmdStr (cdr (assoc $fExt $extAppMap)))
         $cmdStr
         )
    ;; FIXME: Rather than `shell-command' with an `&', better use
    ;; `make-process' or `start-process' since we're not using the shell at all
    ;; (worse, we need to use `shell-quote-argument' to circumvent the shell).
    (setq $cmdStr
          (when $appCmdStr
            (format "%s %s &"
                    $appCmdStr
                    (shell-quote-argument $fname))))
    (when (buffer-modified-p) (save-buffer))
    (run-hooks 'xah-run-current-file-before-hook)
    (cond
     ((string-equal $fExt "el")
      (load $fname))
     ((string-equal $fExt "go")
      (xah-run-current-go-file))
     ((string-match "\\.\\(ws?l\\|m\\|nb\\)\\'" $fExt)
      (if (fboundp 'xah-run-wolfram-script)
          (progn
            (xah-run-wolfram-script nil current-prefix-arg))
        (if $appCmdStr
            (progn
              (message "Running")
              (shell-command $cmdStr $outBuffer))
          (error "%s: Unknown file extension: %s" real-this-command $fExt))))
     ((string-equal $fExt "java")
      (progn
        ;; FIXME: Better use `call-process', or else at least use
        ;; `shell-quote-argument'.
        (shell-command (format "javac %s" $fname) $outBuffer)
        (shell-command (format "java %s" (file-name-sans-extension
                                          (file-name-nondirectory $fname)))
                       $outBuffer)))
     (t (if $appCmdStr
            (progn
              (message "Running 「%s」" $cmdStr)
              (shell-command $cmdStr $outBuffer))
          (error "%s: Unknown file extension: %s" real-this-command $fExt))))
    (run-hooks 'xah-run-current-file-after-hook))
  (setenv "NO_COLOR"))

(defun xah-clean-empty-lines ()
  "Replace repeated blank lines to just 1, in whole buffer or selection.
Respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/elisp_compact_empty_lines.html'
Version: 2017-09-22 2020-09-08"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil 1)
            (replace-match "\n\n")))))))

(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or selection, respects `narrow-to-region'.

URL `http://xahlee.info/emacs/emacs/elisp_compact_empty_lines.html'
Version: 2017-09-22 2021-08-27 2022-08-06"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+\n" nil 1) (replace-match "\n"))
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil 1) (replace-match "\n\n"))
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
  (let (($fname (buffer-file-name))
        ($dateTimeFormat "%Y-%m-%d_%H%M%S"))
    (if $fname
        (let (($backupName
               (concat $fname "~" (format-time-string $dateTimeFormat) "~")))
          (copy-file $fname $backupName t)
          (message (concat "Backup saved at: " $backupName)))
      (if (eq major-mode 'dired-mode)
          (progn
            (mapc (lambda ($x)
                    (let (($backupName
                           (concat $x "~" (format-time-string $dateTimeFormat) "~")))
                      (copy-file $x $backupName t)))
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
  (if (buffer-file-name)
      (progn
        (xah-make-backup)
        (when (buffer-modified-p)
          (save-buffer)))
    (progn
      (xah-make-backup))))

(defun xah-delete-current-file-make-backup ()
  "Delete current file, makes a backup~, close the buffer.
If buffer is not a file, copy content to `kill-ring', delete buffer.

Backup filename is “‹name›~‹dateTimeStamp›~”. Existing file of the same name is overwritten. If buffer is not a file, the backup file name starts with “xx_”.

Call `xah-open-last-closed' to open the backup file.

URL `http://xahlee.info/emacs/emacs/elisp_delete-current-file.html'
Version: 2018-05-15 2021-08-31 2021-09-27 2022-07-08"
  (interactive)
  (if (string-equal 'dired-mode major-mode)
      (message "In dired. Nothing is done.")
    (let* (($fname (buffer-file-name))
           ($backupPath
            (concat (if $fname $fname (format "%sxx" default-directory))
                    (format "~%s~" (format-time-string "%Y-%m-%d_%H%M%S")))))
      (if $fname
          (progn
            (save-buffer $fname)
            (copy-file $fname $backupPath t)
            (when (boundp 'xah-recently-closed-buffers)
              (push (cons nil $backupPath) xah-recently-closed-buffers))
            (message "Deleted. Backup at [%s]. Call `xah-open-last-closed' to open." $backupPath)
            (delete-file $fname))
        (progn
          (widen)
          (kill-new  (buffer-string))))
      (kill-buffer (current-buffer)))))



(defun xah-search-current-word ()
  "Call `isearch' on current word or selection.
“word” here is A to Z, a to z, and hyphen [-] and lowline [_], independent of syntax table.

URL `http://xahlee.info/emacs/emacs/modernization_isearch.html'
Version: 2015-04-09"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq $p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq $p2 (point))))
    (setq mark-active nil)
    (when (< $p1 (point))
      (goto-char $p1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties $p1 $p2))))

(declare-function w32-shell-execute "w32fns.c" (operation document &optional parameters show-flag)) ; (w32-shell-execute "open" default-directory)

(defun xah-show-in-desktop ()
  "Show current file in desktop.
 (Mac Finder, File Explorer, Linux file manager)
This command can be called when in a file buffer or in `dired'.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2020-11-20 2022-04-20 2022-08-19"
  (interactive)
  (let (($path (if (eq major-mode 'dired-mode)
                   (if (eq nil (dired-get-marked-files))
                       default-directory
                     (car (dired-get-marked-files)))
                 (if (buffer-file-name) (buffer-file-name) default-directory))))
    (cond
     ((string-equal system-type "windows-nt")
      (shell-command (format "PowerShell -Command invoke-item '%s'" (expand-file-name default-directory )))
      ;; (let (($cmd (format "Explorer /select,%s"
      ;;                     (replace-regexp-in-string "/" "\\" $path t t)
      ;;                     ;; (shell-quote-argument (replace-regexp-in-string "/" "\\" $path t t ))
      ;;                     )))
      ;;   (shell-command $cmd))
      )
     ((string-equal system-type "darwin")
      (shell-command
       (concat "open -R " (shell-quote-argument $path))))
     ((string-equal system-type "gnu/linux")
      (call-process shell-file-name nil nil nil
                    shell-command-switch
                    (format "%s %s"
                            "xdg-open"
                            (file-name-directory $path)))
      ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
      ))))

(defun xah-open-in-vscode ()
  "Open current file or dir in vscode.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2020-02-13 2021-01-18 2022-08-04"
  (interactive)
  (let (($path (if (buffer-file-name) (buffer-file-name) (expand-file-name default-directory))))
    (message "path is %s" $path)
    (cond
     ((string-equal system-type "darwin")
      (shell-command (format "open -a Visual\\ Studio\\ Code.app %s" (shell-quote-argument $path))))
     ((string-equal system-type "windows-nt")
      (shell-command (format "code.cmd %s" (shell-quote-argument $path))))
     ((string-equal system-type "gnu/linux")
      (shell-command (format "code %s" (shell-quote-argument $path)))))))

(defun xah-open-in-external-app (&optional Fname)
  "Open the current file or dired marked files in external app.
When called in emacs lisp, if Fname is given, open that.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2019-11-04 2021-07-21 2022-08-19"
  (interactive)
  (let ($fileList $doIt )
    (setq $fileList
          (if Fname
              (list Fname)
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
    (setq $doIt (if (<= (length $fileList) 5) t (y-or-n-p "Open more than 5 files? ")))
    (when $doIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (shell-command (concat "PowerShell -Command \"Invoke-Item -LiteralPath\" " "'" (shell-quote-argument (expand-file-name $fpath )) "'")))
         $fileList))
       ((string-equal system-type "darwin")
        (mapc (lambda ($fpath) (shell-command (concat "open " (shell-quote-argument $fpath)))) $fileList))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda ($fpath)
                (call-process shell-file-name nil nil nil
                              shell-command-switch
                              (format "%s %s"
                                      "xdg-open"
                                      (shell-quote-argument $fpath))))
              $fileList))
       ((string-equal system-type "berkeley-unix")
        (mapc (lambda ($fpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" $fpath))) $fileList))))))

(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
On Microsoft Windows, it starts cross-platform PowerShell pwsh. You
need to have it installed.

URL `http://xahlee.info/emacs/emacs/emacs_dired_open_file_in_ext_apps.html'
Version: 2020-11-21 2021-07-21 2022-08-04"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (let (($cmdstr
           (format "pwsh -Command Start-Process pwsh -WorkingDirectory %s" (shell-quote-argument default-directory))))
      (shell-command $cmdstr)))
   ((string-equal system-type "darwin")
    (shell-command (concat "open -a terminal " (shell-quote-argument (expand-file-name default-directory)))))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "x-terminal-emulator" (concat "--working-directory=" default-directory))))
   ((string-equal system-type "berkeley-unix")
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

(push
 '("azerty" . (("." . "e") ("," . "z") ("'" . "a") (";" . "w") ("/" . "^") ("[" . ")")
   ("]" . "=") ("=" . "$") ("-" . "ù") ("a" . "q") ("b" . "n") ("c" . "i")
   ("d" . "h") ("e" . "d") ("f" . "y") ("g" . "u") ("h" . "j") ("i" . "g")
   ("j" . "c") ("k" . "v") ("l" . "p") ("m" . ",") ("n" . "l") ("o" . "s")
   ("p" . "r") ("q" . "x") ("r" . "o") ("s" . "m") ("t" . "k") ("u" . "f")
   ("v" . ":") ("w" . ";") ("x" . "b") ("y" . "t") ("z" . "!") ("1" . "&")
   ("2" . "é") ("3" . "\"") ("4" . "'") ("5" . "(") ("6" . "-") ("7" . "è")
   ("8" . "_") ("9" . "ç") ("0" . "à") ("\\" . "*") ("`" . "²")))
 ;; NOTE: / is a dead key
 xah-fly-layouts)

(push
 '("azerty-be" . (("." . "e") ("," . "z") ("'" . "a") (";" . "w") ("/" . "^") ("[" . ")") ("]" . "-") ("=" . "$") ("-" . "ù") ("a" . "q") ("b" . "n") ("c" . "i") ("d" . "h") ("e" . "d") ("f" . "y") ("g" . "u") ("h" . "j") ("i" . "g") ("j" . "c") ("k" . "v") ("l" . "p") ("m" . ",") ("n" . "l") ("o" . "s") ("p" . "r") ("q" . "x") ("r" . "o") ("s" . "m") ("t" . "k") ("u" . "f") ("v" . ":") ("w" . ";") ("x" . "b") ("y" . "t") ("z" . "=") ("1" . "&") ("2" . "é") ("3" . "\"") ("4" . "'") ("5" . "(") ("6" . "§") ("7" . "è") ("8" . "!") ("9" . "ç") ("0" . "à") ("\\" . "µ") ("`" . "²")))
 ;; NOTE: / is a dead key
xah-fly-layouts)

(push
  ;; NOTE: f is a dead key
 '("beopy" . (("." . "o") ("," . "é") ("'" . "b") (";" . "à") ("/" . "k") ("[" . "=") ("]" . "%") ("=" . "z") ("-" . "m") ("b" . "'") ("c" . "d") ("d" . "c") ("f" . "^") ("g" . "v") ("h" . "t") ("i" . ",") ("j" . "x") ("k" . ".") ("l" . "j") ("m" . "g") ("n" . "r") ("o" . "u") ("q" . "è") ("r" . "l") ("s" . "n") ("t" . "s") ("u" . "i") ("v" . "h") ("w" . "q") ("x" . "w") ("z" . "f") ("1" . "\"") ("2" . "«") ("3" . "»") ("4" . "(") ("5" . ")") ("6" . "@") ("7" . "+") ("8" . "-") ("9" . "/") ("0" . "*") ("\\" . "ç") ("`" . "$")))
 xah-fly-layouts)

(push
 '("colemak" . (("'" . "q") ("," . "w") ("." . "f") ("y" . "g") ("f" . "j") ("g" . "l") ("c" . "u") ("r" . "y") ("l" . ";") ("o" . "r") ("e" . "s") ("u" . "t") ("i" . "d") ("d" . "h") ("h" . "n") ("t" . "e") ("n" . "i") ("s" . "o") (";" . "z") ("q" . "x") ("j" . "c") ("k" . "v") ("x" . "b") ("b" . "k") ("w" . ",") ("v" . ".") ("z" . "/")))
 xah-fly-layouts)

(push
 '("colemak-dhm" . (("'" . "q") ("," . "w") ("." . "f") (";" . "z") ("b" . "k") ("c" . "u") ("d" . "m") ("e" . "s") ("f" . "j") ("g" . "l") ("h" . "n") ("i" . "g") ("j" . "c") ("k" . "d") ("l" . ";") ("m" . "h") ("n" . "i") ("o" . "r") ("q" . "x") ("r" . "y") ("s" . "o") ("t" . "e") ("u" . "t") ("v" . ".") ("w" . ",") ("x" . "v") ("y" . "b") ("z" . "/")))
 xah-fly-layouts)

(push
 '("colemak-dhm-angle" . (("'" . "q") ("," . "w") ("." . "f") (";" . "x") ("b" . "k") ("c" . "u") ("d" . "m") ("e" . "s") ("f" . "j") ("g" . "l") ("h" . "n") ("i" . "g") ("j" . "d") ("k" . "v") ("l" . ";") ("m" . "h") ("n" . "i") ("o" . "r") ("q" . "c") ("r" . "y") ("s" . "o") ("t" . "e") ("u" . "t") ("v" . ".") ("w" . ",") ("x" . "\\") ("y" . "b") ("z" . "/")))
 xah-fly-layouts)

(push
 '("colemak-dhk" . (("'" . "q") ("," . "w") ("." . "f") (";" . "z") ("b" . "m") ("c" . "u") ("d" . "k") ("e" . "s") ("f" . "j") ("g" . "l") ("h" . "n") ("i" . "g") ("j" . "c") ("k" . "d") ("l" . ";") ("m" . "h") ("n" . "i") ("o" . "r") ("q" . "x") ("r" . "y") ("s" . "o") ("t" . "e") ("u" . "t") ("v" . ".") ("w" . ",") ("x" . "v") ("y" . "b") ("z" . "/")))
xah-fly-layouts)

(push
 '("dvorak" . nil)
 xah-fly-layouts)

(push
 '("programer-dvorak" . (
    ;; number row
 ("`" . "$") ("1" . "&") ("2" . "[") ("3" . "{") ("4" . "}") ("5" . "(") ("6" . "=") ("7" . "*") ("8" . ")") ("9" . "+") ("0" . "]") ("[" . "!") ("]" . "#")
    ;; number row, shifted
 ("!" . "%") ("@" . "7") ("#" . "5") ("$" . "3") ("%" . "1") ("^" . "9") ("&" . "0") ("*" . "2") ("(" . "4") (")" . "6") ("{" . "8") ("}" . "`")
    ;; left pinky outwards
 ("'" . ";") ("\"" . ":")
    ;; left pinky inwards
 (";" . "'") (":" . "\"")
    ;; right pinky outwards-sideways
 ("=" . "@") ("+" . "^") ))
 xah-fly-layouts)

(push
 '("qwerty" . (("." . "e") ("," . "w") ("'" . "q") (";" . "z") ("/" . "[") ("[" . "-") ("]" . "=") ("=" . "]") ("-" . "'") ("a" . "a") ("b" . "n") ("c" . "i") ("d" . "h") ("e" . "d") ("f" . "y") ("g" . "u") ("h" . "j") ("i" . "g") ("j" . "c") ("k" . "v") ("l" . "p") ("n" . "l") ("o" . "s") ("p" . "r") ("q" . "x") ("r" . "o") ("s" . ";") ("t" . "k") ("u" . "f") ("v" . ".") ("w" . ",") ("x" . "b") ("y" . "t") ("z" . "/")))
 xah-fly-layouts)

(push
;; QWERTY Norwegian
;; NOTE: ] is a dead key
;; NOTE: = is a dead key
 '("qwerty-no" . (("." . "e") ("," . "w") ("'" . "q") (";" . "z") ("/" . "å") ("[" . "+") ("]" . "´") ("=" . "¨") ("-" . "æ") ("b" . "n") ("c" . "i") ("d" . "h") ("e" . "d") ("f" . "y") ("g" . "u") ("h" . "j") ("i" . "g") ("j" . "c") ("k" . "v") ("l" . "p") ("n" . "l") ("o" . "s") ("p" . "r") ("q" . "x") ("r" . "o") ("s" . "ø") ("t" . "k") ("u" . "f") ("v" . ".") ("w" . ",") ("x" . "b") ("y" . "t") ("z" . "-")))
 xah-fly-layouts)

(push
 '("qwerty-abnt" . (("." . "e") ("," . "w") ("'" . "q") (";" . "z") ("/" . "'") ("[" . "-") ("]" . "=") ("=" . "[") ("-" . "~") ("b" . "n") ("c" . "i") ("d" . "h") ("e" . "d") ("f" . "y") ("g" . "u") ("h" . "j") ("i" . "g") ("j" . "c") ("k" . "v") ("l" . "p") ("n" . "l") ("o" . "s") ("p" . "r") ("q" . "x") ("r" . "o") ("s" . "ç") ("t" . "k") ("u" . "f") ("v" . ".") ("w" . ",") ("x" . "b") ("y" . "t") ("z" . ";")))
 xah-fly-layouts)

(push
 '("qwertz" . (("." . "e") ("," . "w") ("'" . "q") (";" . "y") ("/" . "ü") ("[" . "ß") ("]" . "´") ("=" . "+") ("-" . "ä") ("b" . "n") ("c" . "i") ("d" . "h") ("e" . "d") ("f" . "z") ("g" . "u") ("h" . "j") ("i" . "g") ("j" . "c") ("k" . "v") ("l" . "p") ("n" . "l") ("o" . "s") ("p" . "r") ("q" . "x") ("r" . "o") ("s" . "ö") ("t" . "k") ("u" . "f") ("v" . ".") ("w" . ",") ("x" . "b") ("y" . "t") ("z" . "-")))
 xah-fly-layouts)

(push
 '("workman" . (("'" . "q") ("," . "d") ("." . "r") ("p" . "w") ("y" . "b") ("f" . "j") ("g" . "f") ("c" . "u") ("r" . "p") ("l" . ";") ("o" . "s") ("e" . "h") ("u" . "t") ("i" . "g") ("d" . "y") ("h" . "n") ("t" . "e") ("n" . "o") ("s" . "i") (";" . "z") ("q" . "x") ("j" . "m") ("k" . "c") ("x" . "v") ("b" . "k") ("m" . "l") ("w" . ",") ("v" . ".") ("z" . "/")))
 xah-fly-layouts)

(push
 '("norman" . (("'" . "q") ("," . "w") ("." . "d") ("p" . "f") ("y" . "k") ("f" . "j") ("g" . "u") ("c" . "r") ("r" . "l") ("l" . ";") ("o" . "s") ("u" . "t") ("i" . "g") ("d" . "y") ("h" . "n") ("t" . "i") ("n" . "o") ("s" . "h") (";" . "z") ("q" . "x") ("j" . "c") ("k" . "v") ("x" . "b") ("b" . "p") ("w" . ",") ("v" . ".") ("z" . "/")))
 xah-fly-layouts)

(push
 '("neo2" . (("'" . "x") ("," . "v") ("." . "l") ("p" . "c") ("y" . "w")
 ("f" . "k") ("g" . "h") ("c" . "g") ("r" . "f") ("l" . "q")
 ("a" . "u") ("o" . "i") ("e" . "a") ("u" . "e") ("i" . "o")
 ("d" . "s") ("h" . "n") ("t" . "r") ("n" . "t") ("s" . "d")
 (";" . "ü") ("q" . "ö") ("j" . "ä") ("k" . "p") ("x" . "z")
 ("w" . ",") ("v" . ".") ("z" . "j")
 ("/" . "ß") ("[" . "-") ("-" . "y")))
 xah-fly-layouts)

(push
 '("koy" . (("'" . "k") ("," . ".") ("." . "o") ("p" . ",")
    ("f" . "v") ("r" . "l") ("l" . "ß")
    ("a" . "h") ("o" . "a") ("u" . "i") ("i" . "u")
    ("h" . "t") ("t" . "r")
    (";" . "x") ("j" . "ä") ("k" . "ü") ("x" . "ö")
 ("m" . "p") ("v" . "m") ("z" . "j")))
 xah-fly-layouts)

(push
 '("adnw" . (("'" . "k") ("," . "u") ("." . "ü") ("p" . ".") ("y" . "ä")
 ("f" . "v") ("r" . "l") ("l" . "j") ("/" . "f")
 ("a" . "h") ("o" . "i") ("u" . "a") ("i" . "o")
 ("h" . "t") ("t" . "r") ("-" . "ß")
 (";" . "x") ("q" . "y") ("j" . "ö") ("k" . ",") ("x" . "q")
    ("m" . "p") ("v" . "m")))
 xah-fly-layouts)

(push
 '("pt-nativo" . ((";" . "«") ("/" . "~") ("[" . "º") ("]" . "<") ("=" . "-") ("-" . "´") ("a" . "i") ("b" . "q") ("c" . "t") ("d" . "m") ("e" . "a") ("f" . "w") ("g" . "l") ("h" . "d") ("i" . "u") ("k" . "b") ("l" . "p") ("m" . "v") ("n" . "r") ("o" . "e") ("p" . "h") ("q" . "ç") ("r" . "c") ("s" . "n") ("t" . "s") ("u" . "o") ("v" . "f") ("w" . "g") ("x" . "k") ("y" . "x")))
 xah-fly-layouts)

(push
 '("carpalx-qgmlwy" . (("." . "m") ("," . "g") ("'" . "q") (";" . "z") ("/" . "[") ("[" . "-") ("]" . "=") ("=" . "]") ("-" . "'") ("a" . "d") ("b" . "k") ("c" . "u") ("d" . "i") ("e" . "t") ("f" . "y") ("g" . "f") ("h" . "a") ("i" . "r") ("j" . "c") ("k" . "v") ("l" . ";") ("m" . "p") ("n" . "o") ("o" . "s") ("p" . "l") ("q" . "x") ("r" . "b") ("s" . "h") ("t" . "e") ("u" . "n") ("v" . ".") ("w" . ",") ("x" . "j") ("y" . "w") ("z" . "/")))
 xah-fly-layouts)

(push
 '("carpalx-qgmlwb" . (("." . "m") ("," . "g") ("'" . "q") (";" . "z") ("/" . "[") ("[" . "-") ("]" . "=") ("=" . "]") ("-" . "'") ("a" . "d") ("b" . "k") ("c" . "u") ("d" . "i") ("e" . "t") ("f" . "b") ("g" . "y") ("h" . "a") ("i" . "r") ("j" . "c") ("k" . "f") ("l" . ";") ("m" . "p") ("n" . "o") ("o" . "s") ("p" . "l") ("q" . "x") ("r" . "v") ("s" . "h") ("t" . "e") ("u" . "n") ("v" . ".") ("w" . ",") ("x" . "j") ("y" . "w") ("z" . "/")))
xah-fly-layouts)

(push
 '("carpalx-qfmlwy" . (("." . "m") ("," . "f") ("'" . "q") (";" . "z") ("/" . "[") ("[" . "-") ("]" . "=") ("=" . "]") ("-" . "'") ("a" . "d") ("b" . "p") ("c" . "o") ("d" . "i") ("e" . "t") ("f" . "y") ("g" . "u") ("h" . "a") ("i" . "r") ("j" . "g") ("k" . "c") ("l" . "j") ("m" . "k") ("n" . "h") ("o" . "s") ("p" . "l") ("q" . "v") ("r" . "b") ("s" . ";") ("t" . "e") ("u" . "n") ("v" . ".") ("w" . ",") ("y" . "w") ("z" . "/")))
 xah-fly-layouts)

(push
 '("bepo" . (("'" . "b") ("," . "é") ("." . "p") ("p" . "o") ("y" . "è")
   ("f" . "^") ("g" . "v") ("c" . "d") ("r" . "l") ("l" . "j")
   ("o" . "u") ("e" . "i") ("u" . "e") ("i" . ",")
   ("d" . "c") ("h" . "t") ("t" . "s") ("n" . "r") ("s" . "n")
   (":" . "à") ("q" . "y") ("j" . "x") ("k" . ".") ("x" . "k")
   ("b" . "’") ("m" . "q") ("w" . "g") ("v" . "h") ("z" . "f")
   ("3" . "»") ("4" . "(") ("5" . ")") ("6" . "@") ("7" . "+") ("8" . "-") ("9" . "/")))
 ;; NOTE f is a dead key
 xah-fly-layouts)

(push
 ;; NOTE: - is a dead key
 '("optimot" . (("-" . "^") ("'" . "à") ("," . "j") ("." . "o") (";" . "k") ("/" . "x") ("[" . "#") ("]" . "@") ("=" . "ç") ("a" . "a") ("b" . "g") ("c" . "l") ("d" . "p") ("e" . "e") ("f" . "f") ("g" . "d") ("h" . "t") ("i" . ",") ("j" . "è") ("k" . ".") ("l" . "q") ("m" . "c") ("n" . "r") ("o" . "i") ("p" . "é") ("q" . "y") ("r" . "'") ("s" . "n") ("t" . "s") ("u" . "u") ("v" . "h") ("w" . "m") ("x" . "w") ("y" . "b") ("z" . "v"))) 
 xah-fly-layouts)

(defvar xah-fly-key-current-layout nil
  "The current keyboard layout. Value is a key in `xah-fly-layouts'.
Do not set this variable manually. Use `xah-fly-keys-set-layout' to set it.
If the value is nil, it is automatically set to \"qwerty\".
When this variable changes, suitable change must also be done to `xah-fly--key-convert-table'.
Version 2022-10-22")

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
      (let (($result (assoc x xah-fly--key-convert-table)))
        (if $result (cdr $result) x)))
    (split-string Charstr " +"))
   " "))

(defmacro xah-fly--define-keys (KeymapName KeyCmdAlist &optional Direct-p)
  "Map `define-key' over a alist KeyCmdAlist, with key layout remap.
The key is remapped from Dvorak to the current keyboard layout by `xah-fly--convert-kbd-str'.
If Direct-p is t, do not remap key to current keyboard layout.
Example usage:
;; (xah-fly--define-keys
;;  (define-prefix-command \\='xyz-map)
;;  \\='(
;;    (\"h\" . highlight-symbol-at-point)
;;    (\".\" . isearch-forward-symbol-at-point)
;;    (\"w\" . isearch-forward-word)))
Version: 2020-04-18 2022-10-25"
  (let (($keymapName (make-symbol "keymap-name")))
    `(let ((,$keymapName , KeymapName))
       ,@(mapcar
          (lambda ($pair)
            `(define-key
               ,$keymapName
               (kbd (,(if Direct-p #'identity #'xah-fly--convert-kbd-str) ,(car $pair)))
               ,(list 'quote (cdr $pair))))
          (cadr KeyCmdAlist)))))


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
Version 2022-10-31"
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

       ;; This keymap I've not used. things are here experimentally.
       ;; The TAB key is not in a very good ergonomic position on average keyboards, so 【leader tab ‹somekey›】 probably should not be used much.
       ;; Currently (2018-03-13), these are commands related to completion or indent, and I basically never use any of these (except sometimes complete-symbol).
       ;; For average user, the way it is now is probably justified, because most emacs users don't use these commands.
       ("TAB" . nil)
       ("TAB TAB" . indent-for-tab-command)
       ("TAB i" . complete-symbol)
       ("TAB g" . indent-rigidly)
       ("TAB r" . indent-region)
       ("TAB s" . indent-sexp)
       ("TAB 1" . abbrev-prefix-mark)
       ("TAB 2" . edit-abbrevs)
       ("TAB 3" . expand-abbrev)
       ("TAB 4" . expand-region-abbrevs)
       ("TAB 5" . unexpand-abbrev)
       ("TAB 6" . add-global-abbrev)
       ("TAB 7" . add-mode-abbrev)
       ("TAB 8" . inverse-add-global-abbrev)
       ("TAB 9" . inverse-add-mode-abbrev)
       ("TAB 0" . expand-jump-to-next-slot)
       ("TAB =" . expand-jump-to-previous-slot)

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
       ("c y" . xah-list-recently-closed)

       ("d" . beginning-of-buffer)

       ("e a" . xah-insert-double-angle-bracket)
       ("e b" . xah-insert-black-lenticular-bracket)
       ("e c" . xah-insert-ascii-single-quote)
       ("e d" . xah-insert-double-curly-quote)
       ("e e" . xah-insert-unicode)
       ("e f" . xah-insert-emacs-quote)
       ("e g" . xah-insert-ascii-double-quote)
       ("e h" . xah-insert-brace)
       ("e i" . xah-insert-curly-single-quote)
       ("e j" . insert-char)
       ("e k" . xah-insert-markdown-quote)
       ("e l" . xah-insert-formfeed)
       ("e m" . xah-insert-corner-bracket)
       ("e n" . xah-insert-square-bracket)
       ("e p" . xah-insert-single-angle-quote)
       ("e r" . xah-insert-tortoise-shell-bracket)
       ("e s" . xah-insert-string-assignment)
       ("e t" . xah-insert-paren)
       ("e u" . xah-insert-date)
       ("e v" . xah-insert-markdown-triple-quote)
       ("e w" . xah-insert-angle-bracket)
       ("e y" . xah-insert-double-angle-quote)

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

       ;; dvorak n. commands here are “harmless”, they don't modify text etc. they turn on modes, change display, prompt, start shell, etc.
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
       ("r c" . replace-rectangle)
       ("r d" . delete-rectangle)
       ("r e" . call-last-kbd-macro)
       ("r g" . kill-rectangle)
       ("r h" . xah-change-bracket-pairs)
       ("r i" . xah-space-to-newline)
       ("r j" . xah-slash-to-backslash)
       ("r k" . xah-slash-to-double-backslash)
       ("r l" . clear-rectangle)
       ("r n" . rectangle-number-lines)
       ("r o" . open-rectangle)
       ("r p" . kmacro-end-macro)
       ("r r" . yank-rectangle)
       ("r u" . xah-quote-lines)
       ("r x" . xah-double-backslash-to-slash)
       ("r y" . delete-whitespace-rectangle)

       ("s" . save-buffer)

       ;; most frequently used
       ("t <up>"  . xah-move-block-up)
       ("t <down>"  . xah-move-block-down)
       ("t '" . reverse-region)
       ("t ," . sort-numeric-fields)
       ("t ." . xah-sort-lines)
       ("t 1" . xah-append-to-register-1)
       ("t 2" . xah-clear-register-1)
       ("t 3" . xah-copy-to-register-1)
       ("t 4" . xah-paste-from-register-1)
       ("t 7" . xah-append-to-register-1)
       ("t 8" . xah-clear-register-1)
       ("t c" . goto-char)
       ("t d" . mark-defun)
       ("t e" . list-matching-lines)
       ("t f" . goto-line)
       ("t g" . move-to-column)
       ("t h" . repeat-complex-command)
       ("t i" . delete-non-matching-lines)
       ("t j" . copy-to-register)
       ("t k" . insert-register)
       ("t l" . xah-escape-quotes)
       ("t m" . xah-make-backup-and-save)
       ("t o" . xah-clean-whitespace)
       ("t p" . query-replace-regexp)
       ("t r" . copy-rectangle-to-register)
       ("t t" . repeat)
       ("t u" . delete-matching-lines)
       ("t w" . xah-next-window-or-frame)
       ("t x" . xah-reformat-to-sentence-lines)
       ("t y" . delete-duplicate-lines)

       ("u" . switch-to-buffer)

       ;; dangerous map. run program, delete file, etc
       ("w d" . xah-delete-current-file-make-backup)
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

       ;; (xah-fly--define-keys
       ;;  (define-prefix-command 'xah-coding-system-keymap)
       ;;  '(
       ;;    ("n" . set-file-name-coding-system)
       ;;    ("s" . set-next-selection-coding-system)
       ;;    ("c" . universal-coding-system-argument)
       ;;    ("f" . set-buffer-file-coding-system)
       ;;    ("k" . set-keyboard-coding-system)
       ;;    ("l" . set-language-environment)
       ;;    ("p" . set-buffer-process-coding-system)
       ;;    ("r" . revert-buffer-with-coding-system)
       ;;    ("t" . set-terminal-coding-system)
       ;;    ("x" . set-selection-coding-system)))

       ;;
       ))

    (xah-fly--define-keys
     xah-fly-command-map
     '(("SPC" . xah-fly-leader-key-map)
       ("'" . xah-reformat-lines)
       ("," . xah-shrink-whitespaces)
       ("-" . xah-cycle-hyphen-lowline-space)
       ("." . backward-kill-word)
       (";" . xah-comment-dwim)
       ("/" . hippie-expand)

       ("[" . xah-backward-punct)
       ("]" . xah-forward-punct)
       ("`" . other-frame)

       ;; ("$" . xah-forward-punct)

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
       ;; ("e" . xah-delete-left-char-or-selection)
       ("e" . xah-delete-backward-char-or-bracket-text)
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
  (global-set-key (kbd "M-SPC") #'xah-fly-command-mode-activate)
  (global-set-key (kbd "M-\\") 'nil) ; delete-horizontal-space
  (global-set-key (kbd "M-!") 'nil)  ; shell-command
  (global-set-key (kbd "M-$") 'nil)  ; ispell-word
  (global-set-key (kbd "M-%") 'nil)  ; query-replace
  (global-set-key (kbd "M-&") 'nil)  ; async-shell-command
  (global-set-key (kbd "M-'") 'nil)  ; abbrev-prefix-mark
  (global-set-key (kbd "M-(") 'nil)  ; insert-parentheses
  (global-set-key (kbd "M-)") 'nil)  ; move-past-close-and-reindent
  ;; (global-set-key (kbd "M-,") 'nil) ; xref-pop-marker-stack
  ;; (global-set-key (kbd "M-.") 'nil) ; xref-find-definitions
  (global-set-key (kbd "M-/") 'nil) ; dabbrev-expand
  (global-set-key (kbd "M-:") 'nil) ; eval-expression
  ;; (global-set-key (kbd "M-;") 'nil) ; comment-dwim
  (global-set-key (kbd "M-<") 'nil) ; beginning-of-buffer
  (global-set-key (kbd "M-=") 'nil) ; count-words-region
  (global-set-key (kbd "M->") 'nil) ; end-of-buffer
  ;; (global-set-key (kbd "M-?") 'nil) ; xref-find-references
  (global-set-key (kbd "M-@") 'nil) ; mark-word
  (global-set-key (kbd "M-^") 'nil) ; delete-indentation
  (global-set-key (kbd "M-`") 'nil) ; tmm-menubar
  (global-set-key (kbd "M-a") 'nil) ; backward-sentence
  (global-set-key (kbd "M-b") 'nil) ; backward-word
  (global-set-key (kbd "M-c") 'nil) ; capitalize-word
  (global-set-key (kbd "M-d") 'nil) ;  kill-word
  (global-set-key (kbd "M-e") 'nil) ; forward-sentence
  (global-set-key (kbd "M-f") 'nil) ; forward-word
  (global-set-key (kbd "M-g") 'nil) ; Prefix Command
  (global-set-key (kbd "M-h") 'nil) ; mark-paragraph
  (global-set-key (kbd "M-i") 'nil) ; tab-to-tab-stop
  (global-set-key (kbd "M-j") 'nil) ; default-indent-new-line
  (global-set-key (kbd "M-k") 'nil) ; kill-sentence
  (global-set-key (kbd "M-l") 'nil) ; downcase-word
  (global-set-key (kbd "M-m") 'nil) ; back-to-indentation
  (global-set-key (kbd "M-o") 'nil) ; facemenu-keymap
  (global-set-key (kbd "M-q") 'nil) ; fill-paragraph
  (global-set-key (kbd "M-r") 'nil) ; move-to-window-line-top-bottom
  (global-set-key (kbd "M-s") 'nil) ; Prefix Command
  (global-set-key (kbd "M-t") 'nil) ; transpose-words
  (global-set-key (kbd "M-u") 'nil) ; upcase-word
  (global-set-key (kbd "M-v") 'nil) ; scroll-down-command
  (global-set-key (kbd "M-w") 'nil) ; kill-ring-save
  ;; (global-set-key (kbd "M-x") 'nil) ; execute-extended-command
  ;; (global-set-key (kbd "M-y") 'nil) ; yank-pop
  (global-set-key (kbd "M-z") 'nil)   ; zap-to-char
  (global-set-key (kbd "M-{") 'nil)   ; backward-paragraph
  (global-set-key (kbd "M-|") 'nil)   ; shell-command-on-region
  (global-set-key (kbd "M-}") 'nil)   ; forward-paragraph
  (global-set-key (kbd "M-~") 'nil)   ; not-modified
  (global-set-key (kbd "M-DEL") 'nil) ; backward-kill-word
  )

(when xah-fly-use-control-key
  ;; 2021-08-07 was
  ;; (xah-fly--define-keys
  ;;  xah-fly-shared-map
  ;;  '(
  ;;    ("C-1" . cmd)
  ;;    ("C-2" . cmd)
  ;;    ;; etc
  ;;    )
  ;;  :direct)

 ;; this is a problem.

;; because the control keybinding and meta keybinding are not supposed to change by keyboard layout such as dvorak.
;; They should be letter direct.
;; Also, by setting them with xah-fly-shared-map, it becomes unchangeable, that is, if a major mode set a key for C-t, it will have no effect.
;; Current solution is just to use global-set-key.
;; The disadvantage is that these changes leak, that is, xah-fly-keys is turned off, these ctrl keys are still changed.
;; Still, this is better, because xah fly keys is not really meant to be turned off.
;; You learn it, like it, use it, or leave it.
;; Removing the tons of default emacs control and meta keys is desirable.
;; because there are hundres of them, confusing, and mostly useless.

  (global-set-key (kbd "<C-S-prior>") #'xah-previous-emacs-buffer)
  (global-set-key (kbd "<C-S-next>") #'xah-next-emacs-buffer)

  (global-set-key (kbd "<C-tab>") #'xah-next-user-buffer)
  (global-set-key (kbd "<C-S-tab>") #'xah-previous-user-buffer)
  (global-set-key (kbd "<C-S-iso-lefttab>") #'xah-previous-user-buffer)

  (global-set-key (kbd "<C-prior>") #'xah-previous-user-buffer)
  (global-set-key (kbd "<C-next>") #'xah-next-user-buffer)

  (global-set-key (kbd "<f7>") 'xah-fly-leader-key-map)

  ;; (global-set-key (kbd "C-1") 'nil)
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

  ;; (global-set-key (kbd "C-@") 'nil)

  (global-set-key (kbd "C-a") #'mark-whole-buffer)
  ;; (global-set-key (kbd "C-b") 'nil)
  ;; (global-set-key (kbd "C-c") 'nil)
  ;; (global-set-key (kbd "C-d") 'nil)
  ;; (global-set-key (kbd "C-e") 'nil)
  ;; (global-set-key (kbd "C-f") 'nil)
  ;; (global-set-key (kbd "C-g") 'nil) ; cancel
  ;; (global-set-key (kbd "C-h") 'nil) ; help
  ;; (global-set-key (kbd "C-i") 'nil) ; tab
  ;; (global-set-key (kbd "C-j") 'nil) ; newline
  ;; (global-set-key (kbd "C-k") 'nil)
  ;; (global-set-key (kbd "C-l") 'nil)
  ;; (global-set-key (kbd "C-m") 'nil)
  (global-set-key (kbd "C-n") #'xah-new-empty-buffer)
  (global-set-key (kbd "C-o") #'find-file)
  ;; (global-set-key (kbd "C-p") 'nil)
  ;; (global-set-key (kbd "C-q") 'nil)
  ;; (global-set-key (kbd "C-r") 'nil)
  (global-set-key (kbd "C-s") #'save-buffer)
  (global-set-key (kbd "C-t") #'hippie-expand)
  ;; (global-set-key (kbd "C-u") 'nil)
  (global-set-key (kbd "C-v") #'yank)
  (global-set-key (kbd "C-w") #'xah-close-current-buffer)
  ;; (global-set-key (kbd "C-x") 'nil)

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

;; (xah-fly--define-keys
;;  (define-prefix-command 'xah-leader-vc-keymap)
;;  '(
;;    ("+" . vc-update)
;;    ("=" . vc-diff)
;;    ("D" . vc-root-diff)
;;    ("L" . vc-print-root-log)
;;    ("a" . vc-update-change-log)
;;    ("b" . vc-switch-backend)
;;    ("c" . vc-rollback)
;;    ("d" . vc-dir)
;;    ("g" . vc-annotate)
;;    ("h" . vc-insert-headers)
;;    ("l" . vc-print-log)
;;    ("m" . vc-merge)
;;    ("r" . vc-retrieve-tag)
;;    ("s" . vc-create-tag)
;;    ("u" . vc-revert)
;;    ("v" . vc-next-action)
;;    ("~" . vc-revision-other-window)))



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
  (let (($newlout
         (cond
          ((stringp Layout) Layout)
          ((symbolp Layout) (symbol-name Layout))
          (t (user-error "Layout %s must be a string." Layout))))
        ($oldlout xah-fly-key-current-layout))
    (setq xah-fly-key-current-layout $newlout)
    (setq xah-fly--key-convert-table
          (cdr (assoc xah-fly-key-current-layout xah-fly-layouts)))
    (when (and (featurep 'xah-fly-keys)
               (not (string-equal $oldlout $newlout)))
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
  (when (buffer-file-name)
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
