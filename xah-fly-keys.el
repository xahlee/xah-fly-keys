;;; xah-fly-keys.el --- ergonomic modal keybinding minor mode. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2013-2016, by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 6.0.2
;; Created: 10 Sep 2013
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience, emulations, vim, ergoemacs
;; Homepage: http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; xah-fly-keys is a efficient keybinding for emacs. (more efficient than vim)

;; It is a modal mode like vi, but key choices are based on statistics of command call frequency.

;; --------------------------------------------------
;; MANUAL INSTALL

;; put the file xah-fly-keys.el in ~/.emacs.d/lisp/
;; create the dir if doesn't exist.

;; put the following in your emacs init file:

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'xah-fly-keys)
;; (xah-fly-keys 1)

;; --------------------------------------------------
;; HOW TO USE

;; M-x xah-fly-keys to toggle the mode on/off.

;; Important command/insert mode switch keys:

;; xah-fly-command-mode-activate (press 【<home>】 or 【F8】 or 【Alt+Space】 or 【menu】)

;; xah-fly-insert-mode-activate  (when in command mode, press letter 【u】 key)

;; When in command mode:
;; 【u】 activates insertion mode
;; 【Space】 is a leader key. For example, 【SPACE p】 calls query-replace. Press 【SPACE C-h】 to see the full list.
;; 【Space Space】 also activates insertion mode.
;; 【Space Enter】 calls execute-extended-command or smex (if smex is installed).
;; 【a】 calls execute-extended-command or smex (if smex is installed).

;; The leader key sequence basically replace ALL emacs commands that starts with C-x key.

;; When using xah-fly-keys, you don't need to press Control or Meta, with the following exceptions:

;; C-c for major mode commands.
;; C-g for cancel.
;; C-q for quoted-insert.
;; C-h for getting a list of keys following a prefix/leader key.

;; Leader key

;; You NEVER need to press Ctrl+x

;; Any emacs commands that has a keybinding starting with C-x, has also a key sequence binding in xah-fly-keys. For example,
;; 【C-x b】 switch-to-buffer is 【SPACE u】
;; 【C-x C-f】 find-file is 【SPACE c .】
;; 【C-x n n】 narrow-to-region is 【SPACE n n】
;; The first key we call it leader key. In the above examples, the SPACE is the leader key.

;; When in command mode, the 【SPACE】 is a leader key.

;; globally, the leader key is the 【f9】 key.

;; the following stardard keys with Control are supported:

 ;; 【Ctrl+tab】 'xah-next-user-buffer
 ;; 【Ctrl+shift+tab】 'xah-previous-user-buffer
 ;; 【Ctrl+v】 paste
 ;; 【Ctrl+w】 close
 ;; 【Ctrl+z】 undo
 ;; 【Ctrl+n】 new
 ;; 【Ctrl+o】 open
 ;; 【Ctrl+s】 save
 ;; 【Ctrl+shift+s】 save as
 ;; 【Ctrl+shift+t】 open last clased
 ;; 【Ctrl++】 'text-scale-increase
 ;; 【Ctrl+-】 'text-scale-decrease
 ;; 【Ctrl+0】 (lambda () (interactive) (text-scale-set 0))))

;; On the Mac, I highly recommend using a app called Sail to set your capslock to send Home. So that it acts as xah-fly-command-mode-activate. You can set capslock or one of the cmd key to Home. See http://xahlee.info/kbd/Mac_OS_X_keymapping_keybinding_tools.html

;; I recommend you clone xah-fly-keys.el, and modify it, and use your modified version. Don't worry about upgrade. The main point is use it for your own good. (I still make key tweaks every week, for the past 3 years.)

;; If you have a bug, post on github. If you have question, post on xah-fly-keys home page.

;; For detail about design and other info, see home page at
;; http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; If you like this project, Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.


;;; Code:

(require 'dired) ; in emacs
(require 'dired-x) ; in emacs
(require 'ido) ; in emacs



(defcustom
  xah-fly-key-layout
  nil
  "The layout to use. Value is one of \"dvorak\" or \"qwerty\". If nil, dvorak is default."
  :group 'xah-fly-keys
  )

(setq xah-fly-key-layout "dvorak")

(defvar xah-fly-command-mode-activate-hook nil "Hook for `xah-fly-command-mode-activate'")
(defvar xah-fly-insert-mode-activate-hook nil "Hook for `xah-fly-insert-mode-activate'")

(defvar xah-fly-use-control-key nil "if true, define standard keys for open, close, paste, etc.")
(setq xah-fly-use-control-key t)


;; cursor movement

(defun xah-pop-local-mark-ring ()
  "Move cursor to last mark position of current buffer.
Call this repeatedly will cycle all positions in `mark-ring'.
URL `http://ergoemacs.org/emacs/emacs_jump_to_previous_position.html'
version 2016-04-04"
  (interactive)
  (set-mark-command t))

(defun xah-forward-block (&optional n)
  "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" n)))

(defun xah-backward-block (&optional n)
  "Move cursor to previous text block.
See: `xah-forward-block'
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n))
        (-i 1))
    (while (<= -i n)
      (if (re-search-backward "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq -i n)))
      (setq -i (1+ -i)))))

(defun xah-beginning-of-line-or-block (&optional n)
  "Move cursor to beginning of line, or beginning of current or previous text block.
 (a text block is separated by blank lines)
URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (if (equal n 1)
        (if (or (equal (point) (line-beginning-position))
                (equal last-command this-command )
                ;; (equal last-command 'xah-end-of-line-or-block )
                )
            (xah-backward-block n)
          (beginning-of-line))
      (xah-backward-block n))))

(defun xah-end-of-line-or-block (&optional n)
  "Move cursor to end of line, or end of current or next text block.
 (a text block is separated by blank lines)
URL `http://ergoemacs.org/emacs/emacs_keybinding_design_beginning-of-line-or-block.html'
version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (if (equal n 1)
        (if (or (equal (point) (line-end-position))
                (equal last-command this-command )
                ;; (equal last-command 'xah-beginning-of-line-or-block )
                )
            (xah-forward-block)
          (end-of-line))
      (progn (xah-forward-block n)))))

(defvar xah-brackets nil "string of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
;; make xah-left-brackets based on xah-brackets
  (setq xah-left-brackets '())
  (dotimes (-x (- (length xah-brackets) 1))
    (when (= (% -x 2) 0)
      (push (char-to-string (elt xah-brackets -x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes (-x (- (length xah-brackets) 1))
    (when (= (% -x 2) 1)
      (push (char-to-string (elt xah-brackets -x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defvar xah-punctuation-regex nil "a regex string for the purpose of jumping to punctuations in programing modes.")
(setq xah-punctuation-regex "[\\!\?\"'#$%&*+,/:;<=>@^`|~]+")

(defun xah-forward-punct (&optional n)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `xah-punctuation-regex'"
  (interactive "p")
  (re-search-forward xah-punctuation-regex nil t n))

(defun xah-backward-punct (&optional n)
  "Move cursor to the previous occurrence of punctuation.
See `xah-forward-punct'"
  (interactive "p")
  (re-search-backward xah-punctuation-regex nil t n))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'.
The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
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

(defun xah-forward-equal-quote ()
  "Move cursor to the next occurrence of 「='」 or 「=\"」, with or without space.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-05-05"
  (interactive)
  (re-search-forward "=[ \n]*\\('+\\|\\\"+\\)" nil t))

(defun xah-forward-equal-sign ()
  "Move cursor to the next occurrence of equal sign 「=」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-06-15"
  (interactive)
  (re-search-forward "=+" nil t))

(defun xah-backward-equal-sign ()
  "Move cursor to previous occurrence of equal sign 「=」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-06-15"
  (interactive)
  (when (re-search-backward "=+" nil t)
    (while (search-backward "=" (- (point) 1) t)
      (left-char))))

(defun xah-forward-comma-sign ()
  "Move cursor to the next occurrence of comma 「,」.
Version 2016-01-19"
  (interactive)
  (re-search-forward ",+" nil t))

(defun xah-backward-comma-sign ()
  "Move cursor to previous occurrence of comma sign 「,」.
Version 2016-01-19"
  (interactive)
  (when (re-search-backward ",+" nil t)
    (while (search-backward "," (- (point) 1) t)
      (left-char))))

(defun xah-forward-quote ()
  "Move cursor to the next occurrence of \".
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-07-23"
  (interactive)
  (if (re-search-forward "\\\"+" nil t)
      t
    (progn
      (message "No more quotes after cursor..")
      nil)))

(defun xah-forward-quote-twice ()
  "Call `xah-forward-quote' twice.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-07-23"
  (interactive)
  (when (xah-forward-quote)
    (xah-forward-quote)))

(defun xah-forward-quote-smart ()
  "Move cursor to the current or next string quote.
Place cursor at the position after the left quote.
Repeated call will find the next string.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-11-22"
  (interactive)
  (let ((-pos (point)))
    (if (nth 3 (syntax-ppss))
        (progn
          (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
          (forward-sexp)
          (re-search-forward "\\\"" nil t))
      (progn (re-search-forward "\\\"" nil t)))
    (when (<= (point) -pos)
      (progn (re-search-forward "\\\"" nil t)))))

(defun xah-backward-quote ()
  "Move cursor to the previous occurrence of \".
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2016-07-23"
  (interactive)
  (if (re-search-backward "\\\"+" nil t)
      (when (char-before) ; isn't nil, at beginning of buffer
        (while (char-equal (char-before) (char-after))
          (left-char)
          t))
    (progn
      (message "No more quotes before cursor.")
      nil)))

(defun xah-forward-dot-comma ()
  "Move cursor to the next occurrence of 「.」 「,」 「;」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-03-24"
  (interactive)
  (re-search-forward "\\.+\\|,+\\|;+" nil t))

(defun xah-backward-dot-comma ()
  "Move cursor to the previous occurrence of 「.」 「,」 「;」
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-03-24"
  (interactive)
  (re-search-backward "\\.+\\|,+\\|;+" nil t))

;; (defun goto-point-min ()
;;   "Goto the beginning of buffer.
;; This is different from `beginning-of-buffer'
;; because that marks the previous position."
;;   (interactive)
;;   (goto-char (point-min))
;; )

;; (defun goto-point-max ()
;;   "Goto the end of buffer.
;; This is different from `end-of-buffer'
;; because that marks the previous position."
;;   (interactive)
;;   (goto-char (point-max))
;; )

;; (defun xah-forward-space ()
;;   "Move cursor to the next occurrence of white space."
;;   (interactive)
;;   (re-search-forward "[ \t\n]+" nil t))

;; (defun xah-backward-space ()
;;   "Move cursor to the next occurrence of white space."
;;   (interactive)
;;   ;; (skip-chars-backward "^ \t\n")
;;   ;; (re-search-backward "[ \t\n]+" nil t)
;;   (posix-search-backward "[ \t\n]+" nil t)
;;   )

(defun xah-display-page-break-as-line ()
  "Display the formfeed ^L char as line.
Version 2016-10-11"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (null buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))


;; editing commands

(defun xah-delete-current-line ()
  "Delete current line."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (when (looking-at "\n")
    (delete-char 1)))

;; (defun xah-copy-line-or-region ()
;;   "Copy current line, or text selection.
;; When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

;; URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
;; Version 2015-05-06"
;;   (interactive)
;;   (let (-p1 -p2)
;;     (if current-prefix-arg
;;         (progn (setq -p1 (point-min))
;;                (setq -p2 (point-max)))
;;       (progn (if (use-region-p)
;;                  (progn (setq -p1 (region-beginning))
;;                         (setq -p2 (region-end)))
;;                (progn (setq -p1 (line-beginning-position))
;;                       (setq -p2 (line-end-position))))))
;;     (kill-ring-save -p1 -p2)
;;     (if current-prefix-arg
;;         (message "buffer text copied")
;;       (message "text copied"))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2016-06-18"
  (interactive)
  (let (-p1 -p2)
    (if current-prefix-arg
        (setq -p1 (point-min) -p2 (point-max))
      (if (use-region-p)
          (setq -p1 (region-beginning) -p2 (region-end))
        (setq -p1 (line-beginning-position) -p2 (line-end-position))))
    (if (eq last-command this-command)
        (progn
          (progn ; hack. exit if there's no more next line
            (end-of-line)
            (forward-char)
            (backward-char))
          ;; (push-mark (point) "NOMSG" "ACTIVATE")
          (kill-append "\n" nil)
          (kill-append (buffer-substring-no-properties (line-beginning-position) (line-end-position)) nil)
          (message "Line copy appended"))
      (progn
        (kill-ring-save -p1 -p2)
        (if current-prefix-arg
            (message "Buffer text copied")
          (message "Text copied"))))
    (end-of-line)
    (forward-char)
    ))

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-all-or-region ()
  "Put the whole buffer content to `kill-ring', or text selection if there's one.
Respects `narrow-to-region'.
URL `http://ergoemacs.org/emacs/emacs_copy_cut_all_or_region.html'
Version 2015-08-22"
  (interactive)
  (if (use-region-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (message "Text selection copied."))
    (progn
      (kill-new (buffer-string))
      (message "Buffer content copied."))))

(defun xah-cut-all-or-region ()
  "Cut the whole buffer content to `kill-ring', or text selection if there's one.
Respects `narrow-to-region'.
URL `http://ergoemacs.org/emacs/emacs_copy_cut_all_or_region.html'
Version 2015-08-22"
  (interactive)
  (if (use-region-p)
      (progn
        (kill-new (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
    (progn
      (kill-new (buffer-string))
      (delete-region (point-min) (point-max)))))

(defun xah-copy-all ()
  "Put the whole buffer content into the `kill-ring'.
(respects `narrow-to-region')
Version 2016-10-06"
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copied."))

(defun xah-cut-all ()
  "Cut the whole buffer content into the `kill-ring'.
Respects `narrow-to-region'.
Version 2017-01-03"
  (interactive)
  (kill-new (buffer-string))
  (delete-region (point-min) (point-max)))

(defun xah-paste-or-paste-previous ()
  "Paste. When called repeatedly, paste previous.
This command calls `yank', and if repeated, call `yank-pop'.

URL `http://ergoemacs.org/emacs/emacs_paste_or_paste_previous.html'
Version 2017-01-11"
  (interactive)
  (progn
    (when (and delete-selection-mode (region-active-p))
      (delete-region (region-beginning) (region-end)))
    (if (eq real-last-command this-command)
        (yank-pop 1)
      (yank))))

(defun xah-delete-backward-char-or-bracket-text ()
  "Delete backward 1 character, but if it's a \"quote\" or bracket ()[]{}【】「」 etc, delete bracket and the inner text.
If it's bracket, push the deleted text to `kill-ring'

URL `http://ergoemacs.org/emacs/emacs_delete_backward_char_or_bracket_text.html'
Version 2017-01-16"
  (interactive)
  (let (
        (-right-brackets (regexp-opt '(")" "]" "}" "〕" "】" "〗" "〉" "》" "」" "』" "›" "»")))
        (-left-brackets (regexp-opt '("(" "{" "[" "〔" "【" "〖" "〈" "《" "「" "『" "‹" "«" ))))
    (if (and delete-selection-mode (region-active-p))
        (delete-region (region-beginning) (region-end))
      (cond
       ((looking-back -right-brackets 1)
        (progn
          (backward-sexp)
          (mark-sexp)
          (kill-region (region-beginning) (region-end))))
       ((looking-back -left-brackets 1)
        (progn
          (backward-char )
          (mark-sexp)
          (kill-region (region-beginning) (region-end))))
       ((looking-back "\\s\"" 1)
        (if (nth 3 (syntax-ppss))
            (progn
              (backward-char )
              (mark-sexp)
              (kill-region (region-beginning) (region-end)))
          (progn
            (backward-sexp)
            (mark-sexp)
            (kill-region (region-beginning) (region-end)))))
       (t (delete-char -1))))))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2016-01-08"
  (interactive)
  (let (
        (deactivate-mark nil)
        -p1 -p2)
    (if (use-region-p)
        (setq -p1 (region-beginning)
              -p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]")
        (setq -p1 (point))
        (skip-chars-forward "[:alnum:]")
        (setq -p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region -p1 -p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region -p1 -p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region -p1 -p2)
      (put this-command 'state 0)))))

;; test case
;; test_case some
;; test-case
;; tes▮t-case

(defun xah-toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor.
URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2015-12-22"
  (interactive)
  (let ((case-fold-search nil))
    (left-char 1)
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
    (right-char)))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one or none.
Call this command again to shrink more. 3 calls will remove all whitespaces.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2016-12-18"
  (interactive)
  (let ((-p0 (point))
        -line-has-char-p ; current line contains non-white space chars
        -has-space-tab-neighbor-p
        -space-or-tab-begin -space-or-tab-end
        )
    (save-excursion
      (setq -has-space-tab-neighbor-p
            (or (looking-at " \\|\t") (looking-back " \\|\t" 1)))
      (beginning-of-line)
      (setq -line-has-char-p (re-search-forward "[[:graph:]]" (line-end-position) t))
      (goto-char -p0)
      (skip-chars-backward "\t ")
      (setq -space-or-tab-begin (point))
      (goto-char -p0)
      (skip-chars-forward "\t ")
      (setq -space-or-tab-end (point)))
    (if -line-has-char-p
        (if -has-space-tab-neighbor-p
            (let (-deleted-text)
              ;; remove all whitespaces in the range
              (setq -deleted-text
                    (delete-and-extract-region -space-or-tab-begin -space-or-tab-end))
              ;; insert a whitespace only if we have removed something different than a simple whitespace
              (when (not (string= -deleted-text " "))
                (insert " ")))
          (progn
            (when (equal (char-before) 10) (delete-char -1))
            (when (equal (char-after) 10) (delete-char 1))))
      (progn (delete-blank-lines)))))

(defun xah-fill-or-unfill ()
  "Reformat current paragraph or region to `fill-column', like `fill-paragraph' or “unfill”.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.
URL `http://ergoemacs.org/emacs/modernization_fill-paragraph.html'
Version 2017-01-08"
  (interactive)
  ;; This command symbol has a property “'compact-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( (-compact-p
          (if (eq last-command this-command)
              (get this-command 'compact-p)
            (> (- (line-end-position) (line-beginning-position)) fill-column)))
         (deactivate-mark nil)
         (-blanks-regex "\n[ \t]*\n")
         -p1 -p2
         )
    (if (use-region-p)
        (progn (setq -p1 (region-beginning))
               (setq -p2 (region-end)))
      (save-excursion
        (if (re-search-backward -blanks-regex nil "NOERROR")
            (progn (re-search-forward -blanks-regex)
                   (setq -p1 (point)))
          (setq -p1 (point)))
        (if (re-search-forward -blanks-regex nil "NOERROR")
            (progn (re-search-backward -blanks-regex)
                   (setq -p2 (point)))
          (setq -p2 (point)))))
    (if -compact-p
        (fill-region -p1 -p2)
      (let ((fill-column most-positive-fixnum ))
        (fill-region -p1 -p2)))
    (put this-command 'compact-p (not -compact-p))))

(defun xah-reformat-lines ()
  "Reformat current text block into 1 long line or multiple short lines.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.

When the command is called for the first time, it checks the current line's length to decide to go into 1 line or multiple lines. If current line is short, it'll reformat to 1 long lines. And vice versa.

Repeated call toggles between formatting to 1 long line and multiple lines.
URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2017-01-08"
  (interactive)
  ;; This command symbol has a property “'compact-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let (
        (-compact-p
         (if (eq last-command this-command)
             (get this-command 'compact-p)
           (> (- (line-end-position) (line-beginning-position)) fill-column)))
        (deactivate-mark nil)
        (-blanks-regex "\n[ \t]*\n")
        -p1 -p2
        )
    (if (use-region-p)
        (progn (setq -p1 (region-beginning))
               (setq -p2 (region-end)))
      (save-excursion
        (if (re-search-backward -blanks-regex nil "NOERROR")
            (progn (re-search-forward -blanks-regex)
                   (setq -p1 (point)))
          (setq -p1 (point)))
        (if (re-search-forward -blanks-regex nil "NOERROR")
            (progn (re-search-backward -blanks-regex)
                   (setq -p2 (point)))
          (setq -p2 (point)))))
    (progn
      (if -compact-p
          (xah-reformat-to-multi-lines-region -p1 -p2)
        (xah-reformat-whitespaces-to-one-space -p1 -p2))
      (put this-command 'compact-p (not -compact-p)))))

(defun xah-reformat-whitespaces-to-one-space (*begin *end)
  "Replace whitespaces by one space.
URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2017-01-11"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (goto-char (point-min))
      (while
          (search-forward "\n" nil "NOERROR")
        (replace-match " "))
      (goto-char (point-min))
      (while
          (search-forward "\t" nil "NOERROR")
        (replace-match " "))
      (goto-char (point-min))
      (while
          (re-search-forward "  +" nil "NOERROR")
        (replace-match " ")))))

(defun xah-reformat-to-multi-lines-region (*begin *end)
  "Replace space by a newline char at places so lines are not long.
URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
Version 2016-12-13"
  (interactive "r")
  (save-restriction
    (narrow-to-region *begin *end)
    (goto-char (point-min))
    (while
        (search-forward " " nil "NOERROR")
      (when (> (- (point) (line-beginning-position)) fill-column)
        (replace-match "\n" )))))

;; (defun xah-reformat-to-single-line-region (*begin *end)
;;   "Replace whitespaces at end line by one space or 1 return.
;; Basically, collapse whitespaces. But 2 or more newline will collapse to 1.
;; URL `http://ergoemacs.org/emacs/emacs_reformat_lines.html'
;; Version 2016-10-30"
;;   (interactive "r")
;;   (save-excursion
;;     (save-restriction
;;       (narrow-to-region *begin *end)
;;       (goto-char (point-min))
;;       (while
;;           (re-search-forward "\n\n+" nil "NOERROR")
;;         (replace-match ",,j0mxwz9jiv"))
;;       (goto-char (point-min))
;;       (while
;;           (search-forward "\t" nil "NOERROR")
;;         (replace-match " "))
;;       (goto-char (point-min))
;;       (while
;;           (re-search-forward "\n" nil "NOERROR")
;;         (replace-match " "))
;;       (goto-char (point-min))
;;       (while
;;           (re-search-forward "  +" nil "NOERROR")
;;         (replace-match " "))
;;       (goto-char (point-min))
;;       (while
;;           (search-forward ",,j0mxwz9jiv" nil "NOERROR")
;;         (replace-match "\n\n")))))

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defun xah-unfill-region (*begin *end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region *begin *end)))

(defun xah-comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line.

URL `http://ergoemacs.org/emacs/emacs_toggle_comment_by_line.html'
Version 2016-10-25"
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let ((-lbp (line-beginning-position))
          (-lep (line-end-position)))
      (if (eq -lbp -lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) -lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region -lbp -lep)
            (forward-line )))))))

(defun xah-quote-lines ()
  "Change current text block's lines to quoted lines with comma or other separator char.
When there is a text selection, act on the selection, else, act on a text block separated by blank lines.

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

If the delimiter is any left bracket, the end delimiter is automatically the matching bracket.

URL `http://ergoemacs.org/emacs/emacs_quote_lines.html'
Version 2017-01-11"
  (interactive)
  (let* (
         -p1
         -p2
         (-quoteToUse
          (read-string
           "Quote to use:" "\"" nil
           '(
             ""
             "\""
             "'"
             "("
             "{"
             "["
             )))
         (-separator
          (read-string
           "line separator:" "," nil
           '(
             ""
             ","
             ";"
             )))
         (-beginQuote -quoteToUse)
         (-endQuote
          ;; if begin quote is a bracket, set end quote to the matching one. else, same as begin quote
          (let ((-syntableValue (aref (syntax-table) (string-to-char -beginQuote))))
            (if (eq (car -syntableValue ) 4) ; ; syntax table, code 4 is open paren
                (char-to-string (cdr -syntableValue))
              -quoteToUse
              ))))
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (progn
        (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq -p1 (point)))
          (setq -p1 (point)))
        (re-search-forward "\n[ \t]*\n" nil "NOERROR")
        (skip-chars-backward " \t\n" )
        (setq -p2 (point))))
    (save-excursion
      (save-restriction
        (narrow-to-region -p1 -p2)
        (goto-char (point-min))
        (skip-chars-forward "\t ")
        (insert -beginQuote)
        (goto-char (point-max))
        (insert -endQuote)
        (goto-char (point-min))
        (while (re-search-forward "\n\\([\t ]*\\)" nil "NOERROR" )
          (replace-match
           (concat -endQuote -separator (concat "\n" (match-string 1)) -beginQuote) "FIXEDCASE" "LITERAL"))
        ;;
        ))))

(defun xah-dired-rename-space-to-underscore ()
  "In dired, rename current or marked files by replacing space to underscore _.
If not in `dired', do nothing.
URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2017-01-02"
  (interactive)
  (require 'dired-aux)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "Not in dired")))

(defun xah-dired-rename-space-to-hyphen ()
  "In dired, rename current or marked files by replacing space to hyphen -.
If not in `dired', do nothing.
URL `http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html'
Version 2016-12-22"
  (interactive)
  (require 'dired-aux)
  (if (equal major-mode 'dired-mode)
      (progn
        (mapc (lambda (x)
                (when (string-match " " x )
                  (dired-rename-file x (replace-regexp-in-string " " "_" x) nil)))
              (dired-get-marked-files ))
        (revert-buffer))
    (user-error "Not in dired")))

(defun xah-cycle-hyphen-underscore-space ( &optional *begin *end )
  "Cycle {underscore, space, hypen} chars in selection or inside quote/bracket or line.
When called repeatedly, this command cycles the {“_”, “-”, “ ”} characters, in that order.

The region to work on is by this order:
 ① if there's active region (text selection), use that.
 ② If cursor is string quote or any type of bracket, and is within current line, work on that region.
 ③ else, work on current line.

URL `http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html'
Version 2017-01-11"
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of -charArray.
  (let (-p1 -p2)
    (if (and (not (null *begin)) (not (null *end)))
        (progn (setq -p1 *begin -p2 *end))
      (if (use-region-p)
          (setq -p1 (region-beginning) -p2 (region-end))
        (if (nth 3 (syntax-ppss))
            (save-excursion
              (skip-chars-backward "^\"")
              (setq -p1 (point))
              (skip-chars-forward "^\"")
              (setq -p2 (point)))
          (let (
                (-skipChars
                 (if (boundp 'xah-brackets)
                     (concat "^\"" xah-brackets)
                   "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）")))
            (skip-chars-backward -skipChars (line-beginning-position))
            (setq -p1 (point))
            (skip-chars-forward -skipChars (line-end-position))
            (setq -p2 (point))
            (set-mark -p1)))))
    (let* (
           (-charArray ["_" "-" " "])
           (-length (length -charArray))
           (-regionWasActive-p (region-active-p))
           (-nowState
            (if (equal last-command this-command )
                (get 'xah-cycle-hyphen-underscore-space 'state)
              0 ))
           (-changeTo (elt -charArray -nowState)))
      (save-excursion
        (save-restriction
          (narrow-to-region -p1 -p2)
          (goto-char (point-min))
          (while
              (re-search-forward
               (elt -charArray (% (+ -nowState 2) -length))
               ;; (concat
               ;;  (elt -charArray (% (+ -nowState 1) -length))
               ;;  "\\|"
               ;;  (elt -charArray (% (+ -nowState 2) -length)))
               (point-max)
               "NOERROR")
            (replace-match -changeTo "FIXEDCASE" "LITERAL"))))
      (when (or (string= -changeTo " ") -regionWasActive-p)
        (goto-char -p2)
        (set-mark -p1)
        (setq deactivate-mark nil))
      (put 'xah-cycle-hyphen-underscore-space 'state (% (+ -nowState 1) -length)))))

(defun xah-underscore-to-space-region (*begin *end)
  "Change underscore char to space.
URL `http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html'
Version 2017-01-11"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (goto-char (point-min))
      (while
          (re-search-forward "_" (point-max) "NOERROR")
        (replace-match " " "FIXEDCASE" "LITERAL")))))

(defun xah-copy-file-path (&optional *dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2016-07-17"
  (interactive "P")
  (let ((-fpath
         (if (equal major-mode 'dired-mode)
             (expand-file-name default-directory)
           (if (null (buffer-file-name))
               (user-error "Current buffer is not associated with a file.")
             (buffer-file-name)))))
    (kill-new
     (if (null *dir-path-only-p)
         (progn
           (message "File path copied: 「%s」" -fpath)
           -fpath
           )
       (progn
         (message "Directory path copied: 「%s」" (file-name-directory -fpath))
         (file-name-directory -fpath))))))

(defun xah-delete-text-block ()
  "Delete current/next text block or selection, and also copy to `kill-ring'.

A “block” is text between blank lines.
The “current block” is the block the cursor is at.
If cursor is not on a block, deletes the next block.
If there's a text selection, just delete that region.

URL `http://ergoemacs.org/emacs/emacs_delete_block.html'
Version 2016-10-10"
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (progn
      (beginning-of-line)
      (if (re-search-forward "[[:graph:]]" (line-end-position) "NOERROR")
          (xah-delete-current-text-block)
        (when (re-search-forward "[[:graph:]]" )
          (xah-delete-current-text-block))))))

(defun xah-delete-current-text-block ()
  "Delete the current text block and also copy to `kill-ring'.

A “block” is text between blank lines.
The “current block” is the block the cursor is at.
If cursor is not on a block nor on edge of a block, delete 2 empty lines.
If there's a text selection, ignore it.

URL `http://ergoemacs.org/emacs/emacs_delete_block.html'
Version 2016-10-10"
  (interactive)
  (let (-p1 -p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq -p1 (point)))
        (setq -p1 (point)))
      (re-search-forward "\n[ \t]*\n" nil "NOERROR")
      (setq -p2 (point)))
    (kill-region -p1 -p2)))

(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (let (-p1 -p2)
    (if (region-active-p)
        (progn (setq -p1 (region-beginning))
               (setq -p2 (region-end)))
      (progn (setq -p1 (line-beginning-position))
             (setq -p2 (line-end-position))))
    (copy-to-register ?1 -p1 -p2)
    (message "copied to register 1: 「%s」." (buffer-substring-no-properties -p1 -p2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'.
URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun xah-copy-rectangle-to-kill-ring (*begin *end)
  "Copy region as column (rectangle region) to `kill-ring'
See also: `kill-rectangle', `copy-to-register'.
URL `http://ergoemacs.org/emacs/emacs_copy_rectangle_text_to_clipboard.html'
version 2016-07-17"
  ;; extract-rectangle suggested by YoungFrog, 2012-07-25
  (interactive "r")
  (require 'rect)
  (kill-new (mapconcat 'identity (extract-rectangle *begin *end) "\n")))

(defun xah-upcase-sentence ()
  "Upcase first letters of sentences of current text block or selection.

URL `http://ergoemacs.org/emacs/emacs_upcase_sentence.html'
Version 2017-01-16"
  (interactive)
  (let (-p1 -p2)
    (if (region-active-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn
              (setq -p1 (point))
              (re-search-forward "\n[ \t]*\n"))
          (setq -p1 (point)))
        (progn
          (re-search-forward "\n[ \t]*\n" nil "move")
          (setq -p2 (point)))))
    (save-excursion
      (save-restriction
        (narrow-to-region -p1 -p2)
        (let ((case-fold-search nil))
          (goto-char (point-min))
          (while (re-search-forward "\\. \\{1,2\\}\\([a-z]\\)" nil "move") ; after period
            (upcase-region (match-beginning 1) (match-end 1))
            (overlay-put (make-overlay (match-beginning 1) (match-end 1)) 'face 'highlight)
            ;;
            )
          (goto-char (point-min))
          (while (re-search-forward "\\. ?\n *\\([a-z]\\)" nil "move") ; new line after period
            (upcase-region (match-beginning 1) (match-end 1))
            (overlay-put (make-overlay (match-beginning 1) (match-end 1)) 'face 'highlight)
            ;;
            )
          (goto-char (point-min))
          (while (re-search-forward "\\(\\`\\|\n\n\\)\\([a-z]\\)" nil "move") ; after a blank line, or beginning of buffer
            (upcase-region (match-beginning 2) (match-end 2))
            (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight)
            ;;
            )
          (goto-char (point-min))
          (while (re-search-forward "<p>\\([a-z]\\)" nil "move") ; for HTML. first letter after tag
            (upcase-region (match-beginning 1) (match-end 1))
            (overlay-put (make-overlay (match-beginning 1) (match-end 1)) 'face 'highlight)
            ;;
            ))))))

(defun xah-escape-quotes (*begin *end)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `xah-unescape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" "FIXEDCASE" "LITERAL")))))

(defun xah-unescape-quotes (*begin *end)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
See also: `xah-escape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" "FIXEDCASE" "LITERAL")))))

(defun xah-title-case-region-or-line (*begin *end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, *begin *end are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           -p1
           -p2
           (-skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward -skipChars (line-beginning-position))
         (setq -p1 (point))
         (skip-chars-forward -skipChars (line-end-position))
         (setq -p2 (point)))
       (list -p1 -p2))))
  (let* (
         (-strPairs [
                     [" A " " a "]
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
        (narrow-to-region *begin *end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (-x)
             (goto-char (point-min))
             (while
                 (search-forward (aref -x 0) nil t)
               (replace-match (aref -x 1) "FIXEDCASE" "LITERAL")))
           -strPairs))))))


;; insertion commands

(defun xah-insert-date ()
  "Insert current date and or time.
Insert date in this format: yyyy-mm-dd.
When called with `universal-argument', prompt for a format to use.
If there's text selection, delete it first.

Do not use this function in lisp code. Call `format-time-string' directly.

URL `http://ergoemacs.org/emacs/elisp_insert-date-time.html'
version 2016-12-18"
  (interactive)
  (when (use-region-p) (delete-region (region-beginning) (region-end)))
  (let ((-style
         (if current-prefix-arg
             (string-to-number
              (substring
               (ido-completing-read
                "Style:"
                '(
                  "1 → 2016-10-10 Monday"
                  "2 → 2016-10-10T19:39:47-07:00"
                  "3 → 2016-10-10 19:39:58-07:00"
                  "4 → Monday, October 10, 2016"
                  "5 → Mon, Oct 10, 2016"
                  "6 → October 10, 2016"
                  "7 → Oct 10, 2016"
                  )) 0 1))
           0
           )))
    (insert
     (cond
      ((= -style 0)
       (format-time-string "%Y-%m-%d") ; "2016-10-10"
       )
      ((= -style 1)
       (format-time-string "%Y-%m-%d %A") ; "2016-10-10 Monday"
       )
      ((= -style 2)
       (concat
        (format-time-string "%Y-%m-%dT%T")
        (funcall (lambda (-x) (format "%s:%s" (substring -x 0 3) (substring -x 3 5))) (format-time-string "%z")))
       ;; eg "2016-10-10T19:02:23-07:00"
       )
      ((= -style 3)
       (concat
        (format-time-string "%Y-%m-%d %T")
        (funcall (lambda (-x) (format "%s:%s" (substring -x 0 3) (substring -x 3 5))) (format-time-string "%z")))
       ;; eg "2016-10-10 19:10:09-07:00"
       )
      ((= -style 4)
       (format-time-string "%A, %B %d, %Y")
       ;; eg "Monday, October 10, 2016"
       )
      ((= -style 5)
       (format-time-string "%a, %b %d, %Y")
       ;; eg "Mon, Oct 10, 2016"
       )
      ((= -style 6)
       (format-time-string "%B %d, %Y")
       ;; eg "October 10, 2016"
       )
      ((= -style 7)
       (format-time-string "%b %d, %Y")
       ;; eg "Oct 10, 2016"
       )
      (t
       (format-time-string "%Y-%m-%d"))))))

;; (defun xah-current-date-time-string ()
;;   "Returns current date-time string in full ISO 8601 format.
;; Example: 「2012-04-05T21:08:24-07:00」.

;; Note, for the time zone offset, both the formats 「hhmm」 and 「hh:mm」 are valid ISO 8601. However, Atom Webfeed spec seems to require 「hh:mm」."
;;   (concat
;;    (format-time-string "%Y-%m-%dT%T")
;;    ((lambda (-x) (format "%s:%s" (substring -x 0 3) (substring -x 3 5))) (format-time-string "%z"))))

(defun xah-insert-bracket-pair (*left-bracket *right-bracket &optional *wrap-method)
  "Insert brackets around selection, word, at point, and maybe move cursor in between.

 *left-bracket and *right-bracket are strings. *wrap-method must be either 'line or 'block. 'block means between empty lines.

• if there's a region, add brackets around region.
• If *wrap-method is 'line, wrap around line.
• If *wrap-method is 'block, wrap around block.
• if cursor is at beginning of line and its not empty line and contain at least 1 space, wrap around the line.
• If cursor is at end of a word or buffer, one of the following will happen:
 xyz▮ → xyz(▮)
 xyz▮ → (xyz▮)       if in one of the lisp modes.
• wrap brackets around word if any. e.g. xy▮z → (xyz▮). Or just (▮)

URL `http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html'
Version 2016-12-04"
  (if (use-region-p)
      (progn ; there's active region
        (let (
              (-p1 (region-beginning))
              (-p2 (region-end)))
          (goto-char -p2)
          (insert *right-bracket)
          (goto-char -p1)
          (insert *left-bracket)
          (goto-char (+ -p2 2))))
    (progn ; no text selection
      (let (-p1 -p2)
        (cond
         ((eq *wrap-method 'line)
          (setq -p1 (line-beginning-position) -p2 (line-end-position))
          (goto-char -p2)
          (insert *right-bracket)
          (goto-char -p1)
          (insert *left-bracket)
          (goto-char (+ -p2 (length *left-bracket))))
         ((eq *wrap-method 'block)
          (save-excursion
            (progn
              (if (re-search-backward "\n[ \t]*\n" nil 'move)
                  (progn (re-search-forward "\n[ \t]*\n")
                         (setq -p1 (point)))
                (setq -p1 (point)))
              (if (re-search-forward "\n[ \t]*\n" nil 'move)
                  (progn (re-search-backward "\n[ \t]*\n")
                         (setq -p2 (point)))
                (setq -p2 (point))))
            (goto-char -p2)
            (insert *right-bracket)
            (goto-char -p1)
            (insert *left-bracket)
            (goto-char (+ -p2 (length *left-bracket)))))
         ( ;  do line. line must contain space
          (and
           (eq (point) (line-beginning-position))
           ;; (string-match " " (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           (not (eq (line-beginning-position) (line-end-position))))
          (insert *left-bracket )
          (end-of-line)
          (insert  *right-bracket))
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
            (setq -p1 (point) -p2 (point))
            (insert *left-bracket *right-bracket)
            (search-backward *right-bracket )))
         (t (progn
              ;; wrap around “word”. basically, want all alphanumeric, plus hyphen and underscore, but don't want space or punctuations. Also want chinese chars
              ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
              (skip-chars-backward "-_[:alnum:]")
              (setq -p1 (point))
              (skip-chars-forward "-_[:alnum:]")
              (setq -p2 (point))
              (goto-char -p2)
              (insert *right-bracket)
              (goto-char -p1)
              (insert *left-bracket)
              (goto-char (+ -p2 (length *left-bracket))))))))))

(defun xah-insert-paren () (interactive) (xah-insert-bracket-pair "(" ")") )
(defun xah-insert-square-bracket () (interactive) (xah-insert-bracket-pair "[" "]") )
(defun xah-insert-brace () (interactive) (xah-insert-bracket-pair "{" "}") )

(defun xah-insert-double-curly-quote“” () (interactive) (xah-insert-bracket-pair "“" "”") )
(defun xah-insert-curly-single-quote‘’ () (interactive) (xah-insert-bracket-pair "‘" "’") )
(defun xah-insert-single-angle-quote‹› () (interactive) (xah-insert-bracket-pair "‹" "›") )
(defun xah-insert-double-angle-quote«» () (interactive) (xah-insert-bracket-pair "«" "»") )
(defun xah-insert-ascii-double-quote () (interactive) (xah-insert-bracket-pair "\"" "\"") )
(defun xah-insert-ascii-single-quote () (interactive) (xah-insert-bracket-pair "'" "'") )
(defun xah-insert-emacs-quote () (interactive) (xah-insert-bracket-pair "`" "'") )
(defun xah-insert-corner-bracket「」 () (interactive) (xah-insert-bracket-pair "「" "」" ) )
(defun xah-insert-white-corner-bracket『』 () (interactive) (xah-insert-bracket-pair "『" "』") )
(defun xah-insert-angle-bracket〈〉 () (interactive) (xah-insert-bracket-pair "〈" "〉") )
(defun xah-insert-double-angle-bracket《》 () (interactive) (xah-insert-bracket-pair "《" "》") )
(defun xah-insert-white-lenticular-bracket〖〗 () (interactive) (xah-insert-bracket-pair "〖" "〗") )
(defun xah-insert-black-lenticular-bracket【】 () (interactive) (xah-insert-bracket-pair "【" "】") )
(defun xah-insert-tortoise-shell-bracket〔〕 () (interactive) (xah-insert-bracket-pair "〔" "〕" ) )

(defun xah-insert-hyphen ()
  "Insert a hyphen character."
  (interactive)
  (insert "-"))

(defun xah-insert-string-assignment ()
  "Insert space before cursor"
  (interactive)
  (progn (insert "=\"\"")
         (left-char)))

(defun xah-insert-space-before ()
  "Insert space before cursor"
  (interactive)
  (insert " "))

(defun xah-insert-space-after ()
  "Insert space after cursor"
  (interactive)
  (insert " ")
  (left-char))

(defun xah-insert-form-feed ()
  "insert a form feed char (ASCII 12)"
  (interactive)
  (insert ""))

(defun xah-insert-column-counter (*n)
  "Insert a sequence of numbers vertically.

 (this command is similar to emacs 24.x's `rectangle-number-lines'.)

For example, if your text is:

a b
c d
e f

and your cursor is after “a”, then calling this function with argument
3 will change it to become:

a1 b
c2 d
e3 f

If there are not enough existing lines after the cursor
when this function is called, it aborts at the last line.

This command is conveniently used together with `kill-rectangle' and `string-rectangle'."
  (interactive "nEnter the max integer: ")
  (let ((-i 1) -colpos )
    (setq -colpos (- (point) (line-beginning-position)))
    (while (<= -i *n)
      (insert (number-to-string -i))
      (forward-line) (beginning-of-line) (forward-char -colpos)
      (setq -i (1+ -i)))))

(defun xah-insert-alphabets-az (&optional *use-uppercase-p)
  "Insert letters a to z vertically.
If `universal-argument' is called first, use CAPITAL letters.

URL `http://ergoemacs.org/emacs/emacs_insert-alphabets.html'
Version 2015-11-06"
  (interactive "P")
  (let ((-startChar (if *use-uppercase-p 65 97 )))
    (dotimes (-i 26)
      (insert (format "%c\n" (+ -startChar -i))))))

(defvar xah-unicode-list nil "Associative list of Unicode symbols. First element is a Unicode character, second element is a string used as key shortcut in `ido-completing-read'")
(setq xah-unicode-list
      '(
        ("_" . "underscore" )
        ("•" . ".bullet" )
        ("→" . "tn")
        ("◇" . "3" )
        ("◆" . "4" )
        ("¤" . "2" )
        ("…" . "...ellipsis" )
        (" " . "nbsp" )
        ("、" . "," )
        ("⭑" . "9" )
        ("🎶" . "5" )
        ("—" . "-emdash" )
        ("＆" . "7" )
        ("↓" . "tt")
        ("←" . "th")
        ("↑" . "tc")
        ("👍" . "tu")
        ) )

(defun xah-insert-unicode ()
  "Insert a unicode"
  (interactive)
  (let (gotThis)
    (setq gotThis
          (ido-completing-read "insert:" (mapcar (lambda (x) (concat (car x) (cdr x))) xah-unicode-list)))
    (insert (car (assoc (substring gotThis 0 1) xah-unicode-list)))))


;; text selection

(defun xah-select-current-block ()
  "Select the current block of text between blank lines.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-07-22"
  (interactive)
  (let (-p1)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq -p1 (point)))
        (setq -p1 (point)))
      (re-search-forward "\n[ \t]*\n" nil "move"))
    (set-mark -p1)))

(defun xah-select-block ()
  "Select the current/next block of text between blank lines.
If region is active, extend selection downward by block.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-07-22"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (xah-select-current-block)))

(defun xah-select-current-line ()
  "Select current line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-07-22"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-07-22"
  (interactive)
  (if (region-active-p)
      (progn
        (forward-line 1)
        (end-of-line))
    (xah-select-current-line)))

(defun xah-semnav-up (arg)
"Called by `xah-extend-selection'.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-11-13.
Written by Nikolaj Schumacher, 2008-10-20. Released under GPL 2"
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (setq arg (1- arg) ))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (setq arg (1+ arg) )))
  (up-list arg))

(defun xah-extend-selection-old-2017-01-14 (arg &optional incremental-p)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit.

This command works mostly in lisp syntax.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-11-13.
Written by Nikolaj Schumacher, 2008-10-20. Released under GPL 2."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (or (use-region-p)
             (eq last-command this-command))))
  (if incremental-p
      (progn
        (xah-semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (xah-extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
Subsequent calls expands the selection.

when no selection,
• if cursor is on a bracket, select whole bracketed thing including bracket
• if cursor is on a quote, select whole quoted thing including quoted
• if cursor is on the beginning of line, select the line.
• else, select current word.

when there's a selection, the selection extension behavior is still experimental.
Roughly:
• if 1 line is selected, extend to next line.
• if multiple lines is selected, extend to next line.
• if a bracketed text is selected, extend to include the outer bracket. If there's no outer, select current line.

 to line, or bracket/quoted text,
or text block, whichever is the smallest.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2017-01-15"
  (interactive)
  (if (region-active-p)
      (progn
        (let ((-rb (region-beginning)) (-re (region-end)))
          (goto-char -rb)
          (cond
           ((looking-at "\\s(")
            (if (eq (nth 0 (syntax-ppss)) 0)
                (progn
                  (message "left bracket, depth 0.")
                  (end-of-line) ; select current line
                  (set-mark (line-beginning-position)))
              (progn
                (message "left bracket, depth not 0")
                (up-list -1 t t)
                (mark-sexp))))
           ((eq -rb (line-beginning-position))
            (progn
              (goto-char -rb)
              (let ((-firstLineEndPos (line-end-position)))
                (cond
                 ((eq -re -firstLineEndPos)
                  (progn
                    (message "exactly 1 line. extend to next whole line." )
                    (forward-line 1)
                    (end-of-line)))
                 ((< -re -firstLineEndPos)
                  (progn
                    (message "less than 1 line. complete the line." )
                    (end-of-line)))
                 ((> -re -firstLineEndPos)
                  (progn
                    (message "beginning of line, but end is greater than 1st end of line" )
                    (goto-char -re)
                    (if (eq (point) (line-end-position))
                        (progn
                          (message "exactly multiple lines" )
                          (forward-line 1)
                          (end-of-line))
                      (progn
                        (message "multiple lines but end is not eol. make it so" )
                        (goto-char -re)
                        (end-of-line)))))
                 (t (error "logic error 42946" ))))))
           ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
            (progn
              (message "less than 1 line" )
              (end-of-line) ; select current line
              (set-mark (line-beginning-position))))
           (t (message "last resort" ) nil))))
    (progn
      (cond
       ((looking-at "\\s(")
        (message "left bracket")
        (mark-sexp)) ; left bracket
       ((looking-at "\\s)")
        (message "right bracket")
        (backward-up-list) (mark-sexp))
       ((looking-at "\\s\"")
        (message "string quote")
        (mark-sexp)) ; string quote
       ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
        (message "beginning of line and not empty")
        (end-of-line)
        (set-mark (line-beginning-position)))
       ((or (looking-back "\\s_" 1) (looking-back "\\sw" 1))
        (message "left is word or symbol")
        (skip-syntax-backward "_w" )
        ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
        (mark-sexp))
       ((and (looking-at "\\s ") (looking-back "\\s " 1))
        (message "left and right both space" )
        (skip-chars-backward "\\s " ) (set-mark (point))
        (skip-chars-forward "\\s "))
       ((and (looking-at "\n") (looking-back "\n" 1))
        (message "left and right both newline")
        (skip-chars-forward "\n")
        (set-mark (point))
        (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next text block
       (t (message "just mark sexp" )
          (mark-sexp))
       ;;
       ))))

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command does not properly deal with nested brackets.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2016-12-18"
  (interactive)
  (let (
        (-skipChars
         (if (boundp 'xah-brackets)
             (concat "^\"" xah-brackets)
           "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
        -pos
        )
    (skip-chars-backward -skipChars)
    (setq -pos (point))
    (skip-chars-forward -skipChars)
    (set-mark -pos)))


;; misc

(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
“emacs buffer” here is buffer whose name starts with *.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
“emacs buffer” here is buffer whose name starts with *.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))

(defun xah-new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2016-12-27"
  (interactive)
  (let ((-buf (generate-new-buffer "untitled")))
    (switch-to-buffer -buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))

(defvar xah-recently-closed-buffers nil "alist of recently closed buffers. Each element is (buffer name, file path). The max number to track is controlled by the variable `xah-recently-closed-buffers-max'.")

(defvar xah-recently-closed-buffers-max 40 "The maximum length for `xah-recently-closed-buffers'.")

(defun xah-close-current-buffer ()
  "Close the current buffer.

Similar to `kill-buffer', with the following addition:

• Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• If the buffer is editing a source file in an org-mode file, prompt the user to save before closing.
• If the buffer is a file, add the path to the list `xah-recently-closed-buffers'.
• If it is the minibuffer, exit the minibuffer

URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2016-06-19"
  (interactive)
  (let (-emacs-buff-p
        (-org-p (string-match "^*Org Src" (buffer-name))))

    (setq -emacs-buff-p (if (string-match "^*" (buffer-name)) t nil))

    (if (string= major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit) ; if the buffer is minibuffer
      (progn
        ;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
        (when (and (buffer-modified-p)
                   (not -emacs-buff-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        (when (and (buffer-modified-p)
                   -org-p)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))

        ;; save to a list of closed buffer
        (when (buffer-file-name)
          (setq xah-recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) xah-recently-closed-buffers))
          (when (> (length xah-recently-closed-buffers) xah-recently-closed-buffers-max)
            (setq xah-recently-closed-buffers (butlast xah-recently-closed-buffers 1))))

        ;; close
        (kill-buffer (current-buffer))))))

(defun xah-open-last-closed ()
  "Open the last closed file.
URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2016-06-19"
  (interactive)
  (if (> (length xah-recently-closed-buffers) 0)
      (find-file (cdr (pop xah-recently-closed-buffers)))
    (progn (message "No recently close buffer in this session."))))

(defun xah-open-recently-closed ()
  "Open recently closed file.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2016-06-19"
  (interactive)
  (find-file (ido-completing-read "open:" (mapcar (lambda (f) (cdr f)) xah-recently-closed-buffers))))

(defun xah-list-recently-closed ()
  "List recently closed file.
URL `http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html'
Version 2016-06-19"
  (interactive)
  (let ((-buf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer -buf)
    (mapc (lambda (-f) (insert (cdr -f) "\n"))
          xah-recently-closed-buffers)))



(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2016-01-28"
  (interactive)
  (let (
         (-suffix-map
          ;; (‹extension› . ‹shell program name›)
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("go" . "go run")
            ("js" . "node") ; node.js
            ("ts" . "tsc") ; TypeScript
            ("sh" . "bash")
            ("clj" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
            ("rkt" . "racket")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("tex" . "pdflatex")
            ("latex" . "pdflatex")
            ("java" . "javac")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            ))
         -fname
         -fSuffix
         -prog-name
         -cmd-str)

    (when (null (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))

    (setq -fname (buffer-file-name))
    (setq -fSuffix (file-name-extension -fname))
    (setq -prog-name (cdr (assoc -fSuffix -suffix-map)))
    (setq -cmd-str (concat -prog-name " \""   -fname "\""))

    (cond
     ((string-equal -fSuffix "el") (load -fname))
     ((string-equal -fSuffix "java")
      (progn
        (shell-command -cmd-str "*xah-run-current-file output*" )
        (shell-command
         (format "java %s" (file-name-sans-extension (file-name-nondirectory -fname))))))
     (t (if -prog-name
            (progn
              (message "Running…")
              (shell-command -cmd-str "*xah-run-current-file output*" ))
          (message "No recognized program file suffix for this file."))))))

(defun xah-clean-empty-lines (&optional *begin *end *n)
  "Replace repeated blank lines to just 1.
Works on whole buffer or text selection, respects `narrow-to-region'.

*N is the number of newline chars to use in replacement.
If 0, it means lines will be joined.
By befault, *N is 2. It means, 1 visible blank line.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2016-10-07"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (when (null *begin)
    (setq *begin (point-min) *end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (progn
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil "NOERROR")
          (replace-match (make-string (if (null *n) 2 *n ) 10)))))))

(defun xah-clean-whitespace (&optional *begin *end)
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2016-10-15"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (when (null *begin)
    (setq *begin (point-min)  *end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (progn
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+\n" nil "NOERROR")
          (replace-match "\n")))
      (xah-clean-empty-lines (point-min) (point-max))
      (progn
        (goto-char (point-max))
        (while (equal (char-before) 32) ; char 32 is space
          (delete-char -1))))))

(defun xah-make-backup ()
  "Make a backup copy of current file or dired marked files.
If in dired, backup current file or marked files.
The backup file name is
 ‹name›~‹timestamp›~
example:
 file.html~20150721T014457~
in the same dir. If such a file already exist, it's overwritten.
If the current buffer is not associated with a file, nothing's done.
URL `http://ergoemacs.org/emacs/elisp_make-backup.html'
Version 2015-10-14"
  (interactive)
  (let ((-fname (buffer-file-name)))
    (if -fname
        (let ((-backup-name
               (concat -fname "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
          (copy-file -fname -backup-name t)
          (message (concat "Backup saved at: " -backup-name)))
      (if (string-equal major-mode "dired-mode")
          (progn
            (mapc (lambda (-x)
                    (let ((-backup-name
                           (concat -x "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
                      (copy-file -x -backup-name t)))
                  (dired-get-marked-files))
            (message "marked files backed up"))
        (user-error "buffer not file nor dired")))))

(defun xah-make-backup-and-save ()
  "Backup of current file and save, or backup dired marked files.
For detail, see `xah-make-backup'.
If the current buffer is not associated with a file nor dired, nothing's done.
URL `http://ergoemacs.org/emacs/elisp_make-backup.html'
Version 2015-10-14"
  (interactive)
  (if (buffer-file-name)
      (progn
        (xah-make-backup)
        (when (buffer-modified-p)
          (save-buffer)))
    (progn
      (xah-make-backup))))

(defun xah-delete-current-file-make-backup (&optional *no-backup-p)
  "Delete current file, makes a backup~, closes the buffer.

Backup filename is “‹name›~‹date time stamp›~”. Existing file of the same name is overwritten. If the file is not associated with buffer, the backup file name starts with “xx_”.

When `universal-argument' is called first, don't create backup.

URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2016-07-20"
  (interactive "P")
  (let* (
         (-fname (buffer-file-name))
         (-buffer-is-file-p -fname)
         (-backup-suffix (concat "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
    (if -buffer-is-file-p
        (progn
          (save-buffer -fname)
          (when (not *no-backup-p)
            (copy-file
             -fname
             (concat -fname -backup-suffix)
             t))
          (delete-file -fname)
          (message "Deleted. Backup created at 「%s」." (concat -fname -backup-suffix)))
      (when (not *no-backup-p)
        (widen)
        (write-region (point-min) (point-max) (concat "xx" -backup-suffix))
        (message "Backup created at 「%s」." (concat "xx" -backup-suffix))))
    (kill-buffer (current-buffer))))

(defun xah-delete-current-file-copy-to-kill-ring ()
  "Delete current buffer/file and close the buffer, push content to `kill-ring'.
URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2016-09-03"
  (interactive)
  (let ((-bstr (buffer-string)))
    (when (> (length -bstr) 0)
      (kill-new -bstr)
      (message "Buffer content copied to kill-ring."))
    (when (buffer-file-name)
      (when (file-exists-p (buffer-file-name))
        (progn
          (delete-file (buffer-file-name))
          (message "Deleted file: 「%s」." (buffer-file-name)))))
    (let ((buffer-offer-save nil))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(defun xah-delete-current-file (&optional *no-backup-p)
  "Delete current buffer/file.
If buffer is a file, makes a backup~, else, push file content to `kill-ring'.

This commands calls `xah-delete-current-file-make-backup' or
 `xah-delete-current-file-copy-to-kill-ring'.

If next buffer is dired, refresh it.

URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2016-07-20"
  (interactive "P")
  (if (buffer-file-name)
      (xah-delete-current-file-make-backup *no-backup-p)
    (xah-delete-current-file-copy-to-kill-ring))
  (when (eq major-mode 'dired-mode)
    (revert-buffer)))



(defun xah-search-current-word ()
  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)
  (let ( -p1 -p2 )
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq -p1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq -p2 (point))))
    (setq mark-active nil)
    (when (< -p1 (point))
      (goto-char -p1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties -p1 -p2))))

(defun xah-open-in-desktop ()
  "Show current file in desktop (OS's file manager).
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-11-30"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let (
          (process-connection-type nil)
          (openFileProgram (if (file-exists-p "/usr/bin/gvfs-open")
                               "/usr/bin/gvfs-open"
                             "/usr/bin/xdg-open")))
      (start-process "" nil openFileProgram "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
    )))

(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2016-10-15"
  (interactive)
  (let* (
         (-file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (-do-it-p (if (<= (length -file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when -do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (-fpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" -fpath t t))) -file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (-fpath)
           (shell-command
            (concat "open " (shell-quote-argument -fpath))))  -file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (-fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" -fpath))) -file-list))))))

(defun xah-open-in-terminal ()
  "Open the current dir in a new terminal window.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-12-10"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (message "Microsoft Windows not supported. File a bug report or pull request."))
   ((string-equal system-type "darwin")
    (message "Mac not supported. File a bug report or pull request."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "x-terminal-emulator"
                     (concat "--working-directory=" default-directory) )))))

(defun xah-next-window-or-frame ()
  "Switch to next window or frame.
If current frame has only one window, switch to next frame.
If `universal-argument' is called first, do switch frame."
  (interactive)
  (if (null current-prefix-arg)
      (if (one-window-p)
          (other-frame 1)
        (other-window 1))
    (other-frame 1)))

(defun xah-describe-major-mode ()
  "Display inline doc for current `major-mode'."
  (interactive)
  (describe-function major-mode))



(setq xah-dvorak-to-qwerty-kmap
      '(
        ("a" . "a")
        ("b" . "n")
        ("c" . "i")
        ("d" . "h")
        ("e" . "d")
        ("f" . "y")
        ("g" . "u")
        ("h" . "j")
        ("i" . "g")
        ("j" . "c")
        ("k" . "v")
        ("l" . "p")
        ("m" . "m")
        ("n" . "l")
        ("o" . "s")
        ("p" . "r")
        ("q" . "x")
        ("r" . "o")
        ("s" . ";")
        ("t" . "k")
        ("u" . "f")
        ("v" . ".")
        ("w" . ",")
        ("x" . "b")
        ("y" . "t")
        ("z" . "/")
        ("." . "e")
        ("," . "w")
        ("'" . "q")
        (";" . "z")
        ("/" . "[")
        ("[" . "-")
        ("]" . "=")
        ("=" . "]")
        ("-" . "'")))

(defun xah-dvorak-to-qwerty (charstr)
  "Convert dvorak key to qwerty. charstr is single char string."
  (interactive)
  (cdr (assoc charstr xah-dvorak-to-qwerty-kmap)))

(defun xah-qwerty-to-dvorak (charstr)
  "Convert qwerty key to dvorak. charstr is single char string."
  (interactive)
  (car (rassoc charstr xah-dvorak-to-qwerty-kmap)))

(defun xah-fly-map-keys (*kmap-name *key-cmd-alist)
  "similar to `define-key' but map over a alist."
  (interactive)
  (mapc
   (lambda (-pair)
     (define-key *kmap-name (xah-fly-kbd (car -pair)) (cdr -pair)))
   *key-cmd-alist))

(defun xah-fly-kbd (*arg)
  "similar to `kbd' but are translated to current `xah-fly-key-layout' first,
*arg must be a single char."
  (interactive)
  (if (or (null xah-fly-key-layout)
          (string-equal xah-fly-key-layout "dvorak"))
      (kbd *arg)
    (progn (kbd (xah-dvorak-to-qwerty *arg)))))


;; keymaps

(defvar xah-fly-swapped-1827-p nil "Boolean. If true, it means keys 1 and 8 are swapped, and 2 and 7 are swapped. See: http://xahlee.info/kbd/best_number_key_layout.html")

(defvar xah-fly-key-map nil "Keybinding for `xah-fly-keys' minor mode.")
(progn
  (setq xah-fly-key-map (make-sparse-keymap))

  ;; (define-key xah-fly-key-map (kbd "'") 'self-insert-command)

  )

;; commands in search-map and facemenu-keymap
(xah-fly-map-keys
 (define-prefix-command 'xah-highlight-keymap)
 '(

   ("b" . facemenu-set-bold)
   ("f" . font-lock-fontify-block)
   ("c" . center-line)
   ("d" . facemenu-set-default)

   ("h" . highlight-symbol-at-point)
   ;; temp
   ("." . isearch-forward-symbol-at-point)
   ("1" . hi-lock-find-patterns)
   ("2" . highlight-lines-matching-regexp)
   ("3" . highlight-phrase)
   ("4" . highlight-regexp)
   ("5" . unhighlight-regexp)
   ("6" . hi-lock-write-interactive-patterns)
   ("s" . isearch-forward-symbol)
   ("w" . isearch-forward-word)

   ("i" . facemenu-set-italic)
   ("l" . facemenu-set-bold-italic)
   ("o" . facemenu-set-face)
   ("p" . center-paragraph)

   ("u" . facemenu-set-underline)
   ))

(xah-fly-map-keys
 (define-prefix-command 'xah-leader-tab-keymap)
 '(
   ("TAB" . indent-for-tab-command)

   ("i" . complete-symbol)
   ("g" . indent-rigidly)
   ("r" . indent-region)
   ("s" . indent-sexp)

   ;; temp
   ("1" . abbrev-prefix-mark)
   ("2" . edit-abbrevs)
   ("3" . expand-abbrev)
   ("4" . expand-region-abbrevs)
   ("5" . unexpand-abbrev)
   ("6" . add-global-abbrev)
   ("7" . add-mode-abbrev)
   ("8" . inverse-add-global-abbrev)
   ("9" . inverse-add-mode-abbrev)
   ("0" . expand-jump-to-next-slot)
   ("=" . expand-jump-to-previous-slot)))



(xah-fly-map-keys
 (define-prefix-command 'xah-leader-c-keymap)
 '(
   ("," . xah-open-in-external-app)
   ("." . find-file)
   ("c" . bookmark-bmenu-list)
   ("e" . ibuffer)
   ("u" . find-file-at-point)
   ("h" . recentf-open-files)
   ("l" . bookmark-set)
   ("n" . xah-new-empty-buffer)
   ("o" . xah-open-in-desktop)
   ("p" . xah-open-last-closed)
   ("f" . xah-open-recently-closed)
   ("y" . xah-list-recently-closed)
   ("r" . bookmark-jump)
   ("s" . write-file)
   ))

(xah-fly-map-keys
 (define-prefix-command 'xah-help-keymap)
 '(
   (";" . Info-goto-emacs-command-node)
   ("a" . apropos-command)
   ("b" . describe-bindings)
   ("c" . describe-char)
   ("d" . apropos-documentation)
   ("e" . view-echo-area-messages)
   ("f" . describe-face)
   ("g" . info-lookup-symbol)
   ("h" . describe-function)
   ("i" . info)
   ("j" . man)
   ("k" . describe-key)
   ("K" . Info-goto-emacs-key-command-node)
   ("l" . view-lossage)
   ("m" . xah-describe-major-mode)
   ("n" . describe-variable)
   ("o" . describe-language-environment)
   ("p" . finder-by-keyword)
   ("r" . apropos-variable)
   ("s" . describe-syntax)
   ("u" . elisp-index-search)
   ("v" . apropos-value)
   ("z" . describe-coding-system)))

(xah-fly-map-keys
 (define-prefix-command 'xah-leader-i-keymap) ; commands in goto-map
 '(
   ("TAB" . move-to-column)
   ("c" . goto-char)
   ("t" . goto-line)
   ("n" . next-error)
   ("d" . previous-error
    )))

(xah-fly-map-keys
 ;; commands here are harmless (safe). They don't modify text.
 ;; they turn on minor/major mode, change display, prompt, start shell, etc.
 (define-prefix-command 'xah-harmless-keymap)
 '(
   ("SPC" . whitespace-mode)
   ("RET" . nil)
   ("TAB" . nil)
   ("DEL" . nil)
   ("," . abbrev-mode)
   ("." . toggle-frame-fullscreen)
   ("'" . frame-configuration-to-register)
   (";" . window-configuration-to-register)
   ("1" . set-input-method)
   ("2" . global-hl-line-mode)
   ("4" . linum-mode)
   ("5" . visual-line-mode)
   ("6" . calendar)
   ("7" . calc)
   ("8" . nil)
   ("9" . shell-command)
   ("0" . shell-command-on-region)
   ("a" . text-scale-adjust)
   ("b" . toggle-debug-on-error)
   ("c" . toggle-case-fold-search)
   ("d" . narrow-to-page)
   ("e" . eshell)
   ("f" . nil)
   ("g" . nil)
   ("h" . widen)
   ("i" . make-frame-command)
   ("j" . nil)
   ("k" . menu-bar-open)
   ("l" . toggle-word-wrap)
   ("m" . global-linum-mode)
   ("n" . narrow-to-region)
   ("o" . nil)
   ("p" . read-only-mode) ; toggle-read-only
   ("q" . nil)
   ("r" . nil)
   ("s" . flyspell-buffer)
   ("t" . narrow-to-defun)
   ("u" . shell)
   ("v" . variable-pitch-mode)
   ("w" . eww)
   ("x" . save-some-buffers)
   ("y" . nil)
   ("z" . abort-recursive-edit)))

(xah-fly-map-keys
 ;; kinda replacement related
 (define-prefix-command 'xah-edit-cmds-keymap)
 '(
   ("," . apply-macro-to-region-lines)
   ("." . kmacro-start-macro)
   ("c" . replace-rectangle)
   ("d" . delete-rectangle)
   ("e" . call-last-kbd-macro)
   ("g" . kill-rectangle)
   ("i" . xah-upcase-sentence)
   ("l" . clear-rectangle)
   ("n" . rectangle-number-lines)
   ("o" . open-rectangle)
   ("p" . kmacro-end-macro)
   ("r" . yank-rectangle)
   ("u" . xah-quote-lines)
   ("y" . delete-whitespace-rectangle)
   ("SPC" . rectangle-mark-mode)))

(xah-fly-map-keys
 (define-prefix-command 'xah-leader-t-keymap)
 '(
   ("SPC" . xah-clean-whitespace)
   ("TAB" . nil)
   ("3" . point-to-register)
   ("4" . jump-to-register)
   ("." . sort-lines)
   ("," . sort-numeric-fields)
   ("'" . reverse-region)
   ("d" . mark-defun)
   ("e" . list-matching-lines)
   ("u" . delete-matching-lines)
   ("i" . delete-non-matching-lines)
   ("j" . copy-to-register)
   ("k" . insert-register)
   ("l" . increment-register)
   ("m" . xah-make-backup-and-save)
   ("n" . repeat-complex-command)
   ("p" . query-replace-regexp)
   ("r" . copy-rectangle-to-register)
   ("t" . repeat)
   ("w" . xah-next-window-or-frame)
   ("y" . delete-duplicate-lines)
   ("z" . number-to-register)))

(xah-fly-map-keys
 (define-prefix-command 'xah-danger-keymap)
 '(
   ("DEL" . xah-delete-current-file)
   ("." . eval-buffer)
   ("e" . eval-defun)
   ("m" . eval-last-sexp)
   ("p" . eval-expression)
   ("u" . eval-region)
   ("q" . save-buffers-kill-terminal)
   ("w" . delete-frame)
   ("j" . xah-run-current-file)))

(xah-fly-map-keys
 (define-prefix-command 'xah-insertion-keymap)
 '(
   ("RET" . insert-char)
   ("SPC" . xah-insert-unicode)
   ("b" . xah-insert-black-lenticular-bracket【】)
   ("c" . xah-insert-ascii-single-quote)
   ("d" . xah-insert-double-curly-quote“”)
   ("f" . xah-insert-emacs-quote)
   ("g" . xah-insert-ascii-double-quote)
   ("h" . xah-insert-brace) ; {}
   ("i" . xah-insert-curly-single-quote‘’)
   ("m" . xah-insert-corner-bracket「」)
   ("n" . xah-insert-square-bracket) ; []
   ("p" . xah-insert-single-angle-quote‹›)
   ("r" . xah-insert-tortoise-shell-bracket〔〕 )
   ("s" . xah-insert-string-assignment)
   ("t" . xah-insert-paren)
   ("u" . xah-insert-date)
   ("w" . xah-insert-angle-bracket〈〉)
   ("W" . xah-insert-double-angle-bracket《》)
   ("y" . xah-insert-double-angle-quote«»)))

(xah-fly-map-keys
 (define-prefix-command 'xah-coding-system-keymap)
 '(

   ("n" . set-file-name-coding-system)
   ("s" . set-next-selection-coding-system)
   ("c" . universal-coding-system-argument)
   ("f" . set-buffer-file-coding-system)
   ("k" . set-keyboard-coding-system)
   ("l" . set-language-environment)
   ("p" . set-buffer-process-coding-system)
   ("r" . revert-buffer-with-coding-system)
   ("t" . set-terminal-coding-system)
   ("x" . set-selection-coding-system)

))

(progn
  (define-prefix-command 'xah-fly-leader-key-map)
  (define-key xah-fly-leader-key-map (kbd "SPC") 'xah-fly-insert-mode-activate)
  (define-key xah-fly-leader-key-map (kbd "DEL") 'xah-close-current-buffer)
  (define-key xah-fly-leader-key-map (kbd "RET") (if (fboundp 'smex) 'smex 'execute-extended-command ))
  (define-key xah-fly-leader-key-map (kbd "TAB") xah-leader-tab-keymap)

  (define-key xah-fly-leader-key-map "." xah-highlight-keymap)

  (define-key xah-fly-leader-key-map "'" 'xah-fill-or-unfill)
  (define-key xah-fly-leader-key-map "," 'universal-argument)
  (define-key xah-fly-leader-key-map "-" 'xah-display-page-break-as-line)
  (define-key xah-fly-leader-key-map "/" nil)
  (define-key xah-fly-leader-key-map ";" nil)
  (define-key xah-fly-leader-key-map "=" nil)
  (define-key xah-fly-leader-key-map "[" nil)
  (define-key xah-fly-leader-key-map (kbd "\\") nil)
  (define-key xah-fly-leader-key-map "`" nil)

  (define-key xah-fly-leader-key-map "1" nil)
  (define-key xah-fly-leader-key-map "2" nil)
  (define-key xah-fly-leader-key-map "3" 'delete-window)
  (define-key xah-fly-leader-key-map "4" 'split-window-right)
  (define-key xah-fly-leader-key-map "5" nil)
  (define-key xah-fly-leader-key-map "6" nil)
  (define-key xah-fly-leader-key-map "7" nil)
  (define-key xah-fly-leader-key-map "8" nil)
  (define-key xah-fly-leader-key-map "9" 'ispell-word)
  (define-key xah-fly-leader-key-map "0" nil)

  (define-key xah-fly-leader-key-map "a" 'mark-whole-buffer)
  (define-key xah-fly-leader-key-map "b" 'end-of-buffer)
  (define-key xah-fly-leader-key-map "c" xah-leader-c-keymap)
  (define-key xah-fly-leader-key-map "d" 'beginning-of-buffer)
  (define-key xah-fly-leader-key-map "e" xah-insertion-keymap)
  (define-key xah-fly-leader-key-map "f" 'xah-search-current-word)
  (define-key xah-fly-leader-key-map "g" 'isearch-forward)
  (define-key xah-fly-leader-key-map "h" 'xah-help-keymap)
  (define-key xah-fly-leader-key-map "i" 'xah-copy-file-path)
  (define-key xah-fly-leader-key-map "j" 'xah-cut-all-or-region)
  (define-key xah-fly-leader-key-map "k" 'xah-paste-or-paste-previous)
  (define-key xah-fly-leader-key-map "l" 'recenter-top-bottom)
  (define-key xah-fly-leader-key-map "m" 'dired-jump)
  (define-key xah-fly-leader-key-map "n" xah-harmless-keymap)
  (define-key xah-fly-leader-key-map "o" 'exchange-point-and-mark)
  (define-key xah-fly-leader-key-map "p" 'query-replace)
  (define-key xah-fly-leader-key-map "q" 'xah-copy-all-or-region)
  (define-key xah-fly-leader-key-map "r" xah-edit-cmds-keymap)
  (define-key xah-fly-leader-key-map "s" 'save-buffer)
  (define-key xah-fly-leader-key-map "t" xah-leader-t-keymap)
  (define-key xah-fly-leader-key-map "u" 'switch-to-buffer)
  (define-key xah-fly-leader-key-map "v" nil)
  (define-key xah-fly-leader-key-map "w" xah-danger-keymap)
  (define-key xah-fly-leader-key-map "x" nil)
  (define-key xah-fly-leader-key-map "y" xah-leader-i-keymap)
  (define-key xah-fly-leader-key-map "z" nil))


;;;; misc

;; these commands have keys in emacs, but right now i decided not to give them a key

;; C-x C-p	mark-page
;; C-x C-l	downcase-region
;; C-x C-u	upcase-region

;; C-x C-t	transpose-lines
;; C-x C-o	delete-blank-lines

;; C-x C-r	find-file-read-only
;; C-x C-v	find-alternate-file

;; C-x =	what-cursor-position, use describe-char instead
;; C-x <	scroll-left
;; C-x >	scroll-right
;; C-x [	backward-page
;; C-x ]	forward-page
;; C-x ^	enlarge-window

;; C-x {	shrink-window-horizontally
;; C-x }	enlarge-window-horizontally
;; C-x DEL	backward-kill-sentence

;; C-x C-z	suspend-frame
;; C-x +	balance-windows

;; C-x k	kill-buffer , use xah-close-current-buffer
;; C-x l	count-lines-page
;; C-x m	compose-mail


;; undecided yet

;; C-x e	kmacro-end-and-call-macro
;; C-x q	kbd-macro-query
;; C-x C-k	kmacro-keymap

;; C-x C-d	list-directory
;; C-x C-n	set-goal-column
;; C-x ESC	Prefix Command
;; C-x $	set-selective-display
;; C-x *	calc-dispatch
;; C-x -	shrink-window-if-larger-than-buffer
;; C-x .	set-fill-prefix

;; C-x 4	ctl-x-4-prefix
;; C-x 5	ctl-x-5-prefix
;; C-x 6	2C-command
;; C-x ;	comment-set-column

;; C-x `	next-error
;; C-x f	set-fill-column
;; C-x i	insert-file
;; C-x n	Prefix Command
;; C-x r	Prefix Command

;; C-x C-k C-a	kmacro-add-counter
;; C-x C-k C-c	kmacro-set-counter
;; C-x C-k C-d	kmacro-delete-ring-head
;; C-x C-k C-e	kmacro-edit-macro-repeat
;; C-x C-k C-f	kmacro-set-format
;; C-x C-k TAB	kmacro-insert-counter
;; C-x C-k C-k	kmacro-end-or-call-macro-repeat
;; C-x C-k C-l	kmacro-call-ring-2nd-repeat
;; C-x C-k RET	kmacro-edit-macro
;; C-x C-k C-n	kmacro-cycle-ring-next
;; C-x C-k C-p	kmacro-cycle-ring-previous
;; C-x C-k C-s	kmacro-start-macro
;; C-x C-k C-t	kmacro-swap-ring
;; C-x C-k C-v	kmacro-view-macro-repeat
;; C-x C-k SPC	kmacro-step-edit-macro
;; C-x C-k b	kmacro-bind-to-key
;; C-x C-k e	edit-kbd-macro
;; C-x C-k l	kmacro-edit-lossage
;; C-x C-k n	kmacro-name-last-macro
;; C-x C-k q	kbd-macro-query
;; C-x C-k r	apply-macro-to-region-lines
;; C-x C-k s	kmacro-start-macro



;; C-x 4 C-f	find-file-other-window
;; C-x 4 C-o	display-buffer
;; C-x 4 .	find-tag-other-window
;; C-x 4 0	kill-buffer-and-window
;; C-x 4 a	add-change-log-entry-other-window
;; C-x 4 b	switch-to-buffer-other-window
;; C-x 4 c	clone-indirect-buffer-other-window
;; C-x 4 d	dired-other-window
;; C-x 4 f	find-file-other-window
;; C-x 4 m	compose-mail-other-window
;; C-x 4 r	find-file-read-only-other-window

;; C-x 6 2	2C-two-columns
;; C-x 6 b	2C-associate-buffer
;; C-x 6 s	2C-split
;; C-x 6 <f2>	2C-two-columns

  ;; (define-key xah-leader-i-keymap (kbd "r") ctl-x-5-map)

;; r C-f     find-file-other-frame
;; r C-o     display-buffer-other-frame
;; r .       find-tag-other-frame
;; r 0       delete-frame
;; r 1       delete-other-frames
;; r 2       make-frame-command
;; r b       switch-to-buffer-other-frame
;; r d       dired-other-frame
;; r f       find-file-other-frame
;; r m       compose-mail-other-frame
;; r o       other-frame
;; r r       find-file-read-only-other-frame

;; (xah-fly-map-keys
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

;; ;; 2013-11-04 make emacs auto show suggestions when a prefix key is pressed
;; (require 'guide-key)
;; (guide-key-mode 1)


;; setting keys

(progn
  (when xah-fly-use-control-key
    (progn
      (define-key xah-fly-key-map (kbd "<C-prior>") 'xah-previous-user-buffer)
      (define-key xah-fly-key-map (kbd "<C-next>") 'xah-next-user-buffer)

      (define-key xah-fly-key-map (kbd "<C-S-prior>") 'xah-previous-emacs-buffer)
      (define-key xah-fly-key-map (kbd "<C-S-next>") 'xah-next-emacs-buffer)

      (define-key xah-fly-key-map (kbd "<C-tab>") 'xah-next-user-buffer)
      (define-key xah-fly-key-map (kbd "<C-S-iso-lefttab>") 'xah-previous-user-buffer)

      (define-key xah-fly-key-map (kbd "C-1") 'xah-pop-local-mark-ring)
      (define-key xah-fly-key-map (kbd "C-2") 'pop-global-mark)

      (define-key xah-fly-key-map (kbd "C-7") 'xah-pop-local-mark-ring)
      (define-key xah-fly-key-map (kbd "C-8") 'pop-global-mark)

      (define-key xah-fly-key-map (kbd "C-9") 'scroll-down-command)
      (define-key xah-fly-key-map (kbd "C-0") 'scroll-up-command)

      (define-key xah-fly-key-map (kbd "C-SPC") 'xah-fly-leader-key-map)

      (define-key xah-fly-key-map (kbd "C-a") 'mark-whole-buffer)
      (define-key xah-fly-key-map (kbd "C-n") 'xah-new-empty-buffer)
      (define-key xah-fly-key-map (kbd "C-S-n") 'make-frame-command)
      (define-key xah-fly-key-map (kbd "C-o") 'find-file)
      (define-key xah-fly-key-map (kbd "C-s") 'save-buffer)
      (define-key xah-fly-key-map (kbd "C-S-s") 'write-file)
      (define-key xah-fly-key-map (kbd "C-S-t") 'xah-open-last-closed)
      (define-key xah-fly-key-map (kbd "C-v") 'yank)
      (define-key xah-fly-key-map (kbd "C-w") 'xah-close-current-buffer)
      (define-key xah-fly-key-map (kbd "C-z") 'undo)

      (define-key xah-fly-key-map (kbd "C-+") 'text-scale-increase)
      (define-key xah-fly-key-map (kbd "C--") 'text-scale-decrease)
      (define-key xah-fly-key-map (kbd "C-0") (lambda () (interactive) (text-scale-set 0)))

      (define-key xah-fly-key-map (kbd "C-r") 'hippie-expand)
      (define-key xah-fly-key-map (kbd "C-t") 'xah-toggle-letter-case) ; never do transpose-chars
      ;;
      ))

  (progn ; rule: all commands with meta key defined here must have other shortcuts. that is, meta binding is considered a luxury

    (define-key xah-fly-key-map (kbd "M-RET") 'xah-cycle-hyphen-underscore-space)
    (define-key xah-fly-key-map (kbd "M-c") 'xah-toggle-letter-case )
    (define-key xah-fly-key-map (kbd "M-g") 'hippie-expand )
    (define-key xah-fly-key-map (kbd "M-h") 'xah-insert-brace )
    (define-key xah-fly-key-map (kbd "M-m") xah-insertion-keymap)
    (define-key xah-fly-key-map (kbd "M-n") 'xah-insert-square-bracket)
    (define-key xah-fly-key-map (kbd "M-t") 'xah-insert-paren)
    (define-key xah-fly-key-map (kbd "M-l") 'left-char) ; rid of downcase-word

    (define-key xah-fly-key-map (kbd "M-SPC") 'xah-fly-command-mode-activate))

  (define-key xah-fly-key-map (kbd "<home>") 'xah-fly-command-mode-activate)
  (define-key xah-fly-key-map (kbd "<menu>") 'xah-fly-command-mode-activate)
  (define-key xah-fly-key-map (kbd "<f8>") 'xah-fly-command-mode-activate)

  (define-key xah-fly-key-map (kbd "<f9>") xah-fly-leader-key-map)

  (define-key xah-fly-key-map (kbd "<f11>") 'xah-previous-user-buffer)
  (define-key xah-fly-key-map (kbd "<f12>") 'xah-next-user-buffer)
  (define-key xah-fly-key-map (kbd "<C-f11>") 'xah-previous-emacs-buffer)
  (define-key xah-fly-key-map (kbd "<C-f12>") 'xah-next-emacs-buffer)

  (progn
    ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
    (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
    (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )

    (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
    (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)

    (define-key minibuffer-local-isearch-map (kbd "<left>") 'isearch-reverse-exit-minibuffer)
    (define-key minibuffer-local-isearch-map (kbd "<right>") 'isearch-forward-exit-minibuffer)
    ;;
    )
  ;;
  )



(defvar xah-fly-insert-state-q t "Boolean value. true means insertion mode is on.")
(setq xah-fly-insert-state-q t)

(defun xah-fly-command-mode-init ()
  "set command mode keys"
  (interactive)
  (progn
    (progn ; need shift key
      (define-key xah-fly-key-map "~" nil)
      (define-key xah-fly-key-map ":" nil) ;
      )
    (progn ; special
      (define-key xah-fly-key-map (kbd "'") 'xah-reformat-lines)
      (define-key xah-fly-key-map (kbd ",") 'xah-shrink-whitespaces)
      (define-key xah-fly-key-map (kbd "-") 'xah-cycle-hyphen-underscore-space)
      (define-key xah-fly-key-map (kbd ".") 'backward-kill-word)
      (define-key xah-fly-key-map (kbd ";") 'xah-comment-dwim)
      (define-key xah-fly-key-map (kbd "/") 'xah-backward-equal-sign)
      (define-key xah-fly-key-map (kbd "\\") nil)
      (define-key xah-fly-key-map (kbd "=") 'xah-forward-equal-sign)
      (define-key xah-fly-key-map (kbd "[") 'xah-backward-quote )
      (define-key xah-fly-key-map (kbd "]") 'xah-forward-quote-smart)
      (define-key xah-fly-key-map (kbd "`") 'other-frame)
      (define-key xah-fly-key-map (kbd "SPC") xah-fly-leader-key-map)
      (define-key xah-fly-key-map (kbd "DEL") xah-fly-leader-key-map) ; for kinesis
      )
    (if xah-fly-swapped-1827-p
        (progn
          (define-key xah-fly-key-map (kbd "8") nil)
          (define-key xah-fly-key-map (kbd "7") nil)
          (define-key xah-fly-key-map (kbd "2") 'xah-select-current-line)
          (define-key xah-fly-key-map (kbd "1") 'xah-extend-selection))
      (progn
        (define-key xah-fly-key-map (kbd "1") nil)
        (define-key xah-fly-key-map (kbd "2") nil)
        (define-key xah-fly-key-map (kbd "7") 'xah-select-current-line)
        (define-key xah-fly-key-map (kbd "8") 'xah-extend-selection)))

    (define-key xah-fly-key-map (kbd "3") 'delete-other-windows)
    (define-key xah-fly-key-map (kbd "4") 'split-window-below)
    (define-key xah-fly-key-map (kbd "5") 'delete-char)
    (define-key xah-fly-key-map (kbd "6") 'xah-select-block)
    (define-key xah-fly-key-map (kbd "9") 'xah-select-text-in-quote)
    (define-key xah-fly-key-map (kbd "0") 'xah-backward-punct)

    (define-key xah-fly-key-map (kbd "a") (if (fboundp 'smex) 'smex 'execute-extended-command ))
    (define-key xah-fly-key-map "b" 'isearch-forward)
    (define-key xah-fly-key-map (kbd "c") 'previous-line)
    (define-key xah-fly-key-map (kbd "d") 'xah-beginning-of-line-or-block)
    (define-key xah-fly-key-map (kbd "e") 'xah-delete-backward-char-or-bracket-text)
    (define-key xah-fly-key-map (kbd "f") 'undo)
    (define-key xah-fly-key-map (kbd "g") 'backward-word)
    (define-key xah-fly-key-map (kbd "h") 'backward-char)
    (define-key xah-fly-key-map (kbd "i") 'xah-delete-text-block)
    (define-key xah-fly-key-map (kbd "j") 'xah-cut-line-or-region)
    (define-key xah-fly-key-map (kbd "k") 'xah-paste-or-paste-previous)
    (define-key xah-fly-key-map (kbd "l") 'xah-fly-insert-mode-activate-space-before)
    (define-key xah-fly-key-map (kbd "m") 'xah-backward-left-bracket)
    (define-key xah-fly-key-map (kbd "n") 'forward-char)
    (define-key xah-fly-key-map (kbd "o") 'open-line)
    (define-key xah-fly-key-map (kbd "p") 'kill-word)
    (define-key xah-fly-key-map (kbd "q") 'xah-copy-line-or-region)
    (define-key xah-fly-key-map (kbd "r") 'forward-word)
    (define-key xah-fly-key-map (kbd "s") 'xah-end-of-line-or-block)
    (define-key xah-fly-key-map (kbd "t") 'next-line)
    (define-key xah-fly-key-map (kbd "u") 'xah-fly-insert-mode-activate)
    (define-key xah-fly-key-map (kbd "v") 'xah-forward-right-bracket)
    (define-key xah-fly-key-map (kbd "w") 'xah-next-window-or-frame)
    (define-key xah-fly-key-map (kbd "x") nil)
    (define-key xah-fly-key-map (kbd "y") 'set-mark-command)
    (define-key xah-fly-key-map (kbd "z") 'xah-goto-matching-bracket)

    ;;
    ))

(defun xah-fly-insert-mode-init ()
  "Set insertion mode keys"
  (interactive)
  (progn
    (define-key xah-fly-key-map (kbd "'") nil)
    (define-key xah-fly-key-map (kbd ",") nil)
    (define-key xah-fly-key-map (kbd "-") nil)
    (define-key xah-fly-key-map (kbd ".") nil)
    (define-key xah-fly-key-map (kbd "/") nil)
    (define-key xah-fly-key-map (kbd ";") nil)
    (define-key xah-fly-key-map (kbd "=") nil)
    (define-key xah-fly-key-map (kbd "[") nil)
    (define-key xah-fly-key-map (kbd "\\") nil)
    (define-key xah-fly-key-map (kbd "]") nil)
    (define-key xah-fly-key-map (kbd "`") nil)
    (define-key xah-fly-key-map (kbd "~") nil)
    (define-key xah-fly-key-map (kbd "SPC") nil)
    (define-key xah-fly-key-map (kbd "DEL") nil)

    (define-key xah-fly-key-map (kbd "1") nil)
    (define-key xah-fly-key-map (kbd "2") nil)
    (define-key xah-fly-key-map (kbd "3") nil)
    (define-key xah-fly-key-map (kbd "4") nil)
    (define-key xah-fly-key-map (kbd "5") nil)
    (define-key xah-fly-key-map (kbd "6") nil)
    (define-key xah-fly-key-map (kbd "7") nil)
    (define-key xah-fly-key-map (kbd "8") nil)
    (define-key xah-fly-key-map (kbd "9") nil)
    (define-key xah-fly-key-map (kbd "0") nil)

    (define-key xah-fly-key-map (kbd "a") nil)
    (define-key xah-fly-key-map (kbd "b") nil)
    (define-key xah-fly-key-map (kbd "c") nil)
    (define-key xah-fly-key-map (kbd "d") nil)
    (define-key xah-fly-key-map (kbd "e") nil)
    (define-key xah-fly-key-map (kbd "f") nil)
    (define-key xah-fly-key-map (kbd "g") nil)
    (define-key xah-fly-key-map (kbd "h") nil)
    (define-key xah-fly-key-map (kbd "i") nil)
    (define-key xah-fly-key-map (kbd "j") nil)
    (define-key xah-fly-key-map (kbd "k") nil)
    (define-key xah-fly-key-map (kbd "l") nil)
    (define-key xah-fly-key-map (kbd "m") nil)
    (define-key xah-fly-key-map (kbd "n") nil)
    (define-key xah-fly-key-map (kbd "o") nil)
    (define-key xah-fly-key-map (kbd "p") nil)
    (define-key xah-fly-key-map (kbd "q") nil)
    (define-key xah-fly-key-map (kbd "r") nil)
    (define-key xah-fly-key-map (kbd "s") nil)
    (define-key xah-fly-key-map (kbd "t") nil)
    (define-key xah-fly-key-map (kbd "u") nil)
    (define-key xah-fly-key-map (kbd "v") nil)
    (define-key xah-fly-key-map (kbd "w") nil)
    (define-key xah-fly-key-map (kbd "x") nil)
    (define-key xah-fly-key-map (kbd "y") nil)
    (define-key xah-fly-key-map (kbd "z") nil)

    ;;
))

(defun xah-fly-mode-toggle ()
  "Switch between {insertion, command} modes."
  (interactive)
  (if xah-fly-insert-state-q
      (xah-fly-command-mode-activate)
    (xah-fly-insert-mode-activate)))

;; automatic save buffer when switching to command mode
(add-hook 'xah-fly-command-mode-activate-hook 'xah-fly-save-buffer-if-file)

(defun xah-fly-save-buffer-if-file ()
  "Save current buffer if it is a file."
  (interactive)
  (when (buffer-file-name)
    (save-buffer)))

(defun xah-fly-command-mode-activate ()
  "Activate command mode."
  (interactive)
  (modify-all-frames-parameters (list (cons 'cursor-type 'box)))
  (setq xah-fly-insert-state-q nil )
  (xah-fly-command-mode-init)
  (run-hooks 'xah-fly-command-mode-activate-hook))

(defun xah-fly-insert-mode-activate ()
  "Activate insertion mode."
  (interactive)
  (modify-all-frames-parameters (list (cons 'cursor-type 'bar)))
  (setq xah-fly-insert-state-q t )
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



;; when in going into minibuffer, switch to insertion mode.
(add-hook 'minibuffer-setup-hook 'xah-fly-insert-mode-activate)
(add-hook 'minibuffer-exit-hook 'xah-fly-command-mode-activate)

;; when in shell mode, switch to insertion mode.
(add-hook 'shell-mode-hook 'xah-fly-insert-mode-activate)

;; ;; when in shell mode, switch to insertion mode.
;; (add-hook 'dired-mode-hook 'xah-fly-keys-off)



;; experimental. auto switch back to command mode after some sec of idle time
;; (setq xah-fly-timer-id (run-with-idle-timer 20 t 'xah-fly-command-mode-activate))
;; (cancel-timer xah-fly-timer-id)

(define-minor-mode xah-fly-keys
  "A modal keybinding set, like vim, but based on ergonomic principles, like Dvorak layout."
  t "ξflykeys" xah-fly-key-map
  (xah-fly-command-mode-activate))

(defun xah-fly-keys-off ()
  "Turn off xah-fly-keys minor mode."
  (interactive)
  (xah-fly-keys 0))

(provide 'xah-fly-keys)

;;; xah-fly-keys.el ends here
