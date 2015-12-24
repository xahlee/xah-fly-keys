;;; xah-fly-keys.el --- A efficient modal keybinding set minor mode based on ergonomics.

;; Copyright © 2013-2015, by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Version: 2.4.5
;; Created: 10 Sep 2013
;; Keywords: convenience, emulations, vim, ergoemacs
;; Homepage: http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; xah-fly-keys is a efficient keybinding system for emacs. (more efficient than vim)

;; It is a modal mode, like vi, but key choices are based on statistics of command call frequency, and key position easy-to-press score.

;; xah-fly-keys does not bind any Control key, nor Meta keys (except 3, but you can turn off). use emacs as is, because no Control or Meta are used. Just leave xah-fly-keys in insertion mode.

;; To learn xah-fly-keys, is like learning vi for the first time. You'll need one month to adopt.

;; xah-fly-keys is currently optimized for Dvorak layout only. If you touch-type QWERTY or other, you will need to rebind keys. I recommend you fork it and modify the keys for your own use. See home page for detail: http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; --------------------------------------------------
;; MANUAL INSTALL
;; put the file xah-fly-keys.el in ~/.emacs.d/lisp/

;; put the following in your emacs init file:

;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (require 'xah-fly-keys)
;; (xah-fly-keys 1)

;; --------------------------------------------------
;; USE

;; Important commands and default keys

;; command: xah-fly-keys (【Ctrl+7】) to toggle the mode on/off.

;; It is necessary to toggle xah-fly-keys mode. Because, it gives you access to special modes that use letter keys, such as dired

;; Important command/insert mode switch keys:

;; xah-fly-command-mode-activate (key: 【home】, 【Ctrl+8】)
;; xah-fly-insert-mode-activate (when in command mode, 【SPACE】)
;; xah-fly-mode-toggle (toggle between command/insert modes. No key by default.)

;; When in command mode:
;; 【SPACE】 activates insertion mode
;; 【x】 is a leader key, for emacs one thousands commands

;; When using xah-fly-keys, you don't need to ever press Control or Meta, with the following exceptions:

;; C-c for major mode commands.
;; C-g for cancel. (i recommend you set a easy-key outside of emacs to send C-g)
;; C-q for quoted-insert. (because C-q is a efficient choice. Because the key after C-q needs Control held-down.)

;; Leader keys

;; All emacs C-x keys have a key sequence, starting with a leader key. Most commands are 2 to 3 keys, counting the leader key. For example, isearch is 【‹leader key› g】 (in Dvorak layout), switch-buffer is 【‹leader key› c g】.

;; You NEVER need to press Ctrl+x

;; globally, the leader key is the 【menu】 key. (on typical PC keyboard, it's usually at right side of space bar.) You should change this to a easy-key on YOUR keyboard. For example, make left Alt to send menu key signal in your OS or keyboard firmware.

;; When in command mode, the 【x】 is a leader key.

;; That is it. You should change the above mentioned critical keys to be ones easy to type on YOUR KEYBOARD.

;; I recommend you make a copy, and modify it, and use your modified version. Don't worry about upgrade. (I still make key tweaks every week, for the past 3 years.)

;; If you have a bug, post on github. If you have question, post on xah-fly-keys home page.

;; For detail and tutorial-like explanation, and about how to remap keys such as capslock outside of emacs, see
;; http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; If you like this project, Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.

;;; TODO
;; • 2015-09-04 make it support qwerty Keyboard layout
;; • 2015-09-04 add option to allow support of standard open close save etc keys with Control
;; • 2015-09-04 add option to allow turn off any Control completely. Same for Meta. (as a way to stop habit)


;;; Code:

(require 'dired) ; in emacs
(require 'dired-x) ; in emacs


(defvar xah-fly-command-mode-activate-hook nil "Hook for `xah-fly-command-mode-activate'")
(defvar xah-fly-insert-mode-activate-hook nil "Hook for `xah-fly-insert-mode-activate'")


;; cursor movement

(defun xah-forward-block (&optional φn)
  "Move cursor beginning of next text block.
A text block is separated by blank lines.
This command similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.
Version 2015-07-06"
  (interactive "p")
  (let ((φn (if (null φn) 1 φn)))
    (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" φn)))

(defun xah-backward-block (&optional φn)
  "Move cursor to previous text block.
See: `xah-forward-block'
Version 2015-07-08"
  (interactive "p")
  (let ((φn (if (null φn) 1 φn))
        (ξi 1))
    (while (<= ξi φn)
      (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq ξi φn)))
      (setq ξi (1+ ξi)))))

(defun xah-beginning-of-line-or-block-region (&optional φn)
  "Move cursor to beginning of line, or beginning of current or previous text block.
 (a text block is separated by blank lines)"
  (interactive "p")
  (if (use-region-p)
      (let* ((deactivate-mark nil)
             (ξp1 (region-beginning))
             (ξp2 (region-end))
             (ξtext (buffer-substring ξp1 ξp2))
             ξp3
             )
        (delete-region ξp1 ξp2)
        (setq mark-active nil)
        (xah-beginning-of-line-or-block-raw)
        (setq ξp3 (point))
        (insert ξtext)
        (set-mark ξp3))
    (xah-beginning-of-line-or-block-raw)))

(defun xah-beginning-of-line-or-block (&optional φn)
  "Move cursor to beginning of line, or beginning of current or previous text block.
 (a text block is separated by blank lines)"
  (interactive "p")
  (let ((φn (if (null φn) 1 φn)))
    (if (equal φn 1)
        (if (or (equal (point) (line-beginning-position))
                (equal last-command this-command )
                ;; (equal last-command 'xah-end-of-line-or-block )
                )
            (xah-backward-block φn)
          (beginning-of-line))
      (xah-backward-block φn))))

(defun xah-end-of-line-or-block (&optional φn)
  "Move cursor to end of line, or end of current or next text block.
 (a text block is separated by blank lines)"
  (interactive "p")
  (let ((φn (if (null φn) 1 φn)))
    (if (equal φn 1)
        (if (or (equal (point) (line-end-position))
                (equal last-command this-command )
                ;; (equal last-command 'xah-beginning-of-line-or-block )
                )
            (xah-forward-block)
          (end-of-line))
      (progn (xah-forward-block φn)))))

(defvar xah-brackets nil "string of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
;; make xah-left-brackets based on xah-brackets
  (setq xah-left-brackets '())
  (dotimes (x (- (length xah-brackets) 1))
    (when (= (% x 2) 0)
      (push (char-to-string (elt xah-brackets x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes (x (- (length xah-brackets) 1))
    (when (= (% x 2) 1)
      (push (char-to-string (elt xah-brackets x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defvar xah-punctuation-regex nil "a regex string for the purpose of jumping to punctuations in programing modes.")
(setq xah-punctuation-regex "[\\!\?\"'#$%&*+,/:;<=>@^`|~]+")

(defun xah-forward-punct (&optional φn)
  "Move cursor to the next occurrence of punctuation.
The list of punctuations to jump to is defined by `xah-punctuation-regex'"
  (interactive "p")
  (search-forward-regexp xah-punctuation-regex nil t φn))

(defun xah-backward-punct (&optional φn)
  "Move cursor to the previous occurrence of punctuation.
See `xah-forward-punct'"
  (interactive "p")
  (search-backward-regexp xah-punctuation-regex nil t φn))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (search-backward-regexp (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (search-forward-regexp (regexp-opt xah-right-brackets) nil t))

(defun xah-forward-equal-quote ()
  "Move cursor to the next occurrence of 「='」 or 「=\"」, with or without space.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-05-05"
  (interactive)
  (search-forward-regexp "=[ \n]*\\('+\\|\\\"+\\)" nil t))

(defun xah-forward-equal-sign ()
  "Move cursor to the next occurrence of equal sign 「=」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-06-15"
  (interactive)
  (search-forward-regexp "=+" nil t))

(defun xah-backward-equal-sign ()
  "Move cursor to previous occurrence of equal sign 「=」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-06-15"
  (interactive)
  (when (search-backward-regexp "=+" nil t)
    (while (search-backward "=" (- (point) 1) t)
      (left-char))))

(defun xah-forward-quote ()
  "Move cursor to the next occurrence of ' or \" or `.
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-26"
  (interactive)
  (if (search-forward-regexp "'+\\|`+\\|\\\"+" nil t)
      t
    (progn
      (message "No more quotes after.")
      nil)))

(defun xah-forward-quote-twice ()
  "Call `xah-forward-quote' twice.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-26"
  (interactive)
  (when (xah-forward-quote)
    (xah-forward-quote)))

(defun xah-backward-quote ()
  "Move cursor to the previous occurrence of '' or \" or `.
If there are consecutive quotes of the same char, keep moving until none.
Returns `t' if found, else `nil'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-26"
  (interactive)
  (if (search-backward-regexp "'+\\|`+\\|\\\"+" nil t)
      (when (char-before) ; isn't nil, at beginning of buffer
        (while (char-equal (char-before) (char-after))
          (left-char)
          t))
    (progn
      (message "No more quotes before.")
      nil)))

(defun xah-forward-dot-comma ()
  "Move cursor to the next occurrence of 「.」 「,」 「;」.
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-03-24"
  (interactive)
  (search-forward-regexp "\\.+\\|,+\\|;+" nil t))

(defun xah-backward-dot-comma ()
  "Move cursor to the previous occurrence of 「.」 「,」 「;」
URL `http://ergoemacs.org/emacs/emacs_jump_to_punctuations.html'
Version 2015-03-24"
  (interactive)
  (search-backward-regexp "\\.+\\|,+\\|;+" nil t))

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


;; text selection

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
;;   (let (ξp1 ξp2)
;;     (if current-prefix-arg
;;         (progn (setq ξp1 (point-min))
;;                (setq ξp2 (point-max)))
;;       (progn (if (use-region-p)
;;                  (progn (setq ξp1 (region-beginning))
;;                         (setq ξp2 (region-end)))
;;                (progn (setq ξp1 (line-beginning-position))
;;                       (setq ξp2 (line-end-position))))))
;;     (kill-ring-save ξp1 ξp2)
;;     (if current-prefix-arg
;;         (message "buffer text copied")
;;       (message "text copied"))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-09-18"
  (interactive)
  (let (ξp1 ξp2)
    (if current-prefix-arg
        (progn (setq ξp1 (point-min))
               (setq ξp2 (point-max)))
      (progn
        (if (use-region-p)
            (progn (setq ξp1 (region-beginning))
                   (setq ξp2 (region-end)))
          (progn (setq ξp1 (line-beginning-position))
                 (setq ξp2 (line-end-position))))))
    (if (eq last-command this-command)
        (progn
          ;; (push-mark (point) "NOMSG" "ACTIVATE")
          (kill-append "\n" nil)
          (forward-line 1)
          (end-of-line)
          (kill-append (buffer-substring-no-properties (line-beginning-position) (line-end-position)) nil)
          (message "Line copy appended"))
      (progn
        (kill-ring-save ξp1 ξp2)
        (if current-prefix-arg
            (message "Buffer text copied")
          (message "Text copied"))))))

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
URL `http://ergoemacs.org/emacs/elisp_cut_copy_yank_kill-ring.html'
Version 2015-05-06"
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copied."))

(defun xah-cut-all ()
  "Cut the whole buffer content into the `kill-ring'.
Respects `narrow-to-region'."
  (interactive)
  (kill-new (buffer-string))
  (delete-region (point-min) (point-max)))


;; editing commands

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2015-12-22"
  (interactive)
  (let (
        (deactivate-mark nil)
        ξp1 ξp2)
    (if (use-region-p)
        (setq ξp1 (region-beginning)
              ξp2 (region-end))
      (save-excursion
        (skip-chars-backward "-_[:alnum:]")
        (setq ξp1 (point))
        (skip-chars-forward "-_[:alnum:]")
        (setq ξp2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region ξp1 ξp2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region ξp1 ξp2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region ξp1 ξp2)
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

(defun xah-shrink-whitespaces-old-2015-03-03 ()
  "Remove whitespaces around cursor to just one or none.
If current line does have visible characters: shrink whitespace around cursor to just one space.
If current line does not have visible chars, then shrink all neighboring blank lines to just one.
If current line is a single space, remove that space.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2015-03-03"
  (interactive)
  (let ((pos0 (point))
        ξline-has-char-p ; current line contains non-white space chars
        ξhas-space-tab-neighbor-p
        ξwhitespace-begin ξwhitespace-end
        ξspace-or-tab-begin ξspace-or-tab-end
        )
    (save-excursion
      (setq ξhas-space-tab-neighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil))
      (beginning-of-line)
      (setq ξline-has-char-p (search-forward-regexp "[[:graph:]]" (line-end-position) t))

      (goto-char pos0)
      (skip-chars-backward "\t ")
      (setq ξspace-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq ξwhitespace-begin (point))

      (goto-char pos0)
      (skip-chars-forward "\t ")
      (setq ξspace-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq ξwhitespace-end (point)))

    (if ξline-has-char-p
        (let (ξdeleted-text)
          (when ξhas-space-tab-neighbor-p
            ;; remove all whitespaces in the range
            (setq ξdeleted-text (delete-and-extract-region ξspace-or-tab-begin ξspace-or-tab-end))
            ;; insert a whitespace only if we have removed something different than a simple whitespace
            (if (not (string= ξdeleted-text " "))
                (insert " "))))
      (progn (delete-blank-lines)))))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one or none.
Call this command again to shrink more. 3 calls will remove all whitespaces.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2015-11-04"
  (interactive)
  (let ((pos0 (point))
        ξline-has-char-p ; current line contains non-white space chars
        ξhas-space-tab-neighbor-p
        ξwhitespace-begin ξwhitespace-end
        ξspace-or-tab-begin ξspace-or-tab-end
        )
    (save-excursion
      (setq ξhas-space-tab-neighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil))
      (beginning-of-line)
      (setq ξline-has-char-p (search-forward-regexp "[[:graph:]]" (line-end-position) t))

      (goto-char pos0)
      (skip-chars-backward "\t ")
      (setq ξspace-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq ξwhitespace-begin (point))

      (goto-char pos0)
      (skip-chars-forward "\t ")
      (setq ξspace-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq ξwhitespace-end (point)))

    (if ξline-has-char-p
        (if ξhas-space-tab-neighbor-p
            (let (ξdeleted-text)
              ;; remove all whitespaces in the range
              (setq ξdeleted-text
                    (delete-and-extract-region ξspace-or-tab-begin ξspace-or-tab-end))
              ;; insert a whitespace only if we have removed something different than a simple whitespace
              (when (not (string= ξdeleted-text " "))
                (insert " ")))

          (progn
            (when (equal (char-before) 10) (delete-char -1))
            (when (equal (char-after) 10) (delete-char 1))))
      (progn (delete-blank-lines)))))

(defun xah-compact-uncompact-block ()
  "Remove or insert newline characters on the current block of text.
This is similar to a toggle for `fill-paragraph' and `unfill-paragraph'.

When there is a text selection, act on the the selection, else, act on a text block separated by blank lines.
Version 2015-06-20"
  (interactive)
  ;; This command symbol has a property “'stateIsCompact-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( ξis-compact-p
         (deactivate-mark nil)
         (ξblanks-regex "\n[ \t]*\n")
         ξp1 ξp2
         )
    (progn
      (if (use-region-p)
          (progn (setq ξp1 (region-beginning))
                 (setq ξp2 (region-end)))
        (save-excursion
          (if (re-search-backward ξblanks-regex nil "NOERROR")
              (progn (re-search-forward ξblanks-regex)
                     (setq ξp1 (point)))
            (setq ξp1 (point)))
          (if (re-search-forward ξblanks-regex nil "NOERROR")
              (progn (re-search-backward ξblanks-regex)
                     (setq ξp2 (point)))
            (setq ξp2 (point))))))
    (save-excursion
      (setq ξis-compact-p
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (progn
                (goto-char ξp1)
                (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil))))
      (if ξis-compact-p
          (fill-region ξp1 ξp2)
        (let ((fill-column most-positive-fixnum)) (fill-region ξp1 ξp2)))
      (put this-command 'stateIsCompact-p (if ξis-compact-p nil t)))))

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2015-11-28"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defun xah-unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2015-11-28"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

(defun xah-cycle-hyphen-underscore-space ()
  "Cycle {underscore, space, hypen} chars of current word or text selection.
When called repeatedly, this command cycles the {“_”, “-”, “ ”} characters, in that order.

URL `http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html'
Version 2015-08-17"
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of ξcharArray.
  (let (ξp1 ξp2)
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (let ((ξbounds (bounds-of-thing-at-point 'symbol)))
        (progn
          (setq ξp1 (car ξbounds))
          (setq ξp2 (cdr ξbounds)))))

    (let* ((ξinputText (buffer-substring-no-properties ξp1 ξp2))
           (ξcharArray ["_" "-" " "])
           (ξlength (length ξcharArray))
           (ξregionWasActive-p (region-active-p))
           (ξnowState
            (if (equal last-command this-command )
                (get 'xah-cycle-hyphen-underscore-space 'state)
              0 ))
           (ξchangeTo (elt ξcharArray ξnowState)))
      (save-excursion
        (save-restriction
          (narrow-to-region ξp1 ξp2)
          (goto-char (point-min))
          (while
              (search-forward-regexp
               (concat
                (elt ξcharArray (% (+ ξnowState 1) ξlength))
                "\\|"
                (elt ξcharArray (% (+ ξnowState 2) ξlength)))
               (point-max)
               'NOERROR)
            (replace-match ξchangeTo 'FIXEDCASE 'LITERAL))))
      (when (or (string= ξchangeTo " ") ξregionWasActive-p)
        (goto-char ξp2)
        (set-mark ξp1)
        (setq deactivate-mark nil))
      (put 'xah-cycle-hyphen-underscore-space 'state (% (+ ξnowState 1) ξlength)))))

(defun xah-underscore-to-space-region (φbegin φend)
  "Change  underscore char to space.
URL `http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html'
Version 2015-08-18"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region φbegin φend)
      (goto-char (point-min))
      (while
          (search-forward-regexp "_" (point-max) 'NOERROR)
        (replace-match " " 'FIXEDCASE 'LITERAL)))))

(defun xah-copy-file-path (&optional φdir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2015-12-02"
  (interactive "P")
  (let ((ξfpath
         (if (equal major-mode 'dired-mode)
             (expand-file-name default-directory)
           (if (null (buffer-file-name))
               (user-error "Current buffer is not associated with a file.")
             (buffer-file-name)))))
    (kill-new
     (if (null φdir-path-only-p)
         (progn
           (message "File path copied: 「%s」" ξfpath)
           ξfpath
           )
       (progn
         (message "Directory path copied: 「%s」" (file-name-directory ξfpath))
         (file-name-directory ξfpath))))))

(defun xah-delete-text-block ()
  "Delete the current text block and also put it to `kill-ring'.
Version 2015-12-08"
  (interactive)
  (let (ξp1 ξp2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq ξp1 (point)))
        (setq ξp1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "NOERROR")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq ξp2 (point)))
        (setq ξp2 (point))))
    (kill-region ξp1 ξp2)))

(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'.

URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (let (ξp1 ξp2)
    (if (region-active-p)
        (progn (setq ξp1 (region-beginning))
               (setq ξp2 (region-end)))
      (progn (setq ξp1 (line-beginning-position))
             (setq ξp2 (line-end-position))))
    (copy-to-register ?1 ξp1 ξp2)
    (message "copied to register 1: 「%s」." (buffer-substring-no-properties ξp1 ξp2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'.
URL `http://ergoemacs.org/emacs/elisp_copy-paste_register_1.html'
Version 2015-12-08"
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert-register ?1 t))

(defun xah-copy-rectangle-to-kill-ring (φbegin φend)
  "Copy region as column (rectangle region) to `kill-ring'

See also: `kill-rectangle', `copy-to-register'.
URL `http://ergoemacs.org/emacs/emacs_copy_rectangle_text_to_clipboard.html'
version 2015-11-16"
  ;; extract-rectangle suggested by YoungFrog, 2012-07-25
  (interactive "r")
  (require 'rect)
  (kill-new
   (with-temp-buffer
     (mapc (lambda (ξx) (insert ξx "\n"))
           (extract-rectangle φbegin φend))
     (delete-char -1)
     (buffer-string))))

(defun xah-upcase-sentence ()
  "Upcase sentence.
TODO 2014-09-30 command incomplete
"
  (interactive)
  (let (p1 p2)

    (if (region-active-p)
        (progn
          (setq p1 (region-beginning))
          (setq p2 (region-end)))
      (progn
        (save-excursion
          (progn
            (if (re-search-backward "\n[ \t]*\n" nil "move")
                (progn (re-search-forward "\n[ \t]*\n")
                       (setq p1 (point)))
              (setq p1 (point)))
            (if (re-search-forward "\n[ \t]*\n" nil "move")
                (progn (re-search-backward "\n[ \t]*\n")
                       (setq p2 (point)))
              (setq p2 (point)))))))

    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)

        (goto-char (point-min))
        (while (search-forward "\. \{1,2\}\\([a-z]\\)" nil t)
nil
;; (replace-match "myReplaceStr2")

)))))

(defun xah-escape-quotes (φbegin φend)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `xah-unescape-quotes'
URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2015-05-04"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
      (save-restriction
        (narrow-to-region φbegin φend)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" 'FIXEDCASE 'LITERAL)))))

(defun xah-unescape-quotes (φbegin φend)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
See also: `xah-escape-quotes'
URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2015-05-04"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region φbegin φend)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" 'FIXEDCASE 'LITERAL)))))

(defun xah-title-case-region-or-line (φbegin φend)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, φbegin φend are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2015-05-07"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           ξp1
           ξp2
           (ξskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward ξskipChars (line-beginning-position))
         (setq ξp1 (point))
         (skip-chars-forward ξskipChars (line-end-position))
         (setq ξp2 (point)))
       (list ξp1 ξp2))))
  (let* (
         (ξstrPairs [
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
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region φbegin φend)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (ξx)
             (goto-char (point-min))
             (while
                 (search-forward (aref ξx 0) nil t)
               (replace-match (aref ξx 1) 'FIXEDCASE 'LITERAL)))
           ξstrPairs))))))


;; insertion commands

(defun xah-insert-bracket-pair (φleft-bracket φright-bracket)
  "Wrap or Insert a matching bracket and place cursor in between.

If there's a text selection, wrap brackets around it. Else, smartly decide wrap or insert. (basically, if there's no char after cursor, just insert bracket pair.)

φleft-bracket ＆ φright-bracket are strings.

URL `http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html'
Version 2015-04-19"
  (if (use-region-p)
      (progn
        (let (
              (ξp1 (region-beginning))
              (ξp2 (region-end)))
          (goto-char ξp2)
          (insert φright-bracket)
          (goto-char ξp1)
          (insert φleft-bracket)
          (goto-char (+ ξp2 2))))
    (progn ; no text selection
      (if
          (or
           (looking-at "[^-_[:alnum:]]")
           (eq (point) (point-max)))
          (progn
            (insert φleft-bracket φright-bracket)
            (search-backward φright-bracket ))
        (progn
          (let (ξp1 ξp2)
            ;; basically, want all alphanumeric, plus hyphen and underscore, but don't want space or punctuations. Also want chinese.
            ;; 我有一帘幽梦，不知与谁能共。多少秘密在其中，欲诉无人能懂。
            (skip-chars-backward "-_[:alnum:]")
            (setq ξp1 (point))
            (skip-chars-forward "-_[:alnum:]")
            (setq ξp2 (point))
            (goto-char ξp2)
            (insert φright-bracket)
            (goto-char ξp1)
            (insert φleft-bracket)
            (goto-char (+ ξp2 (length φleft-bracket)))))))))

;; (insert-parentheses)

(defun xah-insert-paren () (interactive) (xah-insert-bracket-pair "(" ")") )
(defun xah-insert-square-bracket () (interactive) (xah-insert-bracket-pair "[" "]") )
(defun xah-insert-brace () (interactive) (xah-insert-bracket-pair "{" "}") )
(defun xah-insert-greater-less () (interactive) (xah-insert-bracket-pair "<" ">") )

(defun xah-insert-double-curly-quote“” () (interactive) (xah-insert-bracket-pair "“" "”") )
(defun xah-insert-curly-single-quote‘’ () (interactive) (xah-insert-bracket-pair "‘" "’") )
(defun xah-insert-single-angle-quote‹› () (interactive) (xah-insert-bracket-pair "‹" "›") )
(defun xah-insert-double-angle-quote«» () (interactive) (xah-insert-bracket-pair "«" "»") )
(defun xah-insert-ascii-double-quote () (interactive) (xah-insert-bracket-pair "\"" "\"") )
(defun xah-insert-ascii-single-quote () (interactive) (xah-insert-bracket-pair "'" "'") )
(defun xah-insert-emacs-quote () (interactive) (xah-insert-bracket-pair "`" "'") )
(defun xah-insert-corner-bracket「」 () (interactive) (xah-insert-bracket-pair "「" "」") )
(defun xah-insert-white-corner-bracket『』 () (interactive) (xah-insert-bracket-pair "『" "』") )
(defun xah-insert-angle-bracket〈〉 () (interactive) (xah-insert-bracket-pair "〈" "〉") )
(defun xah-insert-double-angle-bracket《》 () (interactive) (xah-insert-bracket-pair "《" "》") )
(defun xah-insert-white-lenticular-bracket〖〗 () (interactive) (xah-insert-bracket-pair "〖" "〗") )
(defun xah-insert-black-lenticular-bracket【】 () (interactive) (xah-insert-bracket-pair "【" "】") )
(defun xah-insert-tortoise-shell-bracket〔〕 () (interactive) (xah-insert-bracket-pair "〔" "〕") )

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

(defun xah-insert-column-counter (n)
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
  (let ((i 1) colpos )
    (setq colpos (- (point) (line-beginning-position)))
    (while (<= i n)
      (insert (number-to-string i))
      (forward-line) (beginning-of-line) (forward-char colpos)
      (setq i (1+ i)))))

(defun xah-insert-alphabets-az (&optional φuse-uppercase-p)
  "Insert letters a to z vertically.
If `universal-argument' is called first, use CAPITAL letters.

URL `http://ergoemacs.org/emacs/emacs_insert-alphabets.html'
Version 2015-11-06"
  (interactive "P")
  (let ((startChar (if φuse-uppercase-p 65 97 )))
    (dotimes (ξi 26)
      (insert (format "%c\n" (+ startChar ξi))))))

(defvar xah-unicode-list nil "Associative list of Unicode symbols. First element is a Unicode character, second element is a string used as key shortcut in `ido-completing-read'")
(setq xah-unicode-list
      '(
        ("◇" . "3" )
        ("◆" . "4" )
        ("¤" . "2" )
        ("…" . "." )
        (" " . "s" )
        ("、" . "," )
        ("•" . "8" )
        ("⭑" . "9" )
        ("🎶" . "5" )
        ("—" . "-" )
        ("＆" . "7" )
        ("↓" . "at")
        ("←" . "ah")
        ("→" . "an")
        ("↑" . "ac")
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
Version 2015-02-07"
  (interactive)
  (let (ξp1 ξp2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq ξp1 (point)))
        (setq ξp1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq ξp2 (point)))
        (setq ξp2 (point))))
    (set-mark ξp1)))

(defun xah-select-current-line ()
  "Select current line.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-02-07
"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun xah-semnav-up (φarg)
"Called by `xah-extend-selection'.

URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-11-13.
Written by Nikolaj Schumacher, 2008-10-20. Released under GPL 2"
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> φarg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (setq φarg (1- φarg) ))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (setq φarg (1+ φarg) )))
  (up-list φarg))

(defun xah-extend-selection (φarg &optional φincremental-p)
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
  (if φincremental-p
      (progn
        (xah-semnav-up (- φarg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> φarg 1)
        (xah-extend-selection (1- φarg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters here includes the following chars: \"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）
This command does not properly deal with nested brackets.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-05-16"
  (interactive)
  (let (
        (ξskipChars
         (if (boundp 'xah-brackets)
             (concat "^\"" xah-brackets)
           "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）"))
        ξp1
        ξp2
        )
    (skip-chars-backward ξskipChars)
    (setq ξp1 (point))
    (skip-chars-forward ξskipChars)
    (setq ξp2 (point))
    (set-mark ξp1)))


;; misc

(defvar xah-switch-buffer-ignore-dired t "If t, ignore dired buffer when calling `xah-next-user-buffer' or `xah-previous-user-buffer'")
(setq xah-switch-buffer-ignore-dired t)

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
 “user buffer” is a buffer whose name does not start with “*”.
If `xah-switch-buffer-ignore-dired' is true, also skip directory buffer.
2015-01-05 URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (or
           (string-equal "*" (substring (buffer-name) 0 1))
           (if (string-equal major-mode "dired-mode")
               xah-switch-buffer-ignore-dired
             nil
             ))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
 “user buffer” is a buffer whose name does not start with “*”.
If `xah-switch-buffer-ignore-dired' is true, also skip directory buffer.
2015-01-05 URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (or
           (string-equal "*" (substring (buffer-name) 0 1))
           (if (string-equal major-mode "dired-mode")
               xah-switch-buffer-ignore-dired
             nil
             ))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
 (buffer name that starts with “*”)"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
 (buffer name that starts with “*”)"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer))))

(defvar xah-recently-closed-buffers nil "alist of recently closed buffers. Each element is (buffer name, file path). The max number to track is controlled by the variable `xah-recently-closed-buffers-max'.")

(defvar xah-recently-closed-buffers-max 40 "The maximum length for `xah-recently-closed-buffers'.")

(defun xah-close-current-buffer ()
  "Close the current buffer.

Similar to `kill-buffer', with the following addition:

• Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• Make sure the buffer shown after closing is a user buffer.
• If the buffer is editing a source file in an org-mode file, prompt the user to save before closing.
• If the buffer is a file, add the path to the list `xah-recently-closed-buffers'.
• If it is the minibuffer, exit the minibuffer

A emacs buffer is one whose name starts with *.
Else it is a user buffer."
  (interactive)
  (let (ξemacs-buff-p
        (ξorg-p (string-match "^*Org Src" (buffer-name))))

    (setq ξemacs-buff-p (if (string-match "^*" (buffer-name)) t nil))

    (if (string= major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit) ; if the buffer is minibuffer
      (progn
        ;; offer to save buffers that are non-empty and modified, even for non-file visiting buffer. (because kill-buffer does not offer to save buffers that are not associated with files)
        (when (and (buffer-modified-p)
                   (not ξemacs-buff-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        (when (and (buffer-modified-p)
                   ξorg-p)
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
        (kill-buffer (current-buffer))

        ;; if emacs buffer, switch to a user buffer
        (when (string-match "^*" (buffer-name))
          (next-buffer)
          (let ((i 0))
            (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
              (setq i (1+ i)) (next-buffer))))))))

(defun xah-open-last-closed ()
  "Open the last closed file."
  (interactive)
  (if (> (length xah-recently-closed-buffers) 0)
      (find-file (cdr (pop xah-recently-closed-buffers)))
    (progn (message "No recently close buffer in this session."))))

(defun xah-open-recently-closed ()
  "Open recently closed file.
Prompt for a choice."
  (interactive)
  (find-file (ido-completing-read "open:" (mapcar (lambda (f) (cdr f)) xah-recently-closed-buffers))))

(defun xah-list-recently-closed ()
  "List recently closed file."
  (interactive)
  (let ((ξbuf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer ξbuf)
    (mapc (lambda (ξf) (insert (cdr ξf) "\n"))
          xah-recently-closed-buffers)))

(defun xah-new-empty-buffer ()
  "Open a new empty buffer.
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2015-06-12"
  (interactive)
  (let ((ξbuf (generate-new-buffer "untitled")))
    (switch-to-buffer ξbuf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)))

;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.

(defun xah-delete-current-file-make-backup (&optional φno-backup-p)
  "Delete current file, makes a backup~, closes the buffer.

Backup filename is “‹name›~‹date time stamp›~”. Existing file of the same name is overwritten. If the file is not associated with buffer, the backup file name starts with “xx_”.

When called with `universal-argument', don't create backup.

URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2015-05-26"
  (interactive "P")
  (let* (
         (ξfname (buffer-file-name))
         (ξbuffer-is-file-p ξfname)
         (ξbackup-suffix (concat "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
    (if ξbuffer-is-file-p
        (progn
          (save-buffer ξfname)
          (when (not φno-backup-p)
            (copy-file
             ξfname
             (concat ξfname ξbackup-suffix)
             t))
          (delete-file ξfname)
          (message "Deleted. Backup created at 「%s」." (concat ξfname ξbackup-suffix)))
      (when (not φno-backup-p)
        (widen)
        (write-region (point-min) (point-max) (concat "xx" ξbackup-suffix))
        (message "Backup created at 「%s」." (concat "xx" ξbackup-suffix))))
    (kill-buffer (current-buffer))))

(defun xah-delete-current-file-copy-to-kill-ring ()
  "Delete current buffer/file and close the buffer, push content to `kill-ring'.
URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2015-08-12"
  (interactive)
  (progn
    (kill-new (buffer-string))
    (message "Buffer content copied to kill-ring.")
    (when (buffer-file-name)
      (when (file-exists-p (buffer-file-name))
        (progn
          (delete-file (buffer-file-name))
          (message "Deleted: 「%s」." (buffer-file-name)))))
    (let ((buffer-offer-save nil))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(defun xah-delete-current-file (&optional φno-backup-p)
  "Delete current buffer/file and close the buffer.
If buffer is a file, makes a backup~, else, push file content to `kill-ring'.

The backup filename is “‹filename›~‹date time stamp›~”. Existing file of the same name is overwritten. If the file is not associated with buffer, the backup file name starts with “xx_”.

URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2015-09-02"
  (interactive "P")
  (progn
    (if (buffer-file-name)
        (xah-delete-current-file-make-backup φno-backup-p)
      (xah-delete-current-file-copy-to-kill-ring))))

(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2015-10-08"
  (interactive)
  (let (
         (ξsuffix-map
          ;; (‹extension› . ‹shell program name›)
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("js" . "node") ; node.js
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

         ξfname
         ξfSuffix
         ξprog-name
         ξcmd-str)

    (when (null (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))

    (setq ξfname (buffer-file-name))
    (setq ξfSuffix (file-name-extension ξfname))
    (setq ξprog-name (cdr (assoc ξfSuffix ξsuffix-map)))
    (setq ξcmd-str (concat ξprog-name " \""   ξfname "\""))

    (cond
     ((string-equal ξfSuffix "el") (load ξfname))
     ((string-equal ξfSuffix "java")
      (progn
        (shell-command ξcmd-str "*xah-run-current-file output*" )
        (shell-command
         (format "java %s" (file-name-sans-extension (file-name-nondirectory ξfname))))))
     (t (if ξprog-name
            (progn
              (message "Running…")
              (shell-command ξcmd-str "*xah-run-current-file output*" ))
          (message "No recognized program file suffix for this file."))))))

(defun xah-search-current-word ()
  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (interactive)
  (let ( ξp1 ξp2 )
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (save-excursion
        (skip-chars-backward "-_A-Za-z0-9")
        (setq ξp1 (point))
        (right-char)
        (skip-chars-forward "-_A-Za-z0-9")
        (setq ξp2 (point))))
    (setq mark-active nil)
    (when (< ξp1 (point))
      (goto-char ξp1))
    (isearch-mode t)
    (isearch-yank-string (buffer-substring-no-properties ξp1 ξp2))))

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
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    )))

(defun xah-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2015-01-26"
  (interactive)
  (let* (
         (ξfile-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         (ξdo-it-p (if (<= (length ξfile-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when ξdo-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda (ξfpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" ξfpath t t))) ξfile-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda (ξfpath) (shell-command (format "open \"%s\"" ξfpath)))  ξfile-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda (ξfpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" ξfpath))) ξfile-list))))))

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
If current frame has only one window, switch to next frame."
  (interactive)
  (if (one-window-p)
      (other-frame 1)
    (other-window 1)))


;; keymaps

(defvar xah-fly-swapped-1827-p nil "Boolean. If true, it means keys 1 and 8 are swapped, and 2 and 7 are swapped. See: http://xahlee.info/kbd/best_number_key_layout.html")

(defvar xah-fly-key-map nil "Keybinding for `xah-fly-keys' minor mode.")
(progn
  (setq xah-fly-key-map (make-sparse-keymap))

  (define-key xah-fly-key-map (kbd "'") 'self-insert-command)

  (define-key xah-fly-key-map (kbd "<C-next>") 'xah-next-user-buffer)
  (define-key xah-fly-key-map (kbd "<C-prior>") 'xah-previous-user-buffer))

(progn
  (define-prefix-command 'xah-highlight-keymap) ; commands in search-map and facemenu-keymap
  (define-key xah-highlight-keymap (kbd ".") 'isearch-forward-symbol-at-point)
  (define-key xah-highlight-keymap (kbd "b") 'facemenu-set-bold)
  (define-key xah-highlight-keymap (kbd "f") 'font-lock-fontify-block)
  (define-key xah-highlight-keymap (kbd "c") 'center-line)
  (define-key xah-highlight-keymap (kbd "d") 'facemenu-set-default)
  (define-key xah-highlight-keymap (kbd "h .") 'highlight-symbol-at-point)
  (define-key xah-highlight-keymap (kbd "h f") 'hi-lock-find-patterns)
  (define-key xah-highlight-keymap (kbd "h l") 'highlight-lines-matching-regexp)
  (define-key xah-highlight-keymap (kbd "h p") 'highlight-phrase)
  (define-key xah-highlight-keymap (kbd "h r") 'highlight-regexp)
  (define-key xah-highlight-keymap (kbd "h u") 'unhighlight-regexp)
  (define-key xah-highlight-keymap (kbd "h w") 'hi-lock-write-interactive-patterns)
  (define-key xah-highlight-keymap (kbd "i") 'facemenu-set-italic)
  (define-key xah-highlight-keymap (kbd "l") 'facemenu-set-bold-italic)
  (define-key xah-highlight-keymap (kbd "o") 'facemenu-set-face)
  (define-key xah-highlight-keymap (kbd "p") 'center-paragraph)
  (define-key xah-highlight-keymap (kbd "s") 'isearch-forward-symbol)
  (define-key xah-highlight-keymap (kbd "u") 'facemenu-set-underline)
  (define-key xah-highlight-keymap (kbd "w") 'isearch-forward-word)
  )

(progn
  (define-prefix-command 'xah-leader-tab-keymap)

  (define-key xah-leader-tab-keymap (kbd "TAB") 'indent-for-tab-command)

  (define-key xah-leader-tab-keymap (kbd "i") 'complete-symbol)
  (define-key xah-leader-tab-keymap (kbd "g") 'indent-rigidly)
  (define-key xah-leader-tab-keymap (kbd "r") 'indent-region)
  (define-key xah-leader-tab-keymap (kbd "s") 'indent-sexp)

  (define-key xah-leader-tab-keymap (kbd "e") nil)
  (define-key xah-leader-tab-keymap (kbd "e '") 'abbrev-prefix-mark)
  (define-key xah-leader-tab-keymap (kbd "e e") 'edit-abbrevs)
  (define-key xah-leader-tab-keymap (kbd "e p") 'expand-abbrev)
  (define-key xah-leader-tab-keymap (kbd "e r") 'expand-region-abbrevs)
  (define-key xah-leader-tab-keymap (kbd "e u") 'unexpand-abbrev)
  (define-key xah-leader-tab-keymap (kbd "e g") 'add-global-abbrev)
  (define-key xah-leader-tab-keymap (kbd "e a") 'add-mode-abbrev)
  (define-key xah-leader-tab-keymap (kbd "e v") 'inverse-add-global-abbrev)
  (define-key xah-leader-tab-keymap (kbd "e l") 'inverse-add-mode-abbrev)
  (define-key xah-leader-tab-keymap (kbd "e n") 'expand-jump-to-next-slot)
  (define-key xah-leader-tab-keymap (kbd "e p") 'expand-jump-to-previous-slot))



(progn
  (define-prefix-command 'xah-leader-c-keymap)

  (define-key xah-leader-c-keymap (kbd ",") 'xah-open-in-external-app)
  (define-key xah-leader-c-keymap (kbd ".") 'find-file)
  (define-key xah-leader-c-keymap (kbd "c") 'bookmark-bmenu-list)
  (define-key xah-leader-c-keymap (kbd "e") nil)
  (define-key xah-leader-c-keymap (kbd "g") 'ido-switch-buffer)
  (define-key xah-leader-c-keymap (kbd "h") 'recentf-open-files)
  (define-key xah-leader-c-keymap (kbd "l") 'bookmark-set)
  (define-key xah-leader-c-keymap (kbd "n") 'xah-new-empty-buffer)
  (define-key xah-leader-c-keymap (kbd "o") 'xah-open-in-desktop)
  (define-key xah-leader-c-keymap (kbd "p") 'xah-open-last-closed)
  (define-key xah-leader-c-keymap (kbd "f") 'xah-open-recently-closed)
  (define-key xah-leader-c-keymap (kbd "y") 'xah-list-recently-closed)
  (define-key xah-leader-c-keymap (kbd "r") 'bookmark-jump)
  (define-key xah-leader-c-keymap (kbd "s") 'write-file)
  (define-key xah-leader-c-keymap (kbd "t") 'ibuffer)
  (define-key xah-leader-c-keymap (kbd "u") nil))

(progn
  (define-prefix-command 'xah-help-keymap)

  (define-key xah-help-keymap (kbd ";") 'Info-goto-emacs-command-node)

  (define-key xah-help-keymap (kbd "8") nil)
  (define-key xah-help-keymap (kbd "7") nil)
  (define-key xah-help-keymap (kbd "3") 'man)
  (define-key xah-help-keymap (kbd "4") 'elisp-index-search)
  (define-key xah-help-keymap (kbd "5") 'apropos-variable)
  (define-key xah-help-keymap (kbd "6") 'apropos-value)

  (define-key xah-help-keymap (kbd "a") 'apropos-command)
  (define-key xah-help-keymap (kbd "b") 'describe-bindings)
  (define-key xah-help-keymap (kbd "c") 'describe-char)
  (define-key xah-help-keymap (kbd "d") 'apropos-documentation)
  (define-key xah-help-keymap (kbd "e") 'view-echo-area-messages)
  (define-key xah-help-keymap (kbd "f") 'describe-function)
  (define-key xah-help-keymap (kbd "g") nil)
  (define-key xah-help-keymap (kbd "h") 'describe-face)
  (define-key xah-help-keymap (kbd "i") 'info)
  (define-key xah-help-keymap (kbd "j") nil)
  (define-key xah-help-keymap (kbd "k") 'describe-key)
  (define-key xah-help-keymap (kbd "K") 'Info-goto-emacs-key-command-node)
  (define-key xah-help-keymap (kbd "l") 'view-lossage)
  (define-key xah-help-keymap (kbd "m") 'describe-mode)
  (define-key xah-help-keymap (kbd "n") 'describe-input-method)
  (define-key xah-help-keymap (kbd "o") 'describe-language-environment)
  (define-key xah-help-keymap (kbd "p") 'finder-by-keyword)
  (define-key xah-help-keymap (kbd "q") nil)
  (define-key xah-help-keymap (kbd "r") nil)
  (define-key xah-help-keymap (kbd "s") 'describe-syntax)
  (define-key xah-help-keymap (kbd "t") 'info-lookup-symbol)
  (define-key xah-help-keymap (kbd "u") nil)
  (define-key xah-help-keymap (kbd "v") 'describe-variable)
  (define-key xah-help-keymap (kbd "w") nil)
  (define-key xah-help-keymap (kbd "x") nil)
  (define-key xah-help-keymap (kbd "y") nil)
  (define-key xah-help-keymap (kbd "z") 'describe-coding-system)

  )

(progn
  (define-prefix-command 'xah-leader-i-keymap) ; commands in goto-map
  (define-key xah-leader-i-keymap (kbd "TAB") 'move-to-column)
  (define-key xah-leader-i-keymap (kbd "c") 'goto-char)
  (define-key xah-leader-i-keymap (kbd "g") 'goto-line)
  (define-key xah-leader-i-keymap (kbd "n") 'next-error)
  (define-key xah-leader-i-keymap (kbd "p") 'previous-error))

(progn
  ;; commands here are harmless (safe). They don't modify text.
  ;; they turn on minor/major mode, change display, prompt, start shell, etc.
  (define-prefix-command 'xah-harmless-keymap)

  (define-key xah-harmless-keymap (kbd "SPC") nil)
  (define-key xah-harmless-keymap (kbd "'") 'frame-configuration-to-register)
  (define-key xah-harmless-keymap (kbd ";") 'window-configuration-to-register)

  (if xah-fly-swapped-1827-p
      (progn
        (define-key xah-harmless-keymap (kbd "8") 'set-input-method)
        (define-key xah-harmless-keymap (kbd "7") 'global-hl-line-mode)
        (define-key xah-harmless-keymap (kbd "2") 'calc)
        (define-key xah-harmless-keymap (kbd "1") 'shell))
    (progn
      (define-key xah-harmless-keymap (kbd "1") 'set-input-method)
      (define-key xah-harmless-keymap (kbd "2") 'global-hl-line-mode)
      (define-key xah-harmless-keymap (kbd "7") 'calc)
      (define-key xah-harmless-keymap (kbd "8") 'shell)))

  (define-key xah-harmless-keymap (kbd "3") 'whitespace-mode)
  (define-key xah-harmless-keymap (kbd "4") 'linum-mode)
  (define-key xah-harmless-keymap (kbd "5") 'visual-line-mode)

  (define-key xah-harmless-keymap (kbd "6") 'calendar)
  (define-key xah-harmless-keymap (kbd "9") 'shell-command)
  (define-key xah-harmless-keymap (kbd "0") 'shell-command-on-region)

  (define-key xah-harmless-keymap (kbd "a") 'text-scale-adjust)
  (define-key xah-harmless-keymap (kbd "b") 'toggle-debug-on-error)
  (define-key xah-harmless-keymap (kbd "c") 'toggle-case-fold-search)
  (define-key xah-harmless-keymap (kbd "d") 'narrow-to-page)
  (define-key xah-harmless-keymap (kbd "e") 'eshell)
  (define-key xah-harmless-keymap (kbd "f") nil)
  (define-key xah-harmless-keymap (kbd "g") 'toggle-frame-fullscreen)
  (define-key xah-harmless-keymap (kbd "h") 'widen)
  (define-key xah-harmless-keymap (kbd "i") 'make-frame-command)
  (define-key xah-harmless-keymap (kbd "j") nil)
  (define-key xah-harmless-keymap (kbd "k") 'menu-bar-open)
  (define-key xah-harmless-keymap (kbd "l") 'toggle-word-wrap)
  (define-key xah-harmless-keymap (kbd "m") 'global-linum-mode)
  (define-key xah-harmless-keymap (kbd "n") 'narrow-to-region)
  (define-key xah-harmless-keymap (kbd "o") nil)
  (define-key xah-harmless-keymap (kbd "p") 'read-only-mode) ; toggle-read-only
  (define-key xah-harmless-keymap (kbd "q") nil)
  (define-key xah-harmless-keymap (kbd "q n") 'set-file-name-coding-system)
  (define-key xah-harmless-keymap (kbd "q s") 'set-next-selection-coding-system)
  (define-key xah-harmless-keymap (kbd "q c") 'universal-coding-system-argument)
  (define-key xah-harmless-keymap (kbd "q f") 'set-buffer-file-coding-system)
  (define-key xah-harmless-keymap (kbd "q k") 'set-keyboard-coding-system)
  (define-key xah-harmless-keymap (kbd "q l") 'set-language-environment)
  (define-key xah-harmless-keymap (kbd "q p") 'set-buffer-process-coding-system)
  (define-key xah-harmless-keymap (kbd "q r") 'revert-buffer-with-coding-system)
  (define-key xah-harmless-keymap (kbd "q t") 'set-terminal-coding-system)
  (define-key xah-harmless-keymap (kbd "q x") 'set-selection-coding-system)
  (define-key xah-harmless-keymap (kbd "r") ctl-x-5-map)

;; <menu> n r C-o  display-buffer-other-frame
;; <menu> n r .    find-tag-other-frame
;; <menu> n r 0    delete-frame
;; <menu> n r 1    delete-other-frames
;; <menu> n r 2    make-frame-command
;; <menu> n r b    switch-to-buffer-other-frame
;; <menu> n r d    dired-other-frame
;; <menu> n r f    find-file-other-frame
;; <menu> n r m    compose-mail-other-frame
;; <menu> n r o    other-frame
;; <menu> n r r    find-file-read-only-other-frame

  (define-key xah-harmless-keymap (kbd "s") 'flyspell-buffer)
  (define-key xah-harmless-keymap (kbd "t") 'narrow-to-defun)
  (define-key xah-harmless-keymap (kbd "u") nil)
  (define-key xah-harmless-keymap (kbd "v") 'variable-pitch-mode)
  (define-key xah-harmless-keymap (kbd "w") 'eww)
  (define-key xah-harmless-keymap (kbd "x") 'save-some-buffers)
  (define-key xah-harmless-keymap (kbd "y") 'nil)
  (define-key xah-harmless-keymap (kbd "z") 'abort-recursive-edit)
  )

(progn
  ;; kinda replacement related
  (define-prefix-command 'xah-edit-cmds-keymap)

  (if xah-fly-swapped-1827-p
      (progn
        (define-key xah-edit-cmds-keymap (kbd "8") nil)
        (define-key xah-edit-cmds-keymap (kbd "7") nil)
        (define-key xah-edit-cmds-keymap (kbd "2") 'list-matching-lines)
        (define-key xah-edit-cmds-keymap (kbd "1") 'delete-matching-lines))
    (progn
      (define-key xah-edit-cmds-keymap (kbd "1") nil)
      (define-key xah-edit-cmds-keymap (kbd "2") nil)
      (define-key xah-edit-cmds-keymap (kbd "7") 'list-matching-lines)
      (define-key xah-edit-cmds-keymap (kbd "8") 'delete-matching-lines)))

  (define-key xah-edit-cmds-keymap (kbd "3") 'apply-macro-to-region-lines)
  (define-key xah-edit-cmds-keymap (kbd "4") 'sort-lines)
  (define-key xah-edit-cmds-keymap (kbd "5") 'sort-numeric-fields)
  (define-key xah-edit-cmds-keymap (kbd "6") 'reverse-region)
  (define-key xah-edit-cmds-keymap (kbd "9") 'delete-non-matching-lines)
  (define-key xah-edit-cmds-keymap (kbd "0") 'delete-duplicate-lines)

  (define-key xah-edit-cmds-keymap (kbd ".") 'kmacro-start-macro)
  (define-key xah-edit-cmds-keymap (kbd "p") 'kmacro-end-macro)
  (define-key xah-edit-cmds-keymap (kbd "e") 'call-last-kbd-macro)

  (define-key xah-edit-cmds-keymap (kbd "c") 'replace-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "d") 'delete-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "g") 'kill-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "l") 'clear-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "n") 'rectangle-number-lines)
  (define-key xah-edit-cmds-keymap (kbd "o") 'open-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "r") 'yank-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "t") 'rectangle-mark-mode)
  (define-key xah-edit-cmds-keymap (kbd "y") 'delete-whitespace-rectangle))

(progn
  (define-prefix-command 'xah-leader-t-keymap)

  (if xah-fly-swapped-1827-p
      (progn
        (define-key xah-leader-t-keymap (kbd "1") 'mark-defun))
    (progn
        (define-key xah-leader-t-keymap (kbd "8") 'mark-defun)))

  (define-key xah-leader-t-keymap (kbd "SPC") 'pop-global-mark)
  (define-key xah-leader-t-keymap (kbd "3") 'point-to-register)
  (define-key xah-leader-t-keymap (kbd "4") 'jump-to-register)

  (define-key xah-leader-t-keymap (kbd ".") nil)
  (define-key xah-leader-t-keymap (kbd ",") nil)

  (define-key xah-leader-t-keymap (kbd "a") nil)
  (define-key xah-leader-t-keymap (kbd "b") nil)
  (define-key xah-leader-t-keymap (kbd "c") nil)
  (define-key xah-leader-t-keymap (kbd "d") nil)
  (define-key xah-leader-t-keymap (kbd "e") 'toggle-input-method)
  (define-key xah-leader-t-keymap (kbd "f") nil)
  (define-key xah-leader-t-keymap (kbd "g") nil)
  (define-key xah-leader-t-keymap (kbd "h") (if (fboundp 'xah-close-current-buffer)
                                                (progn 'xah-close-current-buffer)
                                              (progn 'kill-buffer)))
  (define-key xah-leader-t-keymap (kbd "i") nil)
  (define-key xah-leader-t-keymap (kbd "j") 'copy-to-register)
  (define-key xah-leader-t-keymap (kbd "k") 'insert-register)
  (define-key xah-leader-t-keymap (kbd "l") 'increment-register)
  (define-key xah-leader-t-keymap (kbd "m") nil)
  (define-key xah-leader-t-keymap (kbd "n") 'repeat-complex-command)
  (define-key xah-leader-t-keymap (kbd "o") nil)
  (define-key xah-leader-t-keymap (kbd "p") 'query-replace-regexp)
  (define-key xah-leader-t-keymap (kbd "q") nil)
  (define-key xah-leader-t-keymap (kbd "r") 'copy-rectangle-to-register)
  (define-key xah-leader-t-keymap (kbd "s") nil)
  (define-key xah-leader-t-keymap (kbd "t") 'repeat)
  (define-key xah-leader-t-keymap (kbd "u") nil)
  (define-key xah-leader-t-keymap (kbd "v") nil)
  (define-key xah-leader-t-keymap (kbd "w") 'xah-next-window-or-frame)
  (define-key xah-leader-t-keymap (kbd "x") nil)
  (define-key xah-leader-t-keymap (kbd "y") nil)
  (define-key xah-leader-t-keymap (kbd "z") 'number-to-register))

(progn
  (define-prefix-command 'xah-leader-vc-keymap)

  (define-key xah-leader-vc-keymap (kbd "+") 'vc-update)
  (define-key xah-leader-vc-keymap (kbd "=") 'vc-diff)
  (define-key xah-leader-vc-keymap (kbd "D") 'vc-root-diff)
  (define-key xah-leader-vc-keymap (kbd "L") 'vc-print-root-log)
  (define-key xah-leader-vc-keymap (kbd "a") 'vc-update-change-log)
  (define-key xah-leader-vc-keymap (kbd "b") 'vc-switch-backend)
  (define-key xah-leader-vc-keymap (kbd "c") 'vc-rollback)
  (define-key xah-leader-vc-keymap (kbd "d") 'vc-dir)
  (define-key xah-leader-vc-keymap (kbd "g") 'vc-annotate)
  (define-key xah-leader-vc-keymap (kbd "h") 'vc-insert-headers)
  (define-key xah-leader-vc-keymap (kbd "l") 'vc-print-log)
  (define-key xah-leader-vc-keymap (kbd "m") 'vc-merge)
  (define-key xah-leader-vc-keymap (kbd "r") 'vc-retrieve-tag)
  (define-key xah-leader-vc-keymap (kbd "s") 'vc-create-tag)
  (define-key xah-leader-vc-keymap (kbd "u") 'vc-revert)
  (define-key xah-leader-vc-keymap (kbd "v") 'vc-next-action)
  (define-key xah-leader-vc-keymap (kbd "~") 'vc-revision-other-window))

(progn
  (define-prefix-command 'xah-danger-keymap)

  (define-key xah-danger-keymap (kbd ".") 'eval-buffer)
  (define-key xah-danger-keymap (kbd "e") 'eval-defun)
  (define-key xah-danger-keymap (kbd "m") 'eval-last-sexp)
  (define-key xah-danger-keymap (kbd "p") 'eval-expression)
  (define-key xah-danger-keymap (kbd "u") 'eval-region)
  (define-key xah-danger-keymap (kbd "q") 'save-buffers-kill-terminal)
  (define-key xah-danger-keymap (kbd "w") 'delete-frame)
  (define-key xah-danger-keymap (kbd "j") 'xah-run-current-file))

(progn
  (define-prefix-command 'xah-insertion-keymap)

  (define-key xah-insertion-keymap (kbd "RET") 'insert-char)

  (define-key xah-insertion-keymap (kbd ",") nil)

  (define-key xah-insertion-keymap (kbd "a") nil)
  (define-key xah-insertion-keymap (kbd "b") 'xah-insert-black-lenticular-bracket【】)
  (define-key xah-insertion-keymap (kbd "c") 'xah-insert-ascii-single-quote)
  (define-key xah-insertion-keymap (kbd "d") 'xah-insert-double-curly-quote“”)
  (define-key xah-insertion-keymap (kbd "e") 'xah-insert-unicode)
  (define-key xah-insertion-keymap (kbd "f") 'xah-insert-emacs-quote)
  (define-key xah-insertion-keymap (kbd "g") 'xah-insert-ascii-double-quote)
  (define-key xah-insertion-keymap (kbd "h") 'xah-insert-brace) ; {}
  (define-key xah-insertion-keymap (kbd "i") 'xah-insert-curly-single-quote‘’)
  (define-key xah-insertion-keymap (kbd "j") nil)
  (define-key xah-insertion-keymap (kbd "k") nil)
  (define-key xah-insertion-keymap (kbd "m") 'xah-insert-corner-bracket「」)
  (define-key xah-insertion-keymap (kbd "n") 'xah-insert-square-bracket) ; []
  (define-key xah-insertion-keymap (kbd "o") nil)
  (define-key xah-insertion-keymap (kbd "p") 'xah-insert-single-angle-quote‹›)
  (define-key xah-insertion-keymap (kbd "q") nil)
  (define-key xah-insertion-keymap (kbd "r") 'xah-insert-tortoise-shell-bracket〔〕)
  (define-key xah-insertion-keymap (kbd "s") 'xah-insert-string-assignment)
  (define-key xah-insertion-keymap (kbd "t") 'xah-insert-paren)
  (define-key xah-insertion-keymap (kbd "u") 'xah-insert-greater-less)
  (define-key xah-insertion-keymap (kbd "v") nil)
  (define-key xah-insertion-keymap (kbd "w") 'xah-insert-angle-bracket〈〉)
  (define-key xah-insertion-keymap (kbd "W") 'xah-insert-double-angle-bracket《》)
  (define-key xah-insertion-keymap (kbd "x") nil)
  (define-key xah-insertion-keymap (kbd "y") 'xah-insert-double-angle-quote«»))

(progn
  (define-prefix-command 'xah-fly-leader-key-map)
  (define-key xah-fly-leader-key-map (kbd "RET") (if (fboundp 'smex) 'smex 'execute-extended-command ))
  (define-key xah-fly-leader-key-map (kbd "SPC") xah-insertion-keymap)
  (define-key xah-fly-leader-key-map (kbd "TAB") xah-leader-tab-keymap)
  (define-key xah-fly-leader-key-map (kbd "<end>") 'xah-fly-keys)
  (define-key xah-fly-leader-key-map (kbd "DEL") 'xah-delete-current-file)

  (define-key xah-fly-leader-key-map (kbd "<mouse-1>") 'xah-set-mouse-wheel-mode) ; left button
  (define-key xah-fly-leader-key-map (kbd "<mouse-3>") 'xah-set-mouse-scroll-by-50-line) ; right button
  (define-key xah-fly-leader-key-map (kbd "<mouse-4>") 'xah-set-mouse-wheel-normal) ; wheel up
  (define-key xah-fly-leader-key-map (kbd "<mouse-5>") 'xah-set-mouse-scroll-by-block) ; wheel down

  (define-key xah-fly-leader-key-map (kbd ".") xah-highlight-keymap)

  (define-key xah-fly-leader-key-map (kbd "'") 'quoted-insert)
  (define-key xah-fly-leader-key-map (kbd ",") nil)
  (define-key xah-fly-leader-key-map (kbd "-") nil)
  (define-key xah-fly-leader-key-map (kbd "/") nil)
  (define-key xah-fly-leader-key-map (kbd ";") nil)
  (define-key xah-fly-leader-key-map (kbd "=") nil)
  (define-key xah-fly-leader-key-map (kbd "[") nil)
  (define-key xah-fly-leader-key-map (kbd "\\") nil)
  (define-key xah-fly-leader-key-map (kbd "`") nil)

  (if xah-fly-swapped-1827-p
      (progn
        (define-key xah-fly-leader-key-map (kbd "8") nil)
        (define-key xah-fly-leader-key-map (kbd "7") nil)
        (define-key xah-fly-leader-key-map (kbd "2") 'dired-jump)
        (define-key xah-fly-leader-key-map (kbd "1") 'find-file-at-point))
    (progn
      (define-key xah-fly-leader-key-map (kbd "1") nil)
      (define-key xah-fly-leader-key-map (kbd "2") nil)
      (define-key xah-fly-leader-key-map (kbd "7") 'dired-jump)
      (define-key xah-fly-leader-key-map (kbd "8") 'find-file-at-point)))

  (define-key xah-fly-leader-key-map (kbd "3") nil)
  (define-key xah-fly-leader-key-map (kbd "4") 'split-window-right)
  (define-key xah-fly-leader-key-map (kbd "5") nil)

  (define-key xah-fly-leader-key-map (kbd "6") nil)
  (define-key xah-fly-leader-key-map (kbd "9") 'ispell-word)
  (define-key xah-fly-leader-key-map (kbd "0") nil)

  (define-key xah-fly-leader-key-map (kbd "a") 'mark-whole-buffer)
  (define-key xah-fly-leader-key-map (kbd "b") 'end-of-buffer)
  (define-key xah-fly-leader-key-map (kbd "c") xah-leader-c-keymap)
  (define-key xah-fly-leader-key-map (kbd "d") 'beginning-of-buffer)
  (define-key xah-fly-leader-key-map (kbd "f") 'xah-search-current-word)
  (define-key xah-fly-leader-key-map (kbd "g") nil)
  (define-key xah-fly-leader-key-map (kbd "h") 'xah-help-keymap)
  (define-key xah-fly-leader-key-map (kbd "i") xah-leader-i-keymap)
  (define-key xah-fly-leader-key-map (kbd "j") 'xah-cut-all-or-region)
  (define-key xah-fly-leader-key-map (kbd "k") 'yank)
  (define-key xah-fly-leader-key-map (kbd "l") 'recenter-top-bottom)
  (define-key xah-fly-leader-key-map (kbd "m") 'universal-argument)
  (define-key xah-fly-leader-key-map (kbd "n") xah-harmless-keymap)
  (define-key xah-fly-leader-key-map (kbd "o") nil)
  (define-key xah-fly-leader-key-map (kbd "p") 'query-replace)
  (define-key xah-fly-leader-key-map (kbd "q") 'xah-copy-all-or-region)
  (define-key xah-fly-leader-key-map (kbd "r") xah-edit-cmds-keymap)
  (define-key xah-fly-leader-key-map (kbd "s") nil)
  (define-key xah-fly-leader-key-map (kbd "t") xah-leader-t-keymap)
  (define-key xah-fly-leader-key-map (kbd "u") nil)
  (define-key xah-fly-leader-key-map (kbd "v") xah-leader-vc-keymap)
  (define-key xah-fly-leader-key-map (kbd "w") xah-danger-keymap)
  (define-key xah-fly-leader-key-map (kbd "x") 'exchange-point-and-mark)
  (define-key xah-fly-leader-key-map (kbd "y") nil)
  (define-key xah-fly-leader-key-map (kbd "z") 'comment-dwim))


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

;; ;; 2013-11-04 make emacs auto show suggestions when a prefix key is pressed
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence '("<menu> t" "TAB t" ))
;; (guide-key-mode 1)

;; setting keys

(global-set-key (kbd "<menu>") 'xah-fly-leader-key-map)
(global-set-key (kbd "<home>") 'xah-fly-command-mode-activate)

(if xah-fly-swapped-1827-p
    (progn
      (global-set-key (kbd "C-2") 'xah-fly-keys)
      (global-set-key (kbd "C-1") 'xah-fly-command-mode-activate))
  (progn
    (global-set-key (kbd "C-7") 'xah-fly-keys)
    (global-set-key (kbd "C-8") 'xah-fly-command-mode-activate)))

(global-set-key (kbd "<f11>") 'xah-previous-user-buffer)
(global-set-key (kbd "<f12>") 'xah-next-user-buffer)
(global-set-key (kbd "<C-f11>") 'xah-previous-emacs-buffer)
(global-set-key (kbd "<C-f12>") 'xah-next-emacs-buffer)

(global-set-key (kbd "<C-prior>") 'xah-previous-user-buffer)
(global-set-key (kbd "<C-next>") 'xah-next-user-buffer)

(if xah-fly-swapped-1827-p
    (progn
      (global-set-key (kbd "M-2") 'hippie-expand)
      (global-set-key (kbd "M-1") 'xah-toggle-letter-case))
  (progn
    (global-set-key (kbd "M-7") 'hippie-expand)
    (global-set-key (kbd "M-8") 'xah-toggle-letter-case)))

(progn
  ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward))


(defvar xah-fly-major-mode-lead-key nil "Lead key for all major mode's key sequence. By default, it's (kbd \"<menu> e\"). Only supported by xah's modes.")
(setq xah-fly-major-mode-lead-key (kbd "<menu> e"))

(defvar xah-fly-insert-state-q t "Boolean value. true means insertion mode is on.")
(setq xah-fly-insert-state-q t)

(defun xah-fly-command-mode-init ()
  "set command mode keys"
  (interactive)
  (progn

    (define-key xah-fly-key-map (kbd "'") 'xah-compact-uncompact-block)
    (define-key xah-fly-key-map (kbd ",") 'xah-shrink-whitespaces)
    (define-key xah-fly-key-map (kbd "-") nil)
    (define-key xah-fly-key-map (kbd ".") 'backward-kill-word)
    (define-key xah-fly-key-map (kbd ";") nil)
    (define-key xah-fly-key-map (kbd ":") nil)
    (define-key xah-fly-key-map (kbd "/") 'xah-backward-equal-sign)
    (define-key xah-fly-key-map (kbd "\\") nil)
    (define-key xah-fly-key-map (kbd "=") 'xah-forward-equal-sign)
    (define-key xah-fly-key-map (kbd "[") 'xah-backward-quote )
    (define-key xah-fly-key-map (kbd "]") 'xah-forward-quote-twice)
    (define-key xah-fly-key-map (kbd "`") nil)
    (define-key xah-fly-key-map (kbd "SPC") 'xah-fly-insert-mode-activate)

    (if xah-fly-swapped-1827-p
        (progn
          (define-key xah-fly-key-map (kbd "8") 'xah-fly-insert-mode-activate)
          (define-key xah-fly-key-map (kbd "7") 'delete-window)
          (define-key xah-fly-key-map (kbd "2") 'xah-select-current-line)
          (define-key xah-fly-key-map (kbd "1") 'xah-extend-selection))
      (progn
        (define-key xah-fly-key-map (kbd "1") 'xah-fly-insert-mode-activate)
        (define-key xah-fly-key-map (kbd "2") 'delete-window)
        (define-key xah-fly-key-map (kbd "7") 'xah-select-current-line)
        (define-key xah-fly-key-map (kbd "8") 'xah-extend-selection)))

    (define-key xah-fly-key-map (kbd "3") 'delete-other-windows)
    (define-key xah-fly-key-map (kbd "4") 'split-window-below)
    (define-key xah-fly-key-map (kbd "5") 'xah-cycle-hyphen-underscore-space)

    (define-key xah-fly-key-map (kbd "6") 'xah-select-current-block)
    (define-key xah-fly-key-map (kbd "9") 'xah-select-text-in-quote)
    (define-key xah-fly-key-map (kbd "0") 'xah-backward-punct)

    (define-key xah-fly-key-map (kbd "a") (if (fboundp 'smex) 'smex 'execute-extended-command ))
    (define-key xah-fly-key-map (kbd "b") 'isearch-forward)
    (define-key xah-fly-key-map (kbd "c") 'previous-line)
    (define-key xah-fly-key-map (kbd "d") 'xah-beginning-of-line-or-block)
    (define-key xah-fly-key-map (kbd "e") 'delete-backward-char)
    (define-key xah-fly-key-map (kbd "f") 'undo)
    (define-key xah-fly-key-map (kbd "g") 'backward-word)
    (define-key xah-fly-key-map (kbd "h") 'backward-char)
    (define-key xah-fly-key-map (kbd "i") 'kill-line)
    (define-key xah-fly-key-map (kbd "j") 'xah-cut-line-or-region)
    (define-key xah-fly-key-map (kbd "k") 'yank)
    (define-key xah-fly-key-map (kbd "l") 'xah-insert-space-before)
    (define-key xah-fly-key-map (kbd "m") 'xah-backward-left-bracket)
    (define-key xah-fly-key-map (kbd "n") 'forward-char)
    (define-key xah-fly-key-map (kbd "o") 'open-line)
    (define-key xah-fly-key-map (kbd "p") 'kill-word)
    (define-key xah-fly-key-map (kbd "q") 'xah-copy-line-or-region)
    (define-key xah-fly-key-map (kbd "r") 'forward-word)
    (define-key xah-fly-key-map (kbd "s") 'xah-end-of-line-or-block)
    (define-key xah-fly-key-map (kbd "t") 'next-line)
    (define-key xah-fly-key-map (kbd "u") 'delete-char)
    (define-key xah-fly-key-map (kbd "v") 'xah-forward-right-bracket)
    (define-key xah-fly-key-map (kbd "w") 'xah-next-window-or-frame)
    (define-key xah-fly-key-map (kbd "x") 'xah-fly-leader-key-map)
    (define-key xah-fly-key-map (kbd "y") 'set-mark-command)
    (define-key xah-fly-key-map (kbd "z") 'comment-dwim)
    ))

(defun xah-fly-insert-mode-init ()
  "Set insertion mode keys"
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

))

(defun xah-fly-mode-toggle ()
  "Switch between {insertion, command} modes."
  (interactive)
  (if xah-fly-insert-state-q
      (xah-fly-command-mode-activate)
    (xah-fly-insert-mode-activate)))

;; automatic save buffer when switching to command mode
(add-hook 'xah-fly-command-mode-activate-hook 'xah-fly-save-buffer-if-file)
;; remove it if you don't want

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

(defun xah-fly-insert-mode-activate-insert-return ()
  "Activate insertion mode, and insert a newline."
  (interactive)
  (xah-fly-insert-mode-activate)
  (open-line 1))



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
  1 "ξflykeys" xah-fly-key-map
  (xah-fly-command-mode-activate))

(defun xah-fly-keys-off ()
  "Turn off xah-fly-keys minor mode."
  (interactive)
  (xah-fly-keys 0))

(provide 'xah-fly-keys)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-fly-keys.el ends here
