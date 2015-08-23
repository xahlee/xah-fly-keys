;;; xah-fly-keys.el --- A efficient modal keybinding set minor mode based on ergonomics.

;; Copyright © 2013-2015, by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Version: 2.0
;; Created: 10 Sep 2013
;; Keywords: convenience, emulations, vim, ergoemacs
;; Homepage: http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; xah-fly-keys is an efficient keybiding system for remacs. (it is more efficient than vim)

;; It is a modal mode, like vi, but keymaps are based on statistics of command call frequency key position easy-to-press score.

;; xah-fly-keys does not bind any Control key, nor Meta keys (except 3, but you can turn off).  So, you can use emacs as is.

;; for now, see home page for info
;; http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; If you like this project, Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.

;;; INSTALL

;; To manual install.

;; put the file xah-fly-keys.el directory in ~/.emacs.d/lisp/

;; put the following in your emacs init at ~/.emacs.d/init.el

;; (add-to-list 'load-path "~/.emacs.d/lisp/xah-fly-keys/")
;; (require 'xah-fly-keys)
;; (xah-fly-keys 1)

;;; TODO
;; • make it support qwerty Keyboard layout
;; • add feature to allow support of standard open close save etc keys with Control
;; • add feature to allow turn off any Control completely. Same for Meta. (as a way to stop habit)


;;; Code:

(require 'dired) ; in emacs
(require 'dired-x) ; in emacs



(defvar xah-fly-key-map nil "Keybinding for `xah-fly-keys' minor mode.")
(progn
  (setq xah-fly-key-map (make-sparse-keymap))

  (define-key xah-fly-key-map (kbd "'") 'self-insert-command)

  (define-key xah-fly-key-map (kbd "M-k") 'yank-pop)
  (define-key xah-fly-key-map (kbd "M-t") 'xah-toggle-letter-case)
  (define-key xah-fly-key-map (kbd "M-c") 'xah-cycle-hyphen-underscore-space)
  (define-key xah-fly-key-map (kbd "M-r") 'hippie-expand)

  (progn

    (define-key xah-fly-key-map (kbd "<C-next>") 'xah-next-user-buffer)
    (define-key xah-fly-key-map (kbd "<C-prior>") 'xah-previous-user-buffer)))

(defvar xah-fly-major-mode-lead-key nil "Lead key for all major mode's key sequence. By default, it's (kbd \"<menu> e\"). Only supported by xah's modes.")
(setq xah-fly-major-mode-lead-key (kbd "<menu> e"))


;; cursor movement

(defun xah-scroll-down-10-lines ()
  "scroll down 10 lines"
  (interactive)
  (scroll-down 10))

(defun xah-scroll-up-10-lines ()
  "scroll up 10 lines"
  (interactive)
  (scroll-up 10))

(defun xah-cursor-down-50-lines ()
  "Move cursor down 50 logical lines.
Version 2015-07-06"
  (interactive)
  (forward-line 50))

(defun xah-cursor-up-50-lines ()
  "Move cursor up 50 logical lines.
Version 2015-07-06"
  (interactive)
  (forward-line -50))

(defun xah-cursor-down-25-lines ()
  "Move cursor down 25 logical lines.
Version 2015-07-06"
  (interactive)
  (forward-line 25))

(defun xah-cursor-up-25-lines ()
  "Move cursor up 25 logical lines.
Version 2015-07-06"
  (interactive)
  (forward-line -25))

(defvar xah-forward-n-words 4 "integer used by `xah-forward-n-words'")
(setq xah-forward-n-words 4)

(defun xah-forward-n-words ()
  "`forward-word' `xah-forward-n-words' times."
  (interactive)
  (forward-word xah-forward-n-words))

(defun xah-backward-n-words ()
  "`backward-word' `xah-forward-n-words' times."
  (interactive)
  (backward-word xah-forward-n-words))

(defvar xah-forward-n-chars 50 "a integer used by `xah-forward-n-chars'")
(setq xah-forward-n-chars 50)

(defun xah-forward-n-chars ()
  "`forward-char' `xah-forward-n-chars' times."
  (interactive)
  (forward-char xah-forward-n-chars))

(defun xah-backward-n-chars ()
  "`backward-char' `xah-forward-n-chars' times."
  (interactive)
  (backward-char xah-forward-n-chars))

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

(defvar xah-brackets nil "string of brackets")
(setq xah-brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")

(defvar
  xah-left-brackets
  '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
  (setq xah-left-brackets '())
  (dotimes (x (- (length xah-brackets) 1))
    (when (= (% x 2) 0)
      (push (char-to-string (elt xah-brackets x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar
  xah-right-brackets
  '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes (x (- (length xah-brackets) 1))
    (when (= (% x 2) 1)
      (push (char-to-string (elt xah-brackets x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defvar xah-ascii-quotes nil "List of quotation chars.")
(setq xah-ascii-quotes '("'" "\""))

(defvar xah-punctuations nil "list of punctuation chars for easy jump. Typically exclude things that are too common, such as underscore or slash.")
(setq xah-punctuations '("=" ";"))

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

(defun xah-forward-all-bracket (&optional φn)
  "Move cursor to the next occurrence of left bracket or quotation mark.
With prefix ΦN, move forward to the next ΦN left bracket or quotation mark.
With a negative prefix ΦN, move backward to the previous ΦN left bracket or quotation mark.

The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets' and `xah-ascii-quotes'"
  (interactive "p")
  (if (and φn (> 0 φn))
      (xah-backward-left-bracket (- 0 φn))
    (forward-char 1)
    (search-forward-regexp (eval-when-compile (regexp-opt (append xah-left-brackets xah-right-brackets xah-ascii-quotes ))) nil t φn)
    (backward-char 1)))

(defun xah-backward-all-bracket (&optional φn)
  "Move cursor to the previous occurrence of left bracket or quotation mark.
With prefix argument ΦN, move backward ΦN open brackets.
With a negative prefix ΦN, move forward ΦN open brackets.

The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets' and `xah-ascii-quotes'"
  (interactive "p")
  (if (and φn (> 0 φn))
      (xah-forward-left-bracket (- 0 φn))
    (search-backward-regexp (eval-when-compile (regexp-opt (append xah-left-brackets xah-right-brackets xah-ascii-quotes ))) nil t φn)))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-03-24"
  (interactive)
  (search-backward-regexp (eval-when-compile (regexp-opt xah-left-brackets)) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-03-24"
  (interactive)
  (search-forward-regexp (eval-when-compile (regexp-opt xah-right-brackets)) nil t))

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
  "Move cursor to the next occurrence of ' or \".
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-03-24"
  (interactive)
  (search-forward-regexp "'+\\|\\\"+" nil t))

(defun xah-backward-quote ()
  "Move cursor to the previous occurrence of ' or \".
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-03-24"
  (interactive)
  (search-backward-regexp "'+\\|\\\"+" nil t)
  (let ( (thisChar (char-after)))
    (while (char-equal (char-before) thisChar)
      (left-char ))))

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

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-05-06"
  (interactive)
  (let (ξp1 ξp2)
    (if current-prefix-arg
        (progn (setq ξp1 (point-min))
               (setq ξp2 (point-max)))
      (progn (if (use-region-p)
                 (progn (setq ξp1 (region-beginning))
                        (setq ξp2 (region-end)))
               (progn (setq ξp1 (line-beginning-position))
                      (setq ξp2 (line-end-position))))))
    (kill-ring-save ξp1 ξp2)
    (if current-prefix-arg
        (message "buffer text copied")
      (message "text copied"))))

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

(defun xah-toggle-letter-case (φbegin φend)
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

In lisp code, φbegin φend are region boundary.
URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2015-04-09"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let ((ξbds (bounds-of-thing-at-point 'word)))
       (list (car ξbds) (cdr ξbds)))))
  (let ((deactivate-mark nil))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region φbegin φend)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region φbegin φend)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region φbegin φend)
      (put this-command 'state 0)))))

(defun xah-toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor."
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
Remove whitespaces around cursor to just one space, or remove neighboring blank lines to just one or none.
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
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000)) ; 90002000 is just random. you can use `most-positive-fixnum'
    (fill-paragraph)))

(defun xah-unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
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
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2015-08-08"
  (interactive "P")
  (let ((ξfpath
         (if (equal major-mode 'dired-mode)
             default-directory
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
  "delete the current text block (paragraph) and also put it to `kill-ring'.
Version 2015-05-26"
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "NOERROR")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))
    (kill-region p1 p2)))

(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'."
  (interactive)
  (let (p1 p2)
    (if (region-active-p)
        (progn (setq p1 (region-beginning))
               (setq p2 (region-end)))
      (progn (setq p1 (line-beginning-position))
             (setq p2 (line-end-position))))
    (copy-to-register ?1 p1 p2)
    (message "copied to register 1: 「%s」." (buffer-substring-no-properties p1 p2))))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert-register ?1 t))

(defun xah-copy-rectangle-to-clipboard (φbegin φend)
  "Copy region as column (rectangle) to operating system's clipboard.
This command will also put the text in register 0.

See also: `kill-rectangle', `copy-to-register'."
  (interactive "r")
  (let ((x-select-enable-clipboard t))
    (copy-rectangle-to-register ?0 φbegin φend)
    (kill-new
     (with-temp-buffer
       (insert-register ?0)
       (buffer-string) ))))

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
Note: this command is similar to `rectangle-number-lines', starting at 65 or 97, and with a format of 「%c」."
  (interactive "P")
  (let ((startChar (if φuse-uppercase-p 65 97 )))
    (dotimes (ii 26)
      (insert (format "%c\n" (+ startChar ii))))))

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

(defun xah-semnav-up (arg)
"Called by `xah-extend-selection'.
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

(defun xah-extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit.

This command works mostly in lisp syntax.
Written by Nikolaj Schumacher, 2008-10-20. Released under GPL 2"
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (or (use-region-p)
             (eq last-command this-command))))
  (if incremental
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

(defun xah-select-text-in-html-bracket ()
  "Select text between <…> or >…<."
  (interactive)
  (let (ξp0 ξp1< ξp1> ξp2< ξp2>
           distance-p1<
           distance-p1>
           )
    (setq ξp0 (point))
    (search-backward "<" nil "NOERROR" )
    (setq ξp1< (point))
    (goto-char ξp0)
    (search-backward ">" nil "NOERROR" )
    (setq ξp1> (point))
    (setq distance-p1< (abs (- ξp0 ξp1<)))
    (setq distance-p1> (abs (- ξp1> ξp0)))
    (if (< distance-p1< distance-ξp1>)
        (progn
          (goto-char ξp0)
          (search-forward ">" nil "NOERROR" )
          (setq ξp2> (point))
          (goto-char (1+ ξp1<))
          (set-mark (1- ξp2>)))
      (progn
        (goto-char ξp0)
        (search-forward "<" nil "NOERROR" )
        (setq ξp2< (point))
        (goto-char (1+ ξp1>))
        (set-mark (1- ξp2<))))))


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

A emacs buffer is one who's name starts with *.
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
    (funcall (intern initial-major-mode))
    (setq buffer-offer-save t)))

;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extention, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2015-03-20"
  (interactive)
  (let* ((ξinputStr (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (let (ξp0 ξp1 ξp2
                         (ξcharSkipRegex "^  \"\t\n`':|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`"))
                 (setq ξp0 (point))
                 ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                 (skip-chars-backward ξcharSkipRegex)
                 (setq ξp1 (point))
                 (goto-char ξp0)
                 (skip-chars-forward ξcharSkipRegex)
                 (setq ξp2 (point))
                 (goto-char ξp0)
                 (buffer-substring-no-properties ξp1 ξp2))))
         (ξpath (replace-regexp-in-string ":\\'" "" ξinputStr)))
    (if (string-match-p "\\`https?://" ξpath)
        (browse-url ξpath)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" ξpath)
            (progn
              (let (
                    (ξfpath (match-string 1 ξpath))
                    (ξline-num (string-to-number (match-string 2 ξpath))))
                (if (file-exists-p ξfpath)
                    (progn
                      (find-file ξfpath)
                      (goto-char 1)
                      (forward-line (1- ξline-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfpath))
                      (find-file ξfpath))))))
          (progn
            (if (file-exists-p ξpath)
                (find-file ξpath)
              (if (file-exists-p (concat ξpath ".el"))
                  (find-file (concat ξpath ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξpath))
                  (find-file ξpath ))))))))))

(defun xah-open-file-path-under-cursor ()
  "Open the file path under cursor.
If there is text selection, use the text selection for path.
If path starts with “http://”, launch browser vistiting that URL, or open the corresponding file, if it's xah site.

Input path can be {relative, full path, URL}. See: `xahsite-web-path-to-filepath' for types of paths supported.

Version 2015-06-12"
  (interactive)
  (let* (
         (ξinputStr1
          (xah-remove-uri-fragment
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (let (ξp0 ξp1 ξp2
                       (ξcharSkipRegex "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`"))
               (setq ξp0 (point))
               ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
               (skip-chars-backward ξcharSkipRegex)
               (setq ξp1 (point))
               (goto-char ξp0)
               (skip-chars-forward ξcharSkipRegex)
               (setq ξp2 (point))
               (goto-char ξp0)
               (buffer-substring-no-properties ξp1 ξp2)))))
         (ξinputStr2 (replace-regexp-in-string ":\\'" "" ξinputStr1))
          )

    (if (string-equal ξinputStr2 "")
        (progn (user-error "No path under cursor" ))
      (progn

        ;; convenience. if the input string start with a xah domain name, make it a url string
        (setq ξp
              (cond
               ((string-match "\\`//" ξinputStr2 ) (concat "http:" ξinputStr2)) ; relative http protocol, used in css
               ((string-match "\\`ergoemacs\\.org" ξinputStr2 ) (concat "http://" ξinputStr2))
               ((string-match "\\`wordyenglish\\.com" ξinputStr2 ) (concat "http://" ξinputStr2))
               ((string-match "\\`xaharts\\.org" ξinputStr2 ) (concat "http://" ξinputStr2))
               ((string-match "\\`xahlee\\.info" ξinputStr2 ) (concat "http://" ξinputStr2))
               ((string-match "\\`xahlee\\.org" ξinputStr2 ) (concat "http://" ξinputStr2))
               ((string-match "\\`xahmusic\\.org" ξinputStr2 ) (concat "http://" ξinputStr2))
               ((string-match "\\`xahporn\\.org" ξinputStr2 ) (concat "http://" ξinputStr2))
               ((string-match "\\`xahsl\\.org" ξinputStr2 ) (concat "http://" ξinputStr2))
               (t ξinputStr2)))

        (if (string-match-p "\\`https?://" ξp)
            (if (xahsite-url-is-xah-website-p ξp)
                (let ((ξfp (xahsite-url-to-filepath ξp )))
                  (if (file-exists-p ξfp)
                      (find-file ξfp)
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfp)) (find-file ξfp))))
              (browse-url ξp))
          (progn ; not starting “http://”
            (let ((ξfff (xahsite-web-path-to-filepath ξp default-directory)))
              (if (file-exists-p ξfff)
                  (progn (find-file ξfff))
                (if (file-exists-p (concat ξfff ".el"))
                    (progn (find-file (concat ξfff ".el")))
                  (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfff)) (find-file ξfff )))))))))))

(defun xah-open-file-from-clipboard ()
  "Open the file path from OS's clipboard.
The clipboard should contain a file path or url to xah site. Open that file in emacs."
  (interactive)
  (let (
        (ξinputStr
         (with-temp-buffer
           (yank)
           (buffer-string)))
        ξfpath
        )

    (if (string-match-p "\\`http://" ξinputStr)
        (progn
          (setq ξfpath (xahsite-url-to-filepath ξinputStr "addFileName"))
          (if (file-exists-p ξfpath)
              (progn (find-file ξfpath))
            (progn (error "file doesn't exist 「%s」" ξfpath))))
      (progn ; not starting “http://”
        (setq ξinputStr (xah-remove-uri-fragment ξinputStr))
        (setq ξfpath (xahsite-web-path-to-filepath ξinputStr default-directory))
        (if (file-exists-p ξfpath)
            (progn (find-file ξfpath))
          (progn (user-error "file doesn't exist 「%s」" ξfpath)))))))

(defun xah-delete-current-file-make-backup (&optional φno-backup-p)
  "Delete the file associated with the current buffer (also closes the buffer).

A backup file is created with filename appended “~‹date time stamp›~”. Existing file of the same name is overwritten. If the file is not associated with buffer, the backup file name starts with “xx_”.

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

(defun xah-delete-current-file ()
  "Delete the file associated with the current buffer and close the buffer.
Also push file content to `kill-ring'.
If buffer is not file, just close it, and push file content to `kill-ring'.

URL `http://ergoemacs.org/emacs/elisp_delete-current-file.html'
Version 2015-08-12"
  (interactive)
  (progn
    (kill-new (buffer-string))
    (when (buffer-file-name)
      (when (file-exists-p (buffer-file-name))
        (progn
          (delete-file (buffer-file-name))
          (message "Deleted: 「%s」." (buffer-file-name)))))
    (let ((buffer-offer-save nil))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))))

(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file x.py, then it'll call 「python x.py」 in a shell.
The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
version 2015-08-23"
  (interactive)
  (let* (
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
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ("tex" . "pdflatex")
            ("latex" . "pdflatex")
            ("java" . "javac")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            ))
         (ξfname (buffer-file-name))
         (ξfSuffix (file-name-extension ξfname))
         (ξprog-name (cdr (assoc ξfSuffix ξsuffix-map)))
         (ξcmd-str (concat ξprog-name " \""   ξfname "\"")))

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer)))

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
Version 2015-06-12"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
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
         (lambda (ξfpath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" ξfpath))) ξfile-list))))))



;; -*- coding: utf-8 -*-

(progn
  (define-prefix-command 'xah-highlight-keymap) ; commands in search-map
  (define-key xah-highlight-keymap (kbd ".") 'isearch-forward-symbol-at-point)
  (define-key xah-highlight-keymap (kbd "s") 'isearch-forward-symbol)
  (define-key xah-highlight-keymap (kbd "w") 'isearch-forward-word)
  (define-key xah-highlight-keymap (kbd "h .") 'highlight-symbol-at-point)
  (define-key xah-highlight-keymap (kbd "h f") 'hi-lock-find-patterns)
  (define-key xah-highlight-keymap (kbd "h l") 'highlight-lines-matching-regexp)
  (define-key xah-highlight-keymap (kbd "h p") 'highlight-phrase)
  (define-key xah-highlight-keymap (kbd "h r") 'highlight-regexp)
  (define-key xah-highlight-keymap (kbd "h u") 'unhighlight-regexp)
  (define-key xah-highlight-keymap (kbd "h w") 'hi-lock-write-interactive-patterns))

(progn
  (define-prefix-command 'xah-menu-tab-keymap)

  (define-key xah-menu-tab-keymap (kbd "TAB") 'indent-for-tab-command)

  (define-key xah-menu-tab-keymap (kbd "i") 'complete-symbol)
  (define-key xah-menu-tab-keymap (kbd "g") 'indent-rigidly)
  (define-key xah-menu-tab-keymap (kbd "r") 'indent-region)
  (define-key xah-menu-tab-keymap (kbd "s") 'indent-sexp)

  (define-key xah-menu-tab-keymap (kbd "e") nil)
  (define-key xah-menu-tab-keymap (kbd "e '") 'abbrev-prefix-mark)
  (define-key xah-menu-tab-keymap (kbd "e e") 'edit-abbrevs)
  (define-key xah-menu-tab-keymap (kbd "e p") 'expand-abbrev)
  (define-key xah-menu-tab-keymap (kbd "e r") 'expand-region-abbrevs)
  (define-key xah-menu-tab-keymap (kbd "e u") 'unexpand-abbrev)
  (define-key xah-menu-tab-keymap (kbd "e g") 'add-global-abbrev)
  (define-key xah-menu-tab-keymap (kbd "e a") 'add-mode-abbrev)
  (define-key xah-menu-tab-keymap (kbd "e v") 'inverse-add-global-abbrev)
  (define-key xah-menu-tab-keymap (kbd "e l") 'inverse-add-mode-abbrev)
  (define-key xah-menu-tab-keymap (kbd "e n") 'expand-jump-to-next-slot)
  (define-key xah-menu-tab-keymap (kbd "e p") 'expand-jump-to-previous-slot))



(progn
  (define-prefix-command 'xah-menu-c-keymap)

  (define-key xah-menu-c-keymap (kbd ",") 'xah-open-in-external-app)
  (define-key xah-menu-c-keymap (kbd ".") 'find-file)
  (define-key xah-menu-c-keymap (kbd "c") 'bookmark-bmenu-list)
  (define-key xah-menu-c-keymap (kbd "e") nil)
  (define-key xah-menu-c-keymap (kbd "g") 'ido-switch-buffer)
  (define-key xah-menu-c-keymap (kbd "h") 'recentf-open-files)
  (define-key xah-menu-c-keymap (kbd "l") 'bookmark-set)
  (define-key xah-menu-c-keymap (kbd "n") 'xah-new-empty-buffer)
  (define-key xah-menu-c-keymap (kbd "o") 'xah-open-in-desktop)
  (define-key xah-menu-c-keymap (kbd "p") 'xah-open-last-closed)
  (define-key xah-menu-c-keymap (kbd "f") 'xah-open-recently-closed)
  (define-key xah-menu-c-keymap (kbd "y") 'xah-list-recently-closed)
  (define-key xah-menu-c-keymap (kbd "r") 'bookmark-jump)
  (define-key xah-menu-c-keymap (kbd "s") 'write-file)
  (define-key xah-menu-c-keymap (kbd "t") 'ibuffer)
  (define-key xah-menu-c-keymap (kbd "u") nil))

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
  (define-prefix-command 'xah-menu-i-keymap) ; commands in goto-map
  (define-key xah-menu-i-keymap (kbd "TAB") 'move-to-column)
  (define-key xah-menu-i-keymap (kbd "c") 'goto-char)
  (define-key xah-menu-i-keymap (kbd "g") 'goto-line)
  (define-key xah-menu-i-keymap (kbd "n") 'next-error)
  (define-key xah-menu-i-keymap (kbd "p") 'previous-error))

(progn
  ;; commands here are harmless (safe). They don't modify text.
  ;; they turn on minor/major mode, change display, prompt, start shell, etc.
  (define-prefix-command 'xah-harmless-keymap)

  (define-key xah-harmless-keymap (kbd "RET") nil)
  (define-key xah-harmless-keymap (kbd "RET F") 'set-file-name-coding-system)
  (define-key xah-harmless-keymap (kbd "RET X") 'set-next-selection-coding-system)
  (define-key xah-harmless-keymap (kbd "RET c") 'universal-coding-system-argument)
  (define-key xah-harmless-keymap (kbd "RET f") 'set-buffer-file-coding-system)
  (define-key xah-harmless-keymap (kbd "RET k") 'set-keyboard-coding-system)
  (define-key xah-harmless-keymap (kbd "RET l") 'set-language-environment)
  (define-key xah-harmless-keymap (kbd "RET p") 'set-buffer-process-coding-system)
  (define-key xah-harmless-keymap (kbd "RET r") 'revert-buffer-with-coding-system)
  (define-key xah-harmless-keymap (kbd "RET t") 'set-terminal-coding-system)
  (define-key xah-harmless-keymap (kbd "RET x") 'set-selection-coding-system)

  (define-key xah-harmless-keymap (kbd "'") 'frame-configuration-to-register)
  (define-key xah-harmless-keymap (kbd ";") 'window-configuration-to-register)

  (define-key xah-harmless-keymap (kbd "8") 'set-input-method)
  (define-key xah-harmless-keymap (kbd "7") 'global-hl-line-mode)
  (define-key xah-harmless-keymap (kbd "3") 'whitespace-mode)
  (define-key xah-harmless-keymap (kbd "4") 'linum-mode)
  (define-key xah-harmless-keymap (kbd "5") 'visual-line-mode)

  (define-key xah-harmless-keymap (kbd "6") 'calendar)
  (define-key xah-harmless-keymap (kbd "2") 'calc)
  (define-key xah-harmless-keymap (kbd "1") 'shell)
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
  (define-key xah-harmless-keymap (kbd "r") nil)
  (define-key xah-harmless-keymap (kbd "s") 'flyspell-buffer)
  (define-key xah-harmless-keymap (kbd "t") 'narrow-to-defun)
  (define-key xah-harmless-keymap (kbd "u") 'toggle-input-method)
  (define-key xah-harmless-keymap (kbd "v") 'variable-pitch-mode)
  (define-key xah-harmless-keymap (kbd "w") 'eww)
  (define-key xah-harmless-keymap (kbd "x") 'nil)
  (define-key xah-harmless-keymap (kbd "y") 'nil)
  (define-key xah-harmless-keymap (kbd "z") 'abort-recursive-edit)
  )

(progn
  ;; kinda replacement related
  (define-prefix-command 'xah-edit-cmds-keymap)

  (define-key xah-edit-cmds-keymap (kbd "8") nil)
  (define-key xah-edit-cmds-keymap (kbd "7") nil)
  (define-key xah-edit-cmds-keymap (kbd "3") 'apply-macro-to-region-lines)
  (define-key xah-edit-cmds-keymap (kbd "4") 'sort-lines)
  (define-key xah-edit-cmds-keymap (kbd "5") 'sort-numeric-fields)
  (define-key xah-edit-cmds-keymap (kbd "6") 'reverse-region)
  (define-key xah-edit-cmds-keymap (kbd "2") 'list-matching-lines)
  (define-key xah-edit-cmds-keymap (kbd "1") 'delete-matching-lines)
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
  (define-prefix-command 'xah-menu-t-keymap)

  (define-key xah-menu-t-keymap (kbd "RET") 'pop-global-mark)
  (define-key xah-menu-t-keymap (kbd "1") 'mark-defun)
  (define-key xah-menu-t-keymap (kbd "3") 'point-to-register)
  (define-key xah-menu-t-keymap (kbd "4") 'jump-to-register)

  (define-key xah-menu-t-keymap (kbd "a") nil)
  (define-key xah-menu-t-keymap (kbd "b") nil)
  (define-key xah-menu-t-keymap (kbd "c") nil)
  (define-key xah-menu-t-keymap (kbd "d") nil)
  (define-key xah-menu-t-keymap (kbd "e") 'copy-to-register)
  (define-key xah-menu-t-keymap (kbd "f") nil)
  (define-key xah-menu-t-keymap (kbd "g") nil)
  (define-key xah-menu-t-keymap (kbd "h") (if (fboundp 'xah-close-current-buffer)
                                              (progn 'xah-close-current-buffer)
                                            (progn 'kill-buffer)))
  (define-key xah-menu-t-keymap (kbd "i") nil)
  (define-key xah-menu-t-keymap (kbd "j") nil)
  (define-key xah-menu-t-keymap (kbd "k") nil)
  (define-key xah-menu-t-keymap (kbd "l") 'increment-register)
  (define-key xah-menu-t-keymap (kbd "m") nil)
  (define-key xah-menu-t-keymap (kbd "n") 'repeat-complex-command)
  (define-key xah-menu-t-keymap (kbd "o") nil)
  (define-key xah-menu-t-keymap (kbd "p") 'query-replace-regexp)
  (define-key xah-menu-t-keymap (kbd "q") nil)
  (define-key xah-menu-t-keymap (kbd "r") 'copy-rectangle-to-register)
  (define-key xah-menu-t-keymap (kbd "s") nil)
  (define-key xah-menu-t-keymap (kbd "t") 'repeat)
  (define-key xah-menu-t-keymap (kbd "u") 'insert-register)
  (define-key xah-menu-t-keymap (kbd "v") nil)
  (define-key xah-menu-t-keymap (kbd "w") 'other-window)
  (define-key xah-menu-t-keymap (kbd "x") nil)
  (define-key xah-menu-t-keymap (kbd "y") nil)
  (define-key xah-menu-t-keymap (kbd "z") 'number-to-register))

(progn
  (define-prefix-command 'xah-menu-v-keymap)

  (define-key xah-menu-v-keymap (kbd "+") 'vc-update)
  (define-key xah-menu-v-keymap (kbd "=") 'vc-diff)
  (define-key xah-menu-v-keymap (kbd "D") 'vc-root-diff)
  (define-key xah-menu-v-keymap (kbd "L") 'vc-print-root-log)
  (define-key xah-menu-v-keymap (kbd "a") 'vc-update-change-log)
  (define-key xah-menu-v-keymap (kbd "b") 'vc-switch-backend)
  (define-key xah-menu-v-keymap (kbd "c") 'vc-rollback)
  (define-key xah-menu-v-keymap (kbd "d") 'vc-dir)
  (define-key xah-menu-v-keymap (kbd "g") 'vc-annotate)
  (define-key xah-menu-v-keymap (kbd "h") 'vc-insert-headers)
  (define-key xah-menu-v-keymap (kbd "l") 'vc-print-log)
  (define-key xah-menu-v-keymap (kbd "m") 'vc-merge)
  (define-key xah-menu-v-keymap (kbd "r") 'vc-retrieve-tag)
  (define-key xah-menu-v-keymap (kbd "s") 'vc-create-tag)
  (define-key xah-menu-v-keymap (kbd "u") 'vc-revert)
  (define-key xah-menu-v-keymap (kbd "v") 'vc-next-action)
  (define-key xah-menu-v-keymap (kbd "~") 'vc-revision-other-window))

(progn
  (define-prefix-command 'xah-danger-keymap)

  (define-key xah-danger-keymap (kbd "RET") 'xah-run-current-file)

  (define-key xah-danger-keymap (kbd ".") 'eval-buffer)
  (define-key xah-danger-keymap (kbd "e") 'eval-defun)
  (define-key xah-danger-keymap (kbd "m") 'eval-last-sexp)
  (define-key xah-danger-keymap (kbd "p") 'eval-expression)
  (define-key xah-danger-keymap (kbd "u") 'eval-region)
  (define-key xah-danger-keymap (kbd "q") 'save-buffers-kill-terminal)
  (define-key xah-danger-keymap (kbd "w") 'delete-frame))

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
  (define-key xah-insertion-keymap (kbd "w") 'xah-insert-double-angle-bracket《》)
  (define-key xah-insertion-keymap (kbd "W") 'xah-insert-angle-bracket〈〉)
  (define-key xah-insertion-keymap (kbd "x") nil)
  (define-key xah-insertion-keymap (kbd "y") 'xah-insert-double-angle-quote«»))

(progn
  (define-prefix-command 'xah-fly-leader-key-map)

  (define-key xah-fly-leader-key-map (kbd "<end>") 'xah-fly-keys)
  (define-key xah-fly-leader-key-map (kbd "RET") (if (fboundp 'smex) 'smex 'execute-extended-command ))
  (define-key xah-fly-leader-key-map (kbd "DEL") 'xah-delete-current-file-make-backup)
  (define-key xah-fly-leader-key-map (kbd "<delete>") nil)
  (define-key xah-fly-leader-key-map (kbd "SPC") xah-insertion-keymap)
  (define-key xah-fly-leader-key-map (kbd "<menu>") 'exchange-point-and-mark)
  (define-key xah-fly-leader-key-map (kbd "TAB") xah-menu-tab-keymap)

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

  (define-key xah-fly-leader-key-map (kbd "8") nil)
  (define-key xah-fly-leader-key-map (kbd "7") nil)
  (define-key xah-fly-leader-key-map (kbd "3") nil)
  (define-key xah-fly-leader-key-map (kbd "4") 'split-window-right)
  (define-key xah-fly-leader-key-map (kbd "5") nil)

  (define-key xah-fly-leader-key-map (kbd "6") nil)
  (define-key xah-fly-leader-key-map (kbd "2") 'dired-jump)
  (define-key xah-fly-leader-key-map (kbd "1") 'xah-open-file-path-under-cursor)
  (define-key xah-fly-leader-key-map (kbd "9") 'ispell-word)
  (define-key xah-fly-leader-key-map (kbd "0") nil)

  (define-key xah-fly-leader-key-map (kbd "a") 'mark-whole-buffer)
  (define-key xah-fly-leader-key-map (kbd "b") 'end-of-buffer)
  (define-key xah-fly-leader-key-map (kbd "c") xah-menu-c-keymap)
  (define-key xah-fly-leader-key-map (kbd "d") 'beginning-of-buffer)
  (define-key xah-fly-leader-key-map (kbd "f") 'xah-search-current-word)
  (define-key xah-fly-leader-key-map (kbd "g") 'isearch-forward)
  (define-key xah-fly-leader-key-map (kbd "h") 'xah-help-keymap)
  (define-key xah-fly-leader-key-map (kbd "i") xah-menu-i-keymap)
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
  (define-key xah-fly-leader-key-map (kbd "t") xah-menu-t-keymap)
  (define-key xah-fly-leader-key-map (kbd "u") nil)
  (define-key xah-fly-leader-key-map (kbd "v") xah-menu-v-keymap)
  (define-key xah-fly-leader-key-map (kbd "w") xah-danger-keymap)
  (define-key xah-fly-leader-key-map (kbd "x") nil)
  (define-key xah-fly-leader-key-map (kbd "y") nil)
  (define-key xah-fly-leader-key-map (kbd "z") 'comment-dwim))


;;;; misc

;; ~/web/ergoemacs_org/emacs/gnu_emacs_keybinding_C-x.txt

;; some idea about command categories, in context to choosing keys for them

;; • whether a command is frequently needed ⁖ few times a min, hour, day
;; • whether a command has immediate effect, no prompt. ⁖ kill-word vs shell, delete-matching-lines
;; • whether a command is safe to run by mistake. ⁖ whitespace-mode vs eval-buffer

;; idea about key groups
;; all should be sequence of single keys. 2 to 3 keys. All should start with F7. And all commands should be globally useful.
;; • 2 keys vs 3 keys
;; • whether the key ends in a digit key 0 to 9. These probably should be most frequently used, or immediate effect.



;; these commands have keys in emacs, but right now i decided not to give them a key

  ;; C-x 5 C-f  find-file-other-frame
  ;; C-x 5 C-o  display-buffer-other-frame
  ;; C-x 5 .    find-tag-other-frame
  ;; C-x 5 1    delete-other-frames
  ;; C-x 5 b    switch-to-buffer-other-frame
  ;; C-x 5 d    dired-other-frame
  ;; C-x 5 f    find-file-other-frame
  ;; C-x 5 m    compose-mail-other-frame
  ;; C-x 5 r    find-file-read-only-other-frame

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

;; C-x s	save-some-buffers

;; M-o ESC         Prefix Command
;; M-o b           facemenu-set-bold
;; M-o d           facemenu-set-default
;; M-o i           facemenu-set-italic
;; M-o l           facemenu-set-bold-italic
;; M-o o           facemenu-set-face
;; M-o u           facemenu-set-underline
;; M-o M-S         center-paragraph
;; M-o M-o         font-lock-fontify-block
;; M-o M-s         center-line

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


;; -*- coding: utf-8 -*-

(when (string-equal system-type "windows-nt")
  (define-key key-translation-map (kbd "<apps>") (kbd "<menu>")))

(global-set-key (kbd "<menu>") 'xah-fly-leader-key-map)
(global-set-key (kbd "<home>") 'xah-fly-command-mode-activate)



(global-set-key (kbd "<f11>") 'xah-previous-user-buffer)
(global-set-key (kbd "<f12>") 'xah-next-user-buffer)
(global-set-key (kbd "<C-f11>") 'xah-previous-emacs-buffer)
(global-set-key (kbd "<C-f12>") 'xah-next-emacs-buffer)



(global-set-key (kbd "<C-prior>") 'xah-previous-user-buffer)
(global-set-key (kbd "<C-next>") 'xah-next-user-buffer)

(progn
  ;; set arrow keys in isearch. left/right is backward/forward, up/down is history. press Return to exit
  (define-key isearch-mode-map (kbd "<up>") 'isearch-ring-retreat )
  (define-key isearch-mode-map (kbd "<down>") 'isearch-ring-advance )
  (define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward) ; single key, useful
  (define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward) ; single key, useful
 )


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
  1 "ξflykeys" xah-fly-key-map
  (xah-fly-command-mode-activate))

(provide 'xah-fly-keys)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-fly-keys ends here
