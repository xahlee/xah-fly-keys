;; -*- coding: utf-8 -*-
;; created 2015-08-23


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

