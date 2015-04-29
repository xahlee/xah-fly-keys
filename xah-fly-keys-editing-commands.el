;; -*- coding: utf-8 -*-

(defun xah-delete-current-line ()
  "Delete current line."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (when (looking-at "\n")
    (delete-char 1)))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region')."
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
    (kill-ring-save ξp1 ξp2)))

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region')."
  (interactive)
  (let (ξp1 ξp2)
    (if current-prefix-arg        
        (progn (setq ξp1 (point-min))
               (setq ξp2 (point-max)))
      (progn (if (use-region-p)
                 (progn (setq ξp1 (region-beginning))
                        (setq ξp2 (region-end)))
               (progn (setq ξp1 (line-beginning-position))
                      (setq ξp2 (line-beginning-position 2))))))
    (kill-region ξp1 ξp2)))

(defun xah-copy-all ()
  "Put the whole buffer content into the `kill-ring'. (respects `narrow-to-region')"
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copied."))

(defun xah-cut-all ()
  "Cut the whole buffer content into the `kill-ring'. (respects `narrow-to-region')"
  (interactive)
  (kill-new (buffer-string))
  (delete-region (point-min) (point-max)))



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

When there is a text selection, act on the the selection, else, act on a text block separated by blank lines."
  (interactive)
  ;; This command symbol has a property “'stateIsCompact-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( currentStateIsCompact
         (deactivate-mark nil)
         (blanklinesRegex "\n[ \t]*\n")
         ξp1 ξp2
         )

    (progn
      ;; set region boundary ξp1 ξp2
      (if (use-region-p)
          (progn (setq ξp1 (region-beginning))
                 (setq ξp2 (region-end)))
        (save-excursion
          (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
              (progn (re-search-forward "\n[ \t]*\n")
                     (setq ξp1 (point)))
            (setq ξp1 (point)))
          (if (re-search-forward "\n[ \t]*\n" nil "NOERROR")
              (progn (re-search-backward "\n[ \t]*\n")
                     (setq ξp2 (point)))
            (setq ξp2 (point))))))

    (save-excursion
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (progn
                (goto-char ξp1)
                (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil))))

      (if currentStateIsCompact
          (fill-region ξp1 ξp2)
        (xah-replace-newline-whitespaces-to-space ξp1 ξp2))

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000)) ; 90002000 is just random. you can use `most-positive-fixnum'
    (fill-paragraph nil)))

(defun xah-unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'."
  (interactive "r")
  (let ((fill-column 90002000))
    (fill-region start end)))

(defun xah-replace-newline-whitespaces-to-space (&optional φbegin φend)
  "Replace newline with surrounding {tab, space} characters to 1 space, in current text block or selection.
This is similar to `fill-paragraph' or `fill-region' for making a text block into a single line, except that fill command does many other things. For example, if you have

 > some
 > thing

it'll remove the second >."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (save-excursion
       (let (q1 q2)
         (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
             (progn (re-search-forward "\n[ \t]*\n")
                    (setq q1 (point)))
           (setq q1 (point)))
         (if (re-search-forward "\n[ \t]*\n" nil "NOERROR")
             (progn (re-search-backward "\n[ \t]*\n")
                    (setq q2 (point)))
           (setq q2 (point)))
         (list q1 q2)))))
  (save-excursion
    (save-restriction
      (narrow-to-region φbegin φend)
      (goto-char (point-min))
      (while (search-forward-regexp "[ \t]*\n[ \t]*" nil t) (replace-match " ")))))

(defun xah-cycle-hyphen-underscore-space (φbegin φend)
  "Cycle {underscore, space, hypen} chars of current word or text selection.
When called repeatedly, this command cycles the {“_”, “-”, “ ”} characters, in that order.

When called in elisp code, φbegin φend are region begin/end positions.
URL `http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html'
Version 2015-04-13"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let ((ξbounds (bounds-of-thing-at-point 'symbol)))
       (list (car ξbounds) (cdr ξbounds)))))
  ;; this function sets a property 「'state」. Possible values are 0 to length of ξcharArray.
  (let* ((ξinputText (buffer-substring-no-properties φbegin φend))
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
        (narrow-to-region φbegin φend)
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
      (goto-char φend)
      (set-mark φbegin)
      (setq deactivate-mark nil))
    (put 'xah-cycle-hyphen-underscore-space 'state (% (+ ξnowState 1) ξlength))))



(defun xah-copy-file-path (&optional φdir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
If `universal-argument' is called, copy only the dir path.
Version 2015-01-14
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'"
  (interactive "P")
  (let ((fPath
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name))))
    (kill-new
     (if (equal φdir-path-only-p nil)
         fPath
       (file-name-directory fPath)))
    (message "File path copied: 「%s」" fPath)))

(defun xah-delete-text-block ()
  "delete the current text block (paragraph) and also put it to `kill-ring'."
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
    (kill-region p1 p2)
    (delete-blank-lines)))

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

(defun xah-escape-quotes ()
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
That is, add 1 backslash in front of double quote (Unicode codepoint 34).
See also: `xah-unescape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2015-04-29"
  (interactive)
  (let (p1 p2)
    (if (use-region-p)
        (progn (setq p1 (region-beginning))
               (setq p2 (region-end)))
      (progn (setq p1 (line-beginning-position))
             (setq p2 (line-end-position))))
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (goto-char (point-min))
        (while (search-forward "\"" nil t)
          (replace-match "\\\"" 'FIXEDCASE 'LITERAL))))))

(defun xah-unescape-quotes ()
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
That is, remove 1 backslash in front of double quote (Unicode codepoint 34), if exist.
See also: `xah-escape-quotes'

URL `http://ergoemacs.org/emacs/elisp_escape_quotes.html'
Version 2015-04-29"
  (interactive)
  (let (p1 p2)
    (if (use-region-p)
        (progn (setq p1 (region-beginning))
               (setq p2 (region-end)))
      (progn (setq p1 (line-beginning-position))
             (setq p2 (line-end-position))))
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (goto-char (point-min))
        (while (search-forward "\\\"" nil t)
          (replace-match "\"" 'FIXEDCASE 'LITERAL))))))

(defun xah-title-case-region-or-line (φbegin φend)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, φbegin φend are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2015-04-08"
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
         ξstrPairs)))))