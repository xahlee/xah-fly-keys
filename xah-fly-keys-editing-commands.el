;; -*- coding: utf-8 -*-

(defun xah-delete-current-line ()
  "Delete current line."
  (interactive)
  (delete-region (line-beginning-position) (line-end-position))
  (when (looking-at "\n")
    (delete-char 1)))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When `universal-argument' is called first, copy whole buffer (but respect `narrow-to-region')."
  (interactive)
  (let (p1 p2)
    (if (null current-prefix-arg)
        (progn (if (use-region-p)
                   (progn (setq p1 (region-beginning))
                          (setq p2 (region-end)))
                 (progn (setq p1 (line-beginning-position))
                        (setq p2 (line-end-position)))))
      (progn (setq p1 (point-min))
             (setq p2 (point-max))))
    (kill-ring-save p1 p2)))

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (but respect `narrow-to-region')."
  (interactive)
  (let (p1 p2)
    (if (null current-prefix-arg)
        (progn (if (use-region-p)
                   (progn (setq p1 (region-beginning))
                          (setq p2 (region-end)))
                 (progn (setq p1 (line-beginning-position))
                        (setq p2 (line-beginning-position 2)))))
      (progn (setq p1 (point-min))
             (setq p2 (point-max))))
    (kill-region p1 p2)))

(defun xah-copy-all ()
  "Put the whole buffer content into the `kill-ring'.
If `narrow-to-region' is in effect, then copy that region only."
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copied."))

(defun xah-cut-all ()
  "Cut the whole buffer content into the `kill-ring'.
If `narrow-to-region' is in effect, then cut that region only."
  (interactive)
  (kill-new (buffer-string))
  (delete-region (point-min) (point-max)))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)

  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (use-region-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds) p2 (cdr bds))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps"))
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
         (t (put this-command 'state "all lower")))))

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one or none.
If current line does have visible characters: shrink whitespace around cursor to just one space.
If current line does not have visible chars, then shrink all neighboring blank lines to just one.
If current line is a single space, remove that space.
URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
version 2014-10-21"
  (interactive)
  (let ((ξpos (point))
        ξline-has-meat-p ; current line contains non-white space chars
        ξspace-tab-neighbor-p
        ξwhitespace-begin ξwhitespace-end
        ξspace-or-tab-begin ξspace-or-tab-end
        )
    (save-excursion
      (setq ξspace-tab-neighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil))
      (beginning-of-line)
      (setq ξline-has-meat-p (search-forward-regexp "[[:graph:]]" (line-end-position) t))

      (goto-char ξpos)
      (skip-chars-backward "\t ")
      (setq ξspace-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq ξwhitespace-begin (point))

      (goto-char ξpos)
      (skip-chars-forward "\t ")
      (setq ξspace-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq ξwhitespace-end (point)))

    (if ξline-has-meat-p
        (let (ξdeleted-text)
          (when ξspace-tab-neighbor-p
            ;; remove all whitespaces in the range
            (setq ξdeleted-text (delete-and-extract-region ξspace-or-tab-begin ξspace-or-tab-end))
            ;; insert a whitespace only if we have removed something different than a simple whitespace
            (if (not (string= ξdeleted-text " "))
                (insert " "))))
      (progn (delete-blank-lines)))))

(defun xah-compact-uncompact-block ()
  "Remove or insert newline characters on the current block of text.
This is similar to a toggle for `fill-paragraph' and `unfill-paragraph'.

When there is a text selection, act on the the selection, else, act on a text block separated by blank lines."
  (interactive)
  ;; This command symbol has a property “'stateIsCompact-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again
  (let ( currentStateIsCompact
         (deactivate-mark nil)
         (bigFillColumnVal 4333999)
         (blanklinesRegex "\n[ \t]*\n"))

    (save-excursion
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil)))

      (if (use-region-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))))

        (let (p1 p2)
          (progn
            ;; set p1 p2 as boundary of text block
            (if (re-search-backward blanklinesRegex nil "move")
                (progn (re-search-forward blanklinesRegex)
                       (setq p1 (point)))
              (setq p1 (point)))
            (if (re-search-forward blanklinesRegex nil "move")
                (progn (re-search-backward blanklinesRegex)
                       (setq p2 (point)))
              (setq p2 (point))))

          (if currentStateIsCompact
              (fill-region p1 p2)
            (let ((fill-column bigFillColumnVal))
              (fill-region p1 p2)))))

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))

(defun xah-cycle-hyphen-underscore-space ()
  "Cyclically replace {underscore, space, hypen} chars on current word or text selection.
When called repeatedly, this command cycles the {“_”, “-”, “ ”} characters."
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of charArray.
  (let (inputText bds charArray p1 p2 currentState nextState changeFrom
                  changeTo startedWithRegion-p )
    (if (region-active-p)
        (setq startedWithRegion-p t )
      (setq startedWithRegion-p nil ))

    (setq bds (get-selection-or-unit 'glyphs))
    (setq inputText (elt bds 0) p1 (elt bds 1) p2 (elt bds 2))

    (setq charArray [" " "_" "-"])

    ;; when called first time, set state to 0
    (setq currentState
          (if (equal last-command this-command )
              (get 'xah-cycle-hyphen-underscore-space 'state)
            0 ))

    (setq nextState (% (+ currentState 1) (length charArray)))
    (setq changeFrom (elt charArray currentState ))
    (setq changeTo (elt charArray nextState ))

    (setq inputText (replace-regexp-in-string changeFrom changeTo (buffer-substring-no-properties p1 p2)))
    (delete-region p1 p2)
    (insert inputText)

    (when (or (string= changeTo " ") startedWithRegion-p)
      (goto-char p2)
      (set-mark p1)
      (setq deactivate-mark nil))

    (put 'xah-cycle-hyphen-underscore-space 'state nextState)))

(defun xah-toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor."
  (interactive)
  (let ((case-fold-search nil))
    (left-char 1)
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point))))
     )
    (right-char)))



(defun xah-copy-file-path (&optional φdir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
If `universal-argument' is called, copy only the dir path."
  (interactive "P")
  (let ((fPath
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name))))
    (kill-new
     (if (equal φdir-path-only-p nil)
         fPath
       (file-name-directory fPath))))
  (message "File path copied."))

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



(defun xah-copy-rectangle-to-clipboard (φp1 φp2)
  "Copy region as column (rectangle) to operating system's clipboard.
This command will also put the text in register 0.

See also: `kill-rectangle', `copy-to-register'."
  (interactive "r")
  (let ((x-select-enable-clipboard t))
    (copy-rectangle-to-register ?0 φp1 φp2)
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

