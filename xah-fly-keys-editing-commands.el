;; -*- coding: utf-8 -*-

(defun xah-cut-line-or-region ()
  "Cut the current line, or text selection."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))

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
      (let ((bds (bounds-of-thing-at-point 'symbol)))
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
  "Remove white spaces around cursor to just one or none.
If current line does have visible chars, then shrink whitespace surrounding cursor to just one space.
If current line does not have visible chars, then shrink al neighboring blank lines to just one.
If current line is a single space, remove that space.

Calling this command 3 times will always result in no whitespaces around cursor."
  (interactive)
  (let ((pos (point))
        line-has-meat-p ; current line contains non-white space chars
        space-tab-neighbor-p
        whitespace-begin whitespace-end
        space-or-tab-begin space-or-tab-end
        )
    (save-excursion
      ;; todo: might consider whitespace as defined by syntax table, and also consider whitespace chars in unicode if syntax table doesn't already considered it.
      (setq space-tab-neighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil))
      (beginning-of-line)
      (setq line-has-meat-p (search-forward-regexp "[[:graph:]]" (line-end-position) t))

      (goto-char pos)
      (skip-chars-backward "\t ")
      (setq space-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq whitespace-begin (point))

      (goto-char pos)
      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq whitespace-end (point)))

    (if line-has-meat-p
        (let (deleted-text)
          (when space-tab-neighbor-p
            ;; remove all whitespaces in the range
            (setq deleted-text (delete-and-extract-region space-or-tab-begin space-or-tab-end))
            ;; insert a whitespace only if we have removed something
            ;; different that a simple whitespace
            (if (not (string= deleted-text " "))
                (insert " "))))

      (progn
        (delete-blank-lines)
        ;; (delete-region whitespace-begin whitespace-end)
        ;; (insert "\n\n")
        ))))

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