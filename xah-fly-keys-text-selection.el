;; -*- coding: utf-8 -*-

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

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun xah-semnav-up (arg)
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

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun xah-extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit.

This command works mostly in lisp syntax."
  (interactive (list (prefix-numeric-value current-prefix-arg)
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
Delimiters here includes the following chars: \"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕
This command does not properly deal with nested brackets.
URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
Version 2015-04-02"
  (interactive)
  (let (ξp1
        ξp2
        (ξskipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
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
