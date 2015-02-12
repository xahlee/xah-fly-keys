;; -*- coding: utf-8 -*-

(defun xah-select-text-in-quote-by-syntax-table ()
  "Select text between ASCII quotes, single or double."
  (interactive)
  (let (p1 p2 (parse-sexp-lookup-properties nil)
           (ξtemp-syn-table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" ξtemp-syn-table)
    (with-syntax-table ξtemp-syn-table
      (if (nth 3 (syntax-ppss))
          (progn
            (if (>= emacs-major-version 25)
                (backward-up-list 1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING")
              (backward-up-list 1))
            (setq p1 (point))
            (forward-sexp 1)
            (setq p2 (point))
            (goto-char (1+ p1))
            (set-mark (1- p2)))
        (progn
          (user-error "Cursor not inside quote"))))))

(defun xah-select-text-in-bracket-or-quote-by-syntax-table ()
  "Select text between the nearest brackets or quote."
  (interactive)
  (let (pos p1 p2 (parse-sexp-lookup-properties nil)
            (ξtemp-syn-table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" ξtemp-syn-table)
    (modify-syntax-entry ?\« "(»" ξtemp-syn-table)
    (modify-syntax-entry ?\» ")«" ξtemp-syn-table)
    (modify-syntax-entry ?\‹ "(›" ξtemp-syn-table)
    (modify-syntax-entry ?\› ")‹" ξtemp-syn-table)
    (modify-syntax-entry ?\“ "(”" ξtemp-syn-table)
    (modify-syntax-entry ?\” ")“" ξtemp-syn-table)
    (when (or
           (string= major-mode "xah-html-mode")
           (string= major-mode "xml-mode")
           (string= major-mode "nxml-mode")
           (string= major-mode "html-mode"))
      (modify-syntax-entry ?\> "(<" ξtemp-syn-table)
      (modify-syntax-entry ?\< ")>" ξtemp-syn-table))

    (with-syntax-table ξtemp-syn-table
      (if (nth 3 (syntax-ppss))
          (xah-select-text-in-quote-by-syntax-table)
        (xah-select-text-in-bracket-by-syntax-table)))))

(defun xah-select-text-in-bracket-by-syntax-table ()
  "Select text between the nearest brackets.
⁖  () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄ ⟨⟩."
  (interactive)
  (let (pos p1 p2 (parse-sexp-lookup-properties nil)
            (ξtemp-syn-table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" ξtemp-syn-table)
    (modify-syntax-entry ?\« "(»" ξtemp-syn-table)
    (modify-syntax-entry ?\» ")«" ξtemp-syn-table)
    (modify-syntax-entry ?\‹ "(›" ξtemp-syn-table)
    (modify-syntax-entry ?\› ")‹" ξtemp-syn-table)
    (modify-syntax-entry ?\“ "(”" ξtemp-syn-table)
    (modify-syntax-entry ?\” ")“" ξtemp-syn-table)
    (when (or
           (string= major-mode "xah-html-mode")
           (string= major-mode "xml-mode")
           (string= major-mode "nxml-mode")
           (string= major-mode "html-mode"))
      (modify-syntax-entry ?\> "(<" ξtemp-syn-table)
      (modify-syntax-entry ?\< ")>" ξtemp-syn-table))

    (with-syntax-table ξtemp-syn-table
      (setq pos (point))
      (search-backward-regexp "\\s(" nil t )
      (setq p1 (point))
      (forward-sexp 1)
      (setq p2 (point))

      (goto-char (1+ p1))
      (set-mark (1- p2)))))