;; -*- coding: utf-8 -*-

(defun xah-replace-latex-to-unicode (φbegin φend)
  "Replace TeX markup to Unicode in current line or selection.
Example: \\alpha becomes α.
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-pairs-region
   φbegin
   φend
   '(
     ["\\rightarrow" "→"]
     ["\\Sigma" "Σ"]
     ["\\times" "×"]
     ["\\alpha" "α"]
     ["\\beta" "β"]
     ["\\gamma" "γ"]
     ["\\delta" "δ"]
     ["\\Lambda" "Λ"]
     ["\\epsilon" "ε"]
     ["\\omega" "ω"]
     ["\\cup" "∪"]
     ["\\in" "∈"]
     )))

(defun xah-replace-text-to-latex-region (φbegin φend)
  "Replace math function names or symbols by their LaTeX markup.
Work on current line or selection.
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-pairs-region
   φbegin
   φend
   '(
     ["*" "\\ "]
     ["cos(" "\\cos("]
     ["sin(" "\\sin("]
     ["tan(" "\\tan("]
     [" pi" "\\!\\pi"]
     ["R^2" "\\mathbb{R}^2"]
     ["R^3" "\\mathbb{R}^3"]
     )))

(defun xah-replace-mathematica-symbols (φbegin φend)
  "Replace Mathematica's special char markup to Unicode in current line or selection.
For example:
 \\=\\[Infinity] ⇒ ∞
 \\=\\[Equal] ⇒ ==
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-pairs-region
   φbegin
   φend
   '(
     ["\\[Infinity]" "∞"]
     ["\\[Equal]" "=="])))

(defun xah-replace-greeks-to-symbols (φbegin φend)
  "Replace alpha to α etc in current line or selection.
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-pairs-region
   φbegin
   φend
   '(
     ["alpha" "α"]
     ["beta" "β"]
     ["gamma" "γ"]
     ["theta" "θ"]
     ["lambda" "λ"]
     ["delta" "δ"]
     ["epsilon" "φ"]
     ["omega" "ω"]
     ["Pi" "π"])))

(defun xah-replace-mathematica-to-lsl (φbegin φend)
  "Change Mathematica syntax to LSL syntax on region.

LSL is Linden Scripting Language.
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-pairs-region
   φbegin
   φend
   '(
     ["Cos[" "llCos("]
     ["Sin[" "llSin("]
     ["Tan[" "llTan("]
     ["Pi" "PI"]
     ["π" "PI"]
     ["{" "<"]
     ["}" ">"])))

(defun xah-clean-Mathematica-graphics-buffer ()
  "Remove whitespace, truncate numbers, of current buffer of Mathematica graphics file.
This command does several find/replace on the current buffer.
Removing spaces, removing new lines, truncate numbers to 3 decimals, etc.
The goal of these replacement is to reduce the file size of a Mathematica Graphics file (.mgs) that are read over the net by JavaView.
Version 2015-04-28"
  (interactive)

  (goto-char 1)
  (while (search-forward "\n" nil t) (replace-match "" nil t))

  (goto-char 1)
  (while (search-forward-regexp "  +" nil t) (replace-match " " nil t))

  (goto-char 1)
  (while (search-forward ", " nil t) (replace-match "," nil t))

  (goto-char 1)
  (while (search-forward-regexp "\\([0-9]\\)\\.\\([0-9][0-9][0-9]\\)[0-9]+" nil t) (replace-match "\\1.\\2" t nil)))
