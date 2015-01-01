;; -*- coding: utf-8 -*-

(defun xah-latex-to-unicode (φp1 φp2)
  "Replace TeX markup to Unicode.
⁖  \\alpha becomes α"

;; (interactive
;;    (let (p1 p2)
;;      (if (re-search-backward "\n[ \t]*\n" nil "move")
;;          (progn (re-search-forward "\n[ \t]*\n")
;;                 (setq p1 (point)))
;;        (setq p1 (point)))
;;      (if (re-search-forward "\n[ \t]*\n" nil "move")
;;          (progn (re-search-backward "\n[ \t]*\n")
;;                 (setq p2 (point)))
;;        (setq p2 (point))))
;;    (list p1 p2))

(interactive "r")
  ;; (message "%s %s" φp1 φp2)
;; TODO with no text selection, it seems to act on some region starting at point. Expected behavior is error or empty region 
;; check behavior of (interactive "r") when no region. and same for replace-pairs-region
  (replace-pairs-region φp1 φp2 '(
                                  ["\\rightarrow" "→"]
                                  ["\\Sigma" "Σ"]
                                  ["\\times" "×"]
                                  ["\\alpha" "α"]
                                  ["\\beta" "β"]
                                  ["\\gamma" "γ"]
                                  ["\\delta" "δ"]
                                  ["\\Lambda" "Λ"]
                                  ["\\epsilon" "ε"]
                                  ["\\cup" "∪"]
                                  ["\\in" "∈"]
                                  )))

(defun xah-replace-text-to-latex-region (φp1 φp2)
  "Replace some math function names or symbols by their LaTeX markup."
  (interactive "r")
(replace-pairs-region φp1 φp2 '(
["*" "\\ "]
["cos(" "\\cos("]
["sin(" "\\sin("]
["tan(" "\\tan("]
[" pi" "\\!\\pi"]
["R^2" "\\mathbb{R}^2"]
["R^3" "\\mathbb{R}^3"]
)))

(defun xah-replace-mathematica-symbols-region (φp1 φp2)
  "Replace Mathematica's special char markup to Unicode.
For example:
 \\=\\[Infinity] ⇒ ∞
 \\=\\[Equal] ⇒ =="
  (interactive "r")
  (replace-pairs-region φp1 φp2 '(
 ["\\[Infinity]" "∞"]
 ["\\[Equal]" "=="])))

(defun xah-replace-greek-region (φp1 φp2)
  "Replace math symbols. e.g. alpha to α."
  (interactive "r")
(replace-pairs-region φp1 φp2 '(
["alpha" "α"]
["beta" "β"]
["gamma" "γ"]
["theta" "θ"]
["lambda" "λ"]
["delta" "δ"]
["epsilon" "φ"]
["omega" "ω"]
["Pi" "π"])))

(defun xah-mathematica-to-lsl-region (φp1 φp2)
  "Change Mathematica syntax to LSL syntax on region.

LSL is Linden Scripting Language.
This command does simple string replacement only."
  (interactive "r")
(replace-pairs-region φp1 φp2 '(
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
The goal of these replacement is to reduce the file size of a Mathematica Graphics file (.mgs) that are read over the net by JavaView."
  (interactive)

  (goto-char 1)
  (while (search-forward "\n" nil t) (replace-match "" nil t))

  (goto-char 1)
  (while (search-forward-regexp "  +" nil t) (replace-match " " nil t))

  (goto-char 1)
  (while (search-forward ", " nil t) (replace-match "," nil t))

  (goto-char 1)
  (while (search-forward-regexp "\\([0-9]\\)\\.\\([0-9][0-9][0-9]\\)[0-9]+" nil t) (replace-match "\\1.\\2" t nil)))
