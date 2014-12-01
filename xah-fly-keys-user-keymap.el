;; -*- coding: utf-8 -*-

;; (require 'xmsi-math-symbols-input)
;; (define-key xmsi-keymap (kbd "S-SPC") 'xmsi-change-to-symbol)
;; (define-key xmsi-keymap (kbd "<end> <menu>") nil)

(progn
  ;; this should reserved for user-defined keys
  (define-prefix-command 'xah-user-keymap)

  (define-key xah-user-keymap (kbd "SPC") 'xah-toggle-previous-letter-case)
  (define-key xah-user-keymap (kbd "<menu>") nil)
  (define-key xah-user-keymap (kbd "<return>") 'xah-run-current-file)
  (define-key xah-user-keymap (kbd "<backspace>") 'xah-delete-current-file)
  (define-key xah-user-keymap (kbd "<tab>") nil)
  (define-key xah-user-keymap (kbd "<delete>") nil)
  (define-key xah-user-keymap (kbd "<home>") nil)
  (define-key xah-user-keymap (kbd "<end>") nil)

  (define-key xah-user-keymap (kbd "-") 'xah-insert-form-feed)
  (define-key xah-user-keymap (kbd ".") 'title-case-string-region-or-line)

  (define-key xah-user-keymap (kbd "1") 'xah-copy-to-register-1)
  (define-key xah-user-keymap (kbd "2") 'xah-paste-from-register-1)
  (define-key xah-user-keymap (kbd "3") 'xah-asciify-region)
  (define-key xah-user-keymap (kbd "4") nil)
  (define-key xah-user-keymap (kbd "5") nil)
  (define-key xah-user-keymap (kbd "6") nil)
  (define-key xah-user-keymap (kbd "7") nil)
  (define-key xah-user-keymap (kbd "8") nil)
  (define-key xah-user-keymap (kbd "9") nil)
  (define-key xah-user-keymap (kbd "0") nil)

  (define-key xah-user-keymap (kbd "a") nil)
  (define-key xah-user-keymap (kbd "b") 'xah-shell-commands)
  (define-key xah-user-keymap (kbd "c") 'xah-cite)
  (define-key xah-user-keymap (kbd "d") 'insert-date)
  (define-key xah-user-keymap (kbd "e") 'xah-open-file-fast)
  (define-key xah-user-keymap (kbd "f") 'xah-find-text)
  (define-key xah-user-keymap (kbd "g g") 'xah-dired-scale-image)
  (define-key xah-user-keymap (kbd "g r") 'xah-dired-2png)
  (define-key xah-user-keymap (kbd "g c") 'xah-dired-2jpg)
  (define-key xah-user-keymap (kbd "g b") 'xah-dired-crop-image)
  (define-key xah-user-keymap (kbd "g d") 'xah-dired-image-autocrop)
  (define-key xah-user-keymap (kbd "g f") 'xah-dired-2drawing)

  (define-key xah-user-keymap (kbd "h") nil)

  (define-key xah-user-keymap (kbd "i n") 'xah-insert-random-number)
  (define-key xah-user-keymap (kbd "i s") 'xah-insert-random-string)
  (define-key xah-user-keymap (kbd "i u") 'xah-insert-random-uuid)
  (define-key xah-user-keymap (kbd "i x") 'xah-insert-random-hex)
  (define-key xah-user-keymap (kbd "j") nil)

  (define-key xah-user-keymap (kbd "j t") 'xwe-move-word-to-page)
  (define-key xah-user-keymap (kbd "j t") 'xwe-new-word-entry)
  (define-key xah-user-keymap (kbd "j t") 'xwe-insert-word-entry)
  (define-key xah-user-keymap (kbd "j t") 'xwe-add-definition)
  (define-key xah-user-keymap (kbd "j t") 'xwe-add-source)
  (define-key xah-user-keymap (kbd "j t") 'xwe-add-comment)
  (define-key xah-user-keymap (kbd "j t") 'xwe-search-next-unbold)
  (define-key xah-user-keymap (kbd "j t") 'xwe-chinese-linkify)
  (define-key xah-user-keymap (kbd "j k") 'xwe-annotate)
  (define-key xah-user-keymap (kbd "j e") 'xwe-word-etymology-linkify)
  (define-key xah-user-keymap (kbd "j t") 'xwe-query-find-then-bold)
  (define-key xah-user-keymap (kbd "j t") 'xwe-find-word-usage)

  (define-key xah-user-keymap (kbd "k") nil)
  (define-key xah-user-keymap (kbd "l") nil)
  (define-key xah-user-keymap (kbd "m") 'magit-status)
  (define-key xah-user-keymap (kbd "n") 'xah-make-backup)
  (define-key xah-user-keymap (kbd "o") 'xah-open-file-from-clipboard)
  (define-key xah-user-keymap (kbd "p") 'xah-copy-file-path)
  (define-key xah-user-keymap (kbd "q") 'xah-replace-BOM-mark-etc)
  (define-key xah-user-keymap (kbd "r c") 'xah-escape-quotes)
  (define-key xah-user-keymap (kbd "r '") 'xah-replace-straight-quotes)
  (define-key xah-user-keymap (kbd "r ,") 'xah-remove-punctuation-trailing-redundant-space)
  (define-key xah-user-keymap (kbd "r .") 'xah-convert-english-chinese-punctuation)
  (define-key xah-user-keymap (kbd "r [") 'xah-remove-square-brackets)
  (define-key xah-user-keymap (kbd "r u") 'xah-decode-percent-encoded-uri)

  (define-key xah-user-keymap (kbd "r d") 'fix-datetimestamp)
  (define-key xah-user-keymap (kbd "r g") 'xah-convert-latin-alphabet-gothic)
  (define-key xah-user-keymap (kbd "r p") 'xah-convert-asian/ascii-space)
  (define-key xah-user-keymap (kbd "r p") 'xah-replace-profanity)
  (define-key xah-user-keymap (kbd "r t") 'xah-twitterfy)
  (define-key xah-user-keymap (kbd "r w") 'xah-convert-fullwidth-chars)

  (define-key xah-user-keymap (kbd "s") nil)
  (define-key xah-user-keymap (kbd "t") nil)
  (define-key xah-user-keymap (kbd "u") 'xah-find-replace-text)
  (define-key xah-user-keymap (kbd "v") nil)
  (define-key xah-user-keymap (kbd "w") 'xah-angle-brackets-to-html)
  (define-key xah-user-keymap (kbd "y") 'xah-open-last-closed)
  (define-key xah-user-keymap (kbd "z") 'xah-toggle-read-novel-mode)

  )

(progn
  ;; command dump. temp, rare, or whatever. put them here to have a key for now. worry later
  (define-prefix-command 'xah-dump-keymap)

  (define-key xah-dump-keymap (kbd "SPC") nil)
  (define-key xah-dump-keymap (kbd "<return>") nil)

  (define-key xah-dump-keymap (kbd "<f9>") nil)
  (define-key xah-dump-keymap (kbd "<f10>") nil)
  (define-key xah-dump-keymap (kbd "<f11>") nil)
  (define-key xah-dump-keymap (kbd "<f12>") nil)

  (define-key xah-dump-keymap (kbd "<backspace>") 'xah-remove-overlays-region)
  (define-key xah-dump-keymap (kbd "<return>") 'xah-show-overlay-at-point)

  (define-key xah-dump-keymap (kbd "7") 'xah-syntax-bracket-backward)
  (define-key xah-dump-keymap (kbd "8") 'xah-syntax-bracket-forward)

  (define-key xah-dump-keymap (kbd "a") 'xah-show-all-overlays)
  (define-key xah-dump-keymap (kbd "b") 'xah-make-overlay-bold-region)
  (define-key xah-dump-keymap (kbd "c") 'xah-css-mode)
  (define-key xah-dump-keymap (kbd "d") nil)
  (define-key xah-dump-keymap (kbd "e") 'xah-elisp-mode)
  (define-key xah-dump-keymap (kbd "f") nil)
  (define-key xah-dump-keymap (kbd "g") nil)
  (define-key xah-dump-keymap (kbd "h") 'xah-html-mode)
  (define-key xah-dump-keymap (kbd "i") nil)
  (define-key xah-dump-keymap (kbd "j") 'xah-js-mode)
  (define-key xah-dump-keymap (kbd "k") nil)
  (define-key xah-dump-keymap (kbd "l") 'xah-scan-list)
  (define-key xah-dump-keymap (kbd "l") nil)
  (define-key xah-dump-keymap (kbd "m") nil)
  (define-key xah-dump-keymap (kbd "n") nil)
  (define-key xah-dump-keymap (kbd "o") nil)
  (define-key xah-dump-keymap (kbd "p") 'xah-parse-partial-sexp)
  (define-key xah-dump-keymap (kbd "p") nil)
  (define-key xah-dump-keymap (kbd "q") nil)
  (define-key xah-dump-keymap (kbd "r") nil)
  (define-key xah-dump-keymap (kbd "s") 'xah-scan-sexps)
  (define-key xah-dump-keymap (kbd "t") 'xah-clojure-mode)
  (define-key xah-dump-keymap (kbd "u") nil)
  (define-key xah-dump-keymap (kbd "v") nil)
  (define-key xah-dump-keymap (kbd "w") nil)
  (define-key xah-dump-keymap (kbd "x") nil)
  (define-key xah-dump-keymap (kbd "y") nil)
  (define-key xah-dump-keymap (kbd "z") nil)

  )

(progn

  (define-prefix-command 'xah-insert-keymap)

  (define-key xah-insert-keymap (kbd "RET") 'ucs-insert)

  (define-key key-translation-map (kbd "<menu> SPC .") nil)

  (define-key key-translation-map (kbd "<menu> SPC <down>") (kbd "↓"))
  (define-key key-translation-map (kbd "<menu> SPC <left>") (kbd "←"))
  (define-key key-translation-map (kbd "<menu> SPC <right>") (kbd "→"))
  (define-key key-translation-map (kbd "<menu> SPC <up>") (kbd "↑"))
  (define-key key-translation-map (kbd "<menu> SPC \\") (kbd "、")) ; IDEOGRAPHIC COMMA

  (define-key xah-insert-keymap (kbd ".") 'xah-insert-unicode)
  (define-key xah-insert-keymap (kbd ",") nil)

  (define-key key-translation-map (kbd "<menu> SPC 3") (kbd "φ"))
  (define-key key-translation-map (kbd "<menu> SPC 4") (kbd "ξ"))

  (define-key key-translation-map (kbd "<menu> SPC 6") (kbd "ƒ"))
  (define-key key-translation-map (kbd "<menu> SPC 7") (kbd "＆"))
  (define-key key-translation-map (kbd "<menu> SPC 8") (kbd "•"))
  (define-key key-translation-map (kbd "<menu> SPC 9") (kbd "—")) ; EM DASH

  (define-key xah-insert-keymap (kbd "a") nil)
  (define-key xah-insert-keymap (kbd "b") 'xah-insert-black-lenticular-bracket【】)
  (define-key xah-insert-keymap (kbd "c") 'xah-insert-ascii-single-quote)
  (define-key xah-insert-keymap (kbd "d") 'xah-insert-double-curly-quote“”)
  (define-key xah-insert-keymap (kbd "e") 'xah-insert-greater-less)
  (define-key xah-insert-keymap (kbd "f") 'xah-insert-emacs-quote)
  (define-key xah-insert-keymap (kbd "g") 'xah-insert-ascii-double-quote)
  (define-key xah-insert-keymap (kbd "h") 'xah-insert-brace) ; {}
  (define-key xah-insert-keymap (kbd "i") 'xah-insert-curly-single-quote‘’)
  (define-key xah-insert-keymap (kbd "j") nil)
  (define-key xah-insert-keymap (kbd "k") nil)
  (define-key key-translation-map (kbd "<menu> SPC l") (kbd "…")) ; HORIZONTAL ELLIPSIS
  (define-key xah-insert-keymap (kbd "m") 'xah-insert-corner-bracket「」)
  (define-key xah-insert-keymap (kbd "n") 'xah-insert-bracket)            ;[]
  (define-key xah-insert-keymap (kbd "o") nil)
  (define-key xah-insert-keymap (kbd "p") 'xah-insert-single-angle-quote‹›)
  (define-key xah-insert-keymap (kbd "q") nil)
  (define-key xah-insert-keymap (kbd "r") 'xah-insert-tortoise-shell-bracket〔〕)
  (define-key xah-insert-keymap (kbd "s") nil)
  (define-key xah-insert-keymap (kbd "t") 'xah-insert-paren)
  (define-key xah-insert-keymap (kbd "u") nil)
  (define-key xah-insert-keymap (kbd "v") nil)
  (define-key xah-insert-keymap (kbd "w") 'xah-insert-angle-bracket〈〉)
  (define-key xah-insert-keymap (kbd "W") 'xah-insert-double-angle-bracket《》)
  (define-key xah-insert-keymap (kbd "x") nil)
  (define-key xah-insert-keymap (kbd "y") 'xah-insert-double-angle-quote«»)
  (define-key xah-insert-keymap (kbd "z") 'xah-insert-word-3)

  )

;; temp
(global-set-key (kbd "<C-f7>") 'xah-goto-previous-overlay)
(global-set-key (kbd "<C-f8>") 'xah-goto-next-overlay)
(global-set-key (kbd "<C-f9>") 'xah-syntax-bracket-backward)
(global-set-key (kbd "<C-f10>") 'xah-syntax-bracket-forward)
