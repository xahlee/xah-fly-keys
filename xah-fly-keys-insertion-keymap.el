;; -*- coding: utf-8 -*-

(progn

  (define-prefix-command 'xah-insertion-keymap)

  (define-key xah-insertion-keymap (kbd "RET") 'ucs-insert)

  (define-key xah-insertion-keymap (kbd ".") 'xah-insert-unicode)
  (define-key xah-insertion-keymap (kbd ",") nil)

  (define-key xah-insertion-keymap (kbd "7") "＆")
  (define-key xah-insertion-keymap (kbd "8") "•")

  (define-key xah-insertion-keymap (kbd "a") nil)
  (define-key xah-insertion-keymap (kbd "b") 'xah-insert-black-lenticular-bracket【】)
  (define-key xah-insertion-keymap (kbd "c") 'xah-insert-ascii-single-quote)
  (define-key xah-insertion-keymap (kbd "d") 'xah-insert-double-curly-quote“”)
  (define-key xah-insertion-keymap (kbd "e") 'xah-insert-greater-less)
  (define-key xah-insertion-keymap (kbd "f") 'xah-insert-emacs-quote)
  (define-key xah-insertion-keymap (kbd "g") 'xah-insert-ascii-double-quote)
  (define-key xah-insertion-keymap (kbd "h") 'xah-insert-brace) ; {}
  (define-key xah-insertion-keymap (kbd "i") 'xah-insert-curly-single-quote‘’)
  (define-key xah-insertion-keymap (kbd "j") nil)
  (define-key xah-insertion-keymap (kbd "k") nil)
  (define-key xah-insertion-keymap (kbd "m") 'xah-insert-corner-bracket「」)
  (define-key xah-insertion-keymap (kbd "n") 'xah-insert-bracket) ; []
  (define-key xah-insertion-keymap (kbd "o") nil)
  (define-key xah-insertion-keymap (kbd "p") 'xah-insert-single-angle-quote‹›)
  (define-key xah-insertion-keymap (kbd "q") nil)
  (define-key xah-insertion-keymap (kbd "r") 'xah-insert-tortoise-shell-bracket〔〕)
  (define-key xah-insertion-keymap (kbd "s") "= \"\"")
  (define-key xah-insertion-keymap (kbd "t") 'xah-insert-paren)
  (define-key xah-insertion-keymap (kbd "u") nil)
  (define-key xah-insertion-keymap (kbd "v") nil)
  (define-key xah-insertion-keymap (kbd "w") 'xah-insert-double-angle-bracket《》)
  (define-key xah-insertion-keymap (kbd "W") 'xah-insert-angle-bracket〈〉)
  (define-key xah-insertion-keymap (kbd "x") nil)
  (define-key xah-insertion-keymap (kbd "y") 'xah-insert-double-angle-quote«»)
  
  )
