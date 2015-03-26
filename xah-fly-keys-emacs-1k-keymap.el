;; -*- coding: utf-8 -*-
;; xah's emacs keybinding.

;; • 〈Ergoemacs-vi Mode〉 http://ergoemacs.org/misc/ergoemacs_vi_mode.html
;; • 〈Dvorak Keyboard Layout〉 http://xahlee.info/comp/dvorak_keyboard_layout.html
;; • 〈ErgoEmacs Keybinding〉 http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html
;; • 〈xah_autohotkey.ahk〉 http://xahlee.info/mswin/autohotkey.html
;; • 〈Emacs: How to define Hyper ＆ Super Keys〉 http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;; • 〈Emacs: Remapping Keys Using key-translation-map〉 http://ergoemacs.org/emacs/emacs_key-translation-map.html
;; • 〈Emacs: Add Custom Keys to Enhance Productivity〉 http://ergoemacs.org/emacs/emacs_useful_user_keybinding.html
;; • 〈Keyboard Layouts Fight! Dvorak, Maltron, Colemak, NEO, Bépo, Turkish-F, …〉  http://xahlee.info/kbd/dvorak_and_all_keyboard_layouts.html

;; http://ergoemacs.org/emacs/command-frequency/Xah_Lee_2013-09-04.txt

;; Xah Lee
;; created: 2007-06.



;; 2014-06-06 this file are keybinding of key sequences, starting with the menu key
;; the goal is to have a key system to completely replace emacs 1k+ keys.

;; design sketch

;; the keys are now hardcoded for dvorak.

;; some design principles
;; • each key sequence should have 2 or 3 keys. (counting the lead key) some command may have 4 keys, but any command used daily have max of 3 keys.

;; • the most easy finger positions are (dvorak notation):

;; .p gc
;; eu ht

;; should be the most frequently used. Each is 3-keys sequence. ⁖ 【menu e 3】, 【menu u h】,

;; of these 8 keys, the {. p g} are special. Each used for 2 key sequences, for 3 of the most frequently used commands. 【menu p】 is for query-replace. 【menu g】 is isearch.

;; basically, after the menu key, there are a total of 12 keys to start, 6 for each hand. These keys are on the home row or the row above, and are pressed by 2nd 3rd 4th fingers. (thumb is 1st finger) like this:

;; a key sequence involving SPACE is for user's own definition. For example, 【menu SPACE …】, 【menu t SPACE …】,
;; • the 【menu h …】 is for emacs help. basically equivalent to 【C-h ‹key›】
;; • the 【menu u …】 is for inserting brackets (){}[]""''“”‘’ and other brackets, and for inserting “=” “+” any unicode chars.
;; • the 【menu p】 is for query-replace
;; • the 【menu g】 is for isearch
;; • 【menu enter】 is for execute-extended-command
;; • 【menu menu】  is undecided.

;; • the last key can be a number. ⁖ 【menu 8】, 【menu e 3】. For numbers, 3 4 and 7 8 are easiest. (pressed by 2nd ＆ 3rd fingers)

;; • can a key ends with 【enter】 or 【space】 ? Yes, absolutely. These are top easy keys. There should be some scheme for them to make commands with such key sharing some theme/characteristics. ⁖ commands with keys ending in them should prompt… or they are all related to…
;; • can a key ends in 【menu】 key? I think so, but undecided.

;; • note: the binding should be based on command frequency and the key's ease.
;; Emacs vs vi: How to Compute a Keybinding's Efficiency? http://xahlee.info/kbd/efficiency_of_keybinding_emacs_vs_vim.html
;; Emacs's Command Frequency Statistics http://ergoemacs.org/emacs/command-frequency.html

;; • none of the key sequence should be mapped to a “fast-repeat command”. See 《Emacs: Fast-repeat vs Non-fast-repeat Commands ＆ Keys》 http://xahlee.info/kbd/repeatable_vs_non-repeatable_keys_commands.html

;; the above is the sketch of the design. However, i realized that some exceptions is ok, or even optimal. One thing i learned is that a strict regularity or rule may not be optimal, as some exception or irregularity sometimes makes it more convenient, easier to remember, or make your fingers good because they don't always use the same keys. These reasoning may be fallacy, I don't have a solid analysis on it yet.

(global-set-key (kbd "<end>") 'xah-user-keymap)

(when (string-equal system-type "windows-nt")
  (define-key key-translation-map (kbd "<apps>") (kbd "<menu>")))

(when (string-equal system-type "darwin")
  ;; Mac OS X  doesn't have menu, even if using pc keyboard
  (define-key key-translation-map (kbd "C-p") (kbd "<menu>")))

(define-prefix-command 'xah-menu-keymap)
(global-set-key (kbd "<menu>") 'xah-menu-keymap)

(global-set-key (kbd "<menu> RET") 'smex) ; todo check if bound, else execute-extended-command

(global-set-key (kbd "<menu> <backspace>") nil)
(global-set-key (kbd "<menu> <delete>") nil)
(global-set-key (kbd "<menu> SPC") nil)

(global-set-key (kbd "<menu> <menu>") 'exchange-point-and-mark)

(progn
  (define-prefix-command 'xah-highlight-keymap) ; commands in search-map
  (global-set-key (kbd "<menu> .") xah-highlight-keymap)

  (define-key xah-highlight-keymap (kbd ".") 'isearch-forward-symbol-at-point)
  (define-key xah-highlight-keymap (kbd "s") 'isearch-forward-symbol)
  (define-key xah-highlight-keymap (kbd "w") 'isearch-forward-word)
  (define-key xah-highlight-keymap (kbd "h .") 'highlight-symbol-at-point)
  (define-key xah-highlight-keymap (kbd "h f") 'hi-lock-find-patterns)
  (define-key xah-highlight-keymap (kbd "h l") 'highlight-lines-matching-regexp)
  (define-key xah-highlight-keymap (kbd "h p") 'highlight-phrase)
  (define-key xah-highlight-keymap (kbd "h r") 'highlight-regexp)
  (define-key xah-highlight-keymap (kbd "h u") 'unhighlight-regexp)
  (define-key xah-highlight-keymap (kbd "h w") 'hi-lock-write-interactive-patterns)
)

(global-set-key (kbd "<menu> '") 'quoted-insert)
(global-set-key (kbd "<menu> ,") nil)
(global-set-key (kbd "<menu> -") nil)
(global-set-key (kbd "<menu> /") nil)
(global-set-key (kbd "<menu> ;") nil)
(global-set-key (kbd "<menu> =") nil)
(global-set-key (kbd "<menu> [") nil)
(global-set-key (kbd "<menu> \\") nil)
(global-set-key (kbd "<menu> `") nil)
(global-set-key (kbd "<menu> 1") nil)
(global-set-key (kbd "<menu> 2") 'delete-window)
(global-set-key (kbd "<menu> 3") 'delete-other-windows)
(global-set-key (kbd "<menu> 4") 'split-window-vertically)
(global-set-key (kbd "<menu> 5") 'split-window-horizontally)
(global-set-key (kbd "<menu> 6") nil)
(global-set-key (kbd "<menu> 7") nil)
(global-set-key (kbd "<menu> 8") nil)
(global-set-key (kbd "<menu> 9") nil)
(global-set-key (kbd "<menu> 0") 'ispell-word)

(progn
  (define-prefix-command 'xah-menu-tab-keymap)

  (global-set-key (kbd "<menu> TAB") xah-menu-tab-keymap)

  (define-key xah-menu-tab-keymap (kbd "TAB") 'indent-for-tab-command)

  (define-key xah-menu-tab-keymap (kbd "i") 'complete-symbol)
  (define-key xah-menu-tab-keymap (kbd "g") 'indent-rigidly)
  (define-key xah-menu-tab-keymap (kbd "r") 'indent-region)
  (define-key xah-menu-tab-keymap (kbd "s") 'indent-sexp)

  (define-key xah-menu-tab-keymap (kbd "e") nil)
  (define-key xah-menu-tab-keymap (kbd "e '") 'abbrev-prefix-mark)
  (define-key xah-menu-tab-keymap (kbd "e e") 'edit-abbrevs)
  (define-key xah-menu-tab-keymap (kbd "e p") 'expand-abbrev)
  (define-key xah-menu-tab-keymap (kbd "e r") 'expand-region-abbrevs)
  (define-key xah-menu-tab-keymap (kbd "e u") 'unexpand-abbrev)
  (define-key xah-menu-tab-keymap (kbd "e g") 'add-global-abbrev)
  (define-key xah-menu-tab-keymap (kbd "e a") 'add-mode-abbrev)
  (define-key xah-menu-tab-keymap (kbd "e v") 'inverse-add-global-abbrev)
  (define-key xah-menu-tab-keymap (kbd "e l") 'inverse-add-mode-abbrev)
  (define-key xah-menu-tab-keymap (kbd "e n") 'expand-jump-to-next-slot)
  (define-key xah-menu-tab-keymap (kbd "e p") 'expand-jump-to-previous-slot)

  )



(global-set-key (kbd "<menu> a") 'mark-whole-buffer)

(global-set-key (kbd "<menu> b") 'end-of-buffer)

(progn
  (define-prefix-command 'xah-menu-c-keymap)

  (global-set-key (kbd "<menu> c") xah-menu-c-keymap)
  (define-key xah-menu-c-keymap (kbd ",") 'xah-open-in-external-app)
  (define-key xah-menu-c-keymap (kbd ".") 'find-file)
  (define-key xah-menu-c-keymap (kbd "c") 'bookmark-bmenu-list)
  (define-key xah-menu-c-keymap (kbd "e") 'dired-jump)
  (define-key xah-menu-c-keymap (kbd "g") 'ido-switch-buffer)
  (define-key xah-menu-c-keymap (kbd "h") 'recentf-open-files)
  (define-key xah-menu-c-keymap (kbd "l") 'bookmark-set)
  (define-key xah-menu-c-keymap (kbd "n") 'xah-new-empty-buffer)
  (define-key xah-menu-c-keymap (kbd "o") 'xah-open-in-desktop)
  (define-key xah-menu-c-keymap (kbd "p") 'xah-open-last-closed)
  (define-key xah-menu-c-keymap (kbd "f") 'xah-open-recently-closed)
  (define-key xah-menu-c-keymap (kbd "y") 'xah-list-recently-closed)
  (define-key xah-menu-c-keymap (kbd "r") 'bookmark-jump)
  (define-key xah-menu-c-keymap (kbd "t") 'ibuffer)
  (define-key xah-menu-c-keymap (kbd "u") 'xah-open-file-path-under-cursor)

  )

  (global-set-key (kbd "<menu> d") 'beginning-of-buffer)

  ;; (kbd "<menu> e") is mode-specific
  ;; (kbd "<menu> e SPC") is mode-specific user's keys

(global-set-key (kbd "<menu> f") 'xah-search-current-word)

(global-set-key (kbd "<menu> g") 'isearch-forward)

(global-set-key (kbd "<menu> h") 'xah-help-keymap)

(progn
  (define-prefix-command 'xah-menu-i-keymap) ; commands in goto-map
  (global-set-key (kbd "<menu> i") xah-menu-i-keymap)

  (define-key xah-menu-i-keymap (kbd "TAB") 'move-to-column)
  (define-key xah-menu-i-keymap (kbd "c") 'goto-char)
  (define-key xah-menu-i-keymap (kbd "g") 'goto-line)
  (define-key xah-menu-i-keymap (kbd "n") 'next-error)
  (define-key xah-menu-i-keymap (kbd "p") 'previous-error)
  )

(global-set-key (kbd "<menu> j") 'xah-copy-all)

(global-set-key (kbd "<menu> k") 'yank)

(global-set-key (kbd "<menu> l") 'recenter-top-bottom)

(global-set-key (kbd "<menu> m") 'universal-argument)

(progn
  ;; commands here are harmless (safe). They don't modify text.
  ;; they turn on minor/major mode, change display, prompt, start shell, etc.
  (define-prefix-command 'xah-harmless-keymap)
  (global-set-key (kbd "<menu> n") xah-harmless-keymap)

  (define-key xah-harmless-keymap (kbd "RET") nil)
  (define-key xah-harmless-keymap (kbd "RET F") 'set-file-name-coding-system)
  (define-key xah-harmless-keymap (kbd "RET X") 'set-next-selection-coding-system)
  (define-key xah-harmless-keymap (kbd "RET c") 'universal-coding-system-argument)
  (define-key xah-harmless-keymap (kbd "RET f") 'set-buffer-file-coding-system)
  (define-key xah-harmless-keymap (kbd "RET k") 'set-keyboard-coding-system)
  (define-key xah-harmless-keymap (kbd "RET l") 'set-language-environment)
  (define-key xah-harmless-keymap (kbd "RET p") 'set-buffer-process-coding-system)
  (define-key xah-harmless-keymap (kbd "RET r") 'revert-buffer-with-coding-system)
  (define-key xah-harmless-keymap (kbd "RET t") 'set-terminal-coding-system)
  (define-key xah-harmless-keymap (kbd "RET x") 'set-selection-coding-system)

  (define-key xah-harmless-keymap (kbd "'") 'frame-configuration-to-register)
  (define-key xah-harmless-keymap (kbd ";") 'window-configuration-to-register)

  (define-key xah-harmless-keymap (kbd "1") 'set-input-method)
  (define-key xah-harmless-keymap (kbd "2") 'global-hl-line-mode)
  (define-key xah-harmless-keymap (kbd "3") 'whitespace-mode)
  (define-key xah-harmless-keymap (kbd "4") 'linum-mode)
  (define-key xah-harmless-keymap (kbd "5") 'visual-line-mode)
  (define-key xah-harmless-keymap (kbd "6") 'calendar)
  (define-key xah-harmless-keymap (kbd "7") 'calc)
  (define-key xah-harmless-keymap (kbd "8") 'shell)
  (define-key xah-harmless-keymap (kbd "9") 'shell-command)
  (define-key xah-harmless-keymap (kbd "0") 'shell-command-on-region)

  (define-key xah-harmless-keymap (kbd "a") 'text-scale-adjust)
  (define-key xah-harmless-keymap (kbd "b") 'toggle-debug-on-error)
  (define-key xah-harmless-keymap (kbd "c") 'toggle-case-fold-search)
  (define-key xah-harmless-keymap (kbd "d") 'narrow-to-page)
  (define-key xah-harmless-keymap (kbd "e") 'eshell)
  (define-key xah-harmless-keymap (kbd "f") nil)
  (define-key xah-harmless-keymap (kbd "g") 'toggle-frame-fullscreen)
  (define-key xah-harmless-keymap (kbd "h") 'widen)
  (define-key xah-harmless-keymap (kbd "i") 'make-frame-command)
  (define-key xah-harmless-keymap (kbd "j") nil)
  (define-key xah-harmless-keymap (kbd "k") 'menu-bar-open)
  (define-key xah-harmless-keymap (kbd "l") 'toggle-word-wrap)
  (define-key xah-harmless-keymap (kbd "m") 'global-linum-mode)
  (define-key xah-harmless-keymap (kbd "n") 'narrow-to-region)
  (define-key xah-harmless-keymap (kbd "o") nil)
  (define-key xah-harmless-keymap (kbd "p") nil)
  (define-key xah-harmless-keymap (kbd "q") 'read-only-mode) ; toggle-read-only
  (define-key xah-harmless-keymap (kbd "r") nil)
  (define-key xah-harmless-keymap (kbd "s") 'flyspell-buffer)
  (define-key xah-harmless-keymap (kbd "t") 'narrow-to-defun)
  (define-key xah-harmless-keymap (kbd "u") 'toggle-input-method)
  (define-key xah-harmless-keymap (kbd "v") 'variable-pitch-mode)
  (define-key xah-harmless-keymap (kbd "w") 'eww)
  (define-key xah-harmless-keymap (kbd "x") 'nil)
  (define-key xah-harmless-keymap (kbd "y") 'nil)
  (define-key xah-harmless-keymap (kbd "z") 'abort-recursive-edit)

  (progn
    (define-key xah-harmless-keymap (kbd "SPC") nil)
    (define-key xah-harmless-keymap (kbd "SPC h") 'xah-toggle-read-novel-mode)
    (define-key xah-harmless-keymap (kbd "SPC t") 'xah-toggle-margin-right)
    (define-key xah-harmless-keymap (kbd "SPC n") 'xah-toggle-line-spacing))

  )

(progn
  (define-prefix-command 'xah-menu-o-keymap)
  (global-set-key (kbd "<menu> o") xah-menu-o-keymap)
  )

(global-set-key (kbd "<menu> p") 'query-replace)

(global-set-key (kbd "<menu> q") 'xah-cut-all)

(progn
  ;; kinda replacement related
  (define-prefix-command 'xah-edit-cmds-keymap)
  (global-set-key (kbd "<menu> r") xah-edit-cmds-keymap)

  (define-key xah-edit-cmds-keymap (kbd "1") 'kmacro-start-macro)
  (define-key xah-edit-cmds-keymap (kbd "2") 'kmacro-end-macro)
  (define-key xah-edit-cmds-keymap (kbd "3") 'apply-macro-to-region-lines)
  (define-key xah-edit-cmds-keymap (kbd "4") 'sort-lines)
  (define-key xah-edit-cmds-keymap (kbd "5") 'sort-numeric-fields)
  (define-key xah-edit-cmds-keymap (kbd "6") 'reverse-region)
  (define-key xah-edit-cmds-keymap (kbd "7") 'list-matching-lines)
  (define-key xah-edit-cmds-keymap (kbd "8") 'delete-matching-lines)
  (define-key xah-edit-cmds-keymap (kbd "9") 'delete-non-matching-lines)
  (define-key xah-edit-cmds-keymap (kbd "0") 'delete-duplicate-lines)

  (define-key xah-edit-cmds-keymap (kbd "e") 'call-last-kbd-macro)

  (define-key xah-edit-cmds-keymap (kbd "c") 'replace-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "d") 'delete-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "g") 'kill-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "l") 'clear-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "n") 'rectangle-number-lines)
  (define-key xah-edit-cmds-keymap (kbd "o") 'open-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "r") 'yank-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "t") 'rectangle-mark-mode)
  (define-key xah-edit-cmds-keymap (kbd "y") 'delete-whitespace-rectangle)

  )

(global-set-key (kbd "<menu> s") 'save-buffer)

(progn
  (define-prefix-command 'xah-menu-t-keymap)
  (global-set-key (kbd "<menu> t") xah-menu-t-keymap)

  (define-key xah-menu-t-keymap (kbd "RET") 'pop-global-mark)
  (define-key xah-menu-t-keymap (kbd ".") 'insert-char)

  (define-key xah-menu-t-keymap (kbd "3") 'point-to-register)
  (define-key xah-menu-t-keymap (kbd "4") 'jump-to-register)
  (define-key xah-menu-t-keymap (kbd "5") 'number-to-register)
  (define-key xah-menu-t-keymap (kbd "6") 'increment-register)

  (define-key xah-menu-t-keymap (kbd "e") 'copy-to-register)
  (define-key xah-menu-t-keymap (kbd "h") 'xah-close-current-buffer)
  (define-key xah-menu-t-keymap (kbd "j") nil)
  (define-key xah-menu-t-keymap (kbd "k") nil)
  (define-key xah-menu-t-keymap (kbd "n") 'repeat-complex-command)
  (define-key xah-menu-t-keymap (kbd "p") 'query-replace-regexp)
  (define-key xah-menu-t-keymap (kbd "q") nil)
  (define-key xah-menu-t-keymap (kbd "r") 'copy-rectangle-to-register)
  (define-key xah-menu-t-keymap (kbd "s") 'write-file)
  (define-key xah-menu-t-keymap (kbd "t") 'repeat)
  (define-key xah-menu-t-keymap (kbd "u") 'insert-register)

  )

  (global-set-key (kbd "<menu> u") 'xah-insertion-keymap)

(progn
  (define-prefix-command 'xah-menu-v-keymap)
  (global-set-key (kbd "<menu> v") xah-menu-v-keymap)

  (define-key xah-menu-v-keymap (kbd "+") 'vc-update)
  (define-key xah-menu-v-keymap (kbd "=") 'vc-diff)
  (define-key xah-menu-v-keymap (kbd "D") 'vc-root-diff)
  (define-key xah-menu-v-keymap (kbd "L") 'vc-print-root-log)
  (define-key xah-menu-v-keymap (kbd "a") 'vc-update-change-log)
  (define-key xah-menu-v-keymap (kbd "b") 'vc-switch-backend)
  (define-key xah-menu-v-keymap (kbd "c") 'vc-rollback)
  (define-key xah-menu-v-keymap (kbd "d") 'vc-dir)
  (define-key xah-menu-v-keymap (kbd "g") 'vc-annotate)
  (define-key xah-menu-v-keymap (kbd "h") 'vc-insert-headers)
  (define-key xah-menu-v-keymap (kbd "l") 'vc-print-log)
  (define-key xah-menu-v-keymap (kbd "m") 'vc-merge)
  (define-key xah-menu-v-keymap (kbd "r") 'vc-retrieve-tag)
  (define-key xah-menu-v-keymap (kbd "s") 'vc-create-tag)
  (define-key xah-menu-v-keymap (kbd "u") 'vc-revert)
  (define-key xah-menu-v-keymap (kbd "v") 'vc-next-action)
  (define-key xah-menu-v-keymap (kbd "~") 'vc-revision-other-window)

  )

(progn
  (define-prefix-command 'xah-danger-keymap)
  (global-set-key (kbd "<menu> w") xah-danger-keymap)

  (define-key xah-danger-keymap (kbd "RET") 'xah-run-current-file)
  (define-key xah-danger-keymap (kbd "DEL") 'xah-delete-current-file)

  (define-key xah-danger-keymap (kbd ".") 'eval-buffer)
  (define-key xah-danger-keymap (kbd "e") 'eval-defun)
  (define-key xah-danger-keymap (kbd "m") 'eval-last-sexp)
  (define-key xah-danger-keymap (kbd "p") 'eval-expression)
  (define-key xah-danger-keymap (kbd "q") 'save-buffers-kill-terminal)
  (define-key xah-danger-keymap (kbd "u") 'eval-region)
  (define-key xah-danger-keymap (kbd "w") 'delete-frame)

  )

(global-set-key (kbd "<menu> x") nil)

(global-set-key (kbd "<menu> y") nil)

(global-set-key (kbd "<menu> z") 'comment-dwim)


;;;; misc

;; ~/web/ergoemacs_org/emacs/gnu_emacs_keybinding_C-x.txt

;; some idea about command categories, in context to choosing keys for them

;; • whether a command is frequently needed ⁖ few times a min, hour, day
;; • whether a command has immediate effect, no prompt. ⁖ kill-word vs shell, delete-matching-lines
;; • whether a command is safe to run by mistake. ⁖ whitespace-mode vs eval-buffer

;; idea about key groups
;; all should be sequence of single keys. 2 to 3 keys. All should start with F7. And all commands should be globally useful.
;; • 2 keys vs 3 keys
;; • whether the key ends in a digit key 0 to 9. These probably should be most frequently used, or immediate effect.



;; these commands has a key in emacs, but i decided not to have them.

  ;; C-x 5 C-f  find-file-other-frame
  ;; C-x 5 C-o  display-buffer-other-frame
  ;; C-x 5 .    find-tag-other-frame
  ;; C-x 5 1    delete-other-frames
  ;; C-x 5 b    switch-to-buffer-other-frame
  ;; C-x 5 d    dired-other-frame
  ;; C-x 5 f    find-file-other-frame
  ;; C-x 5 m    compose-mail-other-frame
  ;; C-x 5 r    find-file-read-only-other-frame

;; C-x C-p	mark-page
;; C-x C-l	downcase-region
;; C-x C-u	upcase-region

;; C-x C-t	transpose-lines
;; C-x C-o	delete-blank-lines

;; C-x C-r	find-file-read-only
;; C-x C-v	find-alternate-file

;; C-x =	what-cursor-position, use describe-char instead
;; C-x <	scroll-left
;; C-x >	scroll-right
;; C-x [	backward-page
;; C-x ]	forward-page
;; C-x ^	enlarge-window

;; C-x {	shrink-window-horizontally
;; C-x }	enlarge-window-horizontally
;; C-x DEL	backward-kill-sentence

;; C-x s	save-some-buffers

;; M-o ESC         Prefix Command
;; M-o b           facemenu-set-bold
;; M-o d           facemenu-set-default
;; M-o i           facemenu-set-italic
;; M-o l           facemenu-set-bold-italic
;; M-o o           facemenu-set-face
;; M-o u           facemenu-set-underline
;; M-o M-S         center-paragraph
;; M-o M-o         font-lock-fontify-block
;; M-o M-s         center-line

;; C-x C-z	suspend-frame
;; C-x +	balance-windows

;; C-x k	kill-buffer , use xah-close-current-buffer
;; C-x l	count-lines-page
;; C-x m	compose-mail


;; undecided yet

;; C-x e	kmacro-end-and-call-macro
;; C-x q	kbd-macro-query
;; C-x C-k	kmacro-keymap

;; C-x C-d	list-directory
;; C-x C-n	set-goal-column
;; C-x ESC	Prefix Command
;; C-x $	set-selective-display
;; C-x *	calc-dispatch
;; C-x -	shrink-window-if-larger-than-buffer
;; C-x .	set-fill-prefix

;; C-x 4	ctl-x-4-prefix
;; C-x 5	ctl-x-5-prefix
;; C-x 6	2C-command
;; C-x ;	comment-set-column

;; C-x `	next-error
;; C-x f	set-fill-column
;; C-x i	insert-file
;; C-x n	Prefix Command
;; C-x r	Prefix Command

;; C-x C-k C-a	kmacro-add-counter
;; C-x C-k C-c	kmacro-set-counter
;; C-x C-k C-d	kmacro-delete-ring-head
;; C-x C-k C-e	kmacro-edit-macro-repeat
;; C-x C-k C-f	kmacro-set-format
;; C-x C-k TAB	kmacro-insert-counter
;; C-x C-k C-k	kmacro-end-or-call-macro-repeat
;; C-x C-k C-l	kmacro-call-ring-2nd-repeat
;; C-x C-k RET	kmacro-edit-macro
;; C-x C-k C-n	kmacro-cycle-ring-next
;; C-x C-k C-p	kmacro-cycle-ring-previous
;; C-x C-k C-s	kmacro-start-macro
;; C-x C-k C-t	kmacro-swap-ring
;; C-x C-k C-v	kmacro-view-macro-repeat
;; C-x C-k SPC	kmacro-step-edit-macro
;; C-x C-k b	kmacro-bind-to-key
;; C-x C-k e	edit-kbd-macro
;; C-x C-k l	kmacro-edit-lossage
;; C-x C-k n	kmacro-name-last-macro
;; C-x C-k q	kbd-macro-query
;; C-x C-k r	apply-macro-to-region-lines
;; C-x C-k s	kmacro-start-macro



;; C-x 4 C-f	find-file-other-window
;; C-x 4 C-o	display-buffer
;; C-x 4 .	find-tag-other-window
;; C-x 4 0	kill-buffer-and-window
;; C-x 4 a	add-change-log-entry-other-window
;; C-x 4 b	switch-to-buffer-other-window
;; C-x 4 c	clone-indirect-buffer-other-window
;; C-x 4 d	dired-other-window
;; C-x 4 f	find-file-other-window
;; C-x 4 m	compose-mail-other-window
;; C-x 4 r	find-file-read-only-other-window

;; C-x 6 2	2C-two-columns
;; C-x 6 b	2C-associate-buffer
;; C-x 6 s	2C-split
;; C-x 6 <f2>	2C-two-columns

;; ;; todo
;; select all, copy all, open, those standard keys

;; • add all emacs commands to my key sequence system

;; ;; 2013-11-04 make emacs auto show suggestions when a prefix key is pressed
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence '("<menu> t" "TAB t" ))
;; (guide-key-mode 1)
