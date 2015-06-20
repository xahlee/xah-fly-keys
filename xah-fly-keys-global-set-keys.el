;; -*- coding: utf-8 -*-

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
  ;; on Mac OS X, pc keyboard's menu key sends C-p
  (define-key key-translation-map (kbd "C-p") (kbd "<menu>")))

(global-set-key (kbd "<menu>") 'xah-fly-leader-key-map)



