;; -*- coding: utf-8 -*-

(progn
  (global-set-key (kbd "C-'") nil) ; was undefined
  (global-set-key (kbd "C-,") nil) ; was undefined
  (global-set-key (kbd "C--") 'negative-argument)
  (global-set-key (kbd "C-.") nil)   ; was undefined
  (global-set-key (kbd "C-/") 'undo) ; was
  (global-set-key (kbd "C-;") nil)   ; was undefined
  (global-set-key (kbd "C-=") nil)   ; was undefined
  (global-set-key (kbd "C-[") esc-map)
  (global-set-key (kbd "C-]") 'abort-recursive-edit)

  (global-set-key (kbd "C-a") 'move-beginning-of-line)
  (global-set-key (kbd "C-b") 'backward-char)
  ;; (global-set-key (kbd "C-c") nil) ; major mode specific prefix
  (global-set-key (kbd "C-d") 'delete-char)
  (global-set-key (kbd "C-e") 'move-end-of-line)
  (global-set-key (kbd "C-f") 'forward-char)
  (global-set-key (kbd "C-g") 'keyboard-quit)
  (global-set-key (kbd "C-h") help-map)
  (global-set-key (kbd "C-i") 'indent-for-tab-command)
  (global-set-key (kbd "C-j") 'electric-newline-and-maybe-indent)
  (global-set-key (kbd "C-k") 'kill-line)
  (global-set-key (kbd "C-l") 'recenter-top-bottom)
  (global-set-key (kbd "C-m") 'newline)
  (global-set-key (kbd "C-n") 'next-line)
  (global-set-key (kbd "C-o") 'open-line)
  (global-set-key (kbd "C-p") 'previous-line)
  (global-set-key (kbd "C-q") 'quoted-insert)
  (global-set-key (kbd "C-r") 'isearch-backward)
  (global-set-key (kbd "C-s") 'isearch-forward)
  (global-set-key (kbd "C-t") 'transpose-chars)
  (global-set-key (kbd "C-u") 'universal-argument)
  (global-set-key (kbd "C-v") 'scroll-up-command)
  (global-set-key (kbd "C-w") 'kill-region)
  (global-set-key (kbd "C-x") ctl-x-map)
  (global-set-key (kbd "C-y") 'yank)
  (global-set-key (kbd "C-z") 'suspend-frame))
