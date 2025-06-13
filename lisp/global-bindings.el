;;; global-bindings.el --- My emacs init.el
;;; Commentary:
;;  muh Emacs global binds

;;; code:
;; split
(global-set-key (kbd "C-c _") 'split-window-below)
;; open neotree with f3 f4: (overshadows keyboard macro)
(global-set-key (kbd "<f3>") 'neotree-toggle)
;; eshell popup term
(global-set-key (kbd "<f4>") 'eshell-toggle)
;; restart emacs
(global-set-key (kbd "C-M-r") 'restart-emacs)
;; show the dir track of file we are edditing
(global-set-key (kbd "C-c d") #'dir-track)
;; close all other buffers
(global-set-key (kbd "C-c C-K") #'kill-other-buffers)
;; magit status
(global-set-key (kbd "C-x g") #'magit-status)
;; f9 Vterm
(global-set-key (kbd "<f9>") 'vterm)
;; f12 to spellcheck
(global-set-key (kbd "<f12>") `ace-flyspell-dwim)
;; better buffer summary
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; ido find file
(global-set-key (kbd "C-x C-f") #'ido-find-file)
;; find the definition with xref
(global-set-key (kbd "C-c M-d") 'xref-find-definitions)
(global-set-key (kbd "M-0") 'xref-find-definitions)
(global-set-key (kbd "M-9") 'xref-find-references)
(global-set-key (kbd "M-<menu>") 'xref-find-apropos)

(global-set-key (kbd "C-c M-g") 'repo-grep)
(require 'evil-leader)
(require 'eglot)
(evil-leader/set-key (kbd "f") 'eglot-format-buffer)
(provide 'global-bindings)
