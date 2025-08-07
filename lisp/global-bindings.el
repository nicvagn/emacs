;;; global-bindings.el --- My emacs init.el
;;; Commentary:
;;  muh Emacs global binds

;;; code:
;; split
(global-set-key (kbd "C-c _") 'split-window-below)
(global-set-key (kbd "C-c |") 'split-window-right)
;; open neotree with f3 f4: (overshadows keyboard macro)
(global-set-key (kbd "<f3>") 'neotree-toggle)
;; popup term
(global-set-key (kbd "<f4>") 'shell-pop)
;; restart emacs
(global-set-key (kbd "C-M-r") 'restart-emacs)
;; show the dir track of file we are edditing
(global-set-key (kbd "C-c d") #'dir-track)
;; kill this buffer
(global-set-key (kbd "C-c k") #'kill-current-buffer)
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
(global-set-key (kbd "s-d") 'xref-find-definitions)
(global-set-key (kbd "C-c M-d") 'xref-find-definitions)
(global-set-key (kbd "M-0") 'xref-find-definitions)
(global-set-key (kbd "M-9") 'xref-find-references)
(global-set-key (kbd "M-<menu>") 'xref-find-apropos)

;; spelling
(global-set-key (kbd "C-c s") 'flyspell-toggle )
;; swich window - we will see what I use
(global-set-key (kbd "<pause>") 'other-window)
(global-set-key (kbd "C-c o") 'other-window)
(require 'repo-grep)
(global-set-key (kbd "C-c g") 'repo-grep)

(require 'functions-nrv)
(global-set-key (kbd "C-c m") 'zck/move-file)

(provide 'global-bindings)

;;; global-bindings.el ends here

                                        ; LocalWords:  vterm
