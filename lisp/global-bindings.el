;;; global-bindings.el --- My emacs init.el
;;; Commentary:
;;  muh Emacs global binds

;;; code:
(require 'evil)
;; Jumping about
(global-set-key (kbd "C-'") 'evil-jump-backward)
(global-set-key (kbd "C-\"") 'evil-jump-forward)

;; find the definition with xref
(global-set-key (kbd "C-c M-d") 'xref-find-definitions)
(global-set-key (kbd "C-c M-a") 'xref-find-apropos)
(global-set-key (kbd "C-c M-r") 'xref-find-references-and-replace)

;; Window jumping
;; globalize so works for all windows
(global-set-key (kbd "C-c w") 'evil-window-next)
;; swich window - we will see what I use
(global-set-key (kbd "C-c o") 'other-window)

;; window spiting
;; split
(global-set-key (kbd "C-c _") 'split-window-below)
(global-set-key (kbd "C-c |") 'split-window-right)

;; F-keys
;; open neotree with f3 f4: (overshadows keyboard macro)
(global-set-key (kbd "<f3>") 'neotree-toggle)
;; popup term
(global-set-key (kbd "<f4>") 'shell-pop)

;; Emacs management
(require 'functions-nrv)
(global-set-key (kbd "C-c m") 'zck/move-file)
;; restart Emacs
(global-set-key (kbd "C-M-r") 'restart-emacs)
;; show the dir track of file we are edditing
(global-set-key (kbd "C-c D") #'dir-track)
;; kill this buffer
(global-set-key (kbd "C-c k") #'kill-current-buffer)
;; close all other buffers
(global-set-key (kbd "C-c C-K") #'kill-other-buffers)
;; better buffer summary
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; ido find file
(global-set-key (kbd "C-x C-f") #'ido-find-file)

;; spelling
(global-set-key (kbd "C-c s") 'flyspell-toggle )

;; GIT
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(require 'repo-grep)
(global-set-key (kbd "C-c g") 'repo-grep)

(provide 'global-bindings)
;;; global-bindings.el ends here

                                        ; LocalWords:  vterm neotree
