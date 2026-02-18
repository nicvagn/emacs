;;; global-bindings.el --- My emacs init.el  -*- lexical-binding: t; -*-
;;; Commentary:
;;  muh Emacs global binds

;;; code:
(with-eval-after-load 'evil
  ;; Jumping about
  (global-set-key (kbd "C-'") 'evil-jump-backward)
  (global-set-key (kbd "C-\"") 'evil-jump-forward)
  ;; Window jumping
  ;; globalize so works for all windows
  (global-set-key (kbd "C-c w") 'evil-window-next))

;; find the definition with xref
(global-set-key (kbd "C-c M-d") 'xref-find-definitions)
(global-set-key (kbd "C-c M-a") 'xref-find-apropos)
(global-set-key (kbd "C-c M-r") 'xref-find-references)
(global-set-key (kbd "C-c M-R") 'xref-find-references-and-replace)


;; window spiting
;; split
(global-set-key (kbd "C-c _") 'split-window-below)
(global-set-key (kbd "C-c |") 'split-window-right)

;; F-keys
;; open neotree with f3: (overshadows keyboard macro)
(global-set-key (kbd "<f3>") 'neotree-toggle)
;; popup term
(global-set-key (kbd "<f4>") 'shell-pop)
(global-set-key (kbd "<f8>") 'keyboard-quit)

;; Emacs management
(with-eval-after-load 'functions-nrv
  ;; Bind C-c C-c save and quit
  (global-set-key (kbd "C-c C-c" #'nrv/text-save-and-close))
  (global-set-key (kbd "C-c m") 'zck/move-file)
  ;; restart Emacs
  (global-set-key (kbd "C-M-r") 'restart-emacs)
  ;; kill this buffer
  (global-set-key (kbd "C-c k") #'kill-current-buffer)
  ;; close all other buffers
  (global-set-key (kbd "C-c K") #'nrv/kill-other-text-buffers)
  ;; spelling
  (global-set-key (kbd "C-c s") 'flyspell-toggle ))

;; GIT
(with-eval-after-load 'magit
  (global-set-key (kbd "C-x g") 'magit-status))

;; repo-grep
(with-eval-after-load 'repo-grep
  (global-set-key (kbd "C-c g") 'repo-grep))

(provide 'global-bindings)
;;; global-bindings.el ends here

                                        ; LocalWords:  vterm neotree
