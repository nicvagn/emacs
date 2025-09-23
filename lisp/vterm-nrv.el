;;; vterm-nrv.el --- vterm for inferior term in Emacs  -*- lexical-binding: t; -*-
;;; commentary:
;; better than nothing, as everything must be Emacs

;;; code:
(defun use-monospace ()
  "Switch the current buffer to a monospace font."
  (face-remap-add-relative 'default '(:family "Monospace")))

(use-package vterm
  :ensure t
  :custom
  (vterm-always-compile t)
  :hook
  (vterm-mode . (lambda ()
		          (buffer-face-mode t)
		          (text-scale-decrease 1)
		          (use-monospace)
                  (define-key vterm-mode-map (kbd "M-[")  #'centaur-tabs-backward)
                  (define-key vterm-mode-map (kbd "M-]") #'centaur-tabs-forward)
                  (define-key vterm-mode-map (kbd "M-{") #'centaur-tabs-move-current-tab-to-left)
                  (define-key vterm-mode-map (kbd "M-}") #'centaur-tabs-move-current-tab-to-right)
                  (define-key vterm-mode-map (kbd "<f1>") #'centaur-tabs-backward-group)
                  (define-key vterm-mode-map (kbd "<f2>") #'centaur-tabs-forward-group)
                  (define-key vterm-mode-map (kbd "<f3>") #'neotree-toggle)
                  (define-key vterm-mode-map (kbd "<f4>") #'shell-pop)
                  (centaur-tabs-local-mode)) ;; Do not show tab line
              )
  )

;; popup shell
(require 'shell-pop)
;; export $EDITOR to be this Emacs
(add-hook 'vterm-mode-hook  'with-editor-export-editor)
;; why not for all
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)

;; make good and sure it is exported
(keymap-global-set "<remap> <async-shell-command>"
                   #'with-editor-async-shell-command)
(keymap-global-set "<remap> <shell-command>"
                   #'with-editor-shell-command)

(provide 'vterm-nrv)
;;; vterm-nrv.el ends here
