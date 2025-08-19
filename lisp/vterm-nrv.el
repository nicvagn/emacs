;;; vterm-nrv.el --- vterm for inferior term in Emacs
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
                  (define-key vterm-mode-map (kbd "<f4>") #'shell-pop))
              )
  )

;; popup shell
(require 'shell-pop)

(provide 'vterm-nrv)
;;; vterm-nrv.el ends here
