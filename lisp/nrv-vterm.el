;;; nrv-vterm.el --- vterm for inferior term in emacs
;;; commentary:
;; better than nothing, as everything must be Emacs

;;; code:

;; def minor mode for term
;;(define-minor-mode nrv-vterm-mode
 ;; "Nrv's vterm mode."
 ;; :lighter " nrvterm"
 ;; :keymap (let ((map (make-sparse-keymap)))
 ;;   (define-key map (kbd "M-[")  #'centaur-tabs-backward)
 ;;   (define-key map (kbd "M-]") #'centaur-tabs-forward)
 ;;   (define-key map (kbd "M-{") #'centaur-tabs-move-current-tab-to-left)
 ;;   (define-key map (kbd "M-}") #'centaur-tabs-move-current-tab-to-right)
 ;;   (define-key map (kbd "<f1>") #'centaur-tabs-backward-group)
 ;;   (define-key map (kbd "<f2>") #'centaur-tabs-forward-group)))

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
		  (text-scale-adjust 1.05)
		  (use-monospace)
                  (define-key vterm-mode-map (kbd "M-[")  #'centaur-tabs-backward)
                  (define-key vterm-mode-map (kbd "M-]") #'centaur-tabs-forward)
                  (define-key vterm-mode-map (kbd "M-{") #'centaur-tabs-move-current-tab-to-left)
                  (define-key vterm-mode-map (kbd "M-}") #'centaur-tabs-move-current-tab-to-right)
                  (define-key vterm-mode-map (kbd "<f1>") #'centaur-tabs-backward-group)
                  (define-key vterm-mode-map (kbd "<f2>") #'centaur-tabs-forward-group)))
  )

;;;;###autoload
;;(add-hook 'vterm-mode-hook 'nrv-vterm-mode)

(provide 'nrv-vterm)
;;; nrv-vterm.el ends here
