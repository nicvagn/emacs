;;; nrv-vterm.el --- vterm for inferior term in emacs
;;; commentary:
;; better than nothing, as everything must be Emacs

;;; code:
(defun use-monospace ()
  "Switch the current buffer to a monospace font."
  (face-remap-add-relative 'default '(:family "Monospace")))

(use-package vterm
  :ensure t
  :bind
  (("M-[" . centaur-tabs-backward)
   ("M-]" . centaur-tabs-forward)
   ("M-{" . centaur-tabs-move-current-tab-to-left)
   ("M-}" .  centaur-tabs-move-current-tab-to-right)
   ("<f1>" . centaur-tabs-backward-group)
   ("<f2>" . centaur-tabs-forward-group))

  :custom
  (vterm-always-compile t)
  :hook
  (vterm-mode . (lambda ()
		  (buffer-face-mode t)
		  (text-scale-adjust 1.05)
		  (use-monospace)))
  )

(provide 'nrv-vterm)
;;; nrv-vterm.el ends here
