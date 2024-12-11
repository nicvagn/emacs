(defun use-monospace ()
  "Switch the current buffer to a monospace font."
  (face-remap-add-relative 'default '(:family "Monospace")))

(use-package vterm
    :ensure t
    :bind (
          :map vterm-mode-map
           ("<f1>" . centaur-tabs-backward)
           ("<f2>" . centaur-tabs-forward))

    :custom
    (vterm-always-compile t)
    :hook
    (vterm-mode . (lambda ()
		(buffer-face-mode t)
		(text-scale-adjust 1.05)
		(use-monospace)))
)

(provide 'nrv-vterm)
