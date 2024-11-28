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
		(text-scale-adjust 1.2)))
)

(provide 'nrv-vterm)
