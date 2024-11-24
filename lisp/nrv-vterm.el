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
		(setq-local buffer-face-mode-face '(:foreground "#000000"))
		(buffer-face-mode 1)
		(text-scale-adjust 2)))
)

(provide 'nrv-vterm)
