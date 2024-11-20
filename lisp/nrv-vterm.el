(use-package vterm
    :ensure t
    :bind (
	   :map vterm-mode-map
	    ("<f1>" . centaur-tabs-backward)
	    ("<f2>" . centaur-tabs-forward))
)

(provide 'nrv-vterm)
