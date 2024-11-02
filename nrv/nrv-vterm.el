(use-package vterm
    :ensure t
    :bind (
	   :map vterm-mode-map
	   ("<f2>" . next-buffer)
           ("<f1>" . previous-buffer)))

(provide 'nrv-vterm)
