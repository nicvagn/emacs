;;; mode-maps-nrv.el --- My mode maps
;; commentary: Alt-n, Alt-p to next previous
;;  muh Emacs  binds

;;; code:
(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
;; spelling
(define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-word-before-point)

(provide 'mode-maps-nrv)
;;; mode-maps-nrv.el ends here
