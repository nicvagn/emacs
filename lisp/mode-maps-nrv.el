;;; mode-maps-nrv.el --- My mode maps  -*- lexical-binding: t; -*-
;; commentary: Alt-n, Alt-p to next previous
;;  muh Emacs  binds

;;; code:
(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

(provide 'mode-maps-nrv)
;;; mode-maps-nrv.el ends here
