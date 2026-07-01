;;; major-modes-nrv.el --- My major modes modes  -*- lexical-binding: t; -*-
;;; commentary:
;; arduino-mode: made for Arduino

;;; code:

(define-derived-mode arduino-mode
  c++-ts-mode "Arduino" "mode for editing .ino files")


;; major mode remapping based on file name
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.ino\\'" . arduino-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.qss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh-theme\\'" . sh-mode))

(provide 'major-modes-nrv)
