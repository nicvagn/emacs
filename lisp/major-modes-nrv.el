;;; major-modes-nrv.el --- My major modes modes  -*- lexical-binding: t; -*-
;;; commentary:
;; arduino-mode: made for Arduino

;;; code:

(define-derived-mode arduino-mode
  c++-ts-mode "mode for editing .ino files")

(provide 'major-modes-nrv)
