(defun dired-init ()
  "Config `dired'.
URL `http://xahlee.info/emacs/emacs/emacs_dired_tips.html'
Version 2021-07-30 2023-03-15 2023-04-05"
  (interactive)
  (define-key dired-mode-map (kbd "h") #'dired-prev-dirline)
  (define-key dired-mode-map (kbd "t") #'dired-next-dirline)
  (define-key dired-mode-map (kbd "u") #'dired-up-directory)
  (define-key dired-mode-map (kbd "<return>") #'dired-find-file)
  ;;(define-key dired-mode-map (kbd "1") #'dired-do-shell-command)
  ;;(define-key dired-mode-map (kbd "9") #'dired-hide-details-mode)
  ;;(define-key dired-mode-map (kbd "b") #'dired-do-byte-compile)
  ;;(define-key dired-mode-map (kbd "`") #'dired-flag-backup-files)
  ;;(define-key dired-mode-map (kbd "e") nil)
  ;;(define-key dired-mode-map (kbd "e c") #'dired-do-copy)
  ;;(define-key dired-mode-map (kbd "e d") #'dired-do-delete)
  ;;(define-key dired-mode-map (kbd "e g") #'dired-mark-files-containing-regexp)
  ;;(define-key dired-mode-map (kbd "e h") #'dired-hide-details-mode)
  ;;(define-key dired-mode-map (kbd "e m") #'dired-mark-files-regexp)
  ;;(define-key dired-mode-map (kbd "e n") #'dired-create-directory)
  ;;(define-key dired-mode-map (kbd "e r") #'dired-do-rename)
  ;;(define-key dired-mode-map (kbd "e u") #'dired-unmark-all-marks)
  ;;;;
)

(progn
  (require 'dired )
  (add-hook 'dired-mode-hook #'turn-off-evil-mode)
  (add-hook 'dired-mode-hook #'turn-off-evil-dvorak-mode)
  (add-hook 'dired-mode-hook #'dired-init)
 )
(progn
  (require 'vterm)
  ;; evil mode does not play nice w vterm
  (add-hook 'vterm-mode-hook #'turn-off-evil-mode)
  (add-hook 'vterm-mode-hook #'turn-off-evil-dvorak-mode)
)
  

;;jedi python complete
(add-hook 'python-mode-hook 'jedi:setup)

;; add lines to programming mode
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; colour define "(" pairs etc
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;(progn not ready 
;;  (require 'org)
;;  (add-hook 'org-mode-hook #'turn-off-evil-mode)
;;  (add-hook 'org-mode-hook #'turn-off-evil-dvorak-mode)
;;)

(provide 'nrv-modes)
