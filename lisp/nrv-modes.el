(defun dired-init ()
  "Config `dired'.
URL `http://xahlee.info/emacs/emacs/emacs_dired_tips.html'
Version 2021-07-30 2023-03-15 2023-04-05"
  (interactive)
  (define-key dired-mode-map (kbd "h") #'dired-previous-line)
  (define-key dired-mode-map (kbd "t") #'dired-next-line)
  (define-key dired-mode-map (kbd "u") #'dired-up-directory)
  (define-key dired-mode-map (kbd "a") #'dired-create-directory)
  (define-key dired-mode-map (kbd "r") #'dired-do-rename)
  (define-key dired-mode-map (kbd "<return>") #'dired-find-file)
  (define-key dired-mode-map (kbd "C-x C-g c") #'magit-commit)
  (define-key dired-mode-map (kbd "C-x C-g l") #'magit-log-current)
  (define-key dired-mode-map (kbd "C-x C-g p") #'magit-push-current-to-upstream)
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
)
(progn
  (require 'dired )
  (add-hook 'dired-mode-hook #'evil-off)
  (add-hook 'dired-mode-hook #'dired-init)
 )
;; turn evil back on when you load a text buffer
;; TEST (add-hook 'text-mode-hook 'evil-on)
;; python
(add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
(add-hook 'python-mode-hook (lambda ()
                            (guess-style-guess-tab-width)))

;; ---- programming mode ----
;; add lines to programming mode
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; colour define "(" pairs etc
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;; evil-on when you enter a programming buffer
(add-hook 'prog-mode-hook 'evil-on)

;; remove trailing whitespace before saving
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(provide 'nrv-modes)
