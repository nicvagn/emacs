;; mostly keymap's that are mode specific
(progn
  (require 'dired )
  (define-prefix-command 'dired-ring-map)
  (define-key dired-mode-map (kbd "<SPC>") 'dired-ring-map)
  (define-key dired-mode-map (kbd "h") #'dired-previous-line)
  (define-key dired-mode-map (kbd "t") #'dired-next-line)
  (define-key dired-mode-map (kbd "u") #'dired-up-directory)
  (define-key dired-mode-map (kbd "a") #'dired-create-directory)
  (define-key dired-mode-map (kbd "r") #'dired-do-rename)
  (define-key dired-mode-map (kbd "<return>") #'dired-find-file)
  ;; magit
  (define-key dired-mode-map (kbd "C-x C-g c") #'magit-commit)
  (define-key dired-mode-map (kbd "C-x C-g l") #'magit-log-current)

  ;; switch pains with <SPC>
  (define-key dired-ring-map (kbd "<SPC>") #'evil-window-next)
  (define-key dired-ring-map (kbd "e") #'dired-find-file)
  (define-key dired-ring-map (kbd "s") #'evil-window-split)
  (define-key dired-ring-map (kbd "v") #'evil-window-vsplit)
  (define-key dired-ring-map (kbd "x") #'delete-window)
  (define-key dired-ring-map (kbd "k") #'kill-this-buffer)
 )
;; ---- python mode ----
(progn
  (require 'python-isort)
  (add-hook 'python-mode-hook 'python-isort-on-save-mode)
  (add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
  (add-hook 'python-mode-hook (lambda ()
                              (guess-style-guess-tab-width)))
)
;; ---- programming mode ----
(progn
  ;; add lines to programming mode
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  ;; colour define "(" pairs etc
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
)
;; remove trailing whitespace before saving
(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; hook for changing modes
(provide 'nrv-modes)
