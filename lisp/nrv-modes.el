;;; nrv-modes.el --- My emacs modes, mode keymaps
;;; commentary:
;; mostly keymap's that are mode specific

;;; code:
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
  ;; do not enable flymake on temp. buffers
  (add-hook 'python-mode-hook
            (lambda ()
	      (unless (eq buffer-file-name nil) (flymake-mode 1))))
  (add-hook 'python-mode-hook 'python-isort-on-save-mode)
  (add-hook 'python-mode-hook 'guess-style-guess-tabs-mode)
  (add-hook 'python-mode-hook (lambda ()
                              (guess-style-guess-tab-width)))
)
;; ---- programming mode ----
(progn
  (require 'prog-mode)
  ;; add lines to programming mode
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  ;; colour define "(" pairs etc
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; Flymake and codespell
  (add-hook 'prog-mode-hook 'flymake-mode)
  (add-hook 'prog-mode-hook 'flymake-codespell-setup-backend)
  ;; keybinds
  (define-key prog-mode-map (kbd "C-C l") #'flymake-show-buffer-diagnostics)
  (define-key prog-mode-map (kbd "C-C n") #'flymake-goto-next-error)

)
;; remove trailing whitespace before saving
(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; hook for changing modes
(provide 'nrv-modes)
;;; nrv-modes.el ends here
