;;; nrv-modes.el --- My emacs modes, mode keymaps
;;; commentary:
;; mostly keymaps that are mode specific

;;; code:

(defvar python-tab-width 4 "Tab width for python source files.")
(defvar java-tab-width 4 "Tab width for java source files.")
(defvar css-tab-width 2 "Tab width for css source files.")
(defvar js-tab-width 4 "Tab width for js source files.")
(defvar html-tab-width 4 "Tab width for html files.")
(defvar prog-tab-width 4 "Tab width for general programming files.")
(defvar lisp-tab-width 2 "Tab width for Lisp files.")

(defun prepare-dired ()
  "Prepare Dired mode how I like it."
  (require 'dired)
  (define-prefix-command 'dired-ring-map)
  (define-key dired-mode-map (kbd "<SPC>") 'dired-ring-map)
  (define-key dired-mode-map (kbd "h") 'dired-previous-line)
  (define-key dired-mode-map (kbd "t") 'dired-next-line)
  (define-key dired-mode-map (kbd "u") 'dired-up-directory)
  (define-key dired-mode-map (kbd "a") 'dired-create-directory)
  (define-key dired-mode-map (kbd "r") 'dired-do-rename)
  (define-key dired-mode-map (kbd "<return>") 'dired-find-file)

  ;; add leader via dired ring map
  ;; switch pains with <SPC>
  (define-key 'dired-ring-map (kbd "<SPC>") 'evil-window-next)
  (define-key 'dired-ring-map (kbd "e") 'dired-find-file)
  (define-key 'dired-ring-map (kbd "s") 'evil-window-split)
  (define-key 'dired-ring-map (kbd "v") 'evil-window-vsplit)
  (define-key 'dired-ring-map (kbd "x") #'delete-window)
  (define-key 'dired-ring-map (kbd "k") #'kill-this-buffer)
 )
;; ---- css mode ----
(defun prepaire-css ()
  "Setup Emacs for css editing."
  (setq tab-width css-tab-width
        evil-shift-width 2)
)
;; ---- python mode ----
(defun prepare-python ()
  "Prepare to edit python code."
  (require 'python-isort)
  (setq tab-width python-tab-width)
  (add-hook 'python-mode-hook 'python-isort-on-save-mode)
)
;; ---- programming mode ----
(defun prepare-prog ()
  "Prepare to enter \='prog-mode'."
  (setq tab-width prog-tab-width)
  ;; add lines to programming mode
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  ;; colour define "(" pairs etc
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; keybinds
  (define-key prog-mode-map (kbd "C-C l") 'flymake-show-buffer-diagnostics)
  (define-key prog-mode-map (kbd "C-C n") 'flymake-goto-next-error)
)

;; remove trailing whitespace before saving
;; hook for changing modes
(provide 'nrv-modes)
;; Local Variables:
;; byte-compile-warnings: (not free-vars dired-mode-map evil-shift-width)
;; End:
;;; nrv-modes.el ends here
