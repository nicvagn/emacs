;;; modes-nrv.el --- My emacs modes, mode keymaps
;;; commentary:
;; mostly keymaps and tab width that are mode specific

;;; code:
(defvar python-tab-width 4 "Tab width for python source files.")
(defvar java-tab-width 4 "Tab width for java source files.")
(defvar css-tab-width 2 "Tab width for css source files.")
(defvar js-tab-width 2 "Tab width for js source files.")
(defvar web-tab-width 2 "Tab width for web files.")
(defvar html-tab-width 2 "Tab width for web files.")
(defvar prog-tab-width 4 "Tab width for general programming files.")
(defvar lisp-tab-width 2 "Tab width for Lisp files.")

(defun prepare-dired ()
  "Prepare Dired mode how I like it."
  (require 'dired)
  (define-prefix-command 'dired-ring-map)
  (define-key dired-mode-map (kbd "h") 'dired-previous-line)
  (define-key dired-mode-map (kbd "t") 'dired-next-line)
  (define-key dired-mode-map (kbd "u") 'dired-up-directory)
  (define-key dired-mode-map (kbd "a") 'dired-create-directory)
  (define-key dired-mode-map (kbd "r") 'dired-do-rename)
  (define-key dired-mode-map (kbd "<return>") 'dired-find-file)
  ;; "leader"
  (define-key dired-mode-map (kbd "<SPC>") 'dired-ring-map)
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
(defun prepare-css ()
  "Setup Emacs for css editing."
  (setq-local evil-shift-width css-tab-width
        tab-width css-tab-width
        c-basic-offset css-tab-width)
)
;; ---- python mode ----
(defun prepare-python ()
  "Prepare to edit python code."
  (require 'python-isort)
  (setq-local electric-indent-inhibit nil
              tab-width python-tab-width
              evil-shift-width python-tab-width
              c-basic-offset python-tab-width)
  (add-hook 'python-mode-hook 'python-isort-on-save-mode)
  (add-hook 'python-mode-hook 'eglot-ensure)

)

;; ---- web stuff ----
(defun prepare-web ()
  "set up for web mode. (html, js, mustache etc.)"
  (setq-local tab-width web-tab-width
        evil-shift-width web-tab-width
        c-basic-offset web-tab-width)
  (add-hook 'web-mode-hook 'display-line-numbers-mode)
  (add-hook 'web-mode-hook 'rainbow-delimiters-mode)
)

;; ---- html stuff ----
(defun prepare-html ()
  "set up for web mode. (html, js, mustache etc.)"
  (setq-local tab-width html-tab-width
        evil-shift-width html-tab-width
        c-basic-offset html-tab-width)
  (add-hook 'html-mode-hook 'display-line-numbers-mode)
  (add-hook 'html-mode-hook 'rainbow-delimiters-mode)
)

;; ---- programming mode ----
(defun prepare-prog ()
  "Prepare to enter \='prog-mode'."
  (setq-local tab-width prog-tab-width
        evil-shift-width prog-tab-width
        c-basic-offset prog-tab-width)
  ;; add lines to programming mode
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  ;; colour define "(" pairs etc
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  ;; keybinds
  (define-key prog-mode-map (kbd "C-c l") 'flymake-show-buffer-diagnostics)
  (define-key prog-mode-map (kbd "C-c n") 'flymake-goto-next-error)
)

(provide 'modes-nrv)
;; Local Variables:
;; byte-compile-warnings: (not free-vars dired-mode-map evil-shift-width)
;; End:
;;; modes-nrv.el ends here
