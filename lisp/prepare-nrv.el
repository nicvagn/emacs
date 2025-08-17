;;; prepare-nrv.el --- My prepare for mode functions
;;; commentary:
;; mostly for keymaps and tab width that are mode specific

;;; code:
(defconst python-tab-width 4 "Tab width for python source files.")
(defconst java-tab-width 4 "Tab width for java source files.")
(defconst css-tab-width 2 "Tab width for css source files.")
(defconst js-tab-width 2 "Tab width for js source files.")
(defconst web-tab-width 2 "Tab width for web files.")
(defconst html-tab-width 2 "Tab width for html files.")
(defconst prog-tab-width 4 "Tab width for general programming files.")
(defconst lisp-tab-width 2 "Tab width for Lisp files.")

(require 'highlight-indentation) ;; for tab highlights
(require 'rainbow-delimiters)
(require 'dired)
(require 'functions-nrv)
(require 'ido)
(require 'ido-vertical-mode)

(defun prepare-text ()
  "Prepare all text buffers"
  (flyspell-on-for-buffer-type) ;; def in functions-nrv set correct flyspell for buffer type
  (yas-minor-mode-on)
  (text-mode-hook-identify)
  (rainbow-delimiters-mode 1)
  (highlight-indentation-mode 1)
  (local-set-key (kbd "C-<backspace>") #'tjwh/backward-kill-word-on-this-line)
  )

(defun prepare-magit ()
  "Prepare to enter magit"
  (local-set-key (kbd "t") #'magit-next-line)
  (local-set-key (kbd "h") #'magit-previous-line)
  )

(defun prepare-lisp ()
  "Prepare lisp to be eddited how I like."
  (setq-local evil-shift-width lisp-tab-width
              tab-width lisp-tab-width
              c-basic-offset lisp-tab-width)
  (local-set-key (kbd "M-l") 'eval-buffer)
  (prepare-text)
  )

(defun prepare-dired ()
  "Prepare Dired mode how I like it."
  (define-prefix-command 'dired-ring-map)
  (define-key dired-mode-map (kbd "h") 'dired-previous-line)
  (define-key dired-mode-map (kbd "t") 'dired-next-line)
  (define-key dired-mode-map (kbd "u") 'dired-up-directory)
  (define-key dired-mode-map (kbd "a") 'dired-create-directory)
  (define-key dired-mode-map (kbd "r") 'dired-do-rename)
  (define-key dired-mode-map (kbd "<return>") 'dired-find-file)
  (define-key dired-mode-map (kbd "<tab>") 'dired-find-file)

  ;; "leader"
  (define-key dired-mode-map (kbd "<SPC>") 'dired-ring-map)
  ;; add leader via dired ring map
  ;; switch pains with <SPC>
  (define-key 'dired-ring-map (kbd "<SPC>") 'evil-window-next)
  (define-key 'dired-ring-map (kbd "e") 'dired-find-file)
  (define-key 'dired-ring-map (kbd "s") 'evil-window-split)
  (define-key 'dired-ring-map (kbd "v") 'evil-window-vsplit)
  (define-key 'dired-ring-map (kbd "x") #'delete-window)
  (define-key 'dired-ring-map (kbd "k") #'kill-current-buffer)
  )

;; ---- css mode ----
(defun prepare-css ()
  "Setup Emacs for css editing."
  (setq-local evil-shift-width css-tab-width
              tab-width css-tab-width
              c-basic-offset css-tab-width)
  (prepare-text)
  )
;; ---- python mode ----
(defun prepare-python ()
  "Prepare to edit python code."
  (require 'python-nrv)
  (setq-local electric-indent-inhibit nil
              tab-width python-tab-width
              evil-shift-width python-tab-width
              c-basic-offset python-tab-width)
  (eglot-ensure)
  (prepare-text)
  )



;; ---- web stuff ----
(defun prepare-web ()
  "set up for web mode. (html, js, mustache etc.)"
  (setq-local tab-width web-tab-width
              evil-shift-width web-tab-width
              c-basic-offset web-tab-width)
  (prepare-text)
  )

;; ---- html stuff ----
(defun prepare-html ()
  "set up for web mode. (html, js, mustache etc.)"
  (setq-local tab-width html-tab-width
              evil-shift-width html-tab-width
              c-basic-offset html-tab-width)
  (prepare-text)
  )

;; ---- programming mode ----
(defun prepare-prog ()
  "Prepare to enter \='prog-mode'."
  (setq-local tab-width prog-tab-width
              evil-shift-width prog-tab-width
              c-basic-offset prog-tab-width)
  ;; keybinds
  (define-key prog-mode-map (kbd "C-c l") 'flymake-show-buffer-diagnostics)
  (define-key prog-mode-map (kbd "C-c n") 'flymake-goto-next-error)
  (prepare-text)
  )

;; --- IDO ----
(defun disable-ido-everywhere ()
  (ido-everywhere -1)
  )

(defun nrv/ido ()
  "set ido up for nrv"
  (ido-mode 1)
  (ido-vertical-mode 1)
  (ido-ubiquitous-mode +1)
  (flx-ido-mode 1))

(defun prepare-ido ()
  "Prepare ido. keymaps an etc"
  (define-key ido-completion-map (kbd "<tab>") #'ido-complete)
  (define-key ido-completion-map (kbd "C-<tab>") #'ido-next-match)
  (define-key ido-completion-map (kbd "<backtab>") #'ido-prev-match)
  (define-key ido-completion-map (kbd "<f5>") #'ido-exit-minibuffer)
  (define-key ido-completion-map (kbd "<f6>") #'ido-next-match)
  (define-key ido-completion-map (kbd "<f7>") #'ido-prev-match)
  (define-key ido-completion-map (kbd "<f8>") #'abort-minibuffers)
  (nrv/ido)
  )

(provide 'prepare-nrv)
;; Local Variables:
;; byte-compile-warnings: (not free-vars dired-mode-map evil-shift-width)
;; End:
;;; prepare-nrv.el ends here
