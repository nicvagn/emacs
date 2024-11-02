;; add custom dir to load-path
(add-to-list 'load-path "~/.config/emacs/nrv" )
(require 'package)
;; (setq package-enable-at-startup nil) this and package-init are done auto in modern emacs
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;; (package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(spacemacs-dark))
 '(custom-safe-themes
   '("3f75d4633820090be31d1f91fa1e33427b5dc09235efa189157592c822d1843a" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" default))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(yasnippet centaur-tabs magit neotree spacemacs-theme ace-window gnu-elpa-keyring-update evil-leader evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; _-_-_-_-_-_-_-_-_-_-_-_-_-setq var's_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; we want vim C-u
(setq evil-want-C-u-scroll t)
(setq neo-smart-open t)
(setq completion-auto-help t)
(setq completion-cycle-threshold 4) ;; cycle completions when < 5
(setq savehist-file "~/.emacs_histfile")

;; Revert/reload Dired and other buffers on filesystem change 
(setq global-auto-revert-non-file-buffers t)
;; _-_-_-_-_-_-_-_-_-_-_-_-_other emacs settings-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1) ;; reload a file if changed outside of emacs
(global-hl-line-mode 1)
(auto-fill-mode t) ;; complete if only
(savehist-mode) ;; save history
;; _-_-_-_-_-_-_-_-_-_-_-_-_evil-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; evil bah-ha-ha
(require 'evil)
;; leader for emacs
(require 'evil-leader) 
(global-evil-leader-mode) ;; activate leader mode, must be done early
;;my own custom stuff
(require 'nrv-evil-dvorak)
(require 'nrv-vterm)
;; enable modded global dvorak mode 
(global-evil-dvorak-mode 1)
(evil-mode 1)
;; set evil undo to one built into emacs 
(evil-set-undo-system 'undo-redo)

;; _-_-_-_-_-_-_-_-_-_-_-_-_-Packages_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(use-package centaur-tabs
  :ensure
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("<f1>" . centaur-tabs-backward)
  ("<f2>" . centaur-tabs-forward))

(use-package yasnippet
  :ensure t
  :hook ((text-mode
          prog-mode
          conf-mode
          snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dir "~/.config/emacs/snippets"))

(yas-global-mode 1)

;; _-_-_-_-_-_-_-_-_-_-_-_-_-Keymaps-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; make c delete
(define-key evil-normal-state-map (kbd "c") 'evil-delete)
;; NeoTree with F3
(global-set-key (kbd "<f3>") 'neotree-toggle)
(global-set-key (kbd "<f9>") 'vterm)
;; <leader>
(evil-leader/set-leader "<SPC>") ;; set to space
;; define leader mappings
(evil-leader/set-key 
  "w" 'save-buffer
  "k" 'kill-buffer
  "q" 'evil-quit
  "x" 'delete-window
  "1" 'delete-other-windows
  "s" 'evil-window-split
  "v" 'evil-window-vsplit
  "<SPC>" 'evil-window-prev
)

;; _-_-_-_-_-_-_-_-_-_-_-_-_-Mode Hook's_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

(add-hook 'neotree-mode-hook
	(lambda ()
	(define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	(define-key evil-normal-state-local-map (kbd "l") 'neotree-quick-look)
	(define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	(define-key evil-normal-state-local-map (kbd "<return>") 'neotree-enter)
	(define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
	(define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
	(define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
	(define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
	(define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)
	(define-key evil-normal-state-local-map (kbd "a") 'neotree-create-node)
	(define-key evil-normal-state-local-map (kbd "r") 'neotree-delete-node)
    )
)

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
;;
  ;;(define-key dired-mode-map (kbd "b") #'dired-do-byte-compile)
;;
  ;;(define-key dired-mode-map (kbd "`") #'dired-flag-backup-files)
;;
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
  (add-hook 'dered-mode-hook #'turn-off-evil-dvorak-mode)
  (add-hook 'dired-mode-hook #'dired-init)
 )
(progn
    ;; evil mode does not play nice w vterm
    (add-hook 'vterm-mode-hook #'turn-off-evil-mode)
    (add-hook 'vterm-mode-hook #'turn-off-evil-dvorak-mode)
)
