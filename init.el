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
   '(yasnippet centaur-tabs magit spacemacs-theme ace-window gnu-elpa-keyring-update evil-leader evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; _-_-_-_-_-_-_-_-_-_-_-_-_-setq var's_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; we want vim C-u
(setq evil-want-C-u-scroll t)
(setq jedi:complete-on-dot t)  
(setq completion-auto-help t)
(setq completion-cycle-threshold 2) ;; cycle completions only 2 
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
;; Evil bah-ha-ha
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
          snippet-mode) . yas-minor-mode-on )
  :init
  (setq yas-snippet-dir "~/.config/emacs/snippets")
)

(yas-global-mode 1)

;; _-_-_-_-_-_-_-_-_-_-_-_-_-Keymaps-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; global keymap
;; restart emacs
(global-set-key (kbd "C-M-r") #'restart-emacs)
(global-set-key (kbd "M-l") #'eval-buffer)
;; f9 Vterm
(global-set-key (kbd "<f9>") 'vterm)

;; evil
;; make c delete
(define-key evil-normal-state-map (kbd "c") 'evil-delete)
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

(require 'nrv-modes) ;; modular af
