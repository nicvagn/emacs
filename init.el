;; add custom dir to mf load-path
(add-to-list 'load-path "~/.config/emacs/nrv" )
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

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
   '(neotree spacemacs-theme ace-window gnu-elpa-keyring-update evil-leader evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; easy change buffer with f keys (and close other win)
(global-set-key (kbd "<f1>") 'previous-buffer)
(global-set-key (kbd "<f2>") 'next-buffer)
(global-set-key (kbd "<f3>") 'neotree-toggle)
(global-set-key (kbd "<f4>") 'delete-other-windows )
;; _-_-_-_-_-_-_-_-_-_-_-_-_-setq var's_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; we want vim C-u
(setq evil-want-C-u-scroll t)
(setq neo-smart-open t)
(setq completion-auto-help t)
;; _-_-_-_-_-_-_-_-_-_-_-_-_evil-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; evil bah-ha-ha
(require 'evil)
;; leader for emacs
(require 'evil-leader) 
(global-evil-leader-mode) ;; activate leader mode, must be done early
;;my own l-dvorak bindings
(require 'evil-dvorak)
;; enable global dvorak mode 
(global-evil-dvorak-mode 1)
(evil-mode 1)
;; set evil undo to one built into emacs 
(evil-set-undo-system 'undo-redo)
;; _-_-_-_-_-_-_-_-_-_-_-_-_-Keymaps-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; make c delete
(define-key evil-normal-state-map (kbd "c") 'evil-delete)
;; <leader>
(evil-leader/set-leader "<SPC>") ;; set to space
;; define leader mappings
(evil-leader/set-key 
  "w" 'save-buffer
  "s" 'evil-window-split
  "v" 'evil-window-vsplit
  "k" 'kill-buffer
  "q" 'evil-quit
  "x" 'delete-window
  "<SPC>" 'evil-window-prev
)

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
                (define-key evil-normal-state-local-map (kbd "r") 'neotree-delete-node)))
;; _-_-_-_-_-_-_-_-_-_-_-_-_-Keymaps END_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
 

