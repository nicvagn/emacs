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
 '(custom-enabled-themes '(modus-vivendi))
 '(inhibit-startup-screen t)
 '(package-selected-packages '(ace-window gnu-elpa-keyring-update evil-leader evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; easy change buffer with f keys (and close other win)
(global-set-key (kbd "<f1>") 'previous-buffer)
(global-set-key (kbd "<f2>") 'next-buffer)
(global-set-key (kbd "<f3>") 'delete-other-windows)

;; we want vim C-u
(setq evil-want-C-u-scroll t)
;; evil bah-ha-ha
(require 'evil)
;; leader for emacs
(require 'evil-leader) 
(evil-leader/set-leader "<SPC>") ;; set to space
(global-evil-leader-mode) ;; activate leader mode, must be done early
;;my own l-dvorak bindings
(require 'evil-dvorak)

;; make c delete
(define-key evil-normal-state-map (kbd "c") 'evil-delete)

;; define leader mappings
(evil-leader/set-key 
  "w" 'save-buffer
  "s" 'evil-window-split
  "k" 'kill-buffer
  "q" 'evil-quit
  "x" 'delete-window
  "<SPC>" 'evil-window-prev
)
 
;; enable global dvorak mode
(global-evil-dvorak-mode 1)
(evil-mode 1)
;; set evil undo to one built into emacs 
(evil-set-undo-system 'undo-redo)

