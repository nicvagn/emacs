;; add custom dir to mf load-path
(add-to-list 'load-path "~/.config/emacs/nrv" )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(modus-vivendi))
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

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
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
  "x" 'kill-buffer
  "s" 'evil-split
  "q" 'evil-quit
)
 
;; enable global dvorak mode
(global-evil-dvorak-mode 1)
(evil-mode 1)
;; undo 
(evil-set-undo-system 'undo-redo)

;; Smooth scrolling
(pixel-scroll-mode 1)
