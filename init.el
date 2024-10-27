;; _-_-_-_-_-_-_-_-_-_-_-_-_-EMACS CUSTOM VARS-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; we want vim C-u
(setq evil-want-C-u-scroll t
    visible-bell t
    neo-smart-open t
    completion-auto-help t
    version-control t     ;; Use version numbers for backups.
    kept-new-versions 10  ;; Number of newest versions to keep.
    kept-old-versions 0   ;; Number of oldest versions to keep.
    delete-old-versions t ;; Don't ask to delete excess backup versions.
    backup-by-copying t   ;; Copy all files, don't rename them.
    vc-make-backup-files t)

;; add nrv custom dir
(add-to-list 'load-path "~/.config/emacs/nrv")

;; declare all your packages
(setq my-packages
      '(evil
    evil-dvorak
	evil-leader
	neotree
	gnu-elpa-keyring-update
	)
  )
      
;; _-_-_-_-_-_-_-_-_-_-_-_-_-EMACS CUSTOM STUFF_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(evil-leader evil-dvorak neotree gnu-elpa-keyring-update evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;; _-_-_-_-_-_-_-_-_-_-_-_-_MAKE SURE ALL PACKAGES INSTALLED-_-_-_-_-_-_-_-
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))
;; _-_-_-_-_-_-_-_-_-_-_-_-_NRV EVIL-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(require 'nrv-evil)
;; _-_-_-_-_-_-_-_-_-_-_-_-_Commands-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(defalias 'up 'package-refresh-contents)
;; _-_-_-_-_-_-_-_-_-_-_-_-_Backups Start_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs_backups/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs_backups/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;; _-_-_-_-_-_-_-_-_-_-_-_-_Backups End_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
