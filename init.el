(package-initialize) ;; this has to be done first, I think
;; add custom dir to load-path
(add-to-list 'load-path "~/.config/emacs/lisp" )
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
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
 '(column-number-mode t)
 '(custom-safe-themes
   '("5f4b294798037c1abe4be3ee481897f533f2b088465c1f10f1ae8a0f297b4b1d" "ee0785c299c1d228ed30cf278aab82cf1fa05a2dc122e425044e758203f097d2" "3f75d4633820090be31d1f91fa1e33427b5dc09235efa189157592c822d1843a" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" default))
 '(evil-emacs-state-modes
   '(vterm-mode 5x5-mode archive-mode bbdb-mode biblio-selection-mode blackbox-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bs-mode bubbles-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode custom-theme-choose-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dun-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ess-help-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode git-rebase-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode gomoku-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-cherry-mode magit-diff-mode magit-log-mode magit-log-select-mode magit-popup-mode magit-popup-sequence-mode magit-process-mode magit-reflog-mode magit-refs-mode magit-revision-mode magit-stash-mode magit-stashes-mode magit-status-mode mh-folder-mode monky-mode mpuz-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode notmuch-tree-mode occur-mode org-agenda-mode package-menu-mode pdf-outline-buffer-mode pdf-view-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode snake-mode solitaire-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(yasnippet-snippets all-the-icons flymake-codespell corfu jedi python-django vterm org-modern yasnippet centaur-tabs magit spacemacs-theme ace-window gnu-elpa-keyring-update evil-leader evil))
 '(text-mode-hook
   '(turn-on-flyspell yas-minor-mode-on text-mode-hook-identify))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "gray8" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight semi-bold :height 120 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(ansi-color-bright-blue ((t (:background "dodger blue" :foreground "dodger blue"))))
 '(centaur-tabs-default ((t (:background "dark gray" :foreground "dim gray"))))
 '(centaur-tabs-selected ((t (:background "orange" :foreground "black"))))
 '(eglot-highlight-symbol-face ((t (:background "gray5" :foreground "PaleVioletRed1"))))
 '(font-lock-builtin-face ((t (:foreground "pale violet red"))))
 '(font-lock-comment-face ((t (:foreground "chartreuse1" :weight bold))))
 '(font-lock-constant-face ((t (:foreground "light slate blue"))))
 '(font-lock-doc-face ((t (:inherit font-lock-string-face :foreground "pink1"))))
 '(font-lock-string-face ((t (:foreground "spring green"))))
 '(font-lock-variable-name-face ((t (:foreground "pale green" :weight extra-bold))))
 '(font-lock-variable-use-face ((t (:inherit font-lock-variable-name-face))))
 '(hl-line ((t (:extend t :background "grey18"))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "LightGoldenrod4"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "DarkOrange4"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "orchid"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "dark cyan"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "MistyRose1"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "tomato"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "lawn green"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "DeepSkyBlue1"))))
 '(region ((t (:extend t :background "dark cyan"))))
 '(shadow ((t (:foreground "dim gray")))))
;;_-_-_-_-_-_-_-_-_-_-_-_-_elisp I found/require_-_-_-_-_-_-_-_-_-_-_-_-_-_
(require 'rainbow-delimiters)
;; mode hooks
(require 'nrv-modes) ;; modular af
;; org
(require 'org)
;; my own custom stuff
(require 'nrv-vterm)
;;_-_-_-_-_-_-_-_-_-_-_-_-_-setq var's_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;; we want vim C-u
(setq
  ;; EVIL
  evil-want-C-u-scroll t
  evil-scroll-count 10
  evil-want-fine-undo t

  ;; JEDI AUTO complete
  jedi:complete-on-dot t
  completion-auto-help t
  completion-cycle-threshold 2 ;; cycle completions only 2

  ;; history/backup
  savehist-file "~/.emacs_histfile"
  version-control t     ;; Use version numbers for backups.
  kept-new-versions 10  ;; Number of newest versions to keep.
  kept-old-versions 0   ;; Number of oldest versions to keep.
  delete-old-versions t ;; Don't ask to delete excess backup versions.
  backup-by-copying t   ;; Copy all files, don't rename them.

  ;; Revert/reload Dired and other buffers on filesystem change
  global-auto-revert-non-file-buffers t
  ;; but do it quietly
  auto-revert-verbose nil

  ;; centar tabs
  centaur-tabs-style "wave"
  centaur-tabs-height 38
  centaur-tabs-set-icons t
  centaur-tabs-icon-type 'all-the-icons
  centaur-tabs-cycle-scope 'tabs

  ;; org mode
  org-image-actual-width nil)
;;_-_-_-_-_-_-_-_-_-_-_-_-_other emacs settings-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1) ;; reload a file if changed outside of emacs
(global-hl-line-mode 1)
(auto-fill-mode t) ;; complete if only
(savehist-mode) ;; save history
(transient-mark-mode 1)  ;; selection highlighting
(which-function-mode 1)  ;; tell which func.

;;_-_-_-_-_-_-_-_-_-_-_-_-_-Packages_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

(use-package evil
  :ensure t
  :demand
  :config
    ;; make c delete
    (define-key evil-normal-state-map (kbd "c") 'evil-delete)
    ;; set evil undo to one built into emacs
    (evil-set-undo-system 'undo-redo)
    (use-package nrv-evil-dvorak)
    (use-package evil-leader
      :demand
      :config
        ;; <leader>
        (evil-leader/set-leader "<SPC>") ;; set to space
        ;; define leader mappings
        (evil-leader/set-key
          "w" 'save-buffer
          "k" 'kill-this-buffer
          "q" 'evil-quit
          "x" 'delete-window
          "0" 'delete-window
          "1" 'delete-other-windows
          "s" 'evil-window-split
          "v" 'evil-window-vsplit
          "<SPC>" 'evil-window-next)

        (global-evil-leader-mode))

    ;; after modes have been loaded, turn on evil
  (global-evil-leader-mode 1) ;; activate leader mode, must be done early
  (global-evil-dvorak-mode 1)
  (evil-mode t))

(use-package eglot
  :ensure t
  :defer t
  :hook (python-mode . eglot-ensure))

(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  :bind
  ("M-[" . centaur-tabs-backward)
  ("M-]" . centaur-tabs-forward)
  ("<f1>" . centaur-tabs-backward-group)
  ("<f2>" . centaur-tabs-forward-group))
(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dir "~/.config/emacs/snippets")
  (yas-global-mode 1))

;; flymake-codespell - must have codespell installed on your sys
(use-package flymake-codespell
  :ensure t
  :hook (prog-mode . flymake-codespell-setup-backend))

;; corfu autocomplete ui
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)                ;; Enable cycling
  :config
  (global-corfu-mode)
  :bind
  ("<f5>" . corfu-complete)
  ("<f6>" . corfu-next)
  ("<f7>" . corfu-previous)
  :ensure t)

;; all the icons - icons in text
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)
;;_-_-_-_-_-_-_-_-_-_-_-_-_-Keymaps_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; global keymap
;; restart emacs
(global-set-key (kbd "C-M-r") 'restart-emacs)
;; alt - l (lisp) eval buffer
(global-set-key (kbd "M-l") 'eval-buffer)
;; changing buffers
(global-set-key (kbd "M-[") #'centaur-tabs-backward)
(global-set-key (kbd "M-]") #'centaur-tabs-forward)
(global-set-key (kbd "<f1>") #'centaur-tabs-backward-group)
(global-set-key (kbd "<f2>")  #'centaur-tabs-forward-group)
;; f8 evil toggle
(global-set-key (kbd "<f8>") 'evil-toggle)
;; f9 Vterm
(global-set-key (kbd "<f9>") 'vterm)
;;_-_-_-_-_-_-_-_-_-_-_-_-_-Aliases_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(defalias 'up 'package-refresh-contents)
;;_-_-_-_-_-_-_-_-_-_-_-_-_-Backups Start_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.config/emacs/backups/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.config/emacs/backups/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)
