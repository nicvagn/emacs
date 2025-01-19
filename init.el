;;; init.el --- My emacs init.el
;;; commentary:
;; mostly keymaps that are mode specific

(provide 'init)
;;; code:
(when (< emacs-major-version 27)
  (package-initialize))

;; add lisp dir to load-path
(let ((default-directory "/home/nrv/.config/emacs/lisp" ))
  (setq load-path
        (append
         (let ((load-path  (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "gray8" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 117 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(ansi-color-bright-blue ((t (:background "dodger blue" :foreground "dodger blue"))))
 '(centaur-tabs-default ((t (:background "dark gray" :foreground "dim gray"))))
 '(centaur-tabs-selected ((t (:background "orange" :foreground "black"))))
 '(centaur-tabs-unselected ((t (:background "#3D3C3D" :foreground "gray82"))))
 '(cursor ((t (:background "LightGoldenrod4"))))
 '(eglot-highlight-symbol-face ((t (:background "gray5" :foreground "firebrick1"))))
 '(eglot-mode-line ((t (:inherit font-lock-constant-face :foreground "cornflower blue" :weight bold))))
 '(eglot-parameter-hint-face ((t (:inherit eglot-inlay-hint-face :foreground "dark salmon"))))
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
 '(region ((t (:extend t :background "dark cyan")))))

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
 '(eglot-send-changes-idle-time 0.3)
 '(evil-emacs-state-modes
   '(dired-mode vterm-mode 5x5-mode archive-mode bbdb-mode biblio-selection-mode blackbox-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bs-mode bubbles-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode custom-theme-choose-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dun-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ess-help-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode git-rebase-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode gomoku-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-cherry-mode magit-diff-mode magit-log-mode magit-log-select-mode magit-popup-mode magit-popup-sequence-mode magit-process-mode magit-reflog-mode magit-refs-mode magit-revision-mode magit-stash-mode magit-stashes-mode magit-status-mode mh-folder-mode monky-mode mpuz-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode notmuch-tree-mode occur-mode org-agenda-mode package-menu-mode pdf-outline-buffer-mode pdf-view-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode snake-mode solitaire-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(typescript-mode treesit-fallback rainbow-delimiters eglot yasnippet-classic-snippets markup markdown-mode company scroll-on-jump all-the-icons-gnus all-the-icons-nerd-fonts all-the-icons-dired all-the-icons-completion auto-rename-tag ac-html which-key yasnippet-snippets all-the-icons corfu jedi python-django vterm org-modern yasnippet centaur-tabs magit gnu-elpa-keyring-update evil reformatter))
 '(package-vc-selected-packages
   '((treesit-fallback :vc-backend Git :url "https://github.com/renzmann/treesit-fallback.git")))
 '(text-mode-hook
   '(turn-on-flyspell yas-minor-mode-on text-mode-hook-identify))
 '(tool-bar-mode nil))
;;_-_-_-_-_-_-_-_-_-_-_-_-_other emacs settings-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1) ;; reload a file if changed outside of emacs
(global-hl-line-mode 1)
(auto-fill-mode t) ;; complete if only
(savehist-mode) ;; save history
(transient-mark-mode 1)  ;; selection highlighting
(which-function-mode 1)  ;; tell which func.
;; remove the legacy hook from flymake
;;_-_-_-_-_-_-_-_-_-_-_-_-_-Packages_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;; treesit.el an intelligent way to choose between a default mode, such as python-mode, and it’s tree-sitter enhanced version, python-ts-mode, automatically.
(use-package treesit-auto
  :demand t
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package rainbow-delimiters
  :load-path "lisp"
  :config
  (rainbow-delimiters-mode-enable))

(use-package evil
  :ensure t
  :demand t
  :config
  ;; make c delete
  (define-key evil-normal-state-map (kbd "c") 'evil-delete)
  ;; set evil undo to one built into emacs
  (evil-set-undo-system 'undo-redo)
  (use-package nrv-evil-dvorak
    :demand t
    :config
    (global-evil-dvorak-mode 1))
  (use-package evil-leader
    :demand t
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
  (evil-mode t))

;; --- emacs lsp ---
(use-package eglot
  :ensure t
  :defer t
  :bind
  (("C-c b" . xref-go-back)
   ("C-c d" . xref-find-definitions)
   ("C-c r" . xref-find-references)
   ("C-c f" . eglot-format-buffer))
  :hook (python-mode . eglot-ensure)
  (js-mode . eglot-ensure)
  (typescript-ts-mode . eglot-ensure))

(use-package centaur-tabs
  :ensure t
  :demand t
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  :bind
  (
   :map centaur-tabs-mode-map
        ("M-[" . centaur-tabs-backward)
   ("M-]" . centaur-tabs-forward)
   ("M-{" . centaur-tabs-move-current-tab-to-left)
   ("M-}" .  centaur-tabs-move-current-tab-to-right)
   ("<f1>" . centaur-tabs-backward-group)
   ("<f2>" . centaur-tabs-forward-group)))

(use-package yasnippet
  :ensure t
  :demand t
  :config
  (setq yas-snippet-dir "~/.config/emacs/snippets")
  (yas-global-mode 1))

;; display possible keyboard shortcuts
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; corfu autocomplete ui
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)  ;; Enable cycling
  (global-corfu-mode)
  :bind
  (
   ("<f5>" . corfu-complete)
   ("<f6>" . corfu-next)
   ("<f7>" . corfu-previous)
   ("<f8>" . corfu-quit-no-match)
  )
  :ensure t)

;; all the icons - icons in text
;; make sure to M-x: all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

;; magit
(use-package magit
  :ensure t
  :bind
  (
   ;; magit
   ("C-c C-g c" . #'magit-commit)
   ("C-c C-g l" . #'magit-log-current)
   ("C-c C-g d" . #'magit-diff)
   ("C-c C-g p" . #'magit-push-current-to-upstream)
   ("C-c C-g u" . #'magit-pull-from-upstream)
   ("C-c C-g t" . #'magit-tag)
   ("C-c C-g b" . #'magit-branch)
   ("C-c C-g a" . #'magit-stage-buffer-file)
   ("C-c C-g s" . #'magit-status-quick)
  )
)

(use-package typescript-mode
  :ensure t
  :mode ("\\.tsx?\\'" . typescript-mode)
  :config
  (setq typescript-indent-level 2))


;;_-_-_-_-_-_-_-_-_-_-_-_-_-setq var's_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;; Use spaces not tabs
(setq-default indent-tabs-mode nil)
;; we want vim C-u
(setq
 ;; EVIL
 evil-want-C-u-scroll t
 evil-scroll-count 20
 evil-want-fine-undo t

 ;; scrolling
 mouse-wheel-scroll-amount '(0.07)
 mouse-wheel-progressive-speed nil

 ;; JEDI AUTO complete
 jedi:complete-on-dot t
 completion-auto-help t
 completion-cycle-threshold nil ;; don't cycle completions

 ;; history/backup
 savehist-file "~/.config/emacs/backups/emacs_histfile"
 version-control t     ;; Use version numbers for backups.
 kept-new-versions 10  ;; Number of newest versions to keep.
 kept-old-versions 0   ;; Number of oldest versions to keep.
 delete-old-versions t ;; Don't ask to delete excess backup versions.
 backup-by-copying t   ;; Copy all files, don't rename them.

 ;; debug/backtrace
 debug-on-error t
 backtrace-depth 50

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

 ;; corfu
 corfu-auto-delay  0.2 ;; may cause issues due to being fast
 corfu-auto-prefix 0.2

 ;; org mode
 org-image-actual-width nil)

;;_-_-_-_-_-_-_-_-_-_-_-_-_-Global Keymaps-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; restart emacs
(global-set-key (kbd "C-M-r") 'restart-emacs)
;; scroll on jump
(global-set-key (kbd "<C-M-next>")  'diff-hl-next-hunk)
(global-set-key (kbd "<C-M-prior>") 'diff-hl-previous-hunk)
;; alt - l (lisp) eval buffer
(global-set-key (kbd "M-l") 'eval-buffer)
;; changing buffers
;; f9 vterm
(global-set-key (kbd "<f9>") 'vterm)
(global-set-key (kbd "<f1>") 'centaur-tabs-forward-group)
(global-set-key (kbd "<f1>") 'centaur-tabs-backward-group)
(global-set-key (kbd "A-[") 'centaur-tabs-backward)
(global-set-key (kbd "A-]") 'centaur-tabs-forward)
(global-set-key (kbd "<f9>") 'vterm)

;;_-_-_-_-_-_-_-_-_-_-_-_-_elisp I found/require_-_-_-_-_-_-_-_-_-_-_-_-_-_
;; the first require is squiggly no matter
;; mode hooks
(require 'nrv-modes) ;; modular af
;; org
(require 'org)
;; my own custom stuff
(require 'nrv-vterm)

;;_-_-_-_-_-_-_-_-_-_-_-_-_-Hooks_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; remove trailing whitespace before saving and make backup
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'before-save-hook  'force-backup-of-buffer)
(defun css-setup ()
  "Setup Emacs for css editing."
  (require 'evil)
  (setq tab-width 2
        evil-shift-width 2)
)

;; the prepare-XXX are defined in nrv-modes
(add-hook 'prog-mode-hook #'prepare-prog)
(add-hook 'python-mode-hook #'prepare-python)
(add-hook 'dired-mode-hook #'prepare-dired)
(add-hook 'css-mode-hook #'css-setup)

;;_-_-_-_-_-_-_-_-_-_-_-_-_-Aliases_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(defalias 'up 'package-refresh-contents)

;;_-_-_-_-_-_-_-_-_-_-_-_-_-Backups Start_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.config/emacs/backups/per-save")))

(defun force-backup-of-buffer ()
  "Make a special /='per session' backup at the first save of each Emacs session."
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.config/emacs/backups/per-session")))
          (kept-new-versions 6))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))


(provide 'init.el)
;;; init.el ends here
