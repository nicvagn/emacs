;;; init.el --- My emacs init.el
;;; Commentary:
;;  muh Emacs config

;;; code:
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
 '(custom-enabled-themes '(tramp))
 '(custom-safe-themes
   '("4876347a77e6f9b3afad962104962ea2b051922e325dcc24776974c256885e30" "131e0902a346cbd3cb1f944cdd6bd9bf0a3c5270528741ac8b303bd9b2819af5" "14d11e2acebfa2b3d779bf06142c2d23a4b0593706cf35303e8c60f9cfcfd3f8" "5f4b294798037c1abe4be3ee481897f533f2b088465c1f10f1ae8a0f297b4b1d" "ee0785c299c1d228ed30cf278aab82cf1fa05a2dc122e425044e758203f097d2" "3f75d4633820090be31d1f91fa1e33427b5dc09235efa189157592c822d1843a" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" default))
 '(eglot-send-changes-idle-time 0.2)
 '(evil-emacs-state-modes
   '(dired-mode vterm-mode 5x5-mode archive-mode bbdb-mode biblio-selection-mode blackbox-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bs-mode bubbles-mode bzr-annotate-mode calc-mode cfw:calendar-mode completion-list-mode Custom-mode custom-theme-choose-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dun-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ess-help-mode etags-select-mode fj-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode git-rebase-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode gomoku-mode google-maps-static-mode ibuffer-mode jde-javadoc-checker-report-mode magit-cherry-mode magit-diff-mode magit-log-mode magit-log-select-mode magit-popup-mode magit-popup-sequence-mode magit-process-mode magit-reflog-mode magit-refs-mode magit-revision-mode magit-stash-mode magit-stashes-mode magit-status-mode mh-folder-mode monky-mode mpuz-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode notmuch-tree-mode occur-mode org-agenda-mode package-menu-mode pdf-outline-buffer-mode pdf-view-mode proced-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode snake-mode solitaire-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode))
 '(inhibit-startup-screen t)
 '(ispell-personal-dictionary "/home/nrv/.config/emacs/personal_dictionary")
 '(package-selected-packages
   '(use-package tramp-theme cape transient dash llama magit-section flycheck track-changes project rescript-mode scala-mode pyvenv evil-leader flymake-codespell magit-diff-flycheck magit-tbdiff magit-delta magit web-mode flymake-cspell treesit-auto treesit-fallback rainbow-delimiters eglot yasnippet-classic-snippets markup markdown-mode company all-the-icons-gnus all-the-icons-nerd-fonts all-the-icons-dired all-the-icons-completion auto-rename-tag ac-html which-key yasnippet-snippets all-the-icons corfu jedi python-django vterm org-modern yasnippet centaur-tabs gnu-elpa-keyring-update evil reformatter))
 '(package-vc-selected-packages
   '((php-ts-mode :vc-backend Git :url "https://github.com/emacs-php/php-ts-mode")
     (treesit-fallback :vc-backend Git :url "https://github.com/renzmann/treesit-fallback.git")))
 '(text-mode-hook
   '(turn-on-flyspell yas-minor-mode-on text-mode-hook-identify))
 '(tool-bar-mode nil))
;;_-_-_-_-_-_-_-_-_-_-_-_-_-setq var's_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;; Use spaces not tabs
(setq-default indent-tabs-mode nil)

;; default to 4 space width tabs
(setq-default tab-width 4
              c-basic-offset tab-width
              cperl-indent-level tab-width)
;; everything is highlighted
(customize-set-variable 'treesit-font-lock-level 4)

;; mode remaping
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(setq
 mode-line-buffer-identification
 (list 'buffer-file-name
       (propertized-buffer-identification "%12f")
       (propertized-buffer-identification "%12b"))
 ;; debugging + error handling
 debug-on-error nil ;; no backtraces
 user-error-exceptions nil ;; treat errs as real errs
 error-handler #'nrv-error-handler
 ;; tabs and indenting
 ;; if the value is nil, then TAB indents the current line only if
 ;; point is at the left margin or in the line’s indentation;
 ;; otherwise, it inserts a tab character
 tab-always-indent nil
 ;; EVIL
 evil-want-C-u-scroll t
 evil-scroll-count 15
 evil-want-fine-undo t
 ;; scrolling
 mouse-wheel-scroll-amount '(0.07)
 mouse-wheel-progressive-speed nil
 ;; corfu ant jedi complete
 jedi:complete-on-dot t
 completion-auto-help t
 completion-cycle-threshold 1 ;; cycle completions NEVER
 ;; history/backup
 savehist-file "~/.config/emacs/backups/emacs_histfile"
 version-control t     ;; Use version numbers for backups.
 kept-new-versions 10  ;; Number of newest versions to keep.
 kept-old-versions 10   ;; Number of oldest versions to keep.
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
 ;; corfu
 corfu-auto-delay  0.2 ;; may cause issues due to being fast
 corfu-auto-prefix 0.2
 ;; tramp
 tramp-allow-unsafe-temporary-files t
 ;; flymake
 next-error-function 'flymake-goto-next-error
 ;; org mode
 org-image-actual-width nil)

;;_-_-_-_-_-_-_-_-_-_-_-_-_- Global lisp _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; ace-flyspell
(require 'ace-flyspell)
(ace-flyspell-setup)
;; Indentation marks
(require 'highlight-indentation)
;; set C-c ! reopen file with sudo and sudo-find-file C-c C-!
(require 'sudo-nrv)
;; pretty colours
(require 'rainbow-delimiters)
;; functions-nrv -- useful functions?
;; nrv-error-handler -- I don't honestly know handles errors?
;; delete-this-file -- delete the file in a buffer
;; tjwh/backward-kill-word-on-this-line -- kill backwards word but DO NOT
;;                                         kill newline.
(require 'functions-nrv)
;; mode hooks
(require 'modes-nrv) ;; modular af
;; org
(require 'org)
;; my own custom vterm
(require 'vterm-nrv)
;; yapsnippit completion at point
(require 'yasnippet-capf)
;; python ide stuff
(require 'python-nrv)
;; fzf.el -- includes fzf-git and fzf-find-file
(require 'fzf)
;; evil devorak costom evil and keymap
(require 'evil-dvorak-nrv)
;;_-_-_-_-_-_-_-_-_-_-_-_-_other emacs settings-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1) ;; reload a file if changed outside of emacs
(global-hl-line-mode 1)
(auto-fill-mode t) ;; complete if only
(savehist-mode) ;; save history
(transient-mark-mode 1)  ;; selection highlighting
(which-function-mode 1)  ;; tell which func.
(highlight-indentation-mode 1)
;;_-_-_-_-_-_-_-_-_-_-_-_-_-Packages_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

(use-package avy
  :ensure t)

;; --- emacs lsp ---
(use-package eglot
  ;; language server config and mode hooks in language-servers-nrv.
  :ensure t
  :defer t
  :config
  (add-to-list 'eglot-server-programs
               '(html-mode . ("vscode-html-language-server" "--stdio"))
               '(css-mode . ("vscode-css-language-server" "--stdio")))
  ;; Made into two statements because it was not working. IDK if the python srv is valid lisp
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives '(("pyright-langserver" "--stdio")
                                          "jedi-language-server"
                                          "pylsp"))))
  ;; eglot HOOKS! add the correct mode hooks
  :hook
  ((python-ts-mode . eglot-ensure)
   (html-mode . eglot-ensure)
   (js-mode . eglot-ensure)
   (css-mode . eglot-ensure))
  :bind
  ("C-c f" . eglot-format))

(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

;; The language server is handled in language-servers-nrv.el
(use-package rescript-mode
  :hook ((rescript-mode . (lambda () (electric-indent-local-mode -1))))
  :ensure t
  :after
  (eglot)
  :mode
  (("\\.bs.js\\'" . rescript-mode)
   ("\\.res\\'" . rescript-mode)
   ("\\.resi\\'" . rescript-mode)))

(use-package centaur-tabs
  :ensure t
  ;; without this demand, tabs don't show of the bat
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  :bind
  (("M-[" . centaur-tabs-backward)
   ("M-]" . centaur-tabs-forward)
   ("M-}" . centaur-tabs-move-current-tab-to-right)
   ("M-{" . centaur-tabs-move-current-tab-to-left)
   ("<f1>" . centaur-tabs-backward-group)
   ("<f2>" . centaur-tabs-forward-group)))

(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dir "~/.config/emacs/snippets")
  (yas-global-mode 1))

;; display possible keyboard shortcuts
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; flymake-codespell - must have codespell installed on your sys
(use-package flymake-codespell
  :ensure t
  :hook (prog-mode . flymake-codespell-setup-backend))

;; corfu autocomplete ui
(use-package corfu
  :ensure t
  :after eglot
  :custom
  (corfu-auto t)
  (corfu-cycle t)  ;; Enable cycling
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1)
  :bind
  (("<f5>" . corfu-complete)
   ("<f6>" . corfu-next)
   ("<f7>" . corfu-previous)
   ("<f8>" . corfu-quit)))

;; corfu cape extensions
(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...

  :config
  ;; add yasnippit snippits to completion at point
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; all the icons - icons in text
;; make sure to M-x: all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

;; magit
(use-package magit
  :ensure t
  :bind
  (("C-c C-g c" . #'magit-commit)
   ("C-c C-g l" . #'magit-log-current)
   ("C-c C-g d" . #'magit-diff)
   ("C-c C-g g" . #'magit-status)
   ("C-c C-g p" . #'magit-push-current-to-upstream)
   ("C-c C-g u" . #'magit-pull-from-upstream)
   ("C-c C-g t" . #'magit-tag)
   ("C-c C-g b" . #'magit-branch)
   ("C-c C-g a" . #'magit-stage-buffer-file)
   ("C-c C-g s" . #'magit-status-quick))
  :config
  (setq magit-status-show-untracked-files t))

(use-package web-mode
  :ensure t
  :defer t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.ts\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

;; auto use treesitter mode
(use-package treesit-auto
  :demand t
  :ensure t
  :config
  (global-treesit-auto-mode))


;;_-_-_-_-_-_-_-_-_-_-_-_-_Global (ish) hooks-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;; remove the legacy hook from flymake
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)

;;_-_-_-_-_-_-_-_-_-_-_-_-_- Global Key Map -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; restart emacs
(global-set-key (kbd "C-M-r") 'restart-emacs)
;; alt - l (lisp) eval buffer
(global-set-key (kbd "M-l") 'eval-buffer)
;; do not kill back over new line with kill back word
(global-set-key (kbd "C-<backspace>") #'tjwh/backward-kill-word-on-this-line)
;; f9 Vterm
(global-set-key (kbd "<f9>") 'vterm)
;; f12 to spellcheck
(global-set-key (kbd "<f12>") `ace-flyspell-dwim)
;; Horizontal split w alt -
;;_-_-_-_-_-_-_-_-_-_-_-_-_-Mode Hooks-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; hooks are defined in nrv-modes.el
(add-hook 'text-mode-hook #'prepare-text)
(add-hook 'emacs-lisp-mode-hook #'prepare-lisp)
(add-hook 'prog-mode-hook #'prepare-prog)
(add-hook 'python-mode-hook #'prepare-python)
(add-hook 'dired-mode-hook #'prepare-dired)
(add-hook 'css-mode-hook #'prepare-css)
(add-hook 'html-mode-hook #'prepare-html)
(add-hook 'web-mode-hook #'prepare-web)
;; Delete trailing whitespace always
(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; export the EDITOR env var when in ***mode***
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)
(add-hook 'vterm-mode-hook  'with-editor-export-editor)
;;_-_-_-_-_-_-_-_-_-_-_-_-_-Aliases_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(defalias 'up 'package-refresh-contents)
(defalias 'del 'delete-this-file)
;; custom faces, at the bottom bc was in the way
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
 '(region ((t (:extend t :background "dark cyan"))))
 '(web-mode-html-attr-name-face ((t (:foreground "deep pink"))))
 '(web-mode-html-tag-face ((t (:foreground "cornflower blue")))))
;;_-_-_-_-_-_-_-_-_-_-_-_-_-Backups Start_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.config/emacs/backups/per-save")))

(defun force-backup-of-buffer ()
  "Make a special 'per session' backup at the first save of each Emacs session."
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

(add-hook 'before-save-hook  'force-backup-of-buffer)
(provide 'init)
;;; init.el ends here
