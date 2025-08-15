;;; init.el --- My emacs init.el
;;; Commentary:
;;  muh Emacs config

;;; code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "Https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(package-initialize) ;; this has to be done right away
;; add custom dir to load-path
(add-to-list 'load-path "~/.config/emacs/lisp/")
(add-to-list 'load-path "~/.config/emacs/lisp/emacs-neotree/")
(add-to-list 'load-path "~/.config/emacs/lisp/repo-grep/")
(add-to-list 'load-path "~/.config/emacs/lisp/telephone-line")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(tramp))
 '(custom-safe-themes
   '("4876347a77e6f9b3afad962104962ea2b051922e325dcc24776974c256885e30"
     "131e0902a346cbd3cb1f944cdd6bd9bf0a3c5270528741ac8b303bd9b2819af5"
     "14d11e2acebfa2b3d779bf06142c2d23a4b0593706cf35303e8c60f9cfcfd3f8"
     "5f4b294798037c1abe4be3ee481897f533f2b088465c1f10f1ae8a0f297b4b1d"
     "ee0785c299c1d228ed30cf278aab82cf1fa05a2dc122e425044e758203f097d2"
     "3f75d4633820090be31d1f91fa1e33427b5dc09235efa189157592c822d1843a"
     "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf"
     default))
 '(eglot-send-changes-idle-time 0.2)
 '(evil-emacs-state-modes
   '(term-mode dired-mode 5x5-mode archive-mode bbdb-mode
               biblio-selection-mode blackbox-mode bookmark-bmenu-mode
               bookmark-edit-annotation-mode browse-kill-ring-mode
               bs-mode bubbles-mode bzr-annotate-mode calc-mode
               cfw:calendar-mode completion-list-mode Custom-mode
               custom-theme-choose-mode debugger-mode
               delicious-search-mode desktop-menu-blist-mode
               desktop-menu-mode doc-view-mode dun-mode
               dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode
               dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode
               dvc-status-mode dvc-tips-mode ediff-mode
               ediff-meta-mode efs-mode Electric-buffer-menu-mode
               emms-browser-mode emms-mark-mode emms-metaplaylist-mode
               emms-playlist-mode ess-help-mode etags-select-mode
               fj-mode gc-issues-mode gdb-breakpoints-mode
               gdb-disassembly-mode gdb-frames-mode gdb-locals-mode
               gdb-memory-mode gdb-registers-mode gdb-threads-mode
               gist-list-mode git-rebase-mode gnus-article-mode
               gnus-browse-mode gnus-group-mode gnus-server-mode
               gnus-summary-mode gomoku-mode google-maps-static-mode
               ibuffer-mode jde-javadoc-checker-report-mode
               magit-cherry-mode magit-diff-mode magit-log-mode
               magit-log-select-mode magit-popup-mode
               magit-popup-sequence-mode magit-process-mode
               magit-reflog-mode magit-refs-mode magit-revision-mode
               magit-stash-mode magit-stashes-mode magit-status-mode
               mh-folder-mode monky-mode mpuz-mode mu4e-main-mode
               mu4e-headers-mode mu4e-view-mode notmuch-hello-mode
               notmuch-search-mode notmuch-show-mode notmuch-tree-mode
               occur-mode org-agenda-mode package-menu-mode
               pdf-outline-buffer-mode pdf-view-mode proced-mode
               rcirc-mode rebase-mode recentf-dialog-mode
               reftex-select-bib-mode reftex-select-label-mode
               reftex-toc-mode sldb-mode slime-inspector-mode
               slime-thread-control-mode slime-xref-mode snake-mode
               solitaire-mode sr-buttons-mode sr-mode sr-tree-mode
               sr-virtual-mode tar-mode tetris-mode tla-annotate-mode
               tla-archive-list-mode tla-bconfig-mode
               tla-bookmarks-mode tla-branch-list-mode tla-browse-mode
               tla-category-list-mode tla-changelog-mode
               tla-follow-symlinks-mode tla-inventory-file-mode
               tla-inventory-mode tla-lint-mode tla-logs-mode
               tla-revision-list-mode tla-revlog-mode
               tla-tree-lint-mode tla-version-list-mode
               twittering-mode urlview-mode vc-annotate-mode
               vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode
               vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode
               wab-compilation-mode xgit-annotate-mode
               xgit-changelog-mode xgit-diff-mode xgit-revlog-mode
               xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode
               xhg-mq-sub-mode xhg-status-extra-mode))
 '(flx-ido-mode t)
 '(ido-completion-buffer-all-completions t)
 '(ido-cr+-max-items 90000)
 '(ido-create-new-buffer 'always)
 '(ido-default-buffer-method 'selected-window)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-max-window-height 35)
 '(ido-mode 'both nil (ido))
 '(ido-rotate-file-list-default t)
 '(ido-ubiquitous-mode t)
 '(ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
 '(ido-vertical-indicator "-}")
 '(ido-vertical-mode t)
 '(ido-vertical-show-count t)
 '(inhibit-startup-screen t)
 '(ispell-personal-dictionary "/home/nrv/.config/emacs/personal_dictionary")
 '(neo-window-fixed-size nil)
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((treesit-fallback :vc-backend Git :url
                       "https://github.com/renzmann/treesit-fallback.git")
     (php-ts-mode :vc-backend Git :url
                  "https://github.com/emacs-php/php-ts-mode")))
 '(python-shell-virtualenv-root "/home/nrv/emacs/.python-environments/default/")
 '(resize-mini-windows t)
 '(shell-pop-shell-type
   '("vterm" "*vterm*" (lambda nil (vterm shell-pop-term-shell))))
 '(tool-bar-mode nil))

;;_-_-_-_-_-_-_-_-_-_-_-_-_-set env for emacs-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
(setenv "WORKON_HOME" "/home/nrv/.venvs/")
;;_-_-_-_-_-_-_-_-_-_-_-_-_-setq vars-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;; default to 4 space width tabs
(setq-default tab-width 4
              c-basic-offset tab-width
              ;; don't confirm creation of new files
              confirm-nonexistent-file-or-buffer nil
              ;; Use spaces not tabs
              indent-tabs-mode nil
              cperl-indent-level tab-width)
;; everything is highlighted
(customize-set-variable 'treesit-font-lock-level 4)

(setq
 ;; Emacs spell checking
 ispell-program-name "hunspell"
 ispell-local-dictionary "en_CA"
 ;; display full path in frame title
 frame-title-format '("%f")
 ;; pop up file manger theme
 neo-theme (if (display-graphic-p) 'icons 'arrow)
 ;; debugging + error handling
 debug-on-error nil ;; back traces
 user-error-exceptions nil ;; treat errs as real errs
 error-handler #'nrv-error-handler
 ;; tabs and indenting
 ;; if the value is nil, then TAB indents the current line only if
 ;; point is at the left margin or in the line’s indentation;
 ;; otherwise, it inserts a tab character
 tab-always-indent t
 ;; EVIL
 evil-want-C-u-scroll t
 evil-scroll-count 10
 evil-want-fine-undo t
 ;; scrolling
 mouse-wheel-scroll-amount '(0.07)
 mouse-wheel-progressive-speed nil
 ;; completions customizing
 completion-auto-help 1
 completion-cycle-threshold 1 ;; cycle through completions when 1 or less
 ;; history/backup
 savehist-file "~/.config/emacs/backups/emacs_histfile"
 version-control t     ;; Use version numbers for backups.
 kept-new-versions 10  ;; Number of newest versions to keep.
 kept-old-versions 10  ;; Number of oldest versions to keep.
 delete-old-versions t ;; Don't ask to delete excess backup versions.
 backup-by-copying t   ;; Copy all files, don't rename them.
 ;; Revert/reload Dired and other buffers on filesystem change
 global-auto-revert-non-file-buffers t
 ;; but do it quietly
 auto-revert-verbose nil
 ;; centaur tabs
 centaur-tabs-style "wave"
 centaur-tabs-height 38
 centaur-tabs-set-icons t
 centaur-tabs-icon-type 'all-the-icons
 centaur-tabs-cycle-scope 'tabs
 ;; corfu
 corfu-auto-delay  0.15 ;; may cause issues due to being fast
 corfu-auto-prefix 0.15
 ;; tramp
 tramp-allow-unsafe-temporary-files t
 ;; flymake
 next-error-function 'flymake-goto-next-error
 ;; ido
 ido-enable-flex-matching t
 ido-everywhere t
 ido-virtual-buffers t
 ido-use-faces t
 ido-default-buffer-method 'selected-window
 ido-auto-merge-work-directories-length -1
 ido-vertical-define-keys 'C-n-C-p-up-down-left-right
 ido-create-new-buffer 'always
 ;; magit
 magit-completing-read-function 'magit-ido-completing-read
 ;; org mode
 org-image-actual-width nil
 ;; tree sit
 treesit-auto-install t
 ;; use-package
 use-package-always-ensure t
 use-package-verbose t
 use-package-compute-statistics nil

 ;; telephone line -- mode line stuff
 telephone-line-lhs
 '((evil   . (telephone-line-evil-tag-segment))
   (accent . (telephone-line-vc-segment
              telephone-line-erc-modified-channels-segment
              telephone-line-process-segment))
   (nil    . (telephone-line-minor-mode-segment
              telephone-line-buffer-segment)))
 telephone-line-rhs
 '((nil    . (telephone-line-misc-info-segment))
   (accent . (telephone-line-major-mode-segment))
   (evil   . (telephone-line-airline-position-segment)))

 telephone-line-primary-left-separator 'telephone-line-cubed-left
 telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
 telephone-line-primary-right-separator 'telephone-line-cubed-right
 telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right
 telephone-line-height 24
 telephone-line-evil-use-short-tag t)


;;_-_-_-_-_-_-_-_-_-_-_-_-_-Packages_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
(use-package emacs
  :custom
  ;; Corfu recommend
  (text-mode-ispell-word-completion nil)
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate
   #'command-completion-default-include-p))

;; sets exec path from zsh shell
(use-package exec-path-from-shell)

(use-package format-all
  :commands format-all-mode
  :defer t
  :diminish format-all-mode
  :hook (prog-mode . format-all-mode)
  :bind
  ("C-c f" . format-all-region-or-buffer)
  :config
  ;; Define formatters for different modes
  (setq format-all-default-formatters
        '(("Python" black)
          ("JavaScript" prettier)
          ("TypeScript" prettier)
          ("CSS" prettier)
          ("HTML" prettier)
          ("JSON" prettier)
          ("Rust" rustfmt)
          ("Go" gofmt)))
  (add-hook 'prog-mode-hook #'format-all-mode))

(use-package diminish)

;; GNU Emacs package for jumping to visible text using a char-based decision tree.
(use-package avy)

(use-package counsel)

;; ---- IDO start ----
(defun nrv/ido ()
  "set ido up for nrv"
  (ido-mode 1)
  (ido-vertical-mode 1)
  (ido-ubiquitous-mode +1)
  (flx-ido-mode 1))

(use-package ido)

(use-package flx-ido
  :requires ido)

(use-package ido-completing-read+
  :requires ido
  :config
  (setq ido-ubiquitous-max-items 50000
        ido-cr+-max-items 50000))

(use-package ido-vertical-mode
  :requires ido
  :after ido
  :config
  (set-face-attribute 'ido-vertical-first-match-face nil
                      :background 'unspecified
                      :foreground "orange")
  (set-face-attribute 'ido-vertical-only-match-face nil
                      :background 'unspecified
                      :foreground "yellow")
  (set-face-attribute 'ido-vertical-match-face nil
                      :foreground 'unspecified))

;; this enables stuff
(nrv/ido)
;; ---- IDO end ----

;; --- auto complete start ---
(use-package eglot
  :defer t
  :bind
  ;; C-c c for eglot functionality
  (("C-c c r" . eglot-rename)
   ("C-c c f" . eglot-format)
   ("C-c c a" . eglot-code-actions)
   ("C-c c d" . eldoc)
   ("C-c c h" . eldoc-doc-buffer)
   ("C-c c i" . eglot-find-implementation)
   ("C-c c t" . eglot-find-typeDefinition)
   ("C-c c x" . eglot-reconnect)
   ("C-c c s" . eglot-signature-eldoc-function))

  :hook ((python-mode  . eglot-ensure)
         (python-ts-mode  . eglot-ensure)
         (js-mode  . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (css-mode  . eglot-ensure)
         (html-mode  . eglot-ensure)
         (web-mode  . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (prog-mode . eglot-ensure))

  :config
  ;; Performance optimizations
  (setq eglot-events-buffer-size 0)        ; Disable event logging for performance
  (setq eglot-sync-connect nil)            ; Don't block on server connection
  (setq eglot-autoshutdown t)              ; Shutdown server when last buffer is killed
  (setq eglot-send-changes-idle-time 0.5)  ; Debounce changes

  ;; Language server configurations
  (add-to-list 'eglot-server-programs '(html-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(css-mode . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives '(("pyright-langserver" "--stdio")
                                          "jedi-language-server"
                                          "pylsp"))))
  (add-to-list 'eglot-server-programs '(json-mode . ("vscode-json-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(dockerfile-mode . ("docker-langserver" "--stdio"))))

;; Corfu auto complete ui
(use-package corfu
  :after eglot
  :config
  (setq corfu-cycle t                    ; Enable cycling for `corfu-next/previous'
        corfu-auto t                     ; Enable auto completion
        corfu-auto-delay 0.1             ; Auto completion delay
        corfu-auto-prefix 2              ; Minimum prefix for auto completion
        corfu-separator ?\s              ; Orderless field separator
        corfu-preselect 'first           ; Always preselect first option
        corfu-quit-at-boundary nil       ; Never quit at completion boundary
        corfu-quit-no-match t            ; quit if there is no match
        corfu-preview-current 'insert    ; Preview current candidate
        corfu-on-exact-match nil         ; Configure handling of exact matches
        corfu-scroll-margin 5            ; Use scroll margin
        corfu-max-width 100              ; Maximum popup width
        corfu-min-width 15               ; Minimum popup width
        corfu-count 10)                  ; Maximum number of candidates
  ;; Enable Corfu more generally for every minibuffer, as long as no other
  ;; completion UI is active. If you use Mct or Vertico as your main minibuffer
  ;; completion UI. From the Corfu documentation.
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active) ; Useful if using mct
                (bound-and-true-p vertico--input)) ; Useful if using vertico
      (setq-local corfu-auto nil) ; Ensure auto completion is disabled
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  :bind
  (("<f5>" . corfu-complete)
   ("<f6>" . corfu-next)
   ("<f7>" . corfu-previous)
   ("<f8>" . corfu-quit)))

;; Terminal support for Corfu
(use-package corfu-terminal
  :ensure t
  :unless (display-graphic-p)  ; Only load in terminal
  :hook (after-init . corfu-terminal-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion))))

  ;; Configure orderless matching
  (setq orderless-matching-styles
        '(orderless-literal
          orderless-prefixes
          orderless-initialism
          orderless-regexp)))

(use-package cape
  :ensure t
  :init
  ;; Programming modes completion setup
  (defun nrv/setup-programming-capf ()
    "Setup completion-at-point-functions for programming."
    (setq-local completion-at-point-functions
                (list
                 #'eglot-completion-at-point      ; LSP completion (when eglot is active)
                 #'cape-dabbrev                   ; Dynamic abbreviations
                 #'cape-keyword                   ; Language keywords
                 #'cape-file                      ; File name completion
                 #'cape-elisp-block              ; Complete elisp in org/markdown blocks
                 #'cape-abbrev)))                 ; Static abbreviations

  ;; Apply to programming modes
  (dolist (mode-hook '(python-mode-hook
                       python-ts-mode-hook
                       js-mode-hook
                       js-ts-mode-hook
                       typescript-ts-mode-hook
                       css-mode-hook
                       css-ts-mode-hook
                       html-mode-hook
                       html-ts-mode-hook
                       web-mode-hook
                       c-mode-hook
                       c++-mode-hook
                       rust-mode-hook
                       go-mode-hook))
    (add-hook mode-hook #'nrv/setup-programming-capf))

  :config
  ;; Add yasnippet support globally
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; --- auto complete end ---


(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :config (nrv/set-tab 2)
  )

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :interpreter
  ("scala" . scala-mode)
  :config
  ;;set tab width two 2 (I could not get nrv/set-tab to work)
  (setq c-basic-offset 2
        evil-shift-width 2
        cperl-indent-level 2
        ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
        sbt:program-options '("-Dsbt.supershell=false")))

(use-package centaur-tabs
  ;; without this demand, tabs don't show of the bat
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
      ;; "Remote")
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p '(prog-mode html-mode))
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(helpful-mode
                          help-mode))
       "Help")
      ((memq major-mode '(org-mode
                          org-agenda-clockreport-mode
                          org-src-mode
                          org-agenda-mode
                          org-beamer-mode
                          org-indent-mode
                          org-bullets-mode
                          org-cdlatex-mode
                          org-agenda-log-mode
                          diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :bind
  (("M-[" . centaur-tabs-backward)
   ("M-]" . centaur-tabs-forward)
   ("M-}" . centaur-tabs-move-current-tab-to-right)
   ("M-{" . centaur-tabs-move-current-tab-to-left)
   ("<f1>" . centaur-tabs-backward-group)
   ("<f2>" . centaur-tabs-forward-group)))

(use-package yasnippet
  :init
  (setq yas-snippet-dir "~/.config/emacs/snippets")
  (yas-global-mode 1))

;; display possible keyboard shortcuts
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))


;; all the icons - icons in text
;; make sure to M-x: all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p))

;; magit
(use-package magit
  :bind
  (("C-c C-g c" . #'magit-commit)
   ("C-c C-g l" . #'magit-log-current)
   ("C-c C-g d" . #'magit-diff-unstaged)
   ("C-c C-g g" . #'magit-status)
   ("C-c C-g p" . #'magit-push-current-to-upstream)
   ("C-c C-g u" . #'magit-pull-from-upstream)
   ("C-c C-g m" . #'magit-merge)
   ("C-c C-g t" . #'magit-tag)
   ("C-c C-g b" . #'magit-branch)
   ("C-c C-g a" . #'magit-file-stage)
   ("C-c C-g s" . #'magit-status-quick))
  :config
  (setq magit-status-show-untracked-files t))

(use-package web-mode
  :defer t
  :config
  (nrv/set-tab 2)
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(use-package treesit-auto
  :ensure t
  :demand t  ; Load immediately
  :config
  ;; Enable for all supported languages
  (treesit-auto-add-to-auto-mode-alist 'all)

  ;; Global activation
  (global-treesit-auto-mode)

  (setq treesit-auto-install-grammars t)  ; Auto-install missing grammars

  ;; Custom grammar recipes (if needed)
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (go "https://github.com/tree-sitter/tree-sitter-go")))

  (setq treesit-font-lock-level 4))  ; Maximum syntax highlighting

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;;_-_-_-_-_-_-_-_-_-_-_-_-_- Spelling -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)
              ("C-M-;" . flyspell-buffer)))
;;_-_-_-_-_-_-_-_-_-_-_-_-_- Global lisp _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; emacs built-in's
(require 'project)
(require 'flymake)
;; find-file-in-project.el -- elpy wants it
(require 'find-file-in-project)
;; visible indentation marks
(require 'highlight-indentation)
(diminish 'highlight-indentation-mode)
;; nerdtree for files
(require 'neotree)
;; set C-c ! reopen file with sudo and sudo-find-file C-c C-!
(require 'sudo-nrv)
;; pretty colours
(require 'rainbow-delimiters)
;;; functions-nrv -- useful functions?
;; nrv-error-handler -- I don't honestly know handles errors?
;; delete-this-file -- delete the file in a buffer
;; tjwh/backward-kill-word-on-this-line -- kill backwards word but DO NOT
;;                                         kill newline.
;; djoyner/evil-shift-****-visual -- do not loose selection when you shift
;;                                                                  (L or R)
(require 'functions-nrv)
;; mode hooks
(require 'prepare-nrv)
;; org
(require 'org)
;; my own custom vterm
(require 'vterm-nrv)
;; yasnippit completion at point
(require 'yasnippet-capf)
;; python IDE stuff
(require 'python-nrv)
;; fzf.el -- includes fzf-git and fzf-find-file
(require 'fzf)
;; evil dvorak custom evil and key-map
(require 'evil-dvorak-nrv)
;; repo-grep -- does what you expect
(require 'repo-grep)
;; Telephone Line is a new implementation of Powerline for emacs
(require 'telephone-line)

;;_-_-_-_-_-_-_-_-_-_-_-_-_other emacs settings-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1) ;; reload a file if changed outside of emacs
(global-hl-line-mode 1)
(auto-fill-mode 1) ;; complete if only
(savehist-mode 1) ;; save history
(transient-mark-mode 1)  ;; selection highlighting
(which-function-mode 1)  ;; tell which func.
(highlight-indentation-mode 1)
(rainbow-delimiters-mode 1)
(telephone-line-mode 1)
(autoload 'repo-grep "repo-grep")
(autoload 'repo-grep-multi "repo-grep")

;;_-_-_-_-_-_-_-_-_-_-_-_-_- Global Key Map -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(require 'global-bindings) ;; fancy pants
(require 'mode-maps-nrv)
;;_-_-_-_-_-_-_-_-_-_-_-_-_- Advice -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; Ensure ibuffer opens with point at the current buffer's entry.
(defadvice ibuffer
    (around ibuffer-point-to-most-recent) ()
    "Open ibuffer with cursor pointed to most recent buffer name."
    (let ((recent-buffer-name (buffer-name)))
      ad-do-it
      (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)
;;_-_-_-_-_-_-_-_-_-_-_-_-_-Mode Hooks-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; remove hooks
;; remove the legacy hook from flymake
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
;; prepare functions  are defined in prepare-nrv.el
(add-hook 'text-mode-hook #'prepare-text)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'emacs-lisp-mode-hook #'prepare-lisp)
(add-hook 'emacs-lisp-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'prepare-prog)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; python hooks
(add-hook 'python-mode-hook #'prepare-python)
;; sort python import's on save
(add-hook 'python-mode-hook 'python-isort-on-save-mode)
(add-hook 'python-mode-hook 'display-line-numbers-mode)
(add-hook 'dired-mode-hook #'prepare-dired)
;; web hooks
(add-hook 'css-ts-mode-hook #'prepare-css)
(add-hook 'css-ts-mode-hook 'display-line-numbers-mode)
(add-hook 'html-mode-hook #'prepare-html)
(add-hook 'html-mode-hook 'display-line-numbers-mode)
(add-hook 'web-mode-hook #'prepare-web)
(add-hook 'web-mode-hook 'display-line-numbers-mode)
;; Delete trailing white space always
(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; prepare ido
;; ido everywhere messes with dired in vertical ido-mode
(add-hook 'dired-mode-hook #'disable-ido-everywhere)
(add-hook 'ido-setup-hook #'prepare-ido)

;; Neotree -- popup file manager
(add-hook 'neotree-mode-hook
          (lambda ()
            (evil-dvorak-mode -1) ;; buffer local when set
            (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
            (evil-define-key 'normal neotree-mode-map (kbd "l") 'neotree-quick-look)
            (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
            (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
            (evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-refresh)
            (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
            (evil-define-key 'normal neotree-mode-map (kbd "t") 'neotree-next-line)
            (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
            (evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-previous-line)
            (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
            (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)))

;; make sure exec path is path when started as daemon
(when (daemonp)
  (exec-path-from-shell-initialize))

;;_-_-_-_-_-_-_-_-_-_-_-_-_-emacs modes_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(dolist (p '((inferior-python-mode . emacs)
             ;; set *shell modes to use evil emacs state
             (shell-mode . emacs)
             (vterm-mode . emacs)
             (ansi-term-mode . emacs)
             (eshell-mode . emacs)))
  (evil-set-initial-state (car p) (cdr p)))

;;_-_-_-_-_-_-_-_-_-_-_-_-_-Aliases_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(defalias 'up 'package-refresh-contents)
(defalias 'del 'delete-this-file)
;; custom faces, at the bottom bc was in the way
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "gray4" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight bold :height 114 :width normal :foundry "ADBO" :family "Source Code Pro"))))
 '(ansi-color-blue ((t (:background "dodger blue" :foreground "dodger blue"))))
 '(ansi-color-bright-blue ((t (:background "cornflower blue" :foreground "sky blue"))))
 '(ansi-color-bright-red ((t (:background "IndianRed1" :foreground "firebrick1"))))
 '(ansi-color-red ((t (:background "firebrick3" :foreground "firebrick1"))))
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
 '(ido-first-match ((t (:background "gray8" :foreground "green1" :weight heavy))))
 '(ido-vertical-first-match-face ((t (:inherit ido-first-match))))
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
  "Make a special per session backup at the first save of each Emacs session."
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

                                        ; LocalWords:  setq yasnippet
                                        ; LocalWords:  codespell melpa nongnu
                                        ; LocalWords:  emacs scala
                                        ; LocalWords:  unselected LightGoldenrod DarkOrange MistyRose
                                        ; LocalWords:  DeepSkyBlue
                                        ; LocalWords:  daemonp flx
                                        ; LocalWords:  yasnippit
                                        ; LocalWords:  Neotree
                                        ; LocalWords:  Debounce
