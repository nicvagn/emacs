;;; init.el -*- lexical-binding: t; -*-
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


(package-initialize)
(add-to-list 'load-path "~/.config/emacs/lisp/")
(add-to-list 'load-path "~/.config/emacs/lisp/repo-grep/")
(add-to-list 'load-path "~/.config/emacs/lisp/telephone-line")

;; major mode remapping
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))

;; For packages that check for python-mode specifically
(with-eval-after-load 'python-ts-mode
  ;; Add python-ts-mode to relevant hooks
  (add-hook 'python-ts-mode-hook 'python-mode-hook))

(with-eval-after-load 'eglot
  (defun nrv/eglot-ensure-if-server-advice (orig-fun &rest args)
    "Call `eglot-ensure` only if a server is defined for the current major mode."
    (when (cl-find major-mode eglot-server-programs
                   :test (lambda (mode entry)
                           (or (eq mode (car entry))
                               (and (symbolp (car entry))
                                    (provided-mode-derived-p mode (car entry))))))
      (apply orig-fun args)))
  (advice-add 'eglot-ensure :around #'nrv/eglot-ensure-if-server-advice))

;;_-_-_-_-_-_-_-_-_-_-_-_-_-set env for emacs-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
(setenv "WORKON_HOME" "/home/nrv/.venvs/")
(setenv "TERM" "xterm-256color")
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
 ;; debugging + error handling
 debug-on-error nil ;; back traces
 user-error-exceptions nil ;; treat errs as real errs
 error-handler #'nrv-error-handler
 ;; tabs and indenting
 ;; if the value is nil, then TAB indents the current line only if
 ;; point is at the left margin or in the line’s indentation;
 ;; otherwise, it inserts a tab character
 tab-always-indent t
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
 centaur-tabs-height 32
 centaur-tabs-set-icons t
 centaur-tabs-icon-type 'all-the-icons
 centaur-tabs-cycle-scope 'tabs
 ;; Corfu
 corfu-auto-delay  0.15 ;; may cause issues due to being fast
 corfu-auto-prefix 0.15
 ;; tramp
 tramp-allow-unsafe-temporary-files t
 ;; flymake
 next-error-function 'flymake-goto-next-error
 ;; org mode
 org-image-actual-width nil
 ;; tree sit
 treesit-auto-install t
 ;; use-package
 use-package-always-ensure t
 use-package-verbose t
 use-package-compute-statistics nil)

;;_-_-_-_-_-_-_-_-_-_-_-_-_-Packages_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
(use-package emacs
  :custom
  ;; Corfu recommend
  (text-mode-ispell-word-completion nil)
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  (use-short-answers t))

;; sets exec path from zsh shell
(use-package exec-path-from-shell)

(use-package format-all
  :commands format-all-mode
  :defer t
  :diminish format-all-mode
  :hook ((python-mode . format-all-mode)
         (js-mode  . format-all-mode)
         (css-mode  . format-all-mode)
         (html-mode  . format-all-mode)
         (shell-script-mode . format-all-mode)
         (web-mode  . format-all-mode)
         (yaml-mode . format-all-mode)
         (typescript-mode . format-all-mode)
         (typescript-ts-mode . format-all-mode))
  :bind
  ("C-c f" . format-all-region-or-buffer)
  :config
  ;; Define formatters for different modes
  (setq format-all-default-formatters
        '(("Python" . black)
          ("JavaScript" . prettier)
          ("TypeScript" . prettier)
          ("CSS" . prettier)
          ("HTML" . prettier)
          ("JSON" . prettier)
          ("Rust" . rustfmt)
          ("Go" . gofmt))))

(use-package diminish)

;; GNU Emacs package for jumping to visible text using a char-based decision tree.
(use-package avy)

(use-package counsel)

;;;; ++++ MINI-BUFFER start ++++
(use-package vertico
  :init (vertico-mode)
  :config
  (set-face-attribute 'vertico-current nil
                      :background 'unspecified
                      :foreground "orange")
  (set-face-attribute 'completions-first-difference nil
                      :foreground "yellow")
  :custom
  (vertico-cycle t)
  (vertico-count 20)
  (vertico-resize t)
  :bind (:map vertico-map
              ;; vertico-directory-enter checks what kind of completion is active:
              ;; - If it's file completion → does directory/file logic
              ;; - If it's command completion → just executes the command
              ;; - If it's other completion → uses default behaviour
              ("<f5>" . vertico-directory-enter)
              ("<f6>" . vertico-next)
              ("<f7>" . vertico-previous)
              ("<f8>" . keyboard-quit)
              ("DEL" . vertico-directory-delete-char)))

;; Better matching (type parts of words in any order)
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles partial-completion))))
  ;; Configure orderless matching
  (setq orderless-matching-styles
        '(orderless-literal
          orderless-prefixes
          orderless-initialism
          orderless-regexp))
  :custom
  (completion-styles '(orderless basic)))

;; Save completion history
(use-package savehist
  :init (savehist-mode))

;; Show helpful annotations next to completions
(use-package marginalia
  :init (marginalia-mode))

;; Enhanced commands
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("M-y" . consult-yank-pop)
         ("C-s" . consult-line)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("C-x C-r" . consult-recent-file)))

;; ++++ MINI-BUFFER END ++++

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

  ;; added advice to only call if server available
  :hook ((python-mode  . eglot-ensure)
         (js-mode  . eglot-ensure)
         (css-mode  . eglot-ensure)
         (html-mode  . eglot-ensure)
         (web-mode  . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (prog-mode . eglot-ensure)
         (sh-base-mode . eglot-ensure))

  :config
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"]
                                                   :plugins (
                                                             :pycodestyle (:enabled :json-false)
                                                             :mccabe (:enabled :json-false)
                                                             :pyflakes (:enabled :json-false)
                                                             :flake8 (:enabled :json-false
                                                                               :maxLineLength 88)
                                                             :ruff (:enabled t
                                                                             :lineLength 88)
                                                             :pydocstyle (:enabled t
                                                                                   :convention "numpy")
                                                             :yapf (:enabled :json-false)
                                                             :autopep8 (:enabled :json-false)
                                                             :black (:enabled t
                                                                              :line_length 88
                                                                              :cache_config t)
                                                             :mypy (:enabled t
                                                                             :live_mode t))))))

  ;; Performance optimizations
  (setq eglot-events-buffer-size 0        ; Disable event logging for performance
        eglot-sync-connect nil            ; Don't block on server connection
        eglot-autoshutdown t              ; Shutdown server when last buffer is killed
        eglot-send-changes-idle-time 0.5) ; Debounce changes



  ;; Language server configurations
  (add-to-list 'eglot-server-programs '(html-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(typescript-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(css-mode . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives '("pylsp"
                                          ("pyright-langserver" "--stdio")
                                          "jedi-language-server"
                                          ))))
  (add-to-list 'eglot-server-programs '(json-mode . ("vscode-json-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(dockerfile-mode . ("docker-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((scala-mode scala-ts-mode) .
                 ("metals"
                  "-J-Dmetals.http=true"
                  "-J-Dmetals.http-port=5031"
                  "-J-Xmx2G"))))

;; Corfu auto complete ui
(use-package corfu
  :after eglot
  :config
  (setq corfu-cycle t                    ; Enable cycling for `corfu-next/previous'
        corfu-auto t                     ; Enable auto completion
        corfu-auto-delay 0.1             ; Auto completion delay
        corfu-auto-prefix 1              ; Minimum prefix for auto completion
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
                 #'cape-elisp-block               ; Complete elisp in org/markdown blocks
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
  :interpreter
  ("scala" . scala-mode)
  :defer t
  :config
  (setq prettify-symbols-alist scala-prettify-symbols-alist)
  ;; For complex scala files
  (setq max-lisp-eval-depth 50000)
  (setq max-specpdl-size 5000)
  (prettify-symbols-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :defer t
  :interpreter
  ("scala" . scala-mode)
  :config
  (setq
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   sbt:program-options '("-Dsbt.supershell=false")))

(use-package centaur-tabs
  :init
  (centaur-tabs-mode t)
  :config
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
  ;; Override Magit's completion function completely
  (setq magit-completing-read-function 'completing-read)
  ;; Make sure ido doesn't interfere
  (setq magit-ido-mode nil)

  ;; Ensure consistent completion everywhere
  (advice-add 'magit-builtin-completing-read :override #'completing-read)
  (advice-add 'magit-ido-completing-read :override #'completing-read)
  ;; Make sure Magit uses the same completion as everything else
  (advice-add 'magit-builtin-completing-read :override #'completing-read)
  ;; Ensure these use completing-read:
  (setq magit-branch-read-upstream-first 'fallback)
  (setq magit-branch-prefer-remote-upstream '("master" "main"))

  ;; Optional: Configure Git completion
  (setq magit-git-executable "git")
  (setq magit-status-show-untracked-files t)
  :hook
  ;; Ensure Vertico is active in Magit buffers
  (magit-mode . (lambda () (setq-local completion-styles '(orderless basic)))))

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
  ;; Global activation
  (global-treesit-auto-mode)
  ;; Enable for all supported languages
  (treesit-auto-add-to-auto-mode-alist 'all)
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
  :defer t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

(use-package cus-edit
  :ensure nil
  :custom (custom-file (expand-file-name "custom.el" user-emacs-directory))
  :init (load custom-file :noerror))

(use-package flyspell-correct
  :after flyspell
  :defer t
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)
              ("C-M-;" . flyspell-buffer)))
;;_-_-_-_-_-_-_-_-_-_-_-_-_- Global lisp _-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; emacs built-in's
(require 'project)
(require 'flymake)
;; evil dvorak custom evil and key-map
(require 'evil-dvorak-nrv)
;; find-file-in-project.el -- elpy wants it
(require 'find-file-in-project)
;; visible indentation marks
(require 'highlight-indentation)
(diminish 'highlight-indentation-mode)
;; set C-c ! reopen file with sudo and sudo-find-file C-c C-!
(require 'sudo-nrv)
;; pretty colours
(require 'rainbow-delimiters)
(rainbow-delimiters-mode 1)
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
;; repo-grep -- does what you expect
(require 'repo-grep)
(autoload 'repo-grep "repo-grep")
(autoload 'repo-grep-multi "repo-grep")
;; Telephone Line is a new implementation of Powerline for emacs
(require 'telephone-line)
(setq
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
(telephone-line-mode 1)
;;_-_-_-_-_-_-_-_-_-_-_-_-_other emacs settings-_-_-_-_-_-_-_-_-_-_-_-_-_-_
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1) ;; reload a file if changed outside of emacs
(global-hl-line-mode 1)
(auto-fill-mode 1) ;; complete if only
(savehist-mode 1) ;; save history
(transient-mark-mode 1)  ;; selection highlighting
(which-function-mode 1)  ;; tell which function
(highlight-indentation-mode 1)
;;_-_-_-_-_-_-_-_-_-_-_-_-_- Global Key Map -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(require 'global-bindings) ;; fancy pants
(require 'mode-maps-nrv)
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
;; c and c++ hooks
(add-hook 'c-mode-hook #'prepare-c)
(add-hook 'c++-mode-hook #'prepare-cpp)
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
;; scala mode
(add-hook 'scala-mode-hook (lambda () (nrv/set-tab 2)))
;; Delete trailing white space always
(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; Neotree -- popup file manager
(add-hook 'neotree-mode-hook
          (lambda ()
            (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
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
;;_-_-_-_-_-_-_-_-_-_-_-_-_-Custom Variables-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
(defcustom package-last-refresh-date nil
  "Date and time when package lists have been refreshed.

  This variable is then used to check whether
  `package-refresh-contents' call is needed before calling
  `package-install'. The value of this variable is updated when
  `package-refresh-contents' is called.

  See `package-refresh-hour-threshold' for the amount of time needed to
  trigger a refresh."
  :type 'string
  :group 'package)
(defcustom package-automatic-refresh-threshold 24
  "Amount of hours since last `package-refresh-contents' call
  needed to trigger automatic refresh before calling `package-install'."
  :type 'number
  :group 'package)
;;_-_-_-_-_-_-_-_-_-_-_-_-_- Advice -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
;; Ensure ibuffer opens with point at the current buffer's entry.
(defadvice ibuffer
    (around ibuffer-point-to-most-recent) ()
    "Open ibuffer with cursor pointed to most recent buffer name."
    (let ((recent-buffer-name (buffer-name)))
      ad-do-it
      (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)
;; Ensure package data is refreshed before package install
(define-advice package-install (:before (&rest _) package-refresh-contents-maybe)
  (when (or (null package-last-refresh-date)
            (> (/ (float-time
                   (time-subtract (date-to-time (format-time-string "%Y-%m-%dT%H:%M"))
                                  (date-to-time package-last-refresh-date)))
                  3600)
               package-automatic-refresh-threshold))
    (package-refresh-contents)))
(define-advice package-refresh-contents (:after (&rest _) update-package-refresh-date)
  (customize-save-variable 'package-last-refresh-date
                           (format-time-string "%Y-%m-%dT%H:%M")))

(provide 'init)
;;; init.el ends here

                                        ; LocalWords:  setq yasnippet
                                        ; LocalWords:  codespell melpa nongnu
                                        ; LocalWords:  emacs scala
                                        ; LocalWords:  unselected LightGoldenrod DarkOrange MistyRose
                                        ; LocalWords:  DeepSkyBlue
                                        ; LocalWords:  daemonp flx
                                        ; LocalWords:  yasnippit tjwh
                                        ; LocalWords:  Neotree muh
                                        ; LocalWords:  Debounce Xmx4G
                                        ; LocalWords:  nerdtree djoyner
                                        ; LocalWords:  Xmx2G
                                        ; LocalWords:  multimarkdown f9cfcfd3f
