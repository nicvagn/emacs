;;; custom.el  -*- lexical-binding: t; -*-
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
 '(inhibit-startup-screen t)
 '(ispell-personal-dictionary "/home/nrv/.config/emacs/personal_dictionary")
 '(neo-window-fixed-size nil)
 '(package-last-refresh-date "2025-09-13T12:23")
 '(package-selected-packages
   '(all-the-icons all-the-icons-dired all-the-icons-gnus avy cape
                   centaur-tabs consult corfu-candidate-overlay
                   corfu-terminal counsel cus-edit diminish
                   editorconfig eglot eglot-java elpy evil-leader
                   exec-path-from-shell flyspell-correct format-all
                   magit marginalia markdown-mode neotree
                   nerd-icons-corfu orderless php-ts-mode python-black
                   qml-mode sbt-mode scala-mode tramp tramp-theme
                   transient-dwim treesit-auto treesit-fallback
                   undo-tree vertico vterm web-mode yasnippet-capf))
 '(package-vc-selected-packages
   '((treesit-fallback :vc-backend Git :url
                       "https://github.com/renzmann/treesit-fallback.git")
     (php-ts-mode :vc-backend Git :url
                  "https://github.com/emacs-php/php-ts-mode")))
 '(python-shell-virtualenv-root "/home/nrv/.venvs/emacs-venv-root/")
 '(resize-mini-windows t)
 '(shell-pop-shell-type
   '("vterm" "*vterm*" (lambda nil (vterm shell-pop-term-shell))))
 '(tool-bar-mode nil))
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
