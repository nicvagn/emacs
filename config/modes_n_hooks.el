;; modes_n_hooks.el - Major and Minor modes settings

;; modes_n_hooks.el define modes and hooks
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'emacs-lisp-mode-hook #'display-line-numbers-mode)

;; make mark visible - provided by visible-mark.el
(require 'visible-mark)
(global-visible-mark-mode 1)

;; completion cycling for minibuffer with tab - provided by
;;                             minibuffer-complete-cycle.el
(require 'minibuffer-complete-cycle)
