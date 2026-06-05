;;; vterm-nrv.el --- vterm for inferior term in Emacs  -*- lexical-binding: t; -*-
;;; commentary:
;; better than nothing, as everything must be Emacs

;;; code:
(defun use-monospace ()
  "Switch the current buffer to a monospace font."
  (face-remap-add-relative 'default '(:family "Monospace")))

(use-package vterm
  :ensure t
  :after (evil centaur-tabs)
  :init
  (setq vterm-always-compile-module t)
  :bind (:map vterm-mode-map
              ( "C-S-v" .  #'yank-pop)
              ( "M-[" .  #'centaur-tabs-backward)
              ( "M-]" . #'centaur-tabs-forward)
              ( "M-{" . #'centaur-tabs-move-current-tab-to-left)
              ( "M-}" . #'centaur-tabs-move-current-tab-to-right)
              ( "<f1>" . #'centaur-tabs-backward-group)
              ( "<f2>" . #'centaur-tabs-forward-group)
              ( "<f3>" . #'neotree-toggle)
              ( "<f4>" . #'shell-pop)
              ( "C-SPC" . #'evil-window-next)
              ( "C-w" . #'evil-window-next))
  :hook
  (vterm-mode . (lambda ()
                  "Prepare vterm-mode"
		              (buffer-face-mode t)
		              (text-scale-decrease 1)
		              (use-monospace)
                  (centaur-tabs-local-mode)
                  (with-eval-after-load 'evil
                    (evil-define-key 'emacs vterm-mode-map (kbd "C-z")
                      (lambda ()
                        "Sent C-z to vterm"
                        (interactive)
                        (vterm-send "C-z"))))
                  )
              )
  )
;; popup shell
(require 'shell-pop)
;; export $EDITOR to be this Emacs
(add-hook 'vterm-mode-hook  'with-editor-export-editor)
;; why not for all
(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)

;; make good and sure it is exported
(keymap-global-set "<remap> <async-shell-command>"
                   #'with-editor-async-shell-command)
(keymap-global-set "<remap> <shell-command>"
                   #'with-editor-shell-command)

(provide 'vterm-nrv)
;;; vterm-nrv.el ends here
