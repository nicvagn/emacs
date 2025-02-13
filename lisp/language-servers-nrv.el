;;; laguage-servers-nrv.el --- My emacs modes, mode keymaps
;;; commentary:
;; eglot language servers
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(html-mode . ("vscode-html-language-server" "--stdio"))
               '((css-mode css-ts-mode) . ("vscode-css-language-server" "--stdio"))))
(provide 'language-servers-nrv)
;; Local Variables:
;; byte-compile-warnings: (not free-vars dired-mode-map evil-shift-width)
;; End:
;;; modes-nrv.el ends here
