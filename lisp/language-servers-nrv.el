;;; laguage-servers-nrv.el --- Add eglot language servers and mode hooks
;;; commentary:
;; eglot language servers
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(html-mode . ("vscode-html-language-server" "--stdio"))
               '((css-mode css-ts-mode) . ("vscode-css-language-server" "--stdio"))
               '(python-mode . ("pylsp")))

  ;; add the correct mode hooks
  (add-hook  (python-mode-hook . eglot-ensure))
  (add-hook  (html-mode-hook . eglot-ensure))
  (add-hook  (js-mode-hook . eglot-ensure))
  (add-hook  (css-mode-hook . eglot-ensure)))
(provide 'language-servers-nrv)
;; Local Variables:
;; byte-compile-warnings: (not free-vars nil)
;; End:
;;; modes-nrv.el ends here
