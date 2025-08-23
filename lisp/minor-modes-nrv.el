;;; minor-modes-nrv.el --- My prepare my custom minor modes  -*- lexical-binding: t; -*-
;;; commentary:
;; mostly to add path to dir line

;;; code:
(defun add-mode-line-dirtrack ()
  "When editing a file, show the last 2 directories of the current path in the mode line."
  (interactive)
  (add-to-list 'mode-line-buffer-identification
                '(:eval (substring default-directory
                                  (+ 1 (string-match "/[^/]+/[^/]+/$" default-directory)) nil))))

(define-minor-mode mode-line-dirtrack-mode
  "Toggle displaying dirtrack in mode line
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

See the command \\[mode-line-dirtrack]."
  :init-value nil
  :lighter " ML-DT"
  :interactive t
  (if mode-line-dirtrack-mode
      (add-mode-line-dirtrack)))

;; you can not turn it off

(provide 'minor-modes-nrv)
;;; minor-modes-nrv.el ends here
