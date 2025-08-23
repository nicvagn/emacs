;;; sudo-nrv.el --- edit files with root  -*- lexical-binding: t; -*-
;;; Commentary: find file function and open file edditing as root.
;;; Change Log: made tramp not ask for backup permission, because I rmed it
;;; Code:

(require 'sudo-edit)
(defalias 'sudo-find-file 'sudo-edit-find-file)
(defalias 'sudo 'sudo-edit)

;; Turn of backup feature for "remote" files
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; edit the current file as root
(global-set-key (kbd "C-c !") #'sudo)
(global-set-key (kbd "C-c C-!") #'sudo-find-file)

(provide 'sudo-nrv)
;;; sudo-nrv.el ends here
