;;; sudo-nrv.el --- edit files with root
;;; Commentary: find file function and open file edditing as root.
;;; Change Log: made tramp not ask for backup permission, because I rmed it
;;; Code:

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "Sudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun sudo ()
  "edit the file in the buffer as root."
  (interactive)
  (sudo-find-file (buffer-file-name)))


;; Turn of backup feature for "remote" files
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; edit the current file as root
(global-set-key (kbd "C-c !") #'sudo)
(global-set-key (kbd "C-c C-!") #'sudo-find-file)


(provide 'sudo-nrv)
;;; sudo-nrv.el ends here
