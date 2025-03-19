;;; functions-nrv.el --- lisp functions used in my init.el

;; Copyright (C) 3619  nrv

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; nrv-error-handler -- I don't honestly know handles errors?
;; delete-this-file -- delete the file in a buffer
;; tjwh/backward-kill-word-on-this-line -- kill backwards word but DO NOT
;;                                         kill newline.

;;; Code:
;;_-_-_-_-_-_-_-_-_-_-_-_-_-My Functions_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
(defun nrv-error-handler (err)
  "Handle errors by printing them to minibuffer (ERR: error)."
  (message "Error: %S" err))

(defun delete-this-file (&optional forever)
  "Delete the file associated with `current-buffer'.
If FOREVER is non-nil, the file is deleted without being moved to trash."
  (interactive "P")
  (when-let ((file (or (buffer-file-name)
                       (user-error "Current buffer is not visiting a file")))
             ((y-or-n-p "Delete this file? ")))
    (delete-file file (not forever))
    (kill-buffer (current-buffer))))

(defun tjwh/backward-kill-word-on-this-line ()
  "Delete previous word on this line or leave point at the beginning of the line"
  (interactive)
  (let ((orig-point (point)))
    (beginning-of-line)
    (let ((beg-line-point (point)))
      (goto-char orig-point)
      (backward-word)
      (let ((backward-word-point (point)))
        ;; If the position of the beginning of the line is the same or
        ;; before the previous word position, remove previous word
        (goto-char orig-point)
        (if (> beg-line-point backward-word-point)
            (goto-char beg-line-point)
          (backward-kill-word 1))
       ))))

(provide 'functions-nrv)

;;; functions-nrv.el ends here
