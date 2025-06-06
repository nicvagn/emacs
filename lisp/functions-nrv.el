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
;; editing
(defun djoyner/evil-shift-left-visual ()
  "Evil shift left, but do not loose selection"
  (interactive)
  (call-interactively 'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(defun djoyner/evil-shift-right-visual ()
  "Evil shift right, but do not loose selection"
  (interactive)
  (call-interactively 'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

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

(defun nrv/normal-newline ()
  "Create a newline on next line, without breaking the line your on."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;; non editing
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

(defun dir-track ()
  "Show the current directory in the mode-line."
  (interactive)
  (add-to-list 'mode-line-buffer-identification
               '(:propertize ("" default-directory) face mode-line)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun nrv/set-tab (tab-width)
  "set all the tab width vars"
  (interactive)
  (setq c-basic-offset tab-width
        tab-width tab-width
        evil-shift-width tab-width
        cperl-indent-level tab-width))

(provide 'functions-nrv)

;;; functions-nrv.el ends here
