;;; functions-nrv.el --- lisp functions used in my init.el  -*- lexical-binding: t; -*-

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

(defun nrv/open-or-create-file-buffer (path)
  "Open path in a buffer as the only buffer in frame, creating it and parent dirs if needed."
  (interactive "FOpen or create file: ")
  (let* ((abs (expand-file-name path))
         (dir (file-name-directory abs)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (switch-to-buffer (or (get-file-buffer abs)
                          (find-file-noselect abs)))
    (delete-other-windows)
    (princ (format "%s: %s"
                   (if (file-exists-p abs) "Opening" "Creating")
                   abs))))

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

(defun nrv/refresh-before (orig-fun &rest args)
  "Refresh packages before..."
  (package-refresh-contents)
  (message "refreshing package list...")
  (apply orig-fun args))

(defun nrv/shift-line-right ()
  "shift a line right, then put cursor at eol."
  (interactive)
  (evil-shift-right-line 1)
  (evil-end-of-line)
  (forward-char))

;; non editing
(defun nrv-error-handler (err)
  "Handle errors by printing them to minibuffer (ERR: error)."
  (message "Error: %S" err))

(defun delete-this-file (&optional forever)
  "Delete the file associated with `current-buffer'.
If FOREVER is non-nil, the file is deleted without being moved to trash."
  (interactive "P")
  (when-let* ((file (or (buffer-file-name)
                        (user-error "Current buffer is not visiting a file")))
              ((y-or-n-p "Delete this file? ")))
    (delete-file file (not forever))
    (kill-buffer (current-buffer))))

(defun zck/move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one. If given a directory,
keep the file name."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  ;; If new-location is a directory, append the original filename
  (when (file-directory-p new-location)
    (setq new-location (expand-file-name
                        (file-name-nondirectory
                         (or buffer-file-name (buffer-name)))
                        new-location)))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))


(defun nrv/kill-other-text-buffers ()
  "Kill all other buffers, excluding system buffers."
  (interactive)
  (let ((killed 0))
    (dolist (buffer (delq (current-buffer) (buffer-list)))
      (with-current-buffer buffer
        (let ((name (buffer-name buffer)))
          (unless (or (string-prefix-p "*" name)           ; All buffers starting with *
                      (string-prefix-p " " name)           ; Hidden buffers (start with space)
                      (derived-mode-p 'comint-mode)        ; Shells/REPLs
                      (derived-mode-p 'dired-mode)         ; Directory buffers
                      (derived-mode-p 'special-mode)       ; Special modes
                      (eq major-mode 'minibuffer-inactive-mode)) ; Mini-buffers
            (message "Killing: %s (mode: %s)" name major-mode)
            (kill-buffer buffer)
            (setq killed (1+ killed))))))
    (message "Killed %d buffers" killed)))

(defun nrv/set-tab (tab-width)
  "set all the tab width vars"
  (interactive)
  (setq c-basic-offset tab-width
        tab-width tab-width
        evil-shift-width tab-width
        cperl-indent-level tab-width))

(defun flyspell-on-for-buffer-type ()
  "Enable Flyspell appropriately for the major mode of the current buffer.  Uses
`flyspell-prog-mode' for modes derived from `prog-mode', so only strings and
comments get checked.  All other buffers get `flyspell-mode' to check all text.
If flyspell is already enabled, does nothing."
  (interactive)
  (if (not (symbol-value flyspell-mode)) ; if not already on
	    (progn
	      (if (derived-mode-p 'prog-mode)
	          (progn
	            (message "Flyspell on (code)")
	            (flyspell-prog-mode))
	        ;; else
	        (progn
	          (message "Flyspell on (text)")
	          (flyspell-mode 1)))
	      ;; I tried putting (flyspell-buffer) here but it didn't seem to work
	      )))

(defun flyspell-toggle ()
  "Turn Flyspell on if it is off, or off if it is on.  When turning on, it uses
`flyspell-on-for-buffer-type' so code-vs-text is handled appropriately."
  (interactive)
  (if (symbol-value flyspell-mode)
	    (progn ; flyspell is on, turn it off
	      (message "Flyspell off")
	      (flyspell-mode -1))
	  (flyspell-on-for-buffer-type))) ; else - flyspell is off, turn it on

(provide 'functions-nrv)

;;; functions-nrv.el ends here

                                        ; LocalWords:  tjwh zck
