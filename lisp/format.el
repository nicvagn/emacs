;;; format.el
;; Author: Nicolas Vaagen
;; Created: 2026
;; Keywords:  format
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'shfmt)
(defun nrv/format-whatever ()
  "Format the current buffer using whatever you can."
  (interactive)
  (cond

   ((and (fboundp 'eglot-format-buffer)
         (bound-and-true-p eglot--managed-mode))
    (eglot-format-buffer))

   ((derived-mode-p 'emacs-lisp-mode)
    (indent-region (point-min) (point-max)))

   ((derived-mode-p 'org-mode)
    (org-indent-region (point-min) (point-max)))

   ((and (derived-mode-p 'web-mode)
         (fboundp 'web-mode-buffer-indent))
    (web-mode-buffer-indent))

   ((and (derived-mode-p 'sh-mode)
         (fboundp 'shfmt-buffer))
    (shfmt-buffer))

   ;; Fallback: re-indent everything
   (t
    (indent-region (point-min) (point-max))))

  (message "Formatted w: %s"
           (cond
            ((and (fboundp 'eglot-format-buffer)
                  (bound-and-true-p eglot--managed-mode)) "Eglot")
            ((derived-mode-p 'emacs-lisp-mode) "indent-region")
            ((derived-mode-p 'sh-mode) "shfmt")
            ((derived-mode-p 'org-mode) "org-indent")
            ((derived-mode-p 'web-mode) "web-mode")
            (t "indent-region (fallback)"))))

(provide 'format)
;;; format.el ends here

; LocalWords:  shfmt
