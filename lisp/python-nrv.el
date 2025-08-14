;;; python-nrv.el --- nrv python stuff
;; Copyright (C) 2025 Nicolas Vaagen

;; Author: Nicolas Vaagen <nicolasvaagen@gmail.com>
;; Version: 0.0.1

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

;; # Usage
;; Open emacs M-x pyvenv-activate RET dir_to_the_environment/env
;;; Code:

;; Enhanced Python mode configuration
(use-package python
  :config
  ;; Python indentation
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil
        python-indent-guess-indent-offset-verbose nil)

  ;; Python shell configuration
  (setq python-shell-interpreter "python"
        python-shell-interpreter-args "-i"
        python-shell-prompt-detect-failure-warning nil)

  ;; Enhanced syntax highlighting
  (setq python-font-lock-keywords python-font-lock-keywords))


(use-package reformatter
  :ensure t)

;; elpy python "IDE"
(use-package elpy
  :ensure t)

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))

  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python")))))

(use-package python-black
  :ensure t
  :demand t
  :after python
  :hook ((python-mode . python-black-on-save-mode)))

(defgroup python-isort nil
  "Python isort."
  :group 'python
  :prefix "python-isort-")

(defcustom python-isort-command "isort"
  "The name or the path to the `isort' command."
  :type 'string
  :group 'python-isort)

(defcustom python-isort-arguments '("--stdout" "--atomic" "-")
  "Arguments to `python-isort-command'."
  :type '(repeat string)
  :group 'python-isort)

;;;###autoload (autoload 'python-isort-buffer "python-isort" nil t)
;;;###autoload (autoload 'python-isort-region "python-isort" nil t)
;;;###autoload (autoload 'python-isort-on-save-mode "python-isort" nil t)
(reformatter-define python-isort
  :program python-isort-command
  :args python-isort-arguments)

(provide 'python-nrv)
;;; python-nrv.el ends here
