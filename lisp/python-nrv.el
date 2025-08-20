;;; python-nrv.el --- Enhanced Python development configuration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Nicolas Vaagen

;; Author: Nicolas Vaagen <nicolasvaagen@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (pyvenv "1.21") (python-black "1.0.0") (reformatter "0.6"))
;; Keywords: python, development, formatting
;; URL: https://github.com/your-username/python-nrv

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

;; This package provides an enhanced Python development environment for Emacs,
;; including virtual environment management, code formatting with Black and isort,
;; and improved Python mode configuration.

;; ## Features:
;; - Automatic virtual environment detection and activation
;; - Code formatting with Black and isort
;; - Enhanced Python mode with sensible defaults
;; - Support for both python-mode and python-ts-mode
;; - Project-based virtual environment management

;; ## Usage:
;; 1. Manual virtual environment activation:
;;    M-x pyvenv-activate RET /path/to/venv
;;
;; 2. Format code:
;;    C-c f (format buffer with Black)
;;    C-c i (sort imports with isort)
;;
;; 3. Auto-formatting is enabled by default on save

;;; Code:

(require 'python)
(require 'reformatter)

;;; Customization Groups

(defgroup python-nrv nil
  "Enhanced Python development environment."
  :group 'python
  :prefix "python-nrv-")

(defcustom python-nrv-use-ts-mode t
  "Whether to use python-ts-mode instead of python-mode."
  :type 'boolean
  :group 'python-nrv)

(defcustom python-nrv-line-length 88
  "Maximum line length for Python code (Black default)."
  :type 'integer
  :group 'python-nrv)

(defcustom python-nrv-auto-activate-venv t
  "Whether to automatically activate virtual environments."
  :type 'boolean
  :group 'python-nrv)

;;; Python Mode Configuration

(use-package python
  :config
  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil
        python-indent-guess-indent-offset-verbose nil

        ;; Shell configuration
        python-shell-interpreter "python3"
        python-shell-interpreter-args "-i"
        python-shell-prompt-detect-failure-warning nil
        python-shell-completion-native-enable nil  ; Avoid completion issues

        ;; PEP compliance
        python-fill-docstring-style 'pep-257
        python-check-command "flake8")

  :hook
  ((python-mode python-ts-mode) . python-nrv-setup-buffer))

;;; Tree-sitter Mode Setup

(when python-nrv-use-ts-mode
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.pyi\\'" . python-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.pyw\\'" . python-ts-mode))

  ;; Ensure tree-sitter grammar is available
  (when (and (fboundp 'treesit-language-available-p)
             (not (treesit-language-available-p 'python)))
    (message "Python tree-sitter grammar not found. Install with: M-x treesit-install-language-grammar RET python")))

;;; Buffer Setup Function

(defun python-nrv-setup-buffer ()
  "Set up Python buffer with NRV configuration."
  (set-fill-column python-nrv-line-length)
  (display-line-numbers-mode 1)
  (when python-nrv-auto-activate-venv
    (python-nrv-maybe-activate-venv)))

;;; Virtual Environment Management

(use-package pyvenv
  :ensure t
  :init
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))

  :config
  (pyvenv-mode 1)

  ;; Hooks for interpreter management
  (add-hook 'pyvenv-post-activate-hooks #'python-nrv-update-interpreter)
  (add-hook 'pyvenv-post-deactivate-hooks #'python-nrv-reset-interpreter))

(defun python-nrv-update-interpreter ()
  "Update Python interpreter to use virtual environment."
  (when pyvenv-virtual-env
    (let ((venv-python (expand-file-name "bin/python" pyvenv-virtual-env)))
      (when (file-executable-p venv-python)
        (setq python-shell-interpreter venv-python)
        (message "Updated Python interpreter to: %s" venv-python)))))

(defun python-nrv-reset-interpreter ()
  "Reset Python interpreter to system default."
  (setq python-shell-interpreter "python3")
  (message "Reset Python interpreter to system default"))

(defun python-nrv-maybe-activate-venv ()
  "Try to automatically activate virtual environment for current project."
  (when (and (not pyvenv-virtual-env)
             (buffer-file-name))
    (let ((venv-dir (python-nrv-find-venv-directory)))
      (when venv-dir
        (pyvenv-activate venv-dir)
        (message "Auto-activated virtual environment: %s" venv-dir)))))

(defun python-nrv-find-venv-directory ()
  "Find virtual environment directory for current project."
  (when-let ((project-root (python-nrv-project-root)))
    (cl-some (lambda (venv-name)
               (let ((venv-path (expand-file-name venv-name project-root)))
                 (when (and (file-directory-p venv-path)
                            (file-executable-p (expand-file-name "bin/python" venv-path)))
                   venv-path)))
             '("venv" ".venv" "env" ".env"))))

(defun python-nrv-project-root ()
  "Find project root directory."
  (when (buffer-file-name)
    (locate-dominating-file (buffer-file-name)
                            (lambda (dir)
                              (or (file-exists-p (expand-file-name "pyproject.toml" dir))
                                  (file-exists-p (expand-file-name "setup.py" dir))
                                  (file-exists-p (expand-file-name "requirements.txt" dir))
                                  (file-exists-p (expand-file-name ".git" dir)))))))

;;; Code Formatting with Black

(use-package python-black
  :ensure t
  :diminish python-black-on-save-mode
  :bind (:map python-mode-map
              ("C-c f" . python-black-buffer)
              :map python-ts-mode-map
              ("C-c f" . python-black-buffer))
  :hook ((python-mode python-ts-mode) . python-black-on-save-mode)
  :config
  (setq python-black-extra-args (list "--line-length" (number-to-string python-nrv-line-length))))

;;; Import Sorting with isort

(defgroup python-nrv-isort nil
  "Python import sorting with isort."
  :group 'python-nrv
  :prefix "python-nrv-isort-")

(defcustom python-nrv-isort-command "isort"
  "The name or path to the isort command."
  :type 'string
  :group 'python-nrv-isort)

(defcustom python-nrv-isort-arguments
  (list "--stdout" "--atomic" "--profile=black"
        (concat "--line-length=" (number-to-string python-nrv-line-length)) "-")
  "Arguments to pass to isort command."
  :type '(repeat string)
  :group 'python-nrv-isort)

;;;###autoload
(reformatter-define python-nrv-isort
  :program python-nrv-isort-command
  :args python-nrv-isort-arguments
  :lighter " isort")

;; Key bindings for isort
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c i") #'python-nrv-isort-buffer))

(with-eval-after-load 'python-ts-mode
  (define-key python-ts-mode-map (kbd "C-c i") #'python-nrv-isort-buffer))

;;; Utility Functions

;;;###autoload
(defun python-nrv-format-buffer ()
  "Format current Python buffer with both isort and black."
  (interactive)
  (when (derived-mode-p 'python-mode 'python-ts-mode)
    (python-nrv-isort-buffer)
    (python-black-buffer)
    (message "Formatted buffer with isort and black")))

;;;###autoload
(defun python-nrv-toggle-venv ()
  "Toggle between activating and deactivating virtual environment."
  (interactive)
  (if pyvenv-virtual-env
      (pyvenv-deactivate)
    (if-let ((venv-dir (python-nrv-find-venv-directory)))
        (pyvenv-activate venv-dir)
      (call-interactively #'pyvenv-activate))))

;;;###autoload
(defun python-nrv-info ()
  "Display information about current Python environment."
  (interactive)
  (message "Python: %s | Venv: %s | Project: %s"
           python-shell-interpreter
           (or pyvenv-virtual-env "None")
           (or (python-nrv-project-root) "None")))

;;; Key Bindings

(defvar python-nrv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-f") #'python-nrv-format-buffer)
    (define-key map (kbd "C-v") #'python-nrv-toggle-venv)
    (define-key map (kbd "C-i") #'python-nrv-info)
    map)
  "Keymap for python-nrv commands.")

;; Add to both python-mode and python-ts-mode
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c p") python-nrv-mode-map))

(when (boundp 'python-ts-mode-map)
  (define-key python-ts-mode-map (kbd "C-c p") python-nrv-mode-map))

;;; Minor Mode

;;;###autoload
(define-minor-mode python-nrv-mode
  "Enhanced Python development mode."
  :lighter " NRV.py"
  :keymap python-nrv-mode-map
  (if python-nrv-mode
      (message "Python NRV mode enabled")
    (message "Python NRV mode disabled")))

;; set hooks
(add-hook 'python-mode 'python-nrv-mode)
(provide 'python-nrv)
