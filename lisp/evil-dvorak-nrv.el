;;; evil-dvorak.el -- evil with appropriate dvorak bindings and nrv custom
;; l-dvorak version by nrv
;; Copyright (C) 2015 Joshua Branson
;; Author: Joshua Branson
;; Package-Requires: ((evil  "1.0.8"))
;; Created: January 30 2015
;; Keywords:  dvorak evil vim
;; Version: 0.2

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

;; Evil dvorak mode allows the user to use the dvorak keyboard with evil-mode.

;; How it install
;; ===
;; Put this in your emacs initialation file (~.emacs.d/init.el)
;; This turns on evil-dvorak for every file.
;; (require 'evil-dvorak)
;; (global-evil-dvorak-mode 1)

;;; Code:
(require 'evil)
(define-minor-mode evil-dvorak-mode
  "Evil dvorak mode allows you to use evil using the dvorak keyboard layout.  Contributions are welcome."
  :keymap (make-sparse-keymap))

(defun turn-on-evil-dvorak-mode ()
  "Enable evil-dvorak-mode in the current buffer."
  (evil-dvorak-mode 1))

(defun turn-off-evil-dvorak-mode ()
  "Disable evil-dvorak-mode in this buffer."
  (evil-dvorak-mode -1))

(define-globalized-minor-mode global-evil-dvorak-mode
  evil-dvorak-mode turn-on-evil-dvorak-mode
  "Global mode to let you use evil with dvorak friendly keybindings.")

(use-package evil
  :ensure t
  :config
  ;; set evil undo to one built into emacs
  (evil-set-undo-system 'undo-redo)
  ;; after modes have been loaded, turn on evil
  (global-evil-dvorak-mode 1)
  (evil-mode t)
  )

;; The djoyner/** keep visual selection when indenting
(evil-define-key 'visual evil-dvorak-mode-map
  (kbd "<tab>") #'djoyner/evil-shift-right-visual
  (kbd "<backtab>") #'djoyner/evil-shift-left-visual
  (kbd ">") #'djoyner/evil-shift-right-visual
  (kbd "<") #'djoyner/evil-shift-left-visual
  (kbd "t") #'evil-next-line
  (kbd "h") #'evil-previous-line
  (kbd "d") #'evil-backward-char
  (kbd "e") #'evil-forward-char)

(evil-define-key 'normal evil-dvorak-mode-map
  ;; c and d switch
  (kbd "c") #'evil-delete
  ;; Miscellancus
  (kbd "t") #'evil-next-line
  (kbd "h") #'previous-line
  (kbd "d") #'backward-char
  (kbd "e") #'forward-char
  (kbd "k") #'kill-line
  (kbd "K") #'(lambda () (interactive)
                "kill from point to the beginning of the line"
                (kill-line 0))
  ;;move the cursor around
  (kbd "C-l") 'recenter-top-bottom
  ;;line manipulation
  (kbd "J") 'join-line
  (kbd "j") #'(lambda () (interactive)
                "join this line at the end of the line below"
                (join-line 1))
  (kbd "<return>") #'newline-and-indent
  (kbd "<tab>") #'evil-shift-right
  (kbd "<backtab>") #'evil-shift-left
  (kbd "C-n") #'evil-next-line
  (kbd "C-p") #'evil-previous-line
  (kbd "C-b") #'backward-char
  (kbd "C-f") #'forward-char
  (kbd "'") #'evil-goto-mark)

(evil-define-key 'insert evil-dvorak-mode-map
  (kbd "ESC") #'evil-normal-state
  (kbd "C-d") #'delete-char
  (kbd "<backtab>") #'evil-shift-left
  (kbd "C-<tab>") #'evil-shift-right
  (kbd "<backspace>") #'delete-backward-char
  (kbd "<return>") #'newline-and-indent
  (kbd "C-n") #'evil-next-line
  (kbd "C-p") #'evil-previous-line
  (kbd "C-b") #'backward-char
  (kbd "C-f") #'forward-char)

;; Evil Leader, provides leader key shortcuts
(use-package evil-leader
  :ensure t
  :config
  ;; <leader>
  (evil-leader/set-leader "<SPC>") ;; set to space
  ;; define leader mappings
  (evil-leader/set-key
    "w" 'save-buffer
    "k" 'kill-this-buffer
    "q" 'evil-quit
    "x" 'delete-window
    "0" 'delete-window
    "1" 'delete-other-windows
    "s" 'evil-window-split
    "v" 'evil-window-vsplit
    "h" 'evil-open-below
    "t" 'evil-open-above
    "<SPC>" 'evil-window-next)
  (global-evil-leader-mode))

;; set evil undo to one built into emacs
(evil-set-undo-system 'undo-redo)
;; after modes have been loaded, turn on evil
(global-evil-dvorak-mode 1)
(provide 'evil-dvorak-nrv)

;;; evil-dvorak-nrv.el ends here
