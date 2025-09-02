;;; evil-dvorak.el -- evil with appropriate dvorak bindings and nrv custom  -*- lexical-binding: t; -*-
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


(use-package undo-tree
  :ensure t
  :demand t
  :diminish undo-tree-mode
  :bind
  ("C-c t" . #'undo-tree-visualize)
  :config
  ;; Make Evil use undo-tree
  (define-key evil-normal-state-map (kbd "u") 'undo-tree-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-tree-redo)
  ;; undo-tree is not persistent. this is good.
  (setq undo-tree-history-directory-alist '(("." . "/tmp/undo-tree/")))
  (global-undo-tree-mode 1))

(evil-define-key 'insert evil-dvorak-mode-map
  ;; Emacs-style movement
  (kbd "C-f") 'forward-char
  (kbd "C-b") 'backward-char
  (kbd "C-n") 'next-line
  (kbd "C-p") 'previous-line
  (kbd "C-a") 'beginning-of-line
  (kbd "C-e") 'end-of-line
  (kbd "<backtab>") #'evil-shift-left-line)

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
  ;; c is new d
  (kbd "c") #'evil-delete
  ;; Movement
  (kbd "t") #'evil-next-line
  (kbd "h") #'evil-previous-line
  (kbd "d") #'evil-backward-char
  (kbd "e") #'evil-forward-char
  ;; Line operations
  (kbd "k") #'kill-line
  (kbd "K") #'(lambda ()
                "Kill from point to the beginning of the line"
                (interactive)
                (kill-line 0))
  ;; Cursor movement
  (kbd "C-l") #'recenter-top-bottom
  ;; Line manipulation
  (kbd "J") #'join-line ;; Join this line with the one above
  (kbd "j") #'(lambda ()
                "Join this line at the end of the line below"
                (interactive)
                (join-line 1))
  (kbd "<return>") #'nrv/normal-newline
  (kbd "C-<return>") #'newline-and-indent
  (kbd "<tab>") #'evil-shift-right-line
  (kbd "<backtab>") #'evil-shift-left-line
  (kbd "'") #'evil-goto-mark)


;; Evil Leader, provides leader key shortcuts
(use-package evil-leader
  :ensure t
  :demand t
  :config
  ;; <leader>
  (evil-leader/set-leader "<SPC>") ;; set to space
  ;; define leader mappings
  (evil-leader/set-key
    "w" 'save-buffer
    "k" 'kill-current-buffer
    "q" 'evil-quit
    "x" 'delete-window
    "0" 'delete-window
    "1" 'delete-other-windows
    "s" 'evil-window-split
    "v" 'evil-window-vsplit
    "h" 'evil-open-below
    "t" 'evil-open-above
    "f" 'format-all-region-or-buffer
    "<SPC>" 'evil-window-next)
  )

;; invoke stuff
(global-evil-leader-mode 1)
;; after evil leader has been loaded, turn on evil.
;; So leader is availible in all buffers
(global-evil-dvorak-mode 1)
(evil-mode 1)
;; set evil undo to one built into emacs
(evil-set-undo-system 'undo-redo)
(provide 'evil-dvorak-nrv)

;;; evil-dvorak-nrv.el ends here
