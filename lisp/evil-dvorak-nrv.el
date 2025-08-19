;;; evil-dvorak.el -- evil with appropriate dvorak bindings and nrv custom
;; l-dvorak version by nrv
;; Copyright (C) 2015 Joshua Branson
;; Author: Joshua Branson
;; Package-Requires: ((evil  "1.0.8"))
;; Created: January 30 2015
;; Keywords:  dvorak evil vim
;; Version: 0.3

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
;; Put this in your emacs initialization file (~.emacs.d/init.el)
;; This turns on evil-dvorak for every file.
;; (require 'evil-dvorak)
;; (global-evil-dvorak-mode 1)

;;; Code:

(use-package evil
  :ensure t)

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind
  ("C-c t" . #'undo-tree-visualize)
  :config
  ;; undo-tree is not persistent. this is good.
  (setq undo-tree-history-directory-alist '(("." . "/tmp/undo-tree/")))
  (global-undo-tree-mode 1))


(define-minor-mode evil-dvorak-mode
  "evil dvorak mode allows you to use evil using the dvorak keyboard layout.  contributions are welcome."
  :keymap (make-sparse-keymap))

(defun turn-on-evil-dvorak-mode ()
  "enable evil-dvorak-mode in the current buffer."
  (evil-dvorak-mode 1))

(defun turn-off-evil-dvorak-mode ()
  "disable evil-dvorak-mode in this buffer."
  (evil-dvorak-mode -1))

(define-globalized-minor-mode global-evil-dvorak-mode
  evil-dvorak-mode turn-on-evil-dvorak-mode
  "global mode to let you use evil with dvorak friendly keybindings.")


;; the djoyner/** keep visual selection when indenting
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
  ;; miscellancus
  (kbd "t") #'evil-next-line
  (kbd "h") #'previous-line
  (kbd "d") #'backward-char
  (kbd "e") #'forward-char
  (kbd "k") #'kill-line
  (kbd "K") #'(lambda () (interactive)
                "kill from point to the beginning of the line"
                (kill-line 0))
  ;; move the cursor around
  (kbd "c-l") 'recenter-top-bottom
  ;; line manipulation
  (kbd "j") 'join-line
  (kbd "J") #'(lambda () (interactive)
                "join this line at the end of the line below"
                (join-line 1))
  (kbd "<return>") #'nrv/normal-newline
  (kbd "c-<return>") #'newline-and-indent
  (kbd "<tab>") #'evil-shift-right-line
  (kbd "<backtab>") #'evil-shift-left-line
  (kbd "c-n") #'evil-next-line
  (kbd "c-p") #'evil-previous-line
  (kbd "c-b") #'evil-backward-char
  (kbd "c-f") #'evil-forward-char
  (kbd "m-b") #'evil-backward-word
  (kbd "m-f") #'evil-forward-word
  (kbd "'") #'evil-goto-mark)

(evil-define-key 'emacs evil-dvorak-mode-map
  (kbd "C-c w") #'evil-window-next)

;; evil leader, provides leader key shortcuts
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

;; Configure
;; so leader is availible in all buffers
(global-evil-leader-mode 1)
;; after evil leader has been loaded, turn on evil.
(global-evil-dvorak-mode 1)
(evil-mode 1)

(provide 'evil-dvorak-nrv)

;;; evil-dvorak-nrv.el ends here

                                        ; LocalWords:  SPC
