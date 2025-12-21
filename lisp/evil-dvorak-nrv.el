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


(setq
 evil-want-C-u-scroll t
 evil-scroll-count 10
 evil-want-fine-undo t)

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


(evil-define-key 'insert evil-dvorak-mode-map
  ;; Emacs-style movement
  (kbd "C-f") 'forward-char
  (kbd "C-b") 'backward-char
  (kbd "C-n") 'next-line
  (kbd "C-p") 'previous-line
  (kbd "C-a") 'beginning-of-line
  (kbd "C-e") 'end-of-line
  (kbd "<tab>") #'nrv/shift-line-right
  (kbd "<backtab>") #'nrv/shift-line-left)

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
  (kbd "c") #'evil-delete ;; c is new d
  ;; Movement
  (kbd "t") #'evil-next-line
  (kbd "h") #'evil-previous-line
  (kbd "d") #'evil-backward-char
  (kbd "e") #'evil-forward-char
  ;; Reinstate C-e of Emacs
  (kbd "C-e") 'end-of-line
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
  (kbd "<tab>") #'nrv/shift-line-right
  (kbd "<backtab>") #'nrv/shift-line-left
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

;; Neotree -- file pop MANAGER
(use-package neotree
  :config
  (add-hook 'server-after-make-frame-hook
            ;; need, when daemon is created (display-graphics-p) is false
            (lambda ()
              (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))
  ;; set neotree theme as frame may exist
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (evil-define-key 'emacs neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'emacs neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'emacs neotree-mode-map (kbd "e") 'neotree-enter)
  (evil-define-key 'emacs neotree-mode-map (kbd "D") 'neotree-delete-node)
  (evil-define-key 'emacs neotree-mode-map (kbd "l") 'neotree-quick-look)
  (evil-define-key 'emacs neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'emacs neotree-mode-map (kbd "r") 'neotree-refresh)
  (evil-define-key 'emacs neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'emacs neotree-mode-map (kbd "t") 'neotree-next-line)
  (evil-define-key 'emacs neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'emacs neotree-mode-map (kbd "h") 'neotree-previous-line)
  (evil-define-key 'emacs neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'emacs neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

;; invoke stuff
(evil-mode 1)
(evil-set-undo-system 'undo-redo)
(global-evil-leader-mode 1)
;; after evil leader has been loaded, turn on evil.
;; So leader is available in all buffers
(global-evil-dvorak-mode 1)

;; _-_- set evil Emacs state modes _-_-
(dolist (p '((inferior-python-mode . emacs)
             ;; set *shell modes to use evil Emacs state
             (shell-mode . emacs)
             (vterm-mode . emacs)
             (ansi-term-mode . emacs)
             (eshell-mode . emacs)
             (neotree-mode . emacs)
             ))
  (evil-set-initial-state (car p) (cdr p)))

(provide 'evil-dvorak-nrv)

;;; evil-dvorak-nrv.el ends here
