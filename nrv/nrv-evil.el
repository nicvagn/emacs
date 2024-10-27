;; nrv dvorak evil leader custom keymap/plugins/emacs

(defun evil-customize ()
  "nrv's evil customizer"
  (interactive)
  (evil-mode 1) ;; turn on evil mode
  (evil-define-key 'normal evil-dvorak-mode-map
  ;; Miscellancus
  (kbd "t") #'evil-next-line
  (kbd "h") #'previous-line
  (kbd "d") #'backward-char
  (kbd "e") #'forward-char
  "k" 'kill-line
  "K" '(lambda () (interactive)
    "kill from point to the beginning of the line"
    (kill-line 0))

  ;;move the cursor around
  (kbd "C-l") 'recenter-top-bottom

  ;;line manipulation
  "J" 'join-line
  "j" #'(lambda () (interactive)
    "join this line at the end of the line below"
    (join-line 1))
  (kbd "C-h") 'evil-open-below
  (kbd "C-t") 'evil-open-above
  "'" 'evil-goto-mark
  ;;there is no need to set return to newline-and-indent, because electric-indent-mode is now on by default.
  ;;(kbd "<return>") 'newline-and-indent)

  (evil-define-key 'insert evil-dvorak-mode-map
  (kbd "C-z") 'evil-normal-state
  (kbd "ESC") 'evil-normal-state
  (kbd "C-d") 'delete-char
  (kbd "<backspace>") 'delete-backward-char
  (kbd "<return>") 'newline-and-indent
  (kbd "C-n") 'evil-next-line
  (kbd "C-p") 'evil-previous-line
  (kbd "C-b") 'backward-char
  (kbd "C-f") 'forward-char)
  ;; make c delete
  (define-key evil-normal-state-map (kbd "c") 'evil-delete)
  ;; set evil undo to one built into emacs 
  (evil-set-undo-system 'undo-redo)

  (evil-define-key 'visual evil-dvorak-mode-map
      "t" 'evil-next-line
      "h" 'evil-previous-line
      "d" 'evil-backward-char
      "e" 'evil-forward-char
      ;;I what to be able to use vaw (visual around word) and viw (visual inner word)
      ;; that's why in visual mode, u and a are not defined.
      ;; BUT it would be cool to say cie and mean change forward to word-end
      ;; and cio to mean change backward word-end
      ;;(evil-define-key 'visual "u" 'evil-end-of-line)
      ;;(evil-define-key 'visual "a" 'evil-first-non-blank
      ;;(evil-define-key 'visual "u" 'evil-end-of-line)
  )
)

;; evil bah-ha-ha
(use-package evil
  :ensure t
  :config
  (evil-customize)
)
;; load the evil-* stuff
(use-package evil-leader
  :ensure t
  :config 
    (global-evil-leader-mode) ;; activate leader mode
    ;; <leader>
    (evil-leader/set-leader "<SPC>") ;; set to space
    ;; define leader mappings
    (evil-leader/set-key 
    "w" 'save-buffer
    "s" 'evil-window-split
    "v" 'evil-window-vsplit
    "k" 'kill-buffer
    "q" 'evil-quit
    "x" 'delete-window
    "<SPC>" 'other-window
  )
)

;; _-_-_-_-_-_-_-_-_-_-_-_-_-KEYMAP_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
                                                                                                            
;; easy change buffer with f keys (and close other win)
(global-set-key (kbd "<f1>") 'previous-buffer)
(global-set-key (kbd "<f2>") 'next-buffer)
(global-set-key (kbd "<f3>") 'neotree-toggle)
(global-set-key (kbd "<f4>") 'delete-other-windows )

;; neotree stuff
(add-hook 'neotree-mode-hook
  (lambda ()
    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "l") 'neotree-quick-look)
    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
    (define-key evil-normal-state-local-map (kbd "<return>") 'neotree-enter)
    (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
    (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
    (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
    (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
    (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)
    (define-key evil-normal-state-local-map (kbd "a") 'neotree-create-node)
    (define-key evil-normal-state-local-map (kbd "r") 'neotree-delete-node)))

(provide 'nrv-evil)
