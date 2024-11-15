;; keybindings for all modes
;; super-w switch between windows
;; super-x close active window
;; Alt-supper-x eval current buffer
(global-set-key (kbd "s-w") #'ace-window) 
(global-set-key (kbd "s-x") #'delete-window)
(global-set-key (kbd "M-s-x") #'eval-buffer)
;; change window's with Alt-([,])
(global-set-key (kbd "M-]") #'next-multiframe-window)
(global-set-key (kbd "M-[") #'previous-multiframe-window)
;; change buffers with f(1,2)
(global-set-key (kbd "<f1>") #'previous-buffer)
(global-set-key (kbd "<f2>") #'next-buffer)

;; if in minibuffer tab cycles completions
