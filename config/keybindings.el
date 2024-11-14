;; keybindings for all modes
;; super-w switch between windows
;; super-x close active window
(global-set-key (kbd "s-w") #'ace-window) 
(global-set-key (kbd "s-x") #'delete-window)

;; change window's with F(1,2)
(global-set-key (kbd "<f2>") #'next-multiframe-window)
(global-set-key (kbd "<f1>") #'previous-multiframe-window)
