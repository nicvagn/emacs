;;; package-refresh.el --- Keep packages refreshed -*- lexical-binding: t; -*-

;;; Commentary:
;; Refresh packages automatically. To be called after package initialize.

;;; Code:

(defvar nrv/package-refresh-file (expand-file-name "package-refresh-time" user-emacs-directory)
  "File to store the last package refresh time.")
(defvar nrv/last-package-refresh-time nil
  "Time when packages were last refreshed.")
(defvar nrv/package-refresh-interval (* 90 60 60)
  "Interval for automatic package refresh. 90 hours default.")

(defun nrv/load-package-refresh-time ()
  "Load the last package refresh time from file."
  (when (file-exists-p nrv/package-refresh-file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents nrv/package-refresh-file)
          (let ((content (string-trim (buffer-string))))
            (if (string-empty-p content)
                (progn
                  (message "Package refresh file is empty")
                  (setq nrv/last-package-refresh-time nil))
              (setq nrv/last-package-refresh-time
                    (car (read-from-string content))))))
      (error
       (message "Error loading package refresh time: %s" err)
       (setq nrv/last-package-refresh-time nil)))))

(defun nrv/save-package-refresh-time ()
  "Save the current package refresh time to file."
  (condition-case err
      (with-temp-file nrv/package-refresh-file
        (prin1 nrv/last-package-refresh-time (current-buffer))
        (insert "\n"))  ; Add newline for cleaner file
    (error (message "Error saving package refresh time: %s" err))))

(defun nrv/should-refresh-packages-p ()
  "Return t if packages should be refreshed."
  (unless nrv/last-package-refresh-time
    (message "Loading package refresh time from file...")
    (nrv/load-package-refresh-time)
    (message "Loaded time: %s" nrv/last-package-refresh-time))
  (cond
   ((null nrv/last-package-refresh-time)
    (message "No previous refresh time found - should refresh")
    t)
   (t
    (let* ((current (current-time))
           (diff-seconds (float-time (time-subtract current nrv/last-package-refresh-time)))
           (should-refresh (> diff-seconds nrv/package-refresh-interval)))
      (message "Time since last refresh: %.1f hours (threshold: %.1f hours)"
               (/ diff-seconds 3600)
               (/ nrv/package-refresh-interval 3600))
      (message "Should refresh: %s" should-refresh)
      should-refresh))))

(defun nrv/refresh-packages-if-needed (&optional force)
  "Refresh packages only if more than 1 day has passed.
With prefix argument FORCE, refresh regardless of time."
  (interactive "P")
  (if (or force (nrv/should-refresh-packages-p))
      (progn
        (message "Refreshing packages%s..."
                 (if force " (forced)" ""))
        (package-refresh-contents)
        (setq nrv/last-package-refresh-time (current-time))
        (nrv/save-package-refresh-time)
        (message "Package refresh completed at %s"
                 (format-time-string "%Y-%m-%d %H:%M:%S")))
    (message "Packages were refreshed recently, skipping (last: %s)"
             (format-time-string "%Y-%m-%d %H:%M:%S" nrv/last-package-refresh-time))))



(provide 'package-refresh)
