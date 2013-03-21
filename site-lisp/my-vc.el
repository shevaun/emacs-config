(require 'vc-bzr)
(require 'vc)
;; Hack to fix vc-buffer-sync
(defun vc-buffer-sync (&optional not-urgent)
  "Make sure the current buffer and its working file are in sync.
NOT-URGENT means it is ok to continue if the user says not to save."
  (when (and (buffer-modified-p) (buffer-file-name))
    (if (or vc-suppress-confirm
	    (y-or-n-p (format "Buffer %s modified; save it? " (buffer-name))))
	(save-buffer)
      (unless not-urgent
	(error "Aborted")))))
;; End of hack
 

(defun my-vc-bzr-viz ()
  "vizualize tree in bzr"
  (interactive "")
  (call-process "bzr" nil 0 nil "viz"))

(defun my-vc-bzr-gdiff ()
  "vizualize tree in bzr"
  (interactive "")
  (call-process "bzr" nil 0 nil "gdiff"))

(defun my-vc-bzr-push ()
  "vizualize tree in bzr"
  (interactive "")
  (my-bzr-async-cmd '("push" "--no-strict") "bzr-ormerge"))

(defun my-bzr-async-cmd (cmd &optional bzr-cmd)
  "Run bzr command asynchronously and view its output in the *vc* buffer"
  (let ((repo-dir default-directory)
        (vc-buf (get-buffer-create "*vc*")))
    (with-current-buffer vc-buf
      (setq buffer-read-only t)
      (setq default-directory repo-dir)
      (view-mode t)
      (rails-maybe-minor-mode)
      (setq view-exit-action 'quit-window)
      (setq view-no-disable-on-exit t))
    (apply 'start-process "bzr" vc-buf (or bzr-cmd "bzr") cmd)
    (or (get-buffer-window vc-buf) (view-buffer-other-window vc-buf))))


(defun my-vc-bzr-pull ()
  "vizualize tree in bzr"
  (interactive "")
  (my-bzr-async-cmd '("pull")))


(defun my-vc-bzr-merge ()
  "vizualize tree in bzr"
  (interactive "")
  (my-bzr-async-cmd '("merge")))



(defun my-vc-bzr-dir ()
  "bzr status dir"
  (interactive "")
  (vc-dir (vc-bzr-root default-directory)))

(defun my-vc-bzr-checkin ()
  "bzr checkin changes"
  (interactive "")
  (call-interactively 'vc-root-diff)
  (vc-checkin '() 'bzr))

(provide 'my-vc)