(defun custom-mark:make-ring (ring-file &optional size)
  (if (file-exists-p ring-file)
      (with-temp-buffer
        (insert-file-contents ring-file)
        (read (current-buffer)))
    (make-ring (or size 30))))

(defun custom-mark:save-file (ring save-file)
  (if save-file
      (with-temp-message ""
        (write-region (prin1-to-string ring) nil save-file))))

(defun custom-mark:add (ring &optional save-file list)
  "add current buffer-file-name and point and optional list to ring if buffer has a file-name"
  (let ((bfn (or (buffer-file-name) default-directory)))
    (when bfn 
      (ring-insert ring (append (list bfn (point)) (or list '())))
      (custom-mark:save-file ring save-file))))

(defun custom-mark:current-buffer (ring &optional ring-entry)
  (unless (ring-empty-p ring)
    (let ((fn (car (or ring-entry (ring-ref ring 0)))))
      (or (get-file-buffer fn) (and (file-exists-p fn) (find-file-noselect fn))))))


(defun custom-mark:toggle (ring &optional change-hook)
  (when (> (ring-length ring) 1)
    (ring-insert ring (ring-remove ring 1))
    (custom-mark:nth ring 0 change-hook)))

(defun custom-mark:nth (ring nth &optional change-hook)
  "get nth element off ring"
  (unless (ring-empty-p ring)
    (if (< nth 0)
        (let ((c (- nth)))
          (while (> c 0)
            (setq c (1- c))
            (ring-insert ring (ring-remove ring))))
      (let ((c nth))
        (while (> c 0)
          (setq c (1- c))
          (ring-insert-at-beginning ring (ring-remove ring 0)))))
    (let ((ring-entry (ring-ref ring 0)))
      (when ring-entry
        (let ((buf (custom-mark:current-buffer ring ring-entry))
              (mark (cadr ring-entry)))
          (if buf
              (progn
                (if (eq buf (current-buffer))
                    (progn (push-mark) (goto-char mark))
                  (switch-to-buffer buf))
                (when change-hook (funcall change-hook ring-entry))
                ring-entry)
            (custom-mark:delete-current ring)
            ))))))

(defun custom-mark:delete-current (ring)
  (unless (ring-empty-p ring)
    (ring-remove ring 0))
  nil)

(defvar mark-menu-mode-map (make-sparse-keymap)
  "Keymap for custom-mark mode.")



(setq mark-menu-mode-map (make-sparse-keymap))

(define-key mark-menu-mode-map [mouse-2] 'custom-mark:follow-mouse)
(define-key mark-menu-mode-map [return] 'custom-mark:follow)
(define-key mark-menu-mode-map [delete] 'custom-mark:delete)
(define-key mark-menu-mode-map "v" 'custom-mark:view)
(define-key mark-menu-mode-map "f" 'custom-mark:follow)
(define-key mark-menu-mode-map [M-up] 'custom-mark:move-up)
(define-key mark-menu-mode-map [M-down] 'custom-mark:move-down)
(define-key mark-menu-mode-map [follow-link] 'mouse-face)

(defun mark-menu-mode ()
  "Major mode for viewing list of marks.
Commands:
\\{mark-menu-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mark-menu-mode-map)
  (setq mode-name "Mark Menu")
  (setq major-mode 'mark-menu-mode)
  (view-mode)
  (set (make-local-variable 'view-no-disable-on-exit) t)
  (setq view-exit-action
	(lambda (buffer)
	  ;; Use `with-current-buffer' to make sure that `bury-buffer'
	  ;; aluso removes BUFFER from the selected window.
	  (with-current-buffer buffer
	    (bury-buffer))))
  (setq truncate-lines t)
  (setq buffer-read-only t))

(defun custom-mark:sym-value (suffix)
  (symbol-value (intern (concat (symbol-name custom-mark:prefix) suffix))))


(defun custom-mark:follow-mouse (event)
  "put selected line at top of ring and switch to it."
  (interactive "e")
  (with-current-buffer (window-buffer (posn-window (event-end event)))
    (save-excursion
      (goto-char (posn-point (event-end event)))
      (custom-mark:follow))))

(defun custom-mark:move-down (count)
  "move current mark down count lines"
  (interactive "p")
  (custom-mark:move-up (- count)))

(defun custom-mark:move-up (count)
  "move current mark up count lines"
  (interactive "p")
  (let* ((ring (custom-mark:sym-value ":ring"))
         (hd (car ring))
         (ln (cadr ring))
         (vec (cddr ring))
         (veclen (length vec))
         (adder (if (< count 0) 1 -1))
         (index (1- (line-number-at-pos)))
         (nindex (- (line-number-at-pos) 1 count))
         (npos (- nindex (ring-length ring)))
         voi vni
         oldelt)
    (setq voi (ring-index index hd ln veclen))
    (setq oldelt (aref vec voi))
    (while (/= index nindex)
      (setq index (+ index adder))
      (setq vni (ring-index index hd ln veclen))
      (aset vec voi (aref vec vni))
      (setq voi vni))
    (aset vec vni oldelt)
    (custom-mark:save-file ring (custom-mark:sym-value ":ring-file"))
    (custom-mark:draw-menu ring)
    (cond 
     ((< nindex 0)
      (goto-char (point-max)) (forward-line (1+ (* -1 nindex))) (beginning-of-line)
      (custom-mark:current-changed ring))
     ((>= npos 0)
      (goto-char (point-min)) (forward-line npos)
      (custom-mark:current-changed ring))
     (t
      (goto-char (point-min)) (forward-line nindex)
      (if (= 0 nindex) (custom-mark:current-changed ring))))))

(defun custom-mark:current-changed (ring)
  (let ((func (intern (concat (symbol-name custom-mark:prefix) ":current-changed"))))
    (when (functionp func) (funcall func (if (ring-empty-p ring) nil (ring-ref ring 0))))))


(defun custom-mark:view ()
  "view buffer from current line in other window"
  (interactive)
  (let* ((ring (custom-mark:sym-value ":ring"))
         (ref (ring-ref ring (- (line-number-at-pos) 1)))
         (view-buf (custom-mark:current-buffer ring ref))
         (current (current-buffer)))
    (pop-to-buffer view-buf t t)
    (goto-char (cadr ref))
    (pop-to-buffer current t t)))

(defun custom-mark:follow ()
  "put current line at top of ring and switch to it."
  (interactive)
  (let* ((ring (custom-mark:sym-value ":ring"))
         (ref (ring-remove ring (- (line-number-at-pos) 1))))
    (ring-insert ring ref)
    (custom-mark:save-file ring (custom-mark:sym-value ":ring-file"))
    (funcall custom-mark:show-cmd)))

(defun custom-mark:delete ()
  "remove the current line"
  (interactive)
  (let* ((pos (point))
         (ring (custom-mark:sym-value ":ring"))
         (index (1- (line-number-at-pos)))
         (ref (ring-remove ring index)))
    (custom-mark:save-file ring (custom-mark:sym-value ":ring-file"))
    (custom-mark:draw-menu ring)
    (goto-char pos)
    (message (concat "removed " (prin1-to-string ref)))
    (if (= index 0)
        (custom-mark:current-changed ring))))


(defun custom-mark:draw-menu (ring)
  (save-excursion
    (setq buffer-read-only nil)
    (erase-buffer)
    (let (newline-p
          (func (intern (concat (symbol-name custom-mark:prefix) ":extra-args"))))
      (dolist (elt (ring-elements ring))
        (if newline-p (insert "\n") (setq newline-p t))
        (let* ((filename (car elt))
               (buf (get-file-buffer filename))
               (basename (file-name-nondirectory filename))
               (bfn (or (and buf (buffer-name buf)) (if (string= "" basename) (abbreviate-file-name filename) (concat basename " (closed)"))))
               (ln (number-to-string (cadr elt))))
          (add-text-properties 0 (length bfn)
                               `(font-lock-face buffer-menu-buffer
                                                mouse-face highlight
                                                help-echo ,filename) bfn)
          (insert bfn)
          (insert " ")
          (add-text-properties 0 (length ln)
                               `(font-lock-face compilation-line-number mouse-face highlight) ln)
          (insert ln)
          (insert ":")
          (if (caddr elt)
              (insert "  " (if (functionp func) 
                               (funcall func (cddr elt))
                             (prin1-to-string (cddr elt))))))))
    (setq buffer-read-only t)))

(defun custom-mark:list-marks (ring prefix show-cmd)
  (let ((marks-buf (get-buffer-create "*Mark List*")))
    (with-current-buffer marks-buf
      (mark-menu-mode)
      (set (make-local-variable 'custom-mark:prefix) prefix)
      (set (make-local-variable 'custom-mark:show-cmd) show-cmd)
      (custom-mark:draw-menu ring))
      
    (unless (eq (current-buffer) marks-buf)
      (or (get-buffer-window marks-buf) (view-buffer-other-window marks-buf)))))

(defvar work-mark:ring-file "~/.emacs.d/rings/work-mark.ring")

(defun work-mark:load-file (&optional file)
  "Load the work-mark:ring file"
  (interactive)
  (defvar work-mark:ring (custom-mark:make-ring (or file work-mark:ring-file))))

(work-mark:load-file)

(defun work-mark:add (count)
  "Add buffer and point to work-mark:ring"
  (interactive "P")
  (when (not (ring-empty-p work-mark:ring))
    (let ((fn (or (buffer-file-name) default-directory))
          (len (ring-length work-mark:ring)))
      (if count
          (while (/= len 0)
            (setq len (1- len))
            (when (string=  (car (ring-ref work-mark:ring len)) fn)
              (ring-remove work-mark:ring len)))
        (if (and (string=  (car (ring-ref work-mark:ring 0)) fn)
                 (= (cadr (ring-ref work-mark:ring 0)) (point)))
            (ring-remove work-mark:ring 0)))))
  (custom-mark:add work-mark:ring work-mark:ring-file))


(defun work-mark:next (arg)
  "Return to next mark (opposite to work-mark:previous).  With arg return to nth next mark"
  (interactive "p")
    (custom-mark:nth work-mark:ring (- arg)))

(defun work-mark:previous (arg)
  "Return to previous mark.  With arg return to nth previous mark"
  (interactive "p")
  (custom-mark:nth work-mark:ring arg))

(defun work-mark:toggle ()
  "swap top two buffers in ring"
  (interactive)
  (custom-mark:toggle work-mark:ring))

(defun work-mark:current ()
  "Return to current mark"
  (interactive)
  (custom-mark:nth work-mark:ring 0))


(defun work-mark:delete-current ()
  "Delete current mark"
  (interactive)
  (custom-mark:delete-current work-mark:ring)
  (switch-to-buffer (work-mark:current)))

(defun work-mark:list-marks ()
  "List all marks in a buffer"
  (interactive)
  (custom-mark:list-marks work-mark:ring 'work-mark 'work-mark:current))

(provide 'custom-mark)
