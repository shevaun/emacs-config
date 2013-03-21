(defvar ruby-test:gem-history nil)

(defun ruby-test:test-buffer-name (mode)
  "return name of test buffer"
  "*test-run*")

(defun ruby-test:project-file (name)
  (let* ((proj-dir )
         (look-for-file (lambda (name top-dir wildcards)
                          (and wildcards
                               (or 
                                (car (file-expand-wildcards (concat top-dir (car wildcards) name)))
                                (funcall look-for-file name top-dir (cdr wildcards))))))
         (filename (funcall look-for-file name (search-up-for-project-home) '("" "*/" "*/*/" "/*/*/*/" "*/*/*/*/"))))
    (and filename (find-file-noselect filename))))

(define-derived-mode ruby-test:compilation-mode compilation-mode "RubyTest"
  "Major mode for Ruby tests."
  ; replace compilation font-lock-keywords
  (set (make-local-variable 'compilation-mode-font-lock-keywords) rails-test:font-lock-keywords)
  ; skip anythins less that error
  (set (make-local-variable 'compilation-skip-threshold) 2)
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       '((ruby-test "^[^(]+([^]]+) [[]\\(\\([^:]+\\):\\([0-9]+\\)\\)[]]:$" 2 3 nil 2 1)
         (gem-trace  "^[[:blank:]]*\\[?\\(\\([^/[:blank:]][^:(\n]+ ([^)]+) [^:\n]+\\):\\([0-9]+\\)\\):" rails-test:gem-find 3 nil nil 1 (2 compilation-warning-face append))
         (ruby-error  "^[[:blank:]]*\\([[]?\\([^/[:blank:]][^(:[:blank:]\n]+\\):\\([0-9]+\\)\\):in `[^']+'\\]?:?$" 2 3 nil 2 1)
         (ruby-trace  "^[[:blank:]]*\\(\\(/[^[:blank:]:]+\\):\\([0-9]+\\)\\):in `[^']+':?$" 2 3 nil 1 1)
         (ruby-error2  "^[[:blank:]]*\\(?:from \\)\\(\\([^/[:blank:]][^(:\n]+\\):\\([0-9]+\\)\\):in `[^']+'\\]?:?$" 2 3 nil 2 1)
         (ruby-trace2  "^[[:blank:]]*\\(?:from \\)\\(\\(/[^:]+\\):\\([0-9]+\\)\\):in `[^']+':?$" 2 3 nil 1 1)))
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(gem-trace ruby-error ruby-trace ruby-error2 ruby-trace2 ruby-test)))

(defun ruby-test:run-one ()
  "Run the current test in the current buffer or its associated test buffer"
  (interactive)
  (ruby-test:run t))

(defun ruby-test:run (&optional test-current-method)
  "Run the tests in the current buffer or its associated test buffer"
  (interactive "P")
  (if buffer-file-truename (save-buffer))
  (if rails-minor-mode
      (case major-mode
        ('rails-cucumber-mode
         (if test-current-method (rails-cucumber:run-scenario-at-pos) (rails-cucumber:run-all-scenarios-in-buffer))
         (ruby-test:add 'cuke rails-cucumber:last-run))
        ('ruby-mode
         (if test-current-method (rails-test:run-current-method) (rails-test:run-current))
         (ruby-test:add 'rails rails-test:previous-run-single-param)))
    (save-some-buffers)
    (let ((current-method (when (and test-current-method 
                                     (save-excursion
                                       (move-end-of-line 1)
                                       (search-backward-regexp "^[ ]*def \\([a-z0-9_]+\\)" nil t)))
                            (match-string-no-properties 1))))
      (when (and (not (string-match "test_.*\\.rb" (buffer-name))) (not (string-match "_test.rb" (buffer-name))))
        (if current-method (setq current-method (concat "test_" current-method)))
        (let* ((testname (concat "test_" (buffer-name)))
               (bufname (or (ruby-test:project-file testname) (get-buffer testname))))
          (set-buffer (or bufname
                          (find-file-noselect (car (find-file-read-args
                                                      (concat "Please find file matching " 
                                                              (buffer-name) ": ") t)))))))
      (let* ((dir default-directory)
             (file (buffer-name))
             (command (if (string-match "\.rb" (buffer-name))
                          (concat "cd " dir ";ruby -I.:.. " (file-name-nondirectory 
                                                             (buffer-file-name (current-buffer)))
                                  ;; " --runner=emacs --no-use-color"
                                  (and current-method (concat " -n " current-method)))
                        (car ruby-test:gem-history)))
             (test-command (read-from-minibuffer "Test command: "
                                                 command nil nil
                                                 (if (equal (car ruby-test:gem-history) command)
                                                     '(ruby-test:gem-history . 1)
                                                   'ruby-test:gem-history))))
        (compilation-start test-command 'ruby-test:compilation-mode 'ruby-test:test-buffer-name)
        (ruby-test:add 'gem test-command)))))

(defun ruby-test:list-marks ()
  "List all marked tests in a buffer"
  (interactive)
  (custom-mark:list-marks ruby-test:ring 'ruby-test 'ruby-test:find-last))

(defvar ruby-test:ring-file "~/.emacs.d/rings/ruby-test.ring")
(defun ruby-test:load-file (&optional file)
  "Load the ruby-test:ring file"
  (interactive)
  (defvar ruby-test:ring (custom-mark:make-ring (or file ruby-test:ring-file))))

(ruby-test:load-file)


(defun ruby-test:add (type cmd)
  (when (not (ring-empty-p ruby-test:ring))
    (let ((cmp (prin1-to-string cmd))
          (len (ring-length ruby-test:ring)))
      (while (/= len 0)
        (setq len (1- len))
        (when (string= (prin1-to-string (fourth (ring-ref ruby-test:ring len))) cmp)
          (ring-remove ruby-test:ring len)))))
  (custom-mark:add ruby-test:ring ruby-test:ring-file (list type cmd)))


(defun ruby-test:current-buffer (&optional ring-entry)
  (custom-mark:current-buffer ruby-test:ring ring-entry))

(defun ruby-test:next (arg)
  "Return to next test (opposite to ruby-test:previous).  With arg return to nth next test"
  (interactive "p")
  (ruby-test:previous (- arg)))


(defun ruby-test:previous (arg)
  "Return to previous test.  With arg return to nth previous test"
  (interactive "p")
  (custom-mark:nth ruby-test:ring arg 'ruby-test:current-changed))


(defun ruby-test:toggle ()
  "swap top two buffers in test ring"
  (interactive)
  (custom-mark:toggle ruby-test:ring 'ruby-test:current-changed))

      
(defun ruby-test:current-changed (ring-entry)
  (when ring-entry
    (case (third ring-entry)
      ('rails
       (setq rails-test:previous-run-single-param (fourth ring-entry)))
      ('cuke
       (setq rails-cucumber:last-run (fourth ring-entry)))
      (t (setq test-command (fourth ring-entry))))
    (message (ruby-test:extra-args (cddr ring-entry)))))

(defun ruby-test:extra-args (list)
  (let* ((args (case (car list)
                ('rails (or (cadr (cadr list)) ""))
                ('cuke (or (caddr (cadr list)) ""))
                ('gem (cadr list))
                (t "")))
         result)
    (when (or (string-match "-n \\([^' ]*\\) *$" args) (string-match "-n '\\([^']*\\)'" args))
      (setq result (match-string 1 args))
      (add-text-properties 0 (length result) `(font-lock-face font-lock-function-name-face help-echo ,args) result))
    (or result "")))

(defun ruby-test:delete-current ()
  "Delete current test"
  (interactive)
  (custom-mark:delete-current ruby-test:ring)
  (ruby-test:find-last))


(defun ruby-test:rerun ()
  "Rerun the last test"
  (interactive)
  (if (buffer-file-name) (save-buffer))
  (unless (ring-empty-p ruby-test:ring)
    (let* ((entry (ring-ref ruby-test:ring 0))
           (default-directory (file-name-directory (car entry))))
      (case (caddr entry)
        ('rails (rails-test:rerun-last))
        ('cuke (rails-cucumber:run-last))
        ('gem 
         (save-some-buffers)
         (if (and ruby-test:gem-history (car ruby-test:gem-history))
             (compilation-start (car ruby-test:gem-history) 'ruby-test:compilation-mode 'ruby-test:test-buffer-name)
           (ruby-test:run)))))))

(defun ruby-test:find-last ()
  "Find file of last test"
  (interactive)
  (let ((test-buf (ruby-test:current-buffer))
        (c-buf-name (buffer-name)))
    (when test-buf
      (switch-to-buffer test-buf)

      (if rails-minor-mode
          (case major-mode
            ('ruby-mode
             (when rails-test:previous-run-single-param
               (rails-test:find-last-run))
             (if (string= (buffer-name) c-buf-name)
                 (rails-lib:run-primary-switch))))
            
        (let* ((cmd (car ruby-test:gem-history))
               (testlist (and (string-match "^cd \\([^;]+\\);.* \\(test_.*\\.rb\\|[^ ]+_test\\.rb\\)" cmd) (list (match-string 1 cmd) (match-string 2 cmd))))
               (testfile (concat (car testlist) "/" (cadr testlist)))
               (testbuffer (find-file-noselect testfile)))
          (switch-to-buffer (if (not (string= (buffer-name testbuffer) c-buf-name))
                                testbuffer
                              (set-buffer testbuffer)
                              (ruby-test:project-file (substring (cadr testlist) 5)))))))))

(defvar rails-test:hiding-libraries)

(add-hook 'rails-test:compilation-mode-hook 
          (lambda ()
            (setq rails-test:hiding-libraries nil)
            (local-set-key (kbd "C-.") 'rails-test:toggle_libraries)))


(provide 'ruby-test)
