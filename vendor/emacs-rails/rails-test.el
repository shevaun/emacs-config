;;; rails-test.el --- tests integration with the compile library

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;; Keywords: ruby rails languages oop

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:


(defcustom rails-test:quiet 't
  "Do not show test progress in minibuffer."
  :group 'rails
  :type 'boolean
  :tag "Rails Quiet Tests")

(defcustom rails-test:gem-topdir nil
  "Location of gems. Nil means its value will be computed"
  :group 'rails
  :type 'string
  :tag "Rails Gems Tests")

(defvar rails-test:history nil)

(defconst rails-test:result-regexp
  "\\([0-9]+ tests, [0-9]+ assertions, \\([0-9]+\\) failures, \\([0-9]+\\) errors.*$\\)")

(defconst rails-test:progress-regexp
  "^[.EF]+$")

(defvar rails-test:font-lock-keywords
  '(("\\([0-9]+ tests, [0-9]+ assertions, [0-9]+ failures, [1-9][0-9]* errors.*$\\)"
     1 compilation-error-face)
    ("\\([0-9]+ tests, [0-9]+ assertions, [1-9][0-9]* failures, [0-9]+ errors.*$\\)"
     1 compilation-warning-face)
    ("\\([0-9]+ tests, [0-9]+ assertions, 0 failures, 0 errors.*$\\)"
     1 compilation-info-face)
    ("`\\([^']+\\)'"
     1 font-lock-function-name-face t)
    ("^\\([^[:space:]]+\\(Error\\|Exception\\)\\|Errno::[^:]+\\): \\(.*\\)$"
     1 font-lock-warning-face)
    ("^\s+\\([0-9]+)\s+\\(Error\\|Failure\\):\\)"
     1 compilation-line-face)
    ("^\\(test_[a-z0-9_]+\\)(\\([a-zA-Z0-9:]+\\)):?$"
     (1 font-lock-function-name-face)
     (2 font-lock-type-face))
    ("^[.EF]+$" . compilation-info-face)))


(defun rails-test:print-result ()
  "Determine if the output buffer needs to be shown"
  (with-current-buffer (get-buffer rails-script:buffer-name)
    (let ((msg (list))
          (failures 0)
          (errors 0))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward rails-test:result-regexp (point-max) t)
          (setq failures (+ failures (string-to-number (match-string 2))))
          (setq errors (+ errors (string-to-number (match-string 3))))
          (add-to-list 'msg (match-string-no-properties 1))))
      (unless (zerop (length msg))
        (message (strings-join " || " (reverse msg))))
      (when (and (or (not (zerop rails-script:output-mode-ret-value))
                     (not (zerop errors))
                     (not (zerop failures)))
                 (not (buffer-visible-p (current-buffer))))
        (rails-script:popup-buffer))
      (when (and (buffer-visible-p (current-buffer))
                 (eq (point) (point-max)))
        (set-window-start (get-buffer-window (current-buffer)) (point-min))))))

(defun rails-test:print-progress (start end len)
  (let (content)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^Started" end t)
	(line-move 1)
	(save-match-data
	  (let ((progress (string=~ rails-test:progress-regexp
				    (current-line-string) $m)))
	    (when progress
	      (setq content (concat content progress))
	      (setq rails-ui:num-errors 0
		    rails-ui:num-failures 0
		    rails-ui:num-ok 0)
	      (dolist (c (string-to-list content))
		(case c
		  (?\E (setq rails-ui:num-errors (+ 1 rails-ui:num-errors)))
		  (?\F (setq rails-ui:num-failures (+ 1 rails-ui:num-failures)))
		  (?\. (setq rails-ui:num-ok (+ 1 rails-ui:num-ok))))))))))
    (when (and content  
	       (not rails-test:quiet))
      (message "Progress of %s: %s" rails-script:running-script-name content))))

(define-derived-mode rails-test:compilation-mode compilation-mode "RTest"
  "Major mode for RoR tests."
  (rails-script:setup-output-buffer)
  ; replace compilation font-lock-keywords
  (set (make-local-variable 'compilation-mode-font-lock-keywords) rails-test:font-lock-keywords)
  ; skip anythins less that error
  (set (make-local-variable 'compilation-skip-threshold) 2)
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       '((gem-trace  "^[[:blank:]]*\\[?\\(\\([^/[:blank:]][^:(\n]+ ([^)]+) [^:\n]+\\):\\([0-9]+\\)\\):" rails-test:gem-find 3 nil nil 1 (2 compilation-warning-face append))
         (ruby-error  "^[[:blank:]]*\\([[]?\\([.a-zA-Z0-9_-][.a-zA-Z0-9_/-]+\\):\\([0-9]+\\)\\):in `[^']+'\\]?:?.*$" 2 3 nil 2 1)
         (ruby-trace  "^[[:blank:]]*\\(\\(/[.a-zA-Z0-9_/-]+\\):\\([0-9]+\\)\\):in `[^']+':?.*$" 2 3 nil 1 1)
         (ruby-trace2  "^[^[]*\\[\\(\\([.a-zA-Z0-9_/-]+\\):\\([0-9]+\\)\\)\\].*$" 2 3 nil 1 1)
         (ruby-error2  "^[[:blank:]]*\\(?:from \\|FIXME \\|\\[\\)\\(\\([.a-zA-Z0-9_-][.a-zA-Z0-9_/-]+\\):\\([0-9]+\\)\\):in `[^']+'\\]?:?.*$" 2 3 nil 2 1)
         (ruby-trace2  "^[[:blank:]]*\\(?:from \\|FIXME \\|\\[\\)\\(\\(/[.a-zA-Z0-9_/-]+\\):\\([0-9]+\\)\\):in `[^']+':?.*$" 2 3 nil 1 1)))
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(gem-trace ruby-error ruby-trace ruby-trace2 ruby-error2 ruby-trace2))
  (add-hook 'after-change-functions 'rails-test:print-progress nil t)
  (add-hook 'rails-script:run-after-stop-hook 'rails-test:hide-rails-project-root t t)
;;  (add-hook 'rails-script:run-after-stop-hook 'rails-test:scroll-of-buffer t t)
  (add-hook 'rails-script:run-after-stop-hook 'rails-test:print-result t t)
  (add-hook 'rails-script:show-buffer-hook 'rails-test:reset-point-and-height t t))

(defun rails-test:gem-find ()
  (let ((file (match-string-no-properties 2)))
    (save-match-data
      (when (string-match "\\([^ ]+\\) (\\([^)]+\\)) \\(.+\\)$" file)
        (let ((gem (match-string 1 file))
              (version (match-string 2 file))
              (path (match-string 3 file)))
          (when (and (not rails-test:gem-topdir)
                     (string-match "INSTALLATION DIRECTORY: \\(.+\\)$" 
                                   (setq output (shell-command-to-string "gem environment"))))
            (setq rails-test:gem-topdir (concat (match-string 1 output) "/gems")))
          (cons (concat gem "-" version "/" path) rails-test:gem-topdir))))))

;; (defun rails-test:scroll-of-buffer ()
;;   (with-current-buffer "ROutput"
;;     (buffer "ROutput")
;;     (goto-char (point-min))
;;     (scroll-down-nomark (count-lines (point-min) (point-max)))))

(defun rails-test:hide-rails-project-root ()
  "Show files that are relative to the project root as relative filenames
As the buffer is read-only this is merely a change in appearance"
  (rails-project:with-root (root)
    (save-excursion
      (goto-char (point-min))
      (let ((file-regex (concat (regexp-quote root) "[^:]+")))
        (while (re-search-forward file-regex nil t)
          (let* ((orig-filename (match-string 0))
                 (rel-filename (file-relative-name orig-filename root)))
            (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                         'display rel-filename)))))))

(defun rails-test:reset-point-and-height ()
  "Resets the point and resizes the window for the output buffer.
Used when it's determined that the output buffer needs to be shown."
  (let ((win (get-buffer-window (current-buffer))))
    (when (window-live-p win)
      (set-window-point win 0)
      (unless (buffer-visible-p (current-buffer))
        (compilation-set-window-height win)))))

(defun rails-test:list-of-tasks ()
  "Return a list contains test tasks."
  (append (list "all")
          (delete* nil
                   (mapcar
                    #'(lambda (task) (string=~ "^test\\:\\([^ ]+\\)" task $1))
                    (rails-rake:list-of-tasks))
                   :if 'null)))

(defun rails-test:run (task)
  "Run rake tests in RAILS_ROOT."
  (interactive (rails-completing-read "What test run"
                                      (rails-test:list-of-tasks)
                                      'rails-test:history t))
  (rails-ui:reset-error-count)
  (unless task
    (setq task "all")
    (add-to-list rails-test:history task))
  (let ((task-name
         (if (string= "all" task)
             "test"
           (concat "test:" task))))
    (rails-rake:task task-name 'rails-test:compilation-mode (concat "test " task))))

(defvar rails-test:previous-run-single-param nil
  "Hold params of previous run-single-file call.")

(defun rails-test:find-last-run ()
  "Find file containing last run test"
  (interactive)
  (find-file (concat (rails-project:root) (car rails-test:previous-run-single-param)))
  )

(defun rails-test:rerun-last ()
  "Rerun last test"
  (interactive)
  (let ((test-name (car rails-test:previous-run-single-param)))
    (if (string-match "\\([^/\\\\.]+\\)_test\.rb$" test-name)
        (setq test-name (concat "test " (match-string-no-properties 1 test-name))))
    (if (file-exists-p (rails-core:file "test/pre-load"))
        (rails-test:pre-load-run rails-test:previous-run-single-param test-name)
      (rails-script:run "ruby" (append (list (concat "-I" (rails-project:root) "test")) rails-test:previous-run-single-param) 'rails-test:compilation-mode test-name))))

(defvar rails-test:pre-load-process nil
  "The current pre-load process where emacs is interacting with")

(defvar rails-test:pre-load-test-inprogress nil
  "The current pre-load test")

(defun rails-test:kill-pre-load-process ()
  "Stop the running pre-load-process"
  (interactive)
  (setq rails-test:pre-load-test-inprogress nil)
  (and rails-test:pre-load-process (equal (process-status rails-test:pre-load-process) 'run)
       (kill-process rails-test:pre-load-process)))

(defvar rails-test:pre-load-process-root ""
  "the rails-root of the current running test process")
  

(defun rails-test:pre-load-test-inprogress-p ()
  (when (and rails-test:pre-load-process
             (or (not (equal (process-status rails-test:pre-load-process) 'run))
                 (not (string= (rails-project:root) rails-test:pre-load-process-root))))
    (setq rails-test:pre-load-process nil))
    (and rails-test:pre-load-process rails-test:pre-load-test-inprogress))

(defun rails-test:get-pre-load-process ()
  (unless (and rails-test:pre-load-process
               (equal (process-status rails-test:pre-load-process) 'run))
    (setq rails-test:pre-load-process-root (rails-project:root))
    (let* ((default-directory rails-test:pre-load-process-root)
           (proc (start-process "rails-test:pre-load-process"
                                nil
                                "ruby"
                                (concat "-I" (rails-project:root))
                                (concat "-I" (rails-project:root) "test")
                                (concat (file-name-directory (find-lisp-object-file-name 
                                                              'rails-test:get-pre-load-process
                                                              (symbol-function 'rails-test:get-pre-load-process)))
                                        "pre_load_test.rb"))))

      (set-process-filter (setq rails-test:pre-load-process proc) 'rails-test:pre-load-filter)
      (set-process-query-on-exit-flag rails-test:pre-load-process nil)))
  
  rails-test:pre-load-process)

(defun rails-test:pre-load-run (param test-name)
  (if (rails-test:pre-load-test-inprogress-p)
      (message "Only one instance rails-script allowed")
    (save-some-buffers)
    (rails-project:with-root
     (root)
     (with-current-buffer (get-buffer-create rails-script:buffer-name)
       (let ((buffer-read-only nil))
         (delete-region (point-min) (point-max))
         (cd root))
       (rails-test:compilation-mode)))

    (rails-script:toggle-output-window t)
    (message "Starting %s." (concat (car param) " " (cadr param)))
    (setq rails-test:pre-load-test-inprogress param)
    (process-send-string (rails-test:get-pre-load-process) (concat (car param) ":" (cadr param) "\n"))))

(defun rails-test:pre-load-filter (proc string)
  (with-current-buffer (get-buffer rails-script:buffer-name)
    (let ((buffer-read-only nil)
          (moving (= (point) (point-max))))
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (point-max))
        ;; decode ansi color sequences
        (insert (ansi-color-apply string))
        (goto-char (point-max))
        (forward-line -1)
        (when (looking-at "^Exit Status \\([0-9]+\\). Completed in [0-9.]+ seconds$")
          (setq rails-test:pre-load-test-inprogress nil)
          (forward-line -2)
          (set (make-local-variable 'rails-script:output-mode-ret-value) (string-to-number (match-string 1)))
          (run-hooks 'rails-script:run-after-stop-hook)))
      (if moving (goto-char (point-max))))))


(defun rails-test:run-single-file (file &optional param)
  "Run test for single file FILE."
  (when (not (or file param))
    "Refuse to run ruby without an argument: it would never return")
  (rails-ui:reset-error-count)
  (let ((param (if param
                   (list file param)
                 (list file))))
    (setq rails-test:previous-run-single-param param)
    (rails-test:rerun-last)))

(defun rails-test:run-current ()
  "Run a test for the current controller/model/mailer."
  (interactive)
  (let* ((model (rails-core:current-model))
         (controller (rails-core:current-controller))
         (func-test (rails-core:functional-test-file controller))
         (unit-test (rails-core:unit-test-file model))
         (mailer-test (rails-core:unit-test-file controller))
         (file-to-run 
          (cond
           ;; model
           ((and model unit-test) unit-test)
           ;; controller
           ((and controller (not (rails-core:mailer-p controller)) func-test)
            func-test)
           ;; mailer
           ((and controller (rails-core:mailer-p controller) unit-test)
            unit-test)
           ;; otherwise...
           (t (if (string-match "test.*\\.rb" (buffer-file-name))
                  (buffer-file-name)
                nil)))))
    (if file-to-run
        (rails-test:run-single-file file-to-run)
      (if (string= "rails-cucumber-mode" major-mode)
          (rails-cucumber:run-all-scenarios-in-buffer)
        (error "Cannot determine which test file to run.")))))

(defun rails-test:active-support-test-case-current-test ()
  (save-excursion
    (ruby-end-of-block)
    (and (search-backward-regexp "^[ ]*test \"\\([a-z0-9_ ]+\\)\"[ ]*do" nil t)
         (cons (match-string-no-properties 1) (point)))))

(defun rails-test:run-current-method ()
  "Run a test for the current method."
  (interactive)
  (let* ((file (substring (buffer-file-name) (length (rails-project:root))))
         (description (or (rails-test:active-support-test-case-current-test) (rails-shoulda:current-test)))
         (method (rails-core:current-method-name (and description (cdr description)))))
    (cond (method
           (rails-test:run-single-file file (format "-n %s" method)))
          (description
           (rails-test:run-single-file file (format "-n /%s/" (replace-regexp-in-string "[\+\. \'\"\(\)]+" ".+" (car description))))))))

;; These functions were originally defined anonymously in ui. They are defined here so keys
;; can be added to them dryly
(defun rails-test:run-integration ()
  "Run Integration Tests."
  (interactive)
  (rails-test:run "integration"))
(defun rails-test:run-units ()
  "Run Unit Tests."
  (interactive)
  (rails-test:run "units"))
(defun rails-test:run-functionals ()
  "Run Functional Tests."
  (interactive)
  (rails-test:run "functionals"))
(defun rails-test:run-recent ()
  "Run Recent Tests."
  (interactive)
  (rails-test:run "recent"))
(defun rails-test:run-all ()
  "Run All Tests."
  (interactive)
  (rails-test:run "all"))

(provide 'rails-test)
