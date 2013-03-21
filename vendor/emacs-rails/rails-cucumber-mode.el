;; rails-cucumber.el -- Emacs mode for editing plain text user stories
;;
;; Rewrite of Michael Klishin's feature.el by Geoff Jacobsen <geoffjacobsen@gmail.com> Sep 2009
;; to work with emacs-rails.el
;;
;; Copyright (C) 2008 Michael Klishin
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;

(eval-when-compile (require 'cl))

;;
;; Keywords and font locking
;;

(defconst rails-cucumber:blank-line-re "^[[:space:]]*\\(#.*\\)?$")
(defconst rails-cucumber:0-indent-re "^[[:space:]]*\\(@\\|Feature:\\|Background:\\|Scenario\\(?: Outline\\)?:\\)")
(defconst rails-cucumber:1-indent-re "^[[:space:]]*\\(Given\\|When\\|Then\\|But\\|\\(?:More \\)?Examples:\\)")
(defconst rails-cucumber:2-indent-re "^[[:space:]]*\\(And\\||\\|\"\"\"\\)")

(defconst rails-cucumber:font-lock-keywords
  (list
   '("^[[:space:]]*Feature:" (0 font-lock-keyword-face)
     ("\\([^#]+\\)\\(?:#.*\\)?" nil nil (1 font-lock-type-face)))
   '("^[[:space:]]*Scenario\\(?: Outline\\)?:" (0 font-lock-keyword-face)
     ("\\([^#]+\\)\\(?:#.*\\)?" nil nil (1 font-lock-function-name-face t)))
   (cons (concat
          "^[[:space:]]*" 
          (regexp-opt '("In order" "As a" "As an" "I want" 
                        "Given" "When" "Then" "But" "And" 
                        "More Examples" "Examples") t)
          "\\>")
         1)
   '("^[[:space:]]*Background:" . 0)
   '("^[[:space:]]*@.*" . font-lock-preprocessor-face)
   '("<[_[:alnum:]]+>" 0 font-lock-variable-name-face t)
   ))


;;
;; Keymap
;;

(define-keys rails-cucumber:menu-bar-map
  ([Cucumber] (cons "Cucumber" (make-sparse-keymap "Cucumber")))
  ([Cucumber check-all] '("Verify all features" . rails-cucumber:check-all-scenarios-in-project))
  ([Cucumber check-feature] '("Verify this feature" . rails-cucumber:check-all-scenarios-in-buffer))
  ([Cucumber run-all] '("Test all features" . rails-cucumber:run-all-scenarios-in-project))
  ([Cucumber run-feature] '("Test this feature" . rails-cucumber:run-all-scenarios-in-buffer))
  ([Cucumber run] '("Test current scenario" . rails-cucumber:run-scenario-at-pos))
  )

(defvar rails-cucumber:mode-map 
  (make-sparse-keymap))

(define-keys rails-cucumber:mode-map
  ([menu-bar] rails-cucumber:menu-bar-map)
  ((kbd "\C-m") 'newline)
  ((rails-key ",s") 'rails-cucumber:run-scenario-at-pos)
  (rails-minor-mode-test-current-method-key 'rails-cucumber:run-scenario-at-pos)
  ((rails-key ",v") 'rails-cucumber:run-all-scenarios-in-buffer)
  ((rails-key ",c") 'rails-cucumber:check-all-scenarios-in-buffer)
  ((rails-key ",f") 'rails-cucumber:run-all-scenarios-in-project)
  ((rails-key ",d") 'rails-cucumber:check-all-scenarios-in-project)
  ((rails-key ",r") 'rails-cucumber:run-last)
  ((read-kbd-macro yas/trigger-key) 'rails-cucumber:find_gherkin))

;;
;; Syntax table
;;

(defvar rails-cucumber:mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table in use in rails-cucumber:mode buffers.")

;;
;; Variables
;;

(defcustom rails-cucumber:run-program "cucumber"
  "Program to use to run features"
  :type 'string :group 'cucumber)

(defvar rails-cucumber:mode-hook nil
  "Hook run when entering `rails-cucumber-mode'.")

(defcustom rails-cucumber:indent-level 2
  "Indentation of feature statements"
  :type 'integer :group 'cucumber)

(defun rails-cucumber:mode-variables ()
  (set-syntax-table rails-cucumber:mode-syntax-table)
  (setq require-final-newline t)
  (setq comment-start "# ")
  (setq comment-start-skip "#+[ \t]*")
  (setq comment-end "")
  (setq parse-sexp-ignore-comments t)
  (set (make-local-variable 'font-lock-defaults) '((rails-cucumber:font-lock-keywords) nil nil))
  (set (make-local-variable 'font-lock-keywords) rails-cucumber:font-lock-keywords))

(defun rails-cucumber:minor-modes ()
  "Enable all minor modes for feature mode."
  (turn-on-orgtbl)
  (org-defkey orgtbl-mode-map "\C-c\C-c" nil)
  (org-defkey orgtbl-mode-map "\C-c\t" 'orgtbl-ctrl-c-ctrl-c))


;;
;; Mode function
;;

;;;###autoload
(defun rails-cucumber-mode ()
  "Major mode for editing plain text stories"
  (interactive)
  (kill-all-local-variables)
  (use-local-map rails-cucumber:mode-map)
  (setq mode-name "Cucumber")
  (setq major-mode 'rails-cucumber-mode)
  (rails-cucumber:mode-variables)
  (rails-cucumber:minor-modes)
  (set (make-local-variable 'indent-line-function) 'rails-cucumber:indent-line)
  (run-mode-hooks 'rails-cucumber:mode-hook))


;;
;; Indentation
;;


(defun rails-cucumber:keyword-level ()
  "Keyword indent level or nil if not keyword line"
  (cond
   ((looking-at rails-cucumber:0-indent-re) 0)
   ((looking-at rails-cucumber:1-indent-re) 1)
   ((looking-at rails-cucumber:2-indent-re) 2)
   (t nil)))

(defun rails-cucumber:compute-indentation ()
  "Calculate the indentation of the previous line and its level."
  (save-excursion
    (beginning-of-line)
    (progn
      (cond ((not (bobp))
             (forward-line -1)
             (while (and (or (looking-at rails-cucumber:blank-line-re) (not (rails-cucumber:keyword-level)))
                         (> (point) (point-min)))
               (forward-line -1))))
      (list (current-indentation) (rails-cucumber:keyword-level))
      )))


(defun rails-cucumber:indent-line ()
  "Indent the current line."
  (interactive "*")
  (save-excursion
    (beginning-of-line)
    (let* ((cl (rails-cucumber:keyword-level))
          (need (rails-cucumber:compute-indentation))
          (indent (+ (car need) 
                    (* rails-cucumber:indent-level 
                       (if cl
                           (- cl (cadr need))
                         1)))))
      (cond 
       ((/= (current-indentation) indent)
        (delete-horizontal-space)
        (indent-to indent)))))
  (if (< (current-column) (current-indentation))
      (back-to-indentation)))

;;
;; Verifying features
;;

(defconst rails-cucumber:scenario-pattern  "^[[:space:]]*Scenario\\(?: Outline\\)?:[[:space:]]*\\(.*\\)[[:space:]]*$")

(defun rails-cucumber:scenario-name-at-pos (&optional pos)
  "Returns the name of the scenario at the specified position. if pos is not specified the current buffer location will be used."
  (interactive)
  (let ((start (or pos (point))))
    (save-excursion
      (end-of-line)
      (unless (re-search-backward rails-cucumber:scenario-pattern nil t)
	(error "Unable to find an scenario"))
      (match-string-no-properties 1))))

(defun rails-cucumber:run-scenario-at-pos (&optional pos)
  "Run the scenario defined at pos.  If post is not specified the current buffer location will be used."
  (interactive)
  (rails-cucumber:run 
   '()
   :rails-cucumber:file (format "-n '%s' %s" (rails-cucumber:escape-scenario-name (rails-cucumber:scenario-name-at-pos)) (buffer-file-name))))

(defun rails-cucumber:escape-scenario-name (scenario-name)
  "Escapes all the characaters in a scenario name that mess up using in the -n options"
  (replace-regexp-in-string "\\(\"\\)" "\\\\\\\\\\\\\\1" (replace-regexp-in-string "\\([()\']\\|\\[\\|\\]\\)" "\\\\\\1" scenario-name)))


(defun rails-cucumber:run-all-scenarios-in-buffer ()
  "Run all the scenarios defined in current buffer."
  (interactive)
  (rails-cucumber:run '() :rails-cucumber:file (buffer-file-name)))

(defun rails-cucumber:check-all-scenarios-in-buffer ()
  "Check (dry-run) all the scenarios defined in current buffer."
  (interactive)
  (rails-cucumber:run (list "--dry-run") :rails-cucumber:file (buffer-file-name)))

(defun rails-cucumber:run-all-scenarios-in-project ()
  "Run all the scenarios defined in current project."
  (interactive)
  (rails-cucumber:run (list "--format" "progress")))

(defun rails-cucumber:check-all-scenarios-in-project ()
  "Run all the scenarios defined in current project."
  (interactive)
  (rails-cucumber:run (list "--dry-run" "--format" "progress")))

(define-derived-mode rails-cucumber:compilation-mode compilation-mode "RCucumber"
  "Major mode for Cucumber tests."
  (rails-script:setup-output-buffer)
  ; replace compilation font-lock-keywords
  (set (make-local-variable 'compilation-mode-font-lock-keywords) rails-cucumber:font-lock-keywords)
  ; skip anythins less that error
  (set (make-local-variable 'compilation-skip-threshold) 2)
  (set (make-local-variable 'compilation-error-regexp-alist-alist)
       (rails-cucumber:error-regexp-alist))
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(rails-rails-cucumber:error
         rails-rails-cucumber:trace))
  (setq font-lock-unfontify-region-function
        'ansi-color-unfontify-region)
  (add-hook 'after-change-functions 'rails-test:print-progress nil t)
  (add-hook 'rails-script:run-after-stop-hook 'rails-test:hide-rails-project-root t t)
;;  (add-hook 'rails-script:run-after-stop-hook 'rails-test:scroll-of-buffer t t)
  (add-hook 'rails-script:run-after-stop-hook 'rails-cucumber:print-result t t)
;;  (add-hook 'rails-script:show-buffer-hook 'rails-test:reset-point-and-height t t)
)

(defconst rails-cucumber:result-regexp
  "^\\([0-9]+\\) steps .*[^0-9]\\([0-9]+\\) passed)")

(defun rails-cucumber:print-result ()
  "Determine if the output buffer needs to be shown"
  (with-current-buffer (get-buffer rails-script:buffer-name)
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward rails-cucumber:result-regexp (point-min) t)
        (message (match-string 0))
        (when (and (> (string-to-number (match-string 1)) (string-to-number (match-string 2)))
                   (not (buffer-visible-p (current-buffer))))
          (rails-script:popup-buffer))))))

(defun rails-cucumber:line-regexp (&optional append prepend)
  (concat
   append
    "\\([-_/.[:alnum:]]+\\):\\([0-9]+\\)\\(?::in\s*`\\(.*?\\)'\\)?"
   prepend))

(defun rails-cucumber:error-regexp-alist ()
  (list
   (list 'rails-rails-cucumber:trace
         (rails-cucumber:line-regexp "^.*# ") 1 2 nil 0 1)
   (list 'rails-rails-cucumber:error
         (rails-cucumber:line-regexp "^\\(?:cucumber\\)?[[:space:]]*") 1 2 nil 2 1)
   ))

(defun rails-cucumber:run-last ()
  "run the last cucumber command"
  (interactive)
  (when rails-cucumber:last-run
    (let ((default-directory (car rails-cucumber:last-run)))
      (rails-cucumber:run (cadr rails-cucumber:last-run) :rails-cucumber:file (caddr rails-cucumber:last-run)))))

(defvar rails-cucumber:last-run)

(defun rails-cucumber:run (cuke-opts &optional key rails-cucumber:file)
  "Runs cucumber with the specified options"
  (setq rails-cucumber:last-run (list default-directory cuke-opts rails-cucumber:file))

  (let ((all-opts
         (if rails-cucumber:file
             (let ((profile (file-name-nondirectory (directory-file-name (file-name-directory rails-cucumber:file)))))
               (if (string= profile "features")
                   cuke-opts
                 (append cuke-opts (list "-p" profile))))
           cuke-opts)))
    (rails-script:run-color rails-cucumber:run-program (append all-opts (list rails-cucumber:file)) 'rails-cucumber:compilation-mode rails-cucumber:file)))

(defconst rails-cucumber:gherkin-main-re "^[[:space:]]*\\(Given\\|When\\|Then\\)[[:space:]]+\\(.*\\)")
(defconst rails-cucumber:gherkin-and-re "^[[:space:]]*\\(And\\)[[:space:]]+\\(.*\\)")
(defconst rails-cucumber:ruby-steps-prog (concat (file-name-directory load-file-name) "rails-emacs-cuc.rb"))

(defun rails-cucumber:run-steps-prog (gherk-type params)
  "Execute steps command COMMAND and return its output as a string."
  (with-output-to-string
    (with-current-buffer
      standard-output
      (rails-project:with-root
       (root)
       (call-process rails-cucumber:ruby-steps-prog nil t nil (rails-core:file "features/step_definitions") gherk-type params)))))
  

(defun rails-cucumber:gherkin-prompt (step list initial-input)
  (let* ((choices (eval (car (read-from-string list))))
         (chosen (and list
                      (completing-read (concat step ": ")
                                       choices
                                       nil
                                       'require-match
                                       initial-input
                                       nil))))
    chosen))

(defun rails-cucumber:find_gherkin ()
  "search for snippet to run matching current line"
  (interactive)
  (if (save-excursion
           (beginning-of-line)
           (looking-at "^[[:space:]]*$"))
      (insert "And "))
  (indent-for-tab-command)
  (let ((gherk-type
         (save-excursion
           (beginning-of-line)
           (let ((gherk-type
                  (if (looking-at rails-cucumber:gherkin-and-re)
                      (save-excursion
                        (save-match-data
                          (while (and (not (looking-at rails-cucumber:gherkin-main-re))
                                      (> (point) (point-min)))
                            (forward-line -1))
                          (if (looking-at rails-cucumber:gherkin-main-re)
                              (match-string 1))))
                    (if (looking-at rails-cucumber:gherkin-main-re)
                        (match-string 1)))))
             gherk-type))))
    (when gherk-type
      (let ((choice (rails-cucumber:gherkin-prompt gherk-type (rails-cucumber:run-steps-prog gherk-type (match-string 2)) (match-string 2)))
            (mark (point)))
        (beginning-of-line)
        (forward-word)
        (delete-region (point) mark)
        (insert " ")
        (yas/minor-mode t)
        (yas/expand-snippet choice)
        (yas/minor-mode nil)))))

(provide 'rails-cucumber-mode)
