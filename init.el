(push "/usr/local/bin" exec-path)

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/rails-minor-mode"))
;; (require 'rails)

(load-file "~/.emacs.d/rvm.el")
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(global-set-key (kbd "C-x C-e") 'eval-buffer)

(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/color-theme/color-theme-railscasts.el")
(color-theme-railscasts)

(dolist (elm '("emacs-rails" "emacs-rails/rhtml" "yasnippet" "coffee-mode" "full-ack"))
  (add-to-list 'load-path (concat "~/.emacs.d/vendor/" elm)))

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)

(require 'css-mode)
(setq auto-mode-alist (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq cssm-indent-function 'cssm-c-style-indenter)
(setq cssm-mirror-mode nil)
(require 'sass-mode nil 't)
;;(require 'scss-mode)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(require 'hide-lines)

(autoload 'htmlize "htmlize" "HTML a font-locked buffer" t)

(require 'cc-mode)

(defun fix-fontification ()
  (interactive)
  (facemenu-remove-all (point-min) (point-max))
  (font-lock-fontify-buffer))

;; ---- ruby -----

(require 'rhtml-mode)

(setq ri-ruby-script "~/.emacs.d/ri-emacs.rb")
(autoload 'ri "~/.emacs.d/site-lisp/ri-ruby.el" nil t)

(require 'compile)

(require 'ruby-mode)

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.rpdf$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rtex$" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.rsel$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

;; (defvar javascript-identifier-regexp "[a-zA-Z0-9.$_]+")



;; (add-hook 'javascript-mode-hook nil)
;; (add-hook 'javascript-mode-hook (lambda ()
;;                                    (setq imenu-create-index-function 'javascript-imenu-create-index)))

(require 'coffee-mode)

(autoload 'js-mode "js" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

(modify-coding-system-alist 'file "\\.rb$" 'utf-8)


(dolist (elm '(ruby-mode-hook js-mode-hook coffee-mode-hook rhtml-mode-hook))
  (add-hook elm
            (lambda()
              (add-hook 'local-write-file-hooks
                        '(lambda()
                           (save-excursion
                             (untabify (point-min) (point-max))
                             ;; (if (not (string-match "qsr\|assure\|tickit" buffer-file-name)) (delete-trailing-whitespace))
                             ;;(delete-trailing-whitespace)
                             )))
              (imenu-add-to-menubar "IMENU")
              (set (make-local-variable 'tab-width) 2)
              (set (make-local-variable 'indent-tabs-mode) 'nil))))

(add-hook 'ruby-mode-hook
          (lambda()
            ;; (hs-minor-mode t)
            (local-set-key (kbd "\C-c\C-r") 'my-ruby:exec-buffer)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'js-mode-hook
          (lambda()
            ;; (hs-minor-mode t)
            (local-set-key [(meta ?.)] 'find-tag)))

;;(setq auto-mode-alist (cons '("\\.html.erb$" . rhtml-mode) auto-mode-alist))
(add-hook 'rhtml-mode (lambda () (rails-minor-mode 1)))

(require 'highlight-indentation)

(add-hook 'coffee-mode-hook
          (lambda()
            (local-set-key "\C-cr"' coffee-compile-region)
            (local-set-key "\C-cl"' coffee-compile-buffer)))


;;(add-hook 'css-mode-hook nil)

(require 'rails)

(defun my-ruby:exec-buffer ()
  "pass the buffer contents to ruby"
  (interactive)
  (let (exit-status
        (buffer (get-buffer-create "*Shell Command Output*")))
    (let ((directory default-directory))
      (save-excursion
        (set-buffer buffer)
        (setq buffer-read-only nil)
        (setq default-directory directory)
        (erase-buffer)))
    (setq exit-status
          (call-process-region (point-min) (point-max) "ruby" nil buffer nil "-W2" "-I" default-directory))
    (display-buffer (current-buffer))
    (display-message-or-buffer buffer)))

(defun my-backup-enable-predicate (file)
  (if (string-match "bzr_log\.[0-9a-zA-Z]" (file-name-nondirectory file))
      nil
    (normal-backup-enable-predicate file)))

(setq backup-enable-predicate 'my-backup-enable-predicate)

(defun project-grep (&optional count)
  "Finds files in project by grepping for a word"
  (interactive "p")
  (let ((word (find-tag-default)))
    (when (called-interactively-p)
      (setq word (read-from-minibuffer "regexp word: " word nil nil nil "")))
    (if (= count 1)
        (grep (concat "cd " (search-up-for-project-home) ";grep_source '" word "'"))
      (grep (concat "grep_source '" word "'")))
    (if rails-minor-mode
        (with-current-buffer "*grep*"
          (rails-minor-mode t)))))

(defun project-find (&optional count)
  "Finds files in project that partially match a word"
  (interactive "p")
  (let ((word (find-tag-default)))
    (when (called-interactively-p)
      (setq word (read-from-minibuffer "regexp word: " word nil nil nil "")))
    (let ((dir (if (= count 1) (search-up-for-project-home) default-directory)))
      (project-find-1 dir word (get-buffer-create "*find*")))))

(defun project-find-1 (dir word find-buf)
  (with-current-buffer find-buf
    (let ((inhibit-read-only t))
      (setq default-directory dir)
      (widen)
      (delete-region (point-min) (point-max))
      (call-process "find_file" nil find-buf t word)
      (goto-char (point-min))
      (dired-next-line 1)
      (setq view-exit-action 'bury-buffer)
      (setq view-no-disable-on-exit t)
      (or (get-buffer-window find-buf) (view-buffer-other-window find-buf))
      (dired-mode)
      (set (make-local-variable 'dired-subdir-alist) (list (cons default-directory 1)))
      (setq revert-buffer-function (eval (macroexpand (list 'project-find-mac dir word find-buf))))
      (rails-maybe-minor-mode))))

(defmacro project-find-mac (dir word find-buf)
  `(lambda (ignore-auto noconfirm)
     (let ((c-point (point)))
       (project-find-1 ,dir ,word ,find-buf)
       (goto-char c-point))))

(defun search-up-for-project-home ()
  (search-up-for-files '(".git" ".bzr" "configure.in" "TAGS" "setup.rb"))
)

(defun search-up-for-files (files)
  (let* ((look-for-file (lambda (dir files)
                          (if (car files)
                              (if (file-exists-p (concat dir "/" (car files)))
                                  t
                               (funcall look-for-file dir (cdr files))
                               )
                            nil)
                          ))
         (check-dir (lambda (dir files)
                      (if (string= dir "/")
                          default-directory
                        (if (funcall look-for-file dir files)
                            dir
                          (funcall check-dir (file-name-directory (directory-file-name dir)) files))))))
    (funcall check-dir (expand-file-name default-directory) files)
    ))

(defun rails-find-and-goto-error (file)
  "Finds error in rails html log go on error line"
  (interactive)
  (switch-to-buffer "*rails-browser-error*")
  (erase-buffer)
  (html-mode)
  (rails-minor-mode t)
  (compilation-minor-mode t)
  (insert-file-contents-literally file)
  (search-forward-regexp "RAILS_ROOT: \\([^<]*\\)")
  (let ((rails-root (concat (match-string 1) "/")))
    (cd rails-root)
    (search-forward "id=\"Application-Trace\"")
    (search-forward-regexp "\\(<pre><code>\\|^\\)app")
    (search-forward-regexp "\\([^:]*\\):\\([0-9]+\\)")
    (let  ((file (match-string 1))
       (line (match-string 2)))
      ;(kill-buffer (current-buffer))
      (message
       (format "Error found in file \"%s\" on line %s. "  file line))
      (find-file (concat "app" file))
      (goto-char (point-min)) (forward-line (- (string-to-number line) 1)))))

;; ---- end of ruby -----

(setq calendar-latitude -41.2010)
(setq calendar-longitude 174.9489)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

(require 'my-vc)

(setq vcursor-key-bindings t)
(require 'vcursor)

(global-set-key "\C-z" 'undo)
(global-set-key [M-return] 'imenu)
(global-set-key [M-C-delete] 'backward-kill-sexp)
(global-set-key "\C-cH" 'hide-lines)
(global-set-key [S-M-f12] 'previous-buffer)
(global-set-key [M-f12] 'next-buffer)
(global-set-key [S-f12] 'main-rails-project)
(global-set-key [C-S-f12] 'make-main-rails-project)
(global-set-key [f12] 'toggle-buffer)
(global-set-key [C-M-tab] 'ispell-complete-word)
(global-set-key [f6] 'hs-toggle-hiding)
(global-set-key [C-f6] 'hs-hide-all)
(global-set-key [S-f6] 'hs-show-all)
(global-set-key [?\s-.] 'myswitch-to-scratch)
(global-set-key [?\C-o] 'open-line-above)
(global-set-key "\C-xv/" 'my-vc-bzr-viz)
(global-set-key "\C-xv?" 'my-vc-bzr-gdiff)
(global-set-key "\C-xvp" 'my-vc-bzr-push)
(global-set-key "\C-xvP" 'my-vc-bzr-pull)
(global-set-key "\C-xvm" 'my-vc-bzr-merge)
(global-set-key "\C-xvD" 'my-vc-bzr-dir)
(global-set-key [S-M-return] 'my-vc-bzr-dir)
(global-set-key "\C-xv\r" 'my-vc-bzr-checkin)
(global-set-key [f5] 'ack)
(global-set-key [S-f5] 'project-find)
(global-set-key [f10] 'next-match)
(global-set-key [C-f10] 'compile)
(global-set-key [S-f9] 'ruby-test:find-last)
(global-set-key [C-f9] 'ruby-test:run-one)
(global-set-key [C-S-f9] 'ruby-test:run)
(global-set-key [f9] 'ruby-test:rerun)
(global-set-key [S-f10] 'ruby-test:toggle)
(global-set-key [M-f9] 'ruby-test:previous)
(global-set-key [s-M-f9] 'ruby-test:delete-current)
(global-set-key [s-S-f9] 'ruby-test:list-marks)
(global-set-key [S-M-f9] 'ruby-test:next)
(global-set-key [C-f11] 'work-mark:add)
(global-set-key [S-f11] 'work-mark:current)
(global-set-key [M-f11] 'work-mark:previous)
(global-set-key [s-M-f11] 'work-mark:delete-current)
(global-set-key [s-S-f11] 'work-mark:list-marks)
(global-set-key [S-M-f11] 'work-mark:next)
(global-set-key [f11] 'work-mark:toggle)
(global-set-key [?\C-'] 'rails-core:goto-backtrace)
(global-set-key [f7] 'ediff-revision)
(global-set-key [f8] 'ediff-buffers)
(global-set-key [M-f10] 'switch-to-compile-buffer)
(global-set-key [C-tab] 'indent-according-to-mode)
(global-set-key [?\s-D] 'describe-text-properties)
(global-set-key [C-S-iso-lefttab] 'vcursor-swap-point)
;;(global-set-key "\C-x+" 'my-balance-windows)
(global-set-key [?\C-+] 'text-scale-decrease)
(global-set-key [?\M-+] 'text-scale-mode)
(global-set-key [?\C-=] 'text-scale-increase)

;(global-set-key (kbd "C-a") 'back-to-indentation)
;(global-set-key (kbd "M-m") 'aquamacs-move-beginning-of-line)

(define-key ruby-mode-map [M-f1] 'my-search-apidock-rails)

(defvar main-rails-project-selected nil)

(require 'bookmark)

(defun main-rails-project ()
  "switch to the main rails project set by the bookmark named x"
  (interactive)
  (if main-rails-project-selected
      (progn
        (find-file (bookmark-get-filename "x"))
        (when rails-minor-mode (visit-tags-table (rails-core:file "TAGS"))))

    (setq main-rails-project-selected t)
    (bookmark-maybe-load-default-file)
    (find-file (bookmark-get-filename "x"))
    (rails-create-tags)))


(defun make-main-rails-project ()
  "bookmark buffer as the main rails project; bookmark is named x"
  (interactive)
  (bookmark-set "x")
  (bookmark-save))

;; (defun my-balance-windows ()
;;   "balance windows but not ecb"
;;   (interactive "")
;;   (balance-windows)
;;   (ecb-redraw-layout)
;;   )

(defun toggle-buffer ()
  "switch to other-buffer"
  (interactive "")
  (switch-to-buffer (other-buffer))
  )

(defun myshell-cd ()
  "switch to shell with the current default-directory"
  (interactive "")
  (let ((dir (expand-file-name default-directory)))
    (eshell)
    (if (not (string-equal dir (expand-file-name default-directory)))
        (let ((proc (get-buffer-process (current-buffer))))
          (eshell/cd dir)
          (message dir)))))

(defun open-line-above (count)
  "open-line from any point along line"
  (interactive "p")
  (indent-according-to-mode)
  (beginning-of-line)
  (open-line count)
  (indent-according-to-mode)
  )

(defun switch-to-compile-buffer ()
  "switch to Compile-buffer"
  (interactive)
  (switch-to-buffer "*compilation*"))

(defun myswitch-to-scratch ()
  "Switch to the *scratch* buffer"
  (interactive "")
  (switch-to-buffer "*scratch*"))

(require 'custom-mark)
(require 'ruby-test)

(server-start)
(put 'upcase-region 'disabled nil)

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(global-set-key (kbd "C-c +") 'increment-number-at-point)

(defun save-macro (name)
    "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
     (interactive "SName of the macro :")  ; ask for the name of the macro
     (kmacro-name-last-macro name)         ; use this name for the macro
     (find-file (user-init-file))                   ; open ~/.emacs or other user init file
     (goto-char (point-max))               ; go to the end of the .emacs
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro
     (newline)                             ; insert a newline
     (switch-to-buffer nil))               ; return to the initial buffer