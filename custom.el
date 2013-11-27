(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(ansi-color-names-vector ["black" "red" "lime green" "orange" "blue" "magenta" "deep sky blue" "light grey"])
 '(aquamacs-additional-fontsets nil t)
 '(aquamacs-customization-version-id 216 t)
 '(aquamacs-tool-bar-user-customization nil t)
 '(auto-fill-function nil t)
 '(blink-cursor-mode nil)
 '(bookmark-save-flag 1)
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "gnome-www-browser")
 '(c-basic-offset 2)
 '(coffee-args-compile (quote ("-p -c")))
 '(coffee-js-mode (quote js-mode))
 '(dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
 '(dabbrev-case-fold-search nil)
 '(default-frame-alist (quote ((tool-bar-lines . 0) (vertical-scroll-bars . right) (viper-saved-cursor-color-in-replace-mode . "Red3") (vc-annotate-very-old-color . "#0046FF") (top-toolbar-shadow-color . "#fffffbeeffff") (senator-eldoc-use-color . t) (mouse-color . "LawnGreen") (bottom-toolbar-shadow-color . "#79e77df779e7") (border-color . "black") (background-toolbar-color . "#cf3ccf3ccf3c") (cursor-type . box) (internal-border-width . 0) (fringe) (modeline . t) (color-theme-name . my-color-theme) (menu-bar-lines . 1) (right-fringe . 12) (left-fringe . 4) (font . "-apple-Lucida_Grande-medium-normal-normal-*-13-*-*-*-p-0-iso10646-1") (fontsize . 0) (font-backend ns) (background-color . "#232323") (background-mode . dark) (cursor-color . "red") (foreground-color . "#E6E1DC"))))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(enh-ruby-program "~/.rvm/bin/ruby")
 '(fill-column 120)
 '(fringe-indicator-alist (quote ((continuation nil nil) (truncation left-truncation right-truncation) (continuation left-continuation right-continuation) (overlay-arrow . right-triangle) (up . up-arrow) (down . down-arrow) (top top-left-angle top-right-angle) (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle) (top-bottom left-bracket right-bracket top-right-angle top-left-angle) (empty-line . empty-line) (unknown . question-mark))) t)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(javascript-indent-level 2)
 '(js-indent-level 2)
 '(mouse-yank-at-point t)
 '(ns-right-command-modifier (quote meta))
 '(ns-tool-bar-display-mode nil t)
 '(ns-tool-bar-size-mode nil t)
 '(rails-api-root "/opt/ruby191/lib/ruby/gems/1.9.1/doc/")
 '(rails-core:class-dirs (quote ("app/controllers" "app/views/layouts" "app/views" "app/models" "app/helpers" "spec/models" "spec/controllers" "spec/views" "test/fixtures" "lib")))
 '(rails-enable-ruby-electric nil)
 '(rails-environments (quote ("development" "production" "test" "cucumber")))
 '(rails-grep-extensions (quote ("coffee" "js" "builder" "erb" "haml" "liquid" "mab" "rake" "rb" "rhtml" "rjs" "rxml" "yml")))
 '(rails-indent-and-complete nil)
 '(rails-ruby-command "ruby")
 '(rails-tags-command "~/scripts/rails-tag %s %s")
 '(rails-tags-dirs (quote ("app" "lib" "test" "db" "features" "public/javascripts")))
 '(rails-test:rake-test-all-task-name "spec")
 '(rails-ui:show-mode-line nil)
 '(rails-ws:default-server-type "unicorn_rails")
 '(ruby-electric-expand-delimiters-list nil)
 '(safe-local-variable-values (quote ((encoding . utf-8) (add-log-time-format lambda nil (let* ((time (current-time)) (system-time-locale "C") (diff (+ (cadr time) 32400)) (lo (% diff 65536)) (hi (+ (car time) (/ diff 65536)))) (format-time-string "%a %b %e %H:%M:%S %Y" (list hi lo) t))))))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(scss-compile-at-save nil)
 '(semanticdb-default-save-directory "/home/geoffj/.emacs.d/semanticdb" t)
 '(tags-case-fold-search t)
 '(text-mode-hook (quote (smart-spacing-mode)))
 '(tool-bar-mode nil)
 '(truncate-partial-width-windows nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(vc-bzr-log-switches (quote ("--include-merges")))
 '(vc-handled-backends nil)
 '(visible-bell t)
 '(visual-line-mode nil t)
 '(word-wrap t)
 '(x-select-enable-clipboard t)
 '(yas/trigger-key "<A-return>"))

(set-face-attribute 'default nil :height 90)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(autoface-default ((t (:inherit default))))
 '(coffee-mode-default ((t (:inherit autoface-default :height 120 :family "Menlo"))) t)
 '(conf-space-mode-default ((t (:inherit conf-unix-mode-default :height 120 :family "Menlo"))) t)
 '(css-mode-default ((t (:inherit autoface-default :height 120 :family "Menlo"))) t)
 '(emacs-lisp-mode-default ((t (:inherit autoface-default :height 120 :family "Menlo"))) t)
 '(font-latex-sectioning-5-face ((((class color) (background light)) (:inherit variable-pitch :foreground "blue4" :weight bold :family "monospace"))))
 '(highlight-indent-face ((t (:background "grey95"))))
 '(js-mode-default ((t (:inherit autoface-default :height 120 :family "Menlo"))) t)
 '(rhtml-mode-default ((t (:inherit html-mode-default :height 120 :family "Menlo"))) t)
 '(ruby-mode-default ((t (:inherit autoface-default :height 120 :family "Menlo"))) t)
 '(sass-mode-default ((t (:inherit autoface-default :height 120 :family "Menlo"))) t)
 '(slim-mode-default ((t (:inherit slim-parent-mode-default :height 120 :family "Menlo"))) t)
 '(text-mode-default ((t (:inherit autoface-default :stipple nil :strike-through nil :underline nil :slant normal :weight normal :height 130 :width normal :family "Lucida Grande"))))
 '(yaml-mode-default ((t (:inherit autoface-default :height 140 :family "Lucida Grande"))) t))
