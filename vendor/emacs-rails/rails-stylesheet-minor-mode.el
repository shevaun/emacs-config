;;; rails-stylesheet-minor-mode.el --- minor mode for RubyOnRails stylesheets

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Geoff Jacobsen <geoffjacobsen at gmail dot com>

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

(defun rails-stylesheet:find-scss-file-and-line ()
  (save-excursion
    (let* ((epoint (point))
           (spoint (or (re-search-backward "^/\\* line \\([0-9]+\\), \\(.*\\) \\*/") 1))
           (filename (match-string 2))
           (lineno (+ (string-to-number (match-string 1)) -1)))
      (find-file filename)
      (goto-char (point-min)) (forward-line lineno))))

(defun rails-stylesheet:find-css-file-and-line ()
  (let* ((best-line 1) best-point curr-line
         (lineno (1+ (count-lines (point-min) (point))))
         (filename (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
         (main-css-fn (rails-core:file (rails-core:css-file "screen")))
         (main-css-buf (find-buffer-visiting main-css-fn)))
    (if (not (buffer-live-p main-css-buf))
        (find-file main-css-fn)
      (switch-to-buffer main-css-buf)
      (revert-buffer nil t))
    (goto-char (point-min))
    (setq best-point (point))
    (while (re-search-forward (concat "^/\\* line \\([0-9]+\\), \\(.*\\)" filename ".scss \\*/") nil t)
      (setq curr-line (string-to-number (match-string 1)))
      (when (and (> curr-line best-line) (<= curr-line lineno))
        (setq best-line curr-line)
        (setq best-point (point))))
    (goto-char best-point)
    (forward-line)))

(defun rails-stylesheet-layout:switch-to-other ()
  (let* ((filename (buffer-file-name))
         (dirname (directory-file-name (file-name-directory filename)))
         (filebase (file-name-sans-extension (file-name-nondirectory filename)))
         (fileext (file-name-extension (file-name-nondirectory filename))))
    (if (string= fileext "scss")
        (rails-stylesheet:find-css-file-and-line)
      (rails-stylesheet:find-scss-file-and-line))))

(define-minor-mode rails-stylesheet-minor-mode
  "Minor mode for RubyOnRails stylesheets."
  :lighter " Stylesheet"
  (setq yas/mode-symbol 'rails-stylesheet-minor-mode)
  (setq rails-primary-switch-func 'rails-stylesheet-layout:switch-to-other))


(provide 'rails-stylesheet-minor-mode)