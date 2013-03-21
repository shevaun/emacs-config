;;; rails-coffeescript-minor-mode.el --- minor mode for RubyOnRails coffeescripts

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

(defun rails-coffeescript-layout:switch-to-other ()
  (find-file (rails-core:file (rails-core:js-file
                               (file-name-sans-extension (file-name-nondirectory 
                                                          (buffer-file-name)))))))

(define-minor-mode rails-coffeescript-minor-mode
  "Minor mode for RubyOnRails coffeescripts."
  :lighter " Coffeescript"
  (setq yas/mode-symbol 'rails-coffeescript-minor-mode)
  (setq rails-primary-switch-func 'rails-coffeescript-layout:switch-to-other))


(provide 'rails-coffeescript-minor-mode)