;;; rails-find.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

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

(defmacro rails-find:gen (name dir-list)
  "Define new rails-find function"
  (let ((dir (if (listp dir-list) dir-list (concat dir-list "/")))
        (dir-name (if (listp dir-list) (cadr dir-list) dir-list)))
    `(defun ,(intern (concat "rails-find:" name)) ()
       ,(format "Run find-file in Rails \"%s\" dir" dir-name)
       (interactive)
       (let ((default-directory (rails-core:file ,dir)))
         (call-interactively ',(if (fboundp 'ido-find-file)
                                   'ido-find-file
                                 'find-file))))))

(rails-find:gen "controller"  "app/controllers")
(rails-find:gen "view"        "app/views")
(rails-find:gen "layout"      "app/views/layouts")
(rails-find:gen "db"          "db")
(rails-find:gen "public"      "public")
(rails-find:gen "helpers"     "app/helpers")
(rails-find:gen "models"      "app/models")
(rails-find:gen "config"      "config")
(rails-find:gen "features"      "features")
(rails-find:gen "lib"         "lib")
(rails-find:gen "tasks"       "lib/tasks")
(rails-find:gen "stylesheets" '("public/stylesheets/" "app/assets/stylesheets/"))
(rails-find:gen "sassstylesheets" '("app/stylesheets/" "app/assets/stylesheets/"))
(rails-find:gen "javascripts" '("public/javascripts/" "app/assets/javascripts/"))
(rails-find:gen "coffeescripts" '("app/coffeescripts/" "app/assets/javascripts/"))
(rails-find:gen "migrate"     "db/migrate")
(rails-find:gen "fixtures"    "test/fixtures")
(rails-find:gen "test-unit"    "test/unit")
(rails-find:gen "test-functional" "test/functional")

;; Rspec
(rails-find:gen "spec" "spec/")
(rails-find:gen "spec-controllers" "spec/controllers/")
(rails-find:gen "spec-models" "spec/models/")
(rails-find:gen "spec-helpers" "spec/views/")
(rails-find:gen "spec-helpers" "spec/helpers/")
(rails-find:gen "spec-fixtures" "spec/fixtures/")

(provide 'rails-find)