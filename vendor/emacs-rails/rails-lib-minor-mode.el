(define-minor-mode rails-lib-minor-mode
  "Minor mode for RubyOnRails libs."
  :lighter " Lib"
  :keymap (rails-model-layout:keymap :lib)
  (setq yas/mode-symbol 'rails-lib-minor-mode)
  (setq rails-primary-switch-func 'rails-lib-layout:switch-to-rspec-lib)
  (setq rails-secondary-switch-func 'rails-lib-layout:menu))

(provide 'rails-lib-minor-mode)