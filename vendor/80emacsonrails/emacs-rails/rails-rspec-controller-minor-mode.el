(define-minor-mode rails-rspec-controller-minor-mode
  "Minor mode for RubyOnRails controller rspec."
  :lighter " ControllerRSpec"
  :keymap (let ((map (rails-controller-layout:keymap :rspec-controllers)))
            (define-key map rails-minor-mode-test-current-method-key 'rails-rspec:run-current-method)
            (define-key map [menu-bar rails-controller-layout run] '("RSPec current method" . rails-rspec:run-current-method))
            map)
  (setq rails-primary-switch-func 'rails-controller-layout:switch-to-controller)
  (setq rails-secondary-switch-func 'rails-controller-layout:menu))

(provide 'rails-rspec-controller-minor-mode)