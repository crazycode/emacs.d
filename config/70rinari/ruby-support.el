(require 'ruby-mode)
(require 'ruby-test)

 (setq ri-ruby-script "/home/ridge/.emacs.d/vendor/20language/ri-emacs/ri-emacs.rb")
 (autoload 'ri "/home/ridge/.emacs.d/vendor/20language/ri-emacs/ri-ruby.el" nil t)
 (add-hook 'ruby-mode-hook (lambda ()
	 		    (local-set-key 'fi 'ri)
			    (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
			    (local-set-key 'f4 'ri-ruby-show-args)
			    ))

(require 'anything)
(require 'anything-rcodetools)
;; Command to get all RI entries.
(setq rct-get-all-methods-command "PAGER=cat qri -l")
;; See docs
(define-key anything-map "\C-z" 'anything-execute-persistent-action)
(require 'anything-dabbrev-expand)
(global-set-key "\M-/" 'anything-dabbrev-expand)
(define-key anything-dabbrev-map "\M-/" 'anything-dabbrev-find-all-buffers)

(require 'rails-ruby)

(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks  
                      '(lambda()  
                         (save-excursion  
                           (untabify (point-min) (point-max))  
                           (delete-trailing-whitespace)  
                           )))  
            (set (make-local-variable 'indent-tabs-mode) 'nil)  
            (require 'rails-ruby)
            (require 'ruby-electric)  
            (ruby-electric-mode t)  
            (modify-syntax-entry ?! "w" (syntax-table))
            (modify-syntax-entry ?: "w" (syntax-table))
            (modify-syntax-entry ?_ "w" (syntax-table))
            (local-set-key (kbd "C-.") 'complete-tag)
            (local-set-key (if rails-use-another-define-key
                               (kbd "TAB") (kbd "<tab>"))
                           'indent-and-complete)
            (local-set-key (kbd "C-:") 'ruby-toggle-string<>simbol)
            (local-set-key (kbd "<return>") 'ruby-newline-and-indent)))

