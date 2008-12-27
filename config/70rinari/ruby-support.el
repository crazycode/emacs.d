(require 'ruby-test)

(require 'anything)
(require 'anything-rcodetools)
;; Command to get all RI entries.
(setq rct-get-all-methods-command "PAGER=cat qri -l")
;; See docs
(define-key anything-map "\C-z" 'anything-execute-persistent-action)
(require 'anything-dabbrev-expand)
(global-set-key "\M-/" 'anything-dabbrev-expand)
(define-key anything-dabbrev-map "\M-/" 'anything-dabbrev-find-all-buffers)

(add-hook 'ruby-mode-hook  
          (lambda()  
            (add-hook 'local-write-file-hooks  
                      '(lambda()  
                         (save-excursion  
                           (untabify (point-min) (point-max))  
                           (delete-trailing-whitespace)  
                           )))  
            (set (make-local-variable 'indent-tabs-mode) 'nil)  
            (set (make-local-variable 'tab-width) 2)  
            (require 'ruby-electric)  
            (ruby-electric-mode t)  
            ))  

