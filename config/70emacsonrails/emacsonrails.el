(autoload 'ruby-mode "ruby-mode" "Load ruby-mode")


(setq rails-api-root "/usr/lib/ruby/gems/1.8/doc")
(setq ri-ruby-script "~/.emacs.ext/site-lisp/ri-emacs/ri-emacs.rb")
(autoload 'ri "ri-ruby" nil t)

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
            (imenu-add-to-menubar "IMENU")  
            (require 'ruby-electric)  
            (ruby-electric-mode t)  
            (local-set-key "\t" 'hippie-expand)  
            ;;(local-set-key (kbd "<tab>") 'yas/expand)
            ;;(local-set-key "\C-h" 'ri)
            (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
            (local-set-key "\C-c\C-h" 'ri-ruby-show-args)
            ))  

(add-hook 'nxml-mode-hook  
          (lambda ()  
            (setq local-abbrev-table nxml-mode-abbrev-table)))  

;;(defun try-complete-abbrev (old)
;;  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))
    
(require 'psvn)
(require 'snippet)
(require 'rails)

;; associate ruby-mode with .rb files
(add-to-list 'auto-mode-alist '("\.rb$" . ruby-mode))
(setq auto-mode-alist  (cons '("\\.rjs$" . ruby-mode) auto-mode-alist))  
(setq auto-mode-alist  (cons '("\\.rake$" . ruby-mode) auto-mode-alist))  
(setq auto-mode-alist  (cons '("\\.rhtml$" . nxml-mode) auto-mode-alist))  
(setq auto-mode-alist  (cons '("\\.erb$" . nxml-mode) auto-mode-alist)) 

(modify-coding-system-alist 'file "\\.rhtml$" 'utf-8)
(modify-coding-system-alist 'file "\\.erb$" 'utf-8)

;;设置错误行背景颜色
;;(set-face-background (quote flymake-errline) "#962433")

(custom-set-faces
 '(flymake-errline ((((class color)) (:background "DarkRed"))))
 '(flymake-warnline ((((class color)) (:background "DarkBlue")))))
