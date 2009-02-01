(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/25jdee/jde/build/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/25jdee/cedet/common"))
(load-file (expand-file-name "~/.emacs.d/vendor/25jdee/cedet/common/cedet.el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/25jdee/elib"))
(setq defer-loading-jde t)
(if defer-loading-jde
    (progn
      (autoload 'jde-mode "jde" "JDE mode." t)
      (setq auto-mode-alist
	    (append
	     '(("\\.java\\'" . jde-mode))
	     auto-mode-alist)))
  (require 'jde))

(setq semantic-idle-scheduler-idle-time 432000)

(add-hook 'jde-mode-hook 'my-java-mode)
;; 设置imenu的排序方式为按名称排序
(setq imenu-sort-function 'imenu--sort-by-name)

(defun my-java-mode()
  ;; 将回车代替C-j的功能，换行的同时对齐
  (define-key jde-mode-map [return] 'newline-and-indent)
  (interactive)
  ;; 设置C程序的对齐风格
  ;; (c-set-style "K&R")
  ;; 自动模式，在此种模式下当你键入{时，会自动根据你设置的对齐风格对齐
  ;; (c-toggle-auto-state)
  ;; 此模式下，当按Backspace时会删除最多的空格
  (c-toggle-hungry-state)
  ;; TAB键的宽度设置为4
  (setq c-basic-offset 4)
  ;; 在菜单中加入当前Buffer的函数索引
  (imenu-add-menubar-index)
  ;; 在状态条上显示当前光标在哪个函数体内部
  (which-function-mode)
  )

;; Setup Emacs to run bash as its primary shell.
(setq shell-file-name "bash")
(setq shell-command-switch "-c")
(setq explicit-shell-file-name shell-file-name)
(setenv "SHELL" shell-file-name)
(setq explicit-sh-args '("-login" "-i"))
(if (boundp 'w32-quote-process-args)
    (setq w32-quote-process-args ?\")) ;; Include only for MS Windows.

