;; helper function
(defun my-add-subdirs-to-load-path (dir)
  (let ((default-directory (concat dir "/")))
    (setq load-path (cons dir load-path))
    (normal-top-level-add-subdirs-to-load-path)))

(my-add-subdirs-to-load-path "~/.emacs.d/site-lisp") 
(mapc 'load (directory-files "~/.emacs.d/config/01base" t "\.el$")) 
(mapc 'load (directory-files "~/.emacs.d/config/70emacsonrails" t "\.el$")) 
(mapc 'load (directory-files "~/.emacs.d/config/99post" t "\.el$")) 
