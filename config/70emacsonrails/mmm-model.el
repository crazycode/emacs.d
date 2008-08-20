(require 'mmm-mode)
(require 'mmm-auto)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 2)
(set-face-background 'mmm-output-submode-face  "#162433")
(set-face-background 'mmm-code-submode-face    "#162433")
(set-face-background 'mmm-comment-submode-face "#162433")

(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(autoload 'javascript-mode "javascript" nil t)

(mmm-add-classes
 '((erb-code
    :submode ruby-mode
    :match-face (("<%#" . mmm-comment-submode-face)
                 ("<%=" . mmm-output-submode-face)
                 ("<%"  . mmm-code-submode-face))
    :front "<%[#=]?"
    :back "-?%>"
    :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
             (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
             (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @))
    )))

(mmm-add-classes
 '((gettext
    :submode gettext-mode
    :front "_(['\"]"
    :face mmm-special-submode-face
    :back "[\"'])")))

(mmm-add-classes
 '((html-script
    :submode javascript-mode
    :front "<script>"
    :back "</script>")))

(add-hook 'html-mode-hook
	  (lambda ()
	    (setq mmm-classes '(erb-code html-js html-script gettext embedded-css))
	    (mmm-mode-on)))

(add-to-list 'auto-mode-alist '("\.rhtml$" . html-mode))
(add-to-list 'auto-mode-alist '("\.html\.erb$" . html-mode))

(add-to-list 'mmm-mode-ext-classes-alist '(ruby-mode nil gettext))
(global-set-key [f8] 'mmm-parse-buffer)
