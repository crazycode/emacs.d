(require 'weblogger)
(add-hook 'weblogger-entry-mode-hook (lambda ()
                               (turn-off-auto-fill)
                               ))

(require 'blog)
