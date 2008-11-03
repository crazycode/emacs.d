(require 'rinari)

(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
  (lambda () (rinari-launch)))

(setq rinari-tags-file-name "TAGS")

