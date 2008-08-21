;;my buffers key binding.
(require 'my-buffers)

(global-set-key (kbd "C-x q") 'my-kill-other-buffers)
(global-set-key [\C-f4] 'my-kill-current-buffer)
(global-set-key (kbd "C-x w") 'my-kill-current-buffer)


