(defun my-kill-other-buffers (&optional list)
  "Kill other buffers except the current one."
  (interactive)
  (if (null list)
      (setq list (buffer-list)))
  (while list
    (let* ((buffer (car list))
	   (name (buffer-name buffer)))
      (if (not (string-equal name (buffer-name (current-buffer))))
      (and name  ; Can be nil for an indirect buffer, if we killed the base buffer.
	   (not (string-equal name ""))
	   (/= (aref name 0) ?\s)
	   (kill-buffer buffer))))
    (setq list (cdr list))))
(defun my-kill-current-buffer ()
  "Kill current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(provide 'my-buffers)

