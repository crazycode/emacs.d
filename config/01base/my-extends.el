(defun circle-windows ()
  (interactive)
  (let ((owindow (selected-window))
	(obuffer (current-buffer))
	)
    (while (not (equal owindow (next-window)))
      (set-window-buffer (selected-window) (window-buffer (next-window)))
      (select-window (next-window)))
    (set-window-buffer (selected-window) obuffer)
    (select-window owindow)))

(defun move-region-around (direction beg end)
  (let (real-beg
	real-end
	target-beg
	deactivate-mark
	text)
    (save-excursion
      (goto-char beg)
      (setq real-beg (line-beginning-position))
      
      (when (equal direction 'up)
        (setq target-beg (line-beginning-position 0)))
      
      (goto-char end)
      (setq real-end (line-beginning-position 2))
      
      (when (equal direction 'down)
        (setq target-beg (copy-marker (line-beginning-position 3)))) ;must use marker
      
      (setq text (buffer-substring-no-properties real-beg real-end))
      (delete-region real-beg real-end)
      (goto-char target-beg)
      (insert text)
      )

    (set-mark (+ target-beg (- real-end real-beg 1)))
    (goto-char target-beg)
    (setq transient-mark-mode 'only)))


(defun move-region-up (beg end)
  (interactive "r")
  (move-region-around 'up beg end))

(defun move-region-down (beg end)
  (interactive "r")
  (move-region-around 'down beg end))

;全屏
(defun my-fullscreen ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_FULLSCREEN" 0))
)

;最大化
(defun my-maximized ()
  (interactive)
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message
   nil 0 nil "_NET_WM_STATE" 32
   '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
)

;; author: pluskid
;; 调用 stardict 的命令行接口来查辞典
;; 如果选中了 region 就查询 region 的内容，
;; 否则就查询当前光标所在的词
(global-set-key (kbd "C-c d") 'kid-star-dict)
(defun kid-star-dict ()
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (if mark-active
        (setq begin (region-beginning)
              end (region-end))
      (save-excursion
        (backward-word)
        (mark-word)
        (setq begin (region-beginning)
              end (region-end))))
    ;; 有时候 stardict 会很慢，所以在回显区显示一点东西
    ;; 以免觉得 Emacs 在干什么其他奇怪的事情。
    (message "searching for %s ..." (buffer-substring begin end))
    (tooltip-show
     (shell-command-to-string
      (concat "sdcv -n "
              (buffer-substring begin end))))))


;;插入日期时间
;;{{{
;; insert current date
(defun my-insert-date ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d %a")))

;; insert current time
(defun my-insert-time ()
  (interactive)
  (insert (format-time-string "%p %I:%M")))

(global-set-key "\C-c\C-d" 'my-insert-date)
(global-set-key "\C-c\C-t" 'my-insert-time)

;启动时最大化
(my-maximized) 
(global-set-key [\C-f11] 'my-fullscreen)

(global-set-key (quote [M-up]) (quote move-region-up))
(global-set-key (quote [M-down]) (quote move-region-down))
(global-set-key "\C-c\C-c2" 'circle-windows)

(add-hook 'shell-mode-hook 'my-shell-mode-hook-func)
(defun my-shell-mode-hook-func ()
  (set-process-sentinel (get-buffer-process (current-buffer))
         #'my-shell-mode-kill-buffer-on-exit)
     )
(defun my-shell-mode-kill-buffer-on-exit (process state)
     (message "%s" state)
     (if (or
     (string-match "exited abnormally with code.*" state)
     (string-match "finished" state))
     (kill-buffer (current-buffer))))
(global-set-key "\C-x\M-r" 'term)
