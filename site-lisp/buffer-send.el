
(require 'windmove)


;;;###autoload
(defun buf-send-up ()
  "Send the current buffer to the window above the split.
The current window will display the previous buffer.
If there is no split, ie no window above the current one, an
error is signaled."
;;  "Switches between the current buffer, and the buffer above the
;;  split, if possible."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'up))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window above this one")
      ;; swap top with this one
      ;(set-window-buffer (selected-window) (previous-buffer ()))
      (previous-buffer)
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-send-down ()
"Send the current buffer to the window under the split.
The current window will display the previous buffer.
If there is no split, ie no window under the current one, an
error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'down))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (or (null other-win) 
            (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
        (error "No window under this one")
      ;; swap top with this one
      ;(set-window-buffer (selected-window) (window-buffer other-win))
      (previous-buffer)
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-send-left ()
"Send the current buffer to the window on the left of the split.
The current window will display the previous buffer.
If there is no split, ie no window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (previous-buffer)
      ;(set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

;;;###autoload
(defun buf-send-right ()
"Send the current buffer to the window on the right of the split.
The current window will display the previous buffer.
If there is no split, ie no window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
	 (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (previous-buffer)
      ;(set-window-buffer (selected-window) (previous-buffer ()))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))


(provide 'buffer-send)
