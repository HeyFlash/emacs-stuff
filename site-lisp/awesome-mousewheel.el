(defvar amw-original-point-pos 0
  "Stores the position of the cursor when wheel scrolling starts")

(defvar amw-original-cursor-type nil
  "Stores the original look of the cursor.")

(defun amw-init-mouse-wheel-scroll (event)
  "Do the initialization for the mouse wheel mode.
Then scroll according to event"
  (interactive (list last-input-event))
  (setq amw-original-point-pos (point))
  (amw-scrolling-mode 1)
  (setq amw-original-cursor-type cursor-type)
  (setq cursor-type nil)
  (mwheel-scroll event))

(defun amw-end-mouse-wheel-scroll ()
  "End the mouse wheel mode. Set the point to the original pos."
  (interactive)
  (amw-scrolling-mode -1)
  (goto-char amw-original-point-pos)
  (setq cursor-type amw-original-cursor-type))


(defvar amw-temp-keymap nil
  "Keymap while temp-mouse-wheel-mode is active.
Rebinds the mouse wheel to the standard emacs settings.")
(unless amw-temp-keymap
  (setq amw-temp-keymap (make-sparse-keymap))
  (define-key amw-temp-keymap (kbd "<wheel-up>") 'mwheel-scroll)
  (define-key amw-temp-keymap (kbd "<wheel-down>") 'mwheel-scroll)
  (define-key amw-temp-keymap (kbd "SPC") 'amw-end-mouse-wheel-scroll))


(global-set-key (kbd "<wheel-down>") 'amw-init-mouse-wheel-scroll)
(global-set-key (kbd "<wheel-up>") 'amw-init-mouse-wheel-scroll)



(define-minor-mode amw-scrolling-mode
  "Temporary minor mode for awesome mouse wheel."
   nil nil amw-temp-keymap)
