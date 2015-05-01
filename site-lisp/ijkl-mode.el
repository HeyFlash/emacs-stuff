

(define-minor-mode ijkl-moves-cursor-mode
  "A minor mode that uses ijkl as a replacement for the cursor keys."
  nil " IJKL" 
  (let ((ijkl-moves-cursor-mode-map (make-sparse-keymap)))
    (define-key ijkl-moves-cursor-mode-map (kbd "p") 'backward-paragraph)
    (define-key ijkl-moves-cursor-mode-map (kbd "n") 'forward-paragraph)
    (define-key ijkl-moves-cursor-mode-map (kbd "C-i") 'backward-paragraph)
    (define-key ijkl-moves-cursor-mode-map (kbd "C-k") 'forward-paragraph)
    (define-key ijkl-moves-cursor-mode-map (kbd "C-j") 'left-word-intelligently)
    (define-key ijkl-moves-cursor-mode-map (kbd "C-l") 'right-word-intelligently)
    (define-key ijkl-moves-cursor-mode-map (kbd "i") 'previous-line)
    (define-key ijkl-moves-cursor-mode-map (kbd "k") 'next-line)
    (define-key ijkl-moves-cursor-mode-map (kbd "j") 'left-char)
    (define-key ijkl-moves-cursor-mode-map (kbd "l") 'right-char)
    (define-key ijkl-moves-cursor-mode-map (kbd "o") 'move-end-of-line)
    (define-key ijkl-moves-cursor-mode-map (kbd "u") 'move-beginning-of-line)
    ijkl-moves-cursor-mode-map))

(global-set-key (kbd "s-SPC") 'ijkl-moves-cursor-mode)
