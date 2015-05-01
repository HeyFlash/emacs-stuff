
(defun eol-char-p (character)
  "Returns nil iff character is not anything that indicates 
end of line, carriage return, etc."
  (or (equal character ?\n)
      (equal character ?\r)))

(defun equal-syntax-current-next-p (advancer)
  "Returns t if current and next character belong to the same syntax group, nil otherwise."
  (let ((cc (char-after (point)))
	(nc (char-after (+ (point) advancer))))
    (and cc nc
	 (equal (eol-char-p cc)
		(eol-char-p nc))
	 (equal (char-syntax cc)
		(char-syntax nc)))))

(defun equal-syntax-p (char1 char2)
  "Returns t if current and next character belong to the same syntax group, nil otherwise."
  (and char1 char2
       (equal (eol-char-p char1)
	      (eol-char-p char2))
       (equal (char-syntax char1)
	      (char-syntax char2))))

(defun equal-eol-current-next-p (advancer)
  "Returns t if current and next character are neither or both eol characters."
  (let ((cc (char-after (point)))
	(nc (char-after (+ (point) advancer))))
    (and cc nc
	 (or (and (eol-char-p cc)
		  (eol-char-p nc))
	     (not (or (eol-char-p cc)
		      (eol-char-p nc)))))))

(defun forward-word-intelligently (&optional n keep-single-start)
  "Move point forward over n sequences according to the following rules:
A sequence is a sequence of characters belonging to the same syntax group,
optionally preceded by a single character of another syntax group.
If keep-single-start is non-nil, a sequence is only one syntax group.
If n < 0 move backwards and inverse sequence read order, 
i.e. the single character is always at point."
  (interactive "^p")
  (unless n (setq n 1))
  (let ((advancer (if (< n 0) -1 1))
	(moved nil)
	(sequence-length 0))
    (dotimes (i (abs n)) 
      ;;move over the first character if it is single 
      ;;(i.e. it belongs to a different syntax group than the next)
      ;;unless one of the following applies:
      (unless (or keep-single-start ;keeping the first is explicit
		  ;;there is no single first:
		  (equal-syntax-current-next-p advancer)
		  ;;moving forward and at end of line
		  (and (> n 0) (not (equal-eol-current-next-p advancer)))
		  ;;moving backward and just passed end of line
		  (and (< n 0) (not (equal-eol-current-next-p advancer)) 
		        (eol-char-p (point))))
	(forward-char advancer)
	(setq moved t))
      ;;move over all characters of the same syntax group:
      (while (equal-syntax-current-next-p advancer)
	(forward-char advancer)
	(setq moved t)
	(setq sequence-length (1+ sequence-length)))
      ;;move one character beyond the syntax group unless:
      (unless (or (and (< n 0) ;moving backwards,
		       moved)   ;the point was move
		       ;; and we're at a linebreak
		       ;(not (equal-eol-current-next-p advancer)))
		  ;; or
		  (and moved ;the point was moved
		       ;; and the next char would be in a word:
		       (equal (char-syntax (char-after (+ (point) advancer))) ?w)))
	(forward-char advancer)))))

(defun backward-word-intelligently (&optional n keep-single-start)
  "Move point forward over n sequences according to the following rules:
A sequence is a sequence of characters belonging to the same syntax group,
optionally preceded by a single character of another syntax group.
If keep-single-start is non-nil, a sequence is only one syntax group.
If n < 0 move backwards and inverse sequence read order, 
i.e. the single character is always at point."
  (interactive "^p")
  (forward-word-intelligently (- n)))


(defun right-word-intelligently (&optional n keep-single-start)
  "Move point n words to the right (to the left if n is negative).
Depending on the bidirectional context, this may move either forward
or backward in the buffer.  This is in contrast with \\[forward-word-intelligently]
and \\[backward-word-intelligently]."
  (interactive "^p")
  (if (eq (current-bidi-paragraph-direction) 'left-to-right)
      (forward-word-intelligently n keep-single-start)
    (backward-word-intelligently n)))

(defun left-word-intelligently (&optional n keep-single-start)
  "Move point n words to the left (to the right if n is negative).
Depending on the bidirectional context, this may move either backward
or forward in the buffer.  This is in contrast with \\[backward-word-intelligently]
and \\[forward-word-intelligently]."
  (interactive "^p")
  (right-word-intelligently (- n)))

(defun kill-word-intelligently (arg)
  "Kill characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-region (point) (progn (forward-word-intelligently arg) (point))))

(defun backward-kill-word-intelligently (arg)
  "Kill characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-word-intelligently (- arg)))

(defun delete-word-intelligently (arg)
  "Delete characters forward until encountering the end of a word.
Don't add them to the kill ring.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word-intelligently arg) (point))))

(defun backward-delete-word-intelligently (arg)
  "Delete characters backward until encountering the beginning of a word.
Don't add them to the kill ring.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word-intelligently (- arg)))

(global-set-key (kbd "<C-right>")     'right-word-intelligently)
(global-set-key (kbd "<C-left>")      'left-word-intelligently)
