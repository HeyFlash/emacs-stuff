;;;###autoload
(defun ebib-unjabref-filename (filename)
  "Remove the jabref file markings"
  (or (and (> (length filename) 0)
	   (char-equal ?: (elt filename 0))
	   (let ((splitlist (butlast (split-string filename ":" t)))
		 (new-filename))
	     (dolist (cur splitlist new-filename)
	       (setq new-filename (concat new-filename cur)))))
      filename))

;;;###autoload
(defun ebib-call-file-viewer (filename &optional n)
  "Open FILENAME with an external viewer.
FILENAME can also be a string of filenames separated by
`ebib-filename-separator', in which case the Nth file is
opened. If N is NIL, the user is asked to enter a number."
  (let ((files (split-string filename ebib-filename-separator t)))
    (cond
     ((null (cdr files))                ; there's only one file
      (setq n 1))
     ((not (integerp n))  ; the user did not pass a numeric prefix argument
      (setq n (string-to-number (read-string (format "Select file to open [1-%d]: " (length files)))))))
    (if (or (< n 1)    ; if the user provided a number that is out of range
            (> n (length files)))
        (setq n 1))
    (let* ((file (ebib-unjabref-filename (nth (1- n) files)))
           (file-full-path
            (or (locate-file file ebib-file-search-dirs)
                (locate-file (file-name-nondirectory file) ebib-file-search-dirs)
                (expand-file-name file))))
      (if (file-exists-p file-full-path)
          (let ((ext (file-name-extension file-full-path)))
            (ebib-ifstring (viewer (cdr (assoc ext ebib-file-associations)))
                (progn
                  (message "Executing `%s %s'" viewer file-full-path)
                  (start-process (concat "ebib " ext " viewer process") nil viewer file-full-path))
              (message "Opening `%s'" file-full-path)
              (ebib-lower)
              (find-file file-full-path)))
        (error "File not found: `%s'" file)))))

;;;###autoload
(defun ebib-open-cited-file (&optional n)
  "Open the file that is cited at point.
  If the filename of the citation in the bib file is separated by
`ebib-filename-separator', the Nth file is
opened. If N is NIL, the user is asked to enter a number."
  (interactive "^P")
  (let* ((macro (car (reftex-what-macro-safe 1)))
         (key (reftex-this-word "^{}%\n\r, \t")))
    (if (or (null macro) (reftex-in-comment)
    	    (not (string-match "\\`\\\\cite\\|cite\\*?\\'\\|bibentry" macro)))
    	(error "Point not on a citation")
      (let ((filename (ebib-db-get-field-value ebib-standard-file-field key ebib-cur-db 'noerror 'unbraced 'xref)))
	(if (listp filename)
	    (setq filename (car filename)))
	(message (format "macro: %s; key: %s; filename: %s" macro key filename))
    	(if filename
    	    (ebib-call-file-viewer filename n)
    	  (error "Field `%s' is empty" ebib-standard-file-field))))))

;;;###autoload
(defun reftex-goto-label-at-point (&optional other-window)
  "Jump to the label that is under the point."
  (interactive "P")
  (let* ((macro (car (reftex-what-macro-safe 1)))
         (key (reftex-this-word "^{}%\n\r, \t")))
    (if (or (null macro) (reftex-in-comment)
    	    (not (string-match "\\`\\\\ref\\|ref\\*?\\'\\" macro)))
    	(error "Point not on a reference")
      (reftex-goto-label))))

;;;###autoload
(defun reftex-toc-get-current-label ()
  "Return the label at the current line in the toc buffer. Return nil if not at such a label"
  (save-excursion
    (move-beginning-of-line nil)
    (set-mark (point))
    (move-end-of-line nil)
    (let ((line (buffer-substring-no-properties (mark) (point)))
	  (idx 0))
      (if (char-equal ?> (aref line 0))
	  (progn
	    (setq idx (1+ idx))
	    (while (char-equal ?\s (aref line idx))
	      (setq idx (1+ idx)))
	    (substring line idx))
	nil))))

;;;###autoload
(defun reftex-toc-insert-label-previous-window ()
  "Insert the current label in the toc into the other window."
  (interactive)
  (let ((lrbuf (other-buffer (current-buffer) t))
	(label (reftex-toc-get-current-label)))
    (if label
	(progn
	  (pop-to-buffer lrbuf)
	  (insert label))
      (message "No label is currently selected"))))


;;;###autoload
(defun reftex-toc-get-current-reference ()
  "Get the current label in toc as a reference.
   Ask for a reference style, and construct the reference accordingly."
  (let ((reftex-refstyle (when (and (boundp 'reftex-refstyle) reftex-refstyle)
		    reftex-refstyle))
	(reftex-format-ref-function reftex-format-ref-function)
	(form "\\ref{%s}")
	label labels sep sep1 style-alist)
    (unless reftex-refstyle
      (if reftex-ref-macro-prompt
	  (progn
	    ;; Build a temporary list which handles more easily.
	    (dolist (elt reftex-ref-style-alist)
	      (when (member (car elt) (reftex-ref-style-list))
		(mapc (lambda (x)
			(add-to-list 'style-alist (cons (cadr x) (car x)) t))
		      (nth 2 elt))))
	    ;; Prompt the user for the macro.
	    (let ((key (reftex-select-with-char
			"" (concat "SELECT A REFERENCE FORMAT\n\n"
				   (mapconcat
				    (lambda (x)
				      (format "[%c] %s  %s" (car x)
					      (if (> (car x) 31) " " "")
					      (cdr x)))
				    style-alist "\n")))))
	      (setq reftex-refstyle (cdr (assoc key style-alist)))
	      (unless reftex-refstyle
		(error "No reference macro associated with key `%c'" key))))
	;; Get the first macro from `reftex-ref-style-alist' which
	;; matches the first entry in the list of active styles.
	(setq reftex-refstyle
	      (or (caar (nth 2 (assoc (car (reftex-ref-style-list))
				      reftex-ref-style-alist)))
		  ;; Use the first entry in r-r-s-a as a last resort.
		  (caar (nth 2 (car reftex-ref-style-alist)))))))
    (setq label (reftex-toc-get-current-label))
    (if label
	(progn
	  ;; remove ~ 
	  ;;(setq form (substring form 1))
	  ;; do we have a special format?
	  (unless (string= reftex-refstyle "\\ref")
	    (setq reftex-format-ref-function 'reftex-format-special))
	  (if reftex-format-ref-function
	      (funcall reftex-format-ref-function label form reftex-refstyle)
	    (format form label label)))
      nil)))

;;;###autoload
(defun reftex-toc-insert-reference-previous-window ()
  "Insert the current label in the toc as a reference into the other window.
   Ask for a reference style, and construct the reference accordingly."
  (interactive)
  (let ((lrbuf (other-buffer (current-buffer) t))
	(label (reftex-toc-get-current-reference)))
    (if label
	(progn
	  (pop-to-buffer lrbuf)
	  (insert label))
      (message "No label is currently selected"))))


;;;###autoload
(defun reftex-save-all-document-buffers ()
  "Save all documents associated with the current document.
The function is useful after a global action like replacing or renumbering
labels."
  (interactive)
  (let ((files (reftex-all-document-files))
	(saved-buffer-count 0)
        file buffer)
    (save-current-buffer
      (while (setq file (pop files))
        (setq buffer (reftex-get-buffer-visiting file))
        (when buffer
          (set-buffer buffer)
	  (when (buffer-modified-p)
	    (setf saved-buffer-count (1+ saved-buffer-count)))
	  (save-buffer))))
    (message "%i buffers saved." saved-buffer-count)))
