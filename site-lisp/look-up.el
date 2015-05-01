;;; This is look-up to look up the word at point on the internet, on various websites.

;;;###autoload
(defun look-up-leo ()
  "Look up word at point with leo in web browser"
  (interactive)
  (browse-url (format "http://dict.leo.org/ende/?lp=ende&lang=de&searchLoc=0&cmpType=relaxed&sectHdr=on&spellToler=on&chinese=both&pinyin=diacritic&search=%s"
		      (if (use-region-p) 
			  (buffer-substring (region-beginning) (region-end))
			(word-at-point)))))

;;;###autoload
(defun look-up-wikipedia-en ()
  "Look up word at point with wikipedia (English) in web browser"
  (interactive)
  (browse-url (format "http://en.wikipedia.org/wiki/%s" 
		      (if (use-region-p) 
			  (buffer-substring (region-beginning) (region-end))
			(word-at-point)))))

;;;###autoload
(defun look-up-wikipedia-de ()
  "Look up word at point with wikipedia (English) in web browser"
  (interactive)
  (browse-url 
   (format "http://de.wikipedia.org/wiki/%s" 
	   (if (use-region-p) 
	       (buffer-substring (region-beginning) (region-end))
	     (word-at-point)))))

;;;###autoload
(defun look-up-oxford ()
  "Look up word at point with wikipedia (English) in web browser"
  (interactive)
  (browse-url (format "http://www.oxforddictionaries.com/definition/english/%s?q=%s" 
		      (thing-at-point 'word) (thing-at-point 'word))))

;;;###autoload
(defun look-up-duckduckgo ()
  "Look up word at point with DuckDuckGo in web browser"
  (interactive)
  (browse-url (format "https://duckduckgo.com/?q=%s" (word-at-point))))

;;;###autoload
(defun look-up-google ()
  "Look up word at point with Google in web browser"
  (interactive)
  (browse-url (format "https://www.google.de/search?q=%s" 
		      (if (use-region-p) 
			  (buffer-substring (region-beginning) (region-end))
			(word-at-point)))))

