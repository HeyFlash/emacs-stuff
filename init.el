; Local Variables:
; coding: raw-text
; End:
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command-style
   (quote
    (("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))))
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list
   (quote
    (("sumatra"
      ("\"C:/Program Files (x86)/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
       (mode-io-correlate " -forward-search %b %n")
       " %o")))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and start")
     (output-dvi "Yap")
     (output-pdf "sumatra")
     (output-html "start"))))
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auto-revert-verbose nil)
 '(bidi-paragraph-direction (quote left-to-right))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (dark-vs-theme2)))
 '(custom-safe-themes
   (quote
    ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "025354235e98db5e7fd9c1a74622ff53ad31b7bde537d290ff68d85665213d85" "0c6f44db0b0739514a11e7fad989446aeddd0a7b41a32b0f17aa608ea29c9abd" "81969559c60646a805f3d0d022e48832aaf5c522f2107abf95d8da5a0aa43658" default)))
 '(doc-view-continuous t)
 '(doc-view-ghostscript-program "C:\\Program Files\\ghostscript\\gs8.71\\bin\\gswin32c")
 '(ebib-bib-search-dirs (quote ("~" "h:/nexus1/papers")))
 '(ebib-file-associations
   (quote
    (("pdf" . "C:/Program Files (x86)/Foxit Reader/Foxit Reader.exe")
     ("ps" . "gv"))))
 '(ebib-file-search-dirs (quote ("~" "h:/nexus1/papers")))
 '(ebib-index-display-fields (quote (author title year)))
 '(ebib-preload-bib-files (quote ("papers.bib")))
 '(ebib-window-vertical-split nil)
 '(ede-project-directories (quote ("/home/flash/dev/sfml-src")))
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#lisp" "#clasp"))))
 '(erc-nick-uniquifier "-")
 '(fill-column 80)
 '(font-latex-match-biblatex-keywords (quote (("seecite" "{") ("citet" "[{") ("citep" "[{"))))
 '(font-latex-match-function-keywords nil)
 '(font-latex-match-italic-command-keywords (quote (("lstcapemph" "{"))))
 '(font-latex-match-reference-keywords
   (quote
    (("reffig" "{")
     ("reffigpage" "{")
     ("refsec" "{")
     ("refsecname" "{")
     ("refchap" "{")
     ("refchapname" "{")
     ("reftable" "{")
     ("reflst" "{")
     ("reflstpage" "{")
     ("autoref" "{")
     ("refline" "{")
     ("reflines" "{{"))))
 '(font-latex-match-sectioning-1-keywords (quote (("imagechapter" "{") ("tikzchapter" "{"))))
 '(font-latex-match-textual-keywords (quote (("dcaption" "[{") ("dsubcaption" "[{"))))
 '(font-latex-user-keyword-classes
   (quote
    (("XML-Highlighting"
      (("ttag" "{")
       ("attrib" "{")
       ("gtlcode" "{")
       ("attval" "{"))
      (:foreground "cornflower blue")
      command)
     ("Other-Highlighting"
      (("activity" "{"))
      (:foreground "dark turquoise")
      command))))
 '(icicle-file-sort (quote icicle-dirs-first-p))
 '(icicle-find-file-expand-directory-flag t)
 '(org-agenda-files (quote ("h:/nexus1/papers/eigene/diss/org/diss.org")))
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-window-setup (quote current-window))
 '(preview-gs-command "gswin32c.exe")
 '(preview-scale-function 1.2)
 '(recenter-positions (quote (0.25 top bottom)))
 '(reftex-auto-recenter-toc t)
 '(reftex-ref-macro-prompt t)
 '(reftex-ref-style-alist
   (quote
    (("Default" t
      (("\\ref" 13)
       ("\\pageref" 112)))
     ("Varioref" "varioref"
      (("\\vref" 118)
       ("\\vpageref" 103)
       ("\\Vref" 86)
       ("\\Ref" 82)))
     ("Fancyref" "fancyref"
      (("\\fref" 102)
       ("\\Fref" 70)))
     ("Hyperref" "hyperref"
      (("\\autoref" 97)
       ("\\autopageref" 117)))
     ("Myref" "hyperref"
      (("\\ref" 114)
       ("\\reffig" 102)
       ("\\refsec" 115)
       ("\\refchap" 99)
       ("\\reftable" 116)
       ("\\autoref" 97)
       ("\\pageref" 112))))))
 '(reftex-ref-style-default-list (quote ("Myref")))
 '(reftex-section-levels
   (quote
    (("part" . 0)
     ("chapter" . 1)
     ("imagechapter" . 1)
     ("tikzchapter" . 1)
     ("section" . 2)
     ("subsection" . 3)
     ("subsubsection" . 4)
     ("paragraph" . 5)
     ("subparagraph" . 6)
     ("addchap" . -1)
     ("addsec" . -2))))
 '(reftex-toc-max-level 2)
 '(reftex-toc-split-windows-horizontally t)
 '(show-paren-mode t)
 '(slime-auto-connect (quote ask))
 '(slime-auto-start (quote ask))
 '(slime-complete-symbol-function (quote slime-fuzzy-complete-symbol))
 '(tool-bar-mode nil)
 '(transient-mark-mode nil)
 '(unibyte-display-via-language-environment t)
 '(windmove-wrap-around t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#22282A" :foreground "#C8C8C8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 115 :width normal :foundry "bitstream" :family "Courier 10 Pitch"))))
 '(col-highlight ((t (:background "gray28"))))
 '(hl-line ((t (:background "gray28"))))
 '(preview-face ((t nil)))
 '(preview-reference-face ((t (:background "white" :foreground "black")))))
(set-default 'cursor-type 'box)
(setq inhibit-splash-screen t)
(setq column-number-mode t)
;;(prefer-coding-system 'utf-8)

;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
;; (setq w32-pass-lwindow-to-system nil
;;       w32-pass-rwindow-to-system nil
;;       w32-pass-apps-to-system nil
;;       w32-lwindow-modifier 'hyper ; Left Windows key
;;       w32-rwindow-modifier 'hyper ; Right Windows key
;;       w32-apps-modifier 'super) ; Menu key

(setq inferior-lisp-program "sbcl")


;;  This makes Emacs ignore the "-e (make-frame-visible)" 
;;  that it gets passed when started by emacsclientw.
;;
(add-to-list 'command-switch-alist '("(make-frame-visible)" .
                 (lambda (s))))

;package manager - needed for all the package-list-packages to work?
(require 'package)
(add-to-list 'package-archives 
    '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives 
    '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;org-link-minor-mode
;(require 'org-link-minor-mode)

;;  This starts the Emacs server when .emacs gets loaded
;;
(require 'server)
(if (not (server-running-p)) (server-start))

;;  This changes C-x C-c to just hide Emacs until the next
;;  time you use it.  We rebind C-M-c to be the command to
;;  really kill Emacs.
;;
(defun my-done ()
  "Exit server buffers and hide the main Emacs window"
  (interactive)
  (server-edit)
  (make-frame-invisible nil t))

(defun server-remove-kill-buffer-hook () 
  "This function is used to remove the 'buffer still has clients' message"
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)) 
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)

; Add everything here that has to be done after the packages of
; the package manager have been loaded.
(defun after-package ()
  "Initialize everything after package manager has loaded"
  (global-undo-tree-mode)
  (setq sml/theme 'dark)
  (sml/setup)
  (diminish 'undo-tree-mode "UT")
  (paren-activate)
  (icy-mode t)
  (global-auto-complete-mode t)
  (setq slime-contribs '(slime-repl slime-fuzzy slime-references slime-autodoc)))
  ; sublimity for minimap and smooth scrolling
  ;; (require 'sublimity)
  ;; (require 'sublimity-scroll)
  ;; (require 'sublimity-map))

(add-hook 'after-init-hook 'after-package)

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(eval-after-load 'slime
  '(progn
     (define-key slime-mode-map (kbd "C-c h l") 'hyperspec-lookup)))

; Toggle window dedication
; A dedicated window will not switch the displayed buffer automatically
; I.e. anything that tries to find another window to display stuff, ignores it.
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not.
   A dedicated window will not switch the displayed buffer automatically,
   i.e. anything that tries to find another window to display stuff, ignores it."
  (interactive)
  (message 
   (if (let (window (get-buffer-window (current-buffer)))
	 (set-window-dedicated-p window (not (window-dedicated-p window))))
    "Window '%s' is dedicated"
    "Window '%s' is normal")
 (current-buffer)))

;cl is required for ace-jump
;(require 'cl)

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(load "forward-experiment")
(load "look-up")
(require 'buffer-send)


;; ;;; Set up PSGML
;; ; Add PSGML to load-path so Emacs can find it.
;; (setq load-path (append (list nil "../site-lisp/psgml132") load-path))
;; ; Use PSGML for sgml and xml major modes.
;; (autoload 'sgml-mode "psgml" "Major mode to edit SGML files." t)
;; (autoload 'xml-mode "psgml" "Major mode to edit XML files." t)
;; (setq sgml-set-face t)
;; (setq sgml-auto-activate-dtd t)
;; (setq sgml-indent-data t)

;activate visual line-mode with LaTeX
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
; activate reftex with auctex
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
;(add-hook 'LaTeX-mode-hook (lambda () (reftex-mode t)))
;(add-hook 'LaTeX-mode-hook (lambda () (org-link-minor-mode t)))
;(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;(add-hook 'LaTeX-mode-hook 'turn-on-flyspell) ;active flyspell with LaTeX
(setq-default latex-run-command "pdflatex root")
(setq TeX-PDF-mode t)
(setq-default TeX-master nil) ; query for master file
(eval-after-load 'latex 
                 '(define-key LaTeX-mode-map (kbd "C-c p a") 'reftex-parse-all)) ;parse all
(eval-after-load 'latex 
                 '(define-key LaTeX-mode-map (kbd "C-c p f") 'reftex-parse-one)) ;parse file
; set aspell as spell checker
;(setq-default ispell-program-name "aspell")
;(setq flyspell-issue-message-flag nil)

; Deactivate C-tab for org-mode, so that buffer cycling works with org files.
(add-hook 'org-mode-hook
          '(lambda () (define-key org-mode-map [(control tab)] nil)))

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "C-<right>") nil)
     (define-key paredit-mode-map (kbd "C-<left>") nil)
     (define-key paredit-mode-map (kbd "s-s <right>") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "s-s <left>") 'paredit-forward-barf-sexp)
     (define-key paredit-mode-map (kbd "C-M-<right>") nil)
     (define-key paredit-mode-map (kbd "C-M-<left>") nil)
     (define-key paredit-mode-map (kbd "s-s M-<right>") 'paredit-backward-barf-sexp)
     (define-key paredit-mode-map (kbd "s-s M-<left>") 'paredit-backward-slurp-sexp)))

;enable ido - interactively switch buffers & select files
;(require 'ido)
;(ido-mode t)

;undo-tree-mode
;(require 'undo-tree)
;(global-undo-tree-mode)
;(eval-after-load "undo-tree" (global-undo-tree-mode t))

;ace-jump-mode
;(require 'ace-jump-mode)

;enable cycle-buffer
(autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
(autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
;;(autoload 'cycle-buffer-permissive "cycle-buffer" "Cycle forward allowing *buffers*." t)
;;(autoload 'cycle-buffer-backward-permissive "cycle-buffer" "Cycle backward allowing *buffers*." t)
(autoload 'cycle-buffer-toggle-interesting "cycle-buffer" "Toggle if this buffer will be considered." t)
(setq cycle-buffer-allow-visible t)

;autorevert buffers
(global-auto-revert-mode t)

;set find-file-at-point key bindings. Open file under cursor, if available.
;(ffap-bindings)
;this does not bind icicle-find-file, therefore we bind ffap to C-f

(fset 'one-sentence-per-line
      [home C-up down C-right return delete up C-S-down S-left ?\M-% ?\C-q ?\C-j return ?  return ?! home C-up down C-S-down S-left ?\M-% ?. ?  return ?. ?\C-q ?\C-j return ?! C-up C-down])


(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;use minor mode to override auctex's C-c C-f font keybindings.
(define-key my-keys-minor-mode-map (kbd "C-c C-f") 'find-file-at-point)
(define-minor-mode my-keys-minor-mode
  "A minor mode so that some key settings override annoying major modes."
  t "" 'my-keys-minor-mode-map)
(my-keys-minor-mode 1)

;scrolling behaviour
(setq
  scroll-margin 5 ;how many lines away from top or bottom should scrolling start?                  
  scroll-conservatively 100000 ;how far away from the screen center should point be placed?
  scroll-preserve-screen-position 1) ;preserve point position on screen when pgup / pgdown
(defun scroll-up-no-move ()
  "Scroll up the buffer contents by one line without moving the point"
  (interactive)
  (scroll-up 1))
(defun scroll-down-no-move ()
  "Scroll up the buffer contents by one line without moving the point"
  (interactive)
  (scroll-down 1))
(defun scroll-up-keep-point ()
  "Scroll up the buffer contents by one line, keeping the point at the same position in the text"
  (interactive)
  (scroll-up 1)
  (previous-line))
(defun scroll-down-keep-point ()
  "Scroll up the buffer contents by one line, keeping the point at the same position in the text"
  (interactive)
  (scroll-down 1)
  (next-line))

(defun kill-word-gently ()
  "Kill characters forward according to these rules:
If on a word or at its beginning, kill the rest of the word.
If not on a word, kill everything between the cursor and the beginning of the next word."
  (interactive)
  (let*((current (point)) 
	(eol (progn (end-of-line) (point)))
	(after-word (progn (goto-char current) (forward-word) (point)))
	(before-word (progn (backward-word) (point))))
    (goto-char current)
    (cond ((= current eol) (delete-char 1))
	  ((< eol before-word)(kill-region current eol))
	  ((> before-word current) (kill-region current before-word))
	  (t (kill-word nil)))))

(defun insert-brackets-point-outside (open-brackets close-brackets)
  "Insert brackets, and place point outside, depending on where it was placed originally
If a region is active, enclose the region in curly braces, otherwise insert empty braces."
  (interactive)
  (if (use-region-p)
      (let ((beginning (region-beginning))
	    (end (region-end))
	    (orig-pos (point)))
	(goto-char beginning) (insert open-brackets)
	(goto-char end) (forward-char) (insert close-brackets)
	(goto-char orig-pos)
	(if (= orig-pos beginning)
	    (backwar-char 2)
	  (forward-char 2)))
    (insert (concat open-brackets close-brackets))))

(defun insert-brackets-point-after (open-brackets close-brackets)
  "Insert brackets, and place point after them. 
If a region is active, enclose the region in curly braces, otherwise insert empty braces."
  (interactive)
  (if (use-region-p)
      (let ((beginning (region-beginning))
	    (end (region-end)))
	(goto-char beginning) (insert open-brackets)
	(goto-char end) (forward-char) (insert close-brackets))
    (insert (concat open-brackets close-brackets))))

(defun insert-brackets-point-inside (open-brackets close-brackets &optional reverse-pos)
  "Insert brackets, and place point inside, depending on where it was placed originally. 
If a region is active, enclose the region in curly braces, otherwise insert empty braces."
  (interactive)
  (if (use-region-p)
      (let ((beginning (region-beginning))
	    (end (region-end))
	    (end-pos (if reverse-pos (mark) (point))))
	(goto-char beginning) (insert open-brackets)
	(goto-char end) (forward-char) (insert close-brackets)
	(goto-char end-pos) (forward-char))
    (progn (insert (concat open-brackets close-brackets))
	   (backward-char))))

(global-set-key (kbd "s-0") (lambda () (interactive) (insert-brackets-point-after "{" "}")))
(global-set-key (kbd "s-7") (lambda () (interactive) (insert-brackets-point-inside "{" "}")))
(global-set-key (kbd "M-s-0") (lambda () (interactive) (insert-brackets-point-after "{" "}" t)))
(global-set-key (kbd "M-s-7") (lambda () (interactive) (insert-brackets-point-inside "{" "}" t)))
(global-set-key (kbd "s-9") (lambda () (interactive) (insert-brackets-point-after "[" "]")))
(global-set-key (kbd "s-8") (lambda () (interactive) (insert-brackets-point-inside "[" "]")))
(global-set-key (kbd "s-)") (lambda () (interactive) (insert-brackets-point-after "(" ")")))
(global-set-key (kbd "s-(") (lambda () (interactive) (insert-brackets-point-inside "(" ")")))
(global-set-key (kbd "s-2") (lambda () (interactive) (insert-brackets-point-inside "\"" "\"")))

(global-set-key (kbd "C-ö") 'delete-region) ;ömmeln
(global-set-key (kbd "C-x C-c") 'my-done)
(global-set-key [M-f4] 'save-buffers-kill-terminal)
(global-set-key [C-f4] 'kill-this-buffer)
(global-set-key [C-tab] 'cycle-buffer)
(global-set-key [C-S-tab] 'cycle-buffer-backward)
(global-set-key [C-S-iso-lefttab] 'cycle-buffer-backward)
(global-set-key [scroll] 'toggle-window-dedicated)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <M-left>") 'buf-move-left)
(global-set-key (kbd "C-c <M-right>") 'buf-move-right)
(global-set-key (kbd "C-c <M-up>") 'buf-move-up)
(global-set-key (kbd "C-c <M-down>") 'buf-move-down)
(global-set-key (kbd "C-c <C-left>") 'buf-send-left)
(global-set-key (kbd "C-c <C-right>") 'buf-send-right)
(global-set-key (kbd "C-c <C-up>") 'buf-send-up)
(global-set-key (kbd "C-c <C-down>") 'buf-send-down)
(global-set-key (kbd "C-c r") 'windresize)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-s-i") 'backward-paragraph)
(global-set-key (kbd "M-s-k") 'forward-paragraph)
(global-set-key (kbd "M-s-j") 'left-word)
(global-set-key (kbd "M-s-l") 'right-word)
(global-set-key (kbd "s-i") 'previous-line)
(global-set-key (kbd "s-k") 'next-line)
(global-set-key (kbd "s-j") 'left-char)
(global-set-key (kbd "s-l") 'right-char)
(global-set-key (kbd "s-o") 'move-end-of-line)
(global-set-key (kbd "s-u") 'move-beginning-of-line)

(global-set-key (kbd "C-k") nil) ;I accidentally killed lines all the time
(global-set-key (kbd "M-l") nil) ;I accidentally lowercased words as well...
(global-set-key (kbd "C-c t l") 'toggle-truncate-lines)

(global-set-key (kbd "<M-up>") 'scroll-up-no-move)
(global-set-key (kbd "<M-down>") 'scroll-down-no-move)
(global-set-key (kbd "<C-M-up>") 'scroll-up-keep-point)
(global-set-key (kbd "<C-M-down>") 'scroll-down-keep-point)
(global-set-key (kbd "C-c b") 'cycle-buffer-toggle-interesting)
(global-set-key (kbd "C-c f") 'find-file-at-point) ;open file under cursor
(global-set-key (kbd "C-c tab") 'indent-region)
(global-set-key (kbd "C-c u") 'uncomment-region) ;comment uncomment
(global-set-key (kbd "C-c c") 'comment-region)   ;comment comment
(global-set-key (kbd "C-c j") 'ace-jump-mode)  ;ace jump
(global-set-key (kbd "C-c m") 'mc/mark-more-like-this-extended) ;multiple cursor
(global-set-key (kbd "C-c e") 'er/expand-region) ;expand region
(global-set-key (kbd "C-c w") 'flyspell-check-previous-highlighted-word) ;correct word
(global-set-key (kbd "C-c y") 'browse-kill-ring) ;yank visually
;;goto previous change:
(global-set-key (kbd "C-c g p") (lambda (arg) (interactive "P") (goto-last-change arg) (recenter-top-bottom)))
;goto next change:
(global-set-key (kbd "C-c g n") (lambda (arg) (interactive "P") (goto-last-change-reverse arg) (recenter-top-bottom)))
(global-set-key (kbd "C-c x") 'crosshairs)
(global-set-key (kbd "C-c C-x") 'crosshairs-mode)

(global-set-key (kbd "C-c C-.") 'one-sentence-per-line)


(global-set-key (kbd "<C-right>")     'right-word-intelligently)
(global-set-key (kbd "<C-left>")      'left-word-intelligently)
(global-set-key (kbd "<C-delete>")    'kill-word-intelligently)
(global-set-key (kbd "M-d")           'delete-word-intelligently)
(global-set-key (kbd "<C-backspace>") 'backward-delete-word-intelligently)
(global-set-key (kbd "M-s-j")         'left-word-intelligently)
(global-set-key (kbd "M-s-l")         'right-word-intelligently)


(global-set-key (kbd "C-c l l") 'look-up-leo) 
(global-set-key (kbd "C-c l w e") 'look-up-wikipedia-en) 
(global-set-key (kbd "C-c l w d") 'look-up-wikipedia-de) 
(global-set-key (kbd "C-c l o") 'look-up-oxford) 
(global-set-key (kbd "C-c l g") 'look-up-google)

(global-set-key (kbd "s-e") 'lisp-eval-defun)
(global-set-key (kbd "s-s") 'lisp-eval-last-sexp)
(global-set-key (kbd "s-f") 'ac-complete-filename)

(eval-after-load 'undo-tree
  '(progn
     (define-key undo-tree-visualizer-mode-map (kbd "<return>") 'undo-tree-visualizer-quit)
     (define-key undo-tree-visualizer-mode-map (kbd "C-g") 'undo-tree-visualizer-abort)
     (define-key undo-tree-visualizer-mode-map (kbd "i") 'undo-tree-visualize-undo)
     (define-key undo-tree-visualizer-mode-map (kbd "k") 'undo-tree-visualize-redo)
     (define-key undo-tree-visualizer-mode-map (kbd "j") 'undo-tree-visualize-switch-branch-left)
     (define-key undo-tree-visualizer-mode-map (kbd "l") 'undo-tree-visualize-switch-branch-right)))

(eval-after-load 'orgtbl-mode
  '(progn
     (define-key orgtbl-mode-map (kbd "C-c <insert>") 'org-table-edit-field)
     (define-key orgtbl-mode-map (kbd "C-c <return>") 'orgtbl-hijacker-command-23)))

;load icicles
;(require 'icicles)
;(icy-mode t)

