;; Bootstrap straight mode
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Extend execution paths with fish paths
(let*
    ((fish-path (split-string (shell-command-to-string "/opt/homebrew/bin/fish -c 'printf \"%s\\n\" $PATH' 2>/dev/null") "\n"))
     (full-path (cl-remove-if #'string-empty-p (append fish-path (cl-remove-if (lambda (item) (member item fish-path)) exec-path)))))
  (setenv "PATH" (string-join full-path ":"))
  (setq exec-path full-path))


;; Dired mode
(defvar dired-sort-map (make-sparse-keymap))
(define-key dired-mode-map "s" dired-sort-map)
(define-key dired-sort-map "s" (lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
(define-key dired-sort-map "x" (lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
(define-key dired-sort-map "t" (lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
(define-key dired-sort-map "n" (lambda () "sort by Name" (interactive) (dired-sort-other dired-listing-switches)))
(define-key dired-sort-map "d" (lambda () "sort by name grouping Dirs" (interactive) (dired-sort-other (concat dired-listing-switches " --group-directories-first"))))
(define-key dired-mode-map "z" (lambda () (interactive)
                                 (let* ((name (or (dired-get-filename nil t) default-directory))
                                        (localname (or (file-remote-p name 'localname) name)))
                                   (kill-new localname)
                                   (x-set-selection nil localname))))

(use-package dired-single
  :straight t
  :bind (("C-x C-g" . magit-status))
  :config
  )


;; Magit mode
(use-package magit
  :straight t
  ;:bind (("C-x C-g" . magit-status))
    ;:config
  )


;; Org mode (required texinfo)
(use-package org
  :straight (:type built-in)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((http . t) (html-chrome . t) (python . t) (C . t) (haskell . t) (sqlite  . t) (maxima . t)
     (latex . t) (plantuml . t) (dot . t) (ruby . t) (R . t) (gnuplot . t) (scheme . t) (mermaid . t)))

  (custom-set-faces
   '(org-block ((t (:background "#F8F8FF"))))
   '(org-block-begin-line ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
   '(org-block-end-line ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))))

  (custom-set-variables
   '(org-edit-src-content-indentation 0)                            ;; Don't ident in source blocks
   '(org-src-preserve-indentation nil)
   '(org-src-fontify-natively t)
   '(org-babel-results-keyword "results")                           ;; Make babel results blocks lowercase
   '(org-confirm-babel-evaluate nil)                                ;; Do not prompt to confirm evaluation
   '(org-babel-python-command "python3")
   '(org-log-done 'time)
   '(org-support-shift-select 'always)
   '(org-todo-keyword-faces '(("TODO" . "deep pink")
                              ("DONE" . "sea green")
                              ("IDEA" . "IndianRed2")
                              ("INPROGRESS" . "DodgerBlue3")
                              ("REPORT" . "blue")
                              ("BUG" . "red")
                              ("KNOWNCAUSE" . "purple")
                              ("FIXED" . "SpringGreen3")
                              ("CANCELED" . "grey")))

   '(org-capture-templates
     '(("n" "note" entry (file+datetree "~/notes/notes.org") "* %?\nEntered on %U\n  %i"))))

  (add-hook 'org-babel-after-execute-hook (lambda () (condition-case nil (org-display-inline-images) (error nil)))))



;; Utility Functions
(defun context-help ()
  """Show context help based in buffer mode."""
  (interactive)
  (cond
   ((member major-mode '(lisp-mode emacs-lisp-mode))
    (if (eq (variable-at-point) 0) (describe-function (function-called-at-point)) (describe-variable (variable-at-point))))
   ((member major-mode '(c-mode c++-mode))
    (browse-url (format "https://duckduckgo.com/?q=%s+site%3Acppreference.com" (downcase (current-word)))))
   (t (when (> (length (current-word)) 1) (woman (current-word))))))

(defun format-variable-properties (var &optional as-string)
  "Get properties of a customizable variable."
  (interactive
   (list
    (let ((symbol-at-point (thing-at-point 'symbol t)))
      (if symbol-at-point
	  (read-string (format "Variable (default %s): " symbol-at-point) nil nil symbol-at-point)
	(read-string (format "Variable: "))))))
  (let* ((sym (if (symbolp var) var (intern var)))
	 (props (append `("default-toplevel-value" ,(default-toplevel-value sym)) (symbol-plist sym)))
	 (kv (seq-partition props 2))
	 (res (when (or as-string (called-interactively-p 'interactive))
		(string-join (mapcar (lambda (x) (format "  %-32s: %s" (car x) (cadr x))) kv) "\n"))))
    (when (called-interactively-p 'interactive)
      (message res))
     (or res kv)))

(defun set-standard-toplevel-value (var val)
  """Set standard value and customize value if it is not yet set."""
  (put var 'standard-value `(,val))
  (unless (get var 'saved-value)
    (set-default-toplevel-value var val)))


(defvar cyclebuffer-buffer-list nil "List of all buffers; updated every time a new set of cyclebuffer commands are started.")
(defvar cyclebuffer-buffer-index nil "Number indicating the index of the buffer in cyclebuffer-buffer-list that is currently being displayed.")
(defun cyclebuffer-forward (&optional direction)
  "Like switch-to-buffer, but doesn`t prompt.  Repetitive invocations of
   this function select progressively less recently visited buffers."
  (interactive "P")
  ;; If starting a new search, a) make sure the current buffer is at top
  ;; of the list of buffers, and b) set flag to generate a new list
  (if (not (or (eq `cyclebuffer-forward last-command)
	       (eq `cyclebuffer-backward last-command)))
      (progn
	(setq cyclebuffer-buffer-index nil)
	(switch-to-buffer (current-buffer))))

  ;; Generate new list if necessary
  (if (not (numberp cyclebuffer-buffer-index))
      (progn
	(setq cyclebuffer-buffer-list (buffer-list))
	(setq cyclebuffer-buffer-index 0)))

  ;; Cycle through buffers, skipping any invisible buffers (whose
  ;; names start with a blank space)
  (let ((start-buffer (current-buffer))
	(chosen-buffer (current-buffer)))
    (while (or (eq chosen-buffer start-buffer)
	       (char-equal ?  (string-to-char (buffer-name chosen-buffer))))
      (setq start-buffer nil)
      (if (or (null direction) (eq direction 1))
	  (setq cyclebuffer-buffer-index (+ cyclebuffer-buffer-index 1))
	(setq cyclebuffer-buffer-index (- cyclebuffer-buffer-index 1)))
      (setq cyclebuffer-buffer-index
	    (mod cyclebuffer-buffer-index (length cyclebuffer-buffer-list)))
      (setq chosen-buffer (nth cyclebuffer-buffer-index
			       cyclebuffer-buffer-list)))
    (switch-to-buffer chosen-buffer)))

(defun cyclebuffer-backward ()
  "Like cyclebuffer-forward, but selects progressively more recently visited buffers."
  (interactive)
  (cyclebuffer-forward -1))

(defun gh-lines (ref)
  "Open the current line in a git remote repository"
  (interactive (list (magit-read-starting-point "Show line for" nil (magit-get-current-branch))))
  (let* ((remotes (magit-list-remotes))
         (remote (cond
                  ((null remotes) "origin")
                  ((member "upstream" remotes) "upstream")
                  ((member "origin" remotes) "origin")
                  ((t (car remotes)))))
         (remote-url
          (replace-regexp-in-string "\\(.git\\|/+\\)$" "" ; remove trailing slashes or .git suffix
	    (replace-regexp-in-string "://[^@]+@" "://" ; remove user name
            (replace-regexp-in-string "^git@\\(.*\\):\\(.*\\)\\.git$" "https://\\1/\\2" ; most general case
            (replace-regexp-in-string "^git@github.\\([^:]+\\):" "https://github.\\1/" ; change protocol to https
            (replace-regexp-in-string "^git://sourceware.org/git/\\(.*+\\)" "https://sourceware.org/git/?p=\\1.git" ; change protocol to https
            (magit-get "remote" remote "url")))))))
         (from (line-number-at-pos (if (and transient-mark-mode mark-active) (region-beginning) (point)))) ; or (format-mode-line "%l")
         (to (line-number-at-pos (if (and transient-mark-mode mark-active) (- (region-end) (if (= (current-column) 0) 1 0)) (point))))
         (lines (if (>= from to) (format "L%d" from) (format "L%d-L%d" from to)))
         (gh-url
            (cond
             ((cl-search "gitlab" remote-url) (format "%s/-/blob/%s/%s?#%s" remote-url (substring (or (magit-rev-verify ref) (magit-rev-parse "HEAD")) 0 8) (magit-file-relative-name) lines))
             ((cl-search "github" remote-url) (format "%s/blob/%s/%s#%s" remote-url (substring (or (magit-rev-verify ref) (magit-rev-parse "HEAD")) 0 8) (magit-file-relative-name) lines))
             ((cl-search "sourceware.org" remote-url) (format "%s;a=blob;f=%s#l%d" remote-url (magit-file-relative-name) from))
             (t remote-url))))
    (kill-new gh-url)
    (browse-url gh-url)))


(defun blink-paren-first-line ()
  "Show in the message bar the line with the corresponding opening parenthesis."
  (interactive)
  ;; if mode is c++ or lisp and
  (when (and (member major-mode '(c++-mode lisp-mode emacs-lisp-mode qml-mode web-mode))
             (eq (syntax-class (syntax-after (1- (point)))) 5))
    ;; get corresponding opening parenthesis
    (let* ((open (condition-case () (scan-sexps (point) -1) (error nil)))
           sol eol non-space a b c msg)
      (when open
        (save-excursion
          (goto-char open)
          (setq eol (line-end-position))
          (setq non-space (save-excursion (save-restriction (save-match-data (re-search-forward  "[^ \t\r\n]" nil t) (point)))))
          (setq sol (if (< non-space eol) (line-beginning-position)
                (save-excursion (save-restriction (save-match-data (goto-char (1- open)) (re-search-backward  "[^ \t\r\n]" nil t) (line-beginning-position))))))
          (setq a (buffer-substring  sol open) b (buffer-substring  open (1+ open)) c (buffer-substring  (1+ open) eol)))
        (setq a (replace-regexp-in-string "[\n]+\\s-*" " " a)) ;; remove internal newlines
        (setq msg (concat a b c))
        ;; (message (format "%d %d %d %d |%s|%s|%s" sol open eol non-space a b c))
        (put-text-property (length a) (1+ (length a)) 'face '(background-color . "turquoise" ) msg)
        (let ((message-log-max nil))
          (message "%s" msg))))))
(run-with-idle-timer 3.14 t 'blink-paren-first-line)

;; Global Minor Modes
(tool-bar-mode -1)
(delete-selection-mode)
(save-place-mode)
(savehist-mode)

;; Global hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)  ;; configuration required (remove-hook 'before-save-hook 'delete-trailing-whitespace)

;; Global key bindings
(mapc (lambda (args) (apply #'keymap-global-set args))
      '(("<f1>" context-help)
	("<f4>" query-replace-regexp)
	("<f5>" revert-buffer)
	("C-S-s" search-forward-regexp)
	("C-c ;" comment-region)
	("C-c '" uncomment-region)
	("C-`" ispell-word)
	;; Clipboard
	("C-<help>" clipboard-kill-ring-save)
	("S-<help>" clipboard-yank)
	("S-<delete>" clipboard-kill-region)
	;; Window movement
	("<home>" move-beginning-of-line)
	("<end>" move-end-of-line)
	("C-<up>" scroll-down-command)
	("C-<down>" scroll-up-command)
	("M-?" goto-line)
	("M-<left>" move-beginning-of-line)
   	("M-<right>" move-end-of-line)
	("M-<up>" beginning-of-buffer)
	("M-<down>" end-of-buffer)
	("s-<left>" windmove-left)
   	("s-<right>" windmove-right)
	("s-<up>" windmove-up)
	("s-<down>" windmove-down)
	;; Buffer cycling
	("C-<tab>" cyclebuffer-forward)
	("C-S-<tab>" cyclebuffer-backward)
	;; Bootstrap Org mode
	("C-c c" org-capture)
	;; Bootstrap Magit mode
	("C-c s" magit-status)
	("C-c b" magit-blame)
	("C-c l" magit-log-buffer-file)
	("C-c g" gh-lines)))

;; Global customization variables
(setq custom-file (expand-file-name (format "%s/custom.el" user-emacs-directory)))
(setq custom-system-file (expand-file-name (format "%s/custom.%s.el" user-emacs-directory (system-name))))
(if (file-exists-p custom-file)
    (load custom-file))
(if (file-readable-p custom-system-file)
    (load custom-system-file)
  (write-region ";; -*- mode: lisp -*-" nil custom-system-file))

(mapc (lambda (args) (apply #'set-standard-toplevel-value args))
      `((default-frame-alist ((top . 0) (left . 1.0) (width . 0.92) (fullscreen . fullheight)))
	(blink-cursor-mode nil)
	(grep-command "grep --exclude-dir=\"tmp\" -nHriIZ -e ")
	(use-short-answers t)
	(history-length t)
	(undo-limit 24000000)
	(ispell-program-name "aspell")
	(ispell-check-comments t)
	(scroll-error-top-bottom t)))
