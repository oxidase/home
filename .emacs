;; Bootstrap elpaca mode
(let ((boostrap-hash "32c533034925812679cddc4b2c8d874fa42299b99b06df87508d9c5d23e5ae29")
      (bootstrap-file (expand-file-name "elpaca/installer.el" user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (let ((bootstrap-dir (file-name-directory bootstrap-file))
	  (download-buffer (url-retrieve-synchronously "https://raw.githubusercontent.com/progfolio/elpaca/refs/heads/master/doc/installer.el")))
    (when download-buffer
      (save-excursion
	(set-buffer download-buffer)
	(let* ((content (buffer-substring-no-properties (1+ url-http-end-of-headers) (point-max)))
	       (hash (secure-hash 'sha256 content)))
	  (unless (string= boostrap-hash hash) (error (format "elpaca installer has SHA-256 %s, expected %s" hash boostrap-hash)))
	  (unless (file-exists-p bootstrap-dir) (make-directory bootstrap-dir t))x
	  (with-temp-file bootstrap-file (insert content)))))))
  (load bootstrap-file nil 'nomessage)
  (elpaca elpaca-use-package
    (elpaca-use-package-mode)))


;; Extend execution paths with fish paths
(let*
    ((fish-path (split-string (shell-command-to-string "/opt/homebrew/bin/fish -c 'printf \"%s\\n\" $PATH' 2>/dev/null") "\n"))
     (full-path (cl-remove-if #'string-empty-p (append fish-path (cl-remove-if (lambda (item) (member item fish-path)) exec-path)))))
  (setenv "PATH" (string-join full-path ":"))
  (setq exec-path full-path))


;; Dired mode
(require 'dired)
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
  :ensure (:host github :repo "emacsattic/dired-single" :branch "master")
  :bind (:map dired-mode-map
	 ([remap dired-find-file] . 'dired-single-buffer)
	 ([remap dired-mouse-find-file-other-window] . 'dired-single-buffer-mouse)
	 ("DEL" . 'dired-single-up-directory)))

(use-package ag
  :ensure t
  :init (require 'thingatpt)
  :bind (("<s-f3>" . (lambda () (interactive) (ag/search (word-at-point) (ag/project-root default-directory)))))
  :config
  (custom-set-variables
   '(ag-ignore-list '("TAGS" "*.bin" "bundle.js" "*.ipynb" "*.html" "*node_modules*"))
   '(ag-highlight-search t))

  ;; use ag-project-root in local variables of polyrepos
  (advice-add 'ag/project-root :around (lambda (orig file-path)
                                         (if (boundp 'ag-project-root)
                                             ag-project-root
                                           (funcall orig file-path)))))

(use-package bm
  :ensure t
  :bind (("C-<f2>" . bm-toggle)
	 ("<f2>" . bm-next)
	 ("S-<f2>" . bm-previous)))

(use-package transient :ensure t)

(use-package magit
  :ensure t
  :bind (:map magit-mode-map
	 (("C-<tab>" . nil)))
  :config
  (set-standard-toplevel-value 'git-commit-summary-max-length 99)
  (put 'magit-log-mode 'magit-log-default-arguments
       (append (get 'magit-log-mode 'magit-log-default-arguments) '("--no-merges"))))

;; Org mode (required texinfo)
(use-package ob-http :ensure)
(use-package ob-html-chrome :ensure)
(use-package ob-mermaid :ensure)
(use-package org
  :ensure t
  :init
  (use-package ox-reveal :ensure t)

  :config
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

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (C . t) (haskell . t) (sqlite . t) (maxima . t) (latex . t) (dot . t) (R . t) (gnuplot . t) (scheme . t)
     (http . t) (html-chrome . t) (mermaid . t)))


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

(defvar insert-date-format "%e %B %Y %T" "*Format for \\[insert-date].")
(defun insert-date ()
  "Insert the current date according to the variable \"insert-date-format\"."
  (interactive "*")
  (insert (format-time-string insert-date-format (current-time))))


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
	(find-file-visit-truename t)
	(grep-command "grep --exclude-dir=\"tmp\" -nHriIZ -e ")
	(use-short-answers t)
	(find-file-visit-truename t)
	(history-length t)
	(version-control t)
	(undo-limit 24000000)
	(ispell-program-name "aspell")
	(ispell-check-comments t)
	(scroll-error-top-bottom t)))

(set-default-toplevel-value 'backup-directory-alist ;; Save all backup file in this directory.
      (list `(".*" . ,(expand-file-name ".backups" user-emacs-directory))))


;;; TODO cleanup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C and C++ modes
(require 'cc-langs)
(require 'cc-mode)
(use-package cmake-mode :ensure t)
(require 'gud)
(require 'gdb-mi)

(setq gud-tooltip-mode t)
(setq compilation-ask-about-save nil)
(setq gdb-debuginfod-enable-setting t)

(defun get-bazel-root (dir)
  (or (vc-find-root dir "WORKSPACE") (vc-find-root dir "MODULE.bazel")))

(defun get-yarn-root (dir)
  (when (member major-mode '(web-mode json-mode)) (vc-find-root dir "package.json")))

(defun get-buffer-file-name()
  (or (buffer-file-name) (concat "*" (symbol-name major-mode) "*")))

(defun get-compile-command ()
  (let* ((abs (get-buffer-file-name))
         (name (file-name-nondirectory abs))
         (stem (file-name-sans-extension name))
         (ext (file-name-extension name))
         (dir (expand-file-name default-directory))
         (bazel-root (get-bazel-root dir))
         (yarn-root (get-yarn-root dir)))
    (cond
      (bazel-root (format "bazel test //%s..." (substring dir (length (expand-file-name bazel-root)))))
      ((eq major-mode 'c++-mode)
       (cond
         ((string-equal ext "nxc") (format "nbc %s -O=%s.rxe; nxt_push %s.rxe" name stem stem))
         ((string-equal ext "cu") (format "nvcc -O0 -g %s -o %s" name stem))
         ((string-equal ext "c") (format "gcc -Wall -O0 -g %s -o %s" name stem))
         (t (format "g++ -std=c++20 -Wall -O0 -g %s -o %s" name stem))))
      ((eq major-mode 'cmake-mode) "cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug && cmake --build build --target install -j4")
      ((eq major-mode 'python-mode) (format "python3 %s" name))
      ((eq major-mode 'haskell-mode) ("ghc %s -o %s" name stem))
      ((member major-mode '(makefile-gmake-mode makefile-bsdmake-mode)) "make")
      (yarn-root
       (cond
        ((string-match-p ".+[\\._]test$" stem)
         (format "yarn test %s" (substring abs (length (expand-file-name yarn-root)))))
         (t "yarn test")))
      (t "make -k "))))

(defun do-compile (command)
  ;; Get compile command and cached it for next invocations
  (interactive (list (if (< compiled-times 2) (read-string "Compile command: " compile-command) compile-command)))
  (setq-local compiled-times (if (string= compile-command command) (1+ compiled-times) 0))
  (setq-local compile-command command)

  ;; Get compile directory, modify default-directory, compile, and restore default-directory
  (let* ((saved-default-directory default-directory)
         (bazel (string-match-p "^\s*bazel" command))
         (yarn-root (get-yarn-root default-directory))
         (project-root (and (fboundp 'ag/project-root) (ag/project-root default-directory)))
         (default-directory (or (and bazel (get-bazel-root default-directory)) yarn-root default-directory)))
    (compile command)
    (setq default-directory saved-default-directory)))

(defun do-run(command)
  ;; Get run command and cached it for next invocations
  (interactive (list (if (< run-times 2) (read-string "Run command: " run-command) run-command)))
  (setq-local run-times (if (string= run-command command) (1+ run-times) 0))
  (setq-local run-command command)

  (let*
      ((buffer-name "*std output*")
       (output-buffer (or (get-buffer buffer-name) (generate-new-buffer buffer-name)))
       (output-window (async-shell-command command output-buffer))
       (proc (get-buffer-process output-buffer)))
    (if (process-live-p proc)
        (set-process-sentinel
         proc #'(lambda (process signal)
                  (when (memq (process-status process) '(exit signal))
                    (message "Process %d exited with code %d" (process-id process) (process-exit-status process))))))
    (other-window 1)
    (switch-to-buffer output-buffer)
    (other-window 1)))

(defun gud-set-clear () (interactive)
  (message "gud-set-clear")
  (let ((buf (current-buffer))
        (pnt (point))
        (set-break (not (eq (car (fringe-bitmaps-at-pos (point))) 'breakpoint)))
        (pos (car (gdb-line-posns (line-number-at-pos)))))
    (if set-break (gud-break nil) (gud-remove nil))
    (when (eq major-mode 'python-mode)
      (if set-break (gdb-put-breakpoint-icon t 0) (gdb-remove-breakpoint-icons (- pos 1) (+ pos 1)))
      (sleep-for .2)
      (switch-to-buffer buf) (goto-char pnt)
      (delete-other-windows)
      (gud-split-window)
      )))

(defun gud-split-window () (interactive)
  (when (buffer-name gud-comint-buffer)
    (delete-other-windows)
    (if (eq (current-buffer) gud-comint-buffer)
        (gud-refresh)
      (progn  (split-window-vertically) (other-window 1) (switch-to-buffer gud-comint-buffer) (other-window 1)))))

(defun development-mode-hook ()
  ;;(print (mapcar (lambda (x) (car x)) (buffer-local-variables)))
  ;;(print (remove-if-not (lambda (x) (eq 'compile-command (car x))) (buffer-local-variables)))
  ;;(ggtags-mode 1)

  (setq show-trailing-whitespace t)
  (unless (eq major-mode 'js-mode)
    (defun c-font-lock-invalid-string () t)              ;; Turn off invalid string highlight
    (c-toggle-auto-newline -1)                           ;; Turn off auto-newline feature
    (c-set-offset 'substatement-open 0))                 ;; Project brace indent style

  (unless (eq major-mode 'gud-mode)
    ;; Compile command
    (set (make-local-variable 'compiled-times) 0)
    (set 'compile-command (get-compile-command))
    (local-set-key "\C-c\C-c" 'do-compile)

    ;; Run command, allow only commands in that starts with "./"
    (set (make-local-variable 'run-times) 0)
    (set
     (make-local-variable 'run-command)
     (cond
      ((eq major-mode 'python-mode) (format "python3 %s" (file-name-nondirectory (get-buffer-file-name))))
      (t (format "./%s" (file-name-base (get-buffer-file-name))))))
    (put 'run-command 'safe-local-variable 'run-command-safe-variable)
    (defun run-command-safe-variable (var)
      (or
       (string-match "^[ \t\n\r]*\\(qml\\(scene\\|viewer\\)\\|optirun\\)[ \t\n\r]*\./.+" var)
       (string-match "/usr/bin/curl.+" var)))
    (local-set-key "\C-c\C-v" 'do-run))

    ;; settings depending on the mode
  (when (or (eq major-mode 'c++-mode) (eq major-mode 'fortran-mode)
            (eq major-mode 'jam-mode) (eq major-mode 'objc-mode))
    (setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))
    (flyspell-prog-mode)
    (local-set-key '[C-f8]   'flyspell-buffer)
    ;; other settings
    (setq indent-tabs-mode nil))

  ;; TAGS lookup
  (when (or (eq major-mode 'c++-mode) (eq major-mode 'fortran-mode)
            (eq major-mode 'jam-mode) (eq major-mode 'gud-mode))
    (local-set-key (kbd "M-/") 'xref-find-references)
    (setq-local xref-prompt-for-identifier nil))

  (when (eq major-mode 'fortran-mode)
    (local-set-key "\C-c'" 'fortran-comment-region))

  (when (or (eq major-mode 'c++-mode) (eq major-mode 'fortran-mode)
            (eq major-mode 'gud-mode) (eq major-mode 'python-mode)
            (eq major-mode 'go-mode) (eq major-mode 'objc-mode))
  ;; (string-match "\\*gud-\\(.+\\)\\*" (buffer-name gud-comint-buffer))
    ;; debug functions
    (gud-def gud-frame "frame" "\C-g" "Select and print a stack frame.")

    ;; debug keys
    (local-set-key '[(super f11)]  'gud-until)
    (local-set-key '[f9]     'gud-set-clear)
    (local-set-key '[S-f9]   'gud-break)
    (local-set-key '[C-f9]   'gud-remove)
    (local-set-key '[f10]    'gud-next)
    (local-set-key '[f11]    'gud-step)
    (local-set-key '[f12]    'gud-finish))

    ;; auto complete
  (when (and (boundp 'ac-sources) (listp ac-sources))
    (add-to-list 'ac-sources 'ac-source-semantic-raw))
  )
(modify-syntax-entry ?_ "w" c++-mode-syntax-table)
(modify-syntax-entry ?_ "w" java-mode-syntax-table)
(modify-syntax-entry ?_ "w" objc-mode-syntax-table)


(add-hook 'c++-mode-hook
      #'(lambda()
        (font-lock-add-keywords
         nil '(;; complete some fundamental keywords
           ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
           ;; add the new C++11 keywords
           ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
           ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
           ;; PREPROCESSOR_CONSTANT
           ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
           ;; hexadecimal numbers
           ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
           ;; integer/float/scientific numbers
           ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
           ;; user-types (customize!)
           ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|type\\|ptr\\)\\>" . font-lock-type-face)
           ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
           ))
        )
      t)

(eval-after-load "c++-mode"
  '(define-key c++-mode-map (kbd "C-c C-p") nil))


;; set context-dependent tabulation widths
(add-hook 'c++-mode-hook
  #'(lambda ()
     (when buffer-file-name
       (cond
        ((or (string-match "^/usr/include/c++" buffer-file-name)
             (string-match "^/usr/include/x86_64-linux-gnu/c++" buffer-file-name))
         (make-variable-buffer-local 'tab-width)
         (set-variable 'tab-width 8))))))

;; set global GDB properties and keys
(setf gdb-show-threads-by-default t)
(setf gdb-show-main t)
(setf gdb-mi-decode-strings t)  ;; decode strings for UTF-8 support
;;(defun set-window-undedicated-p (window flag) "Never set window dedicated." flag)
;;(advice-add 'set-window-dedicated-p :override #'set-window-undedicated-p)
;;(global-set-key (kbd "s-`") (lambda () (interactive)
;;     (when (buffer-name gud-comint-buffer)
;;       (gdb-restore-windows)
;;       (set-window-dedicated-p (selected-window) nil))))
(global-set-key (kbd "s-`") (lambda () (interactive)
  (when (buffer-name gud-comint-buffer)
    (switch-to-buffer gud-comint-buffer)
    (delete-other-windows)
    (let* ((win (split-window (get-buffer-window gud-comint-buffer) 16))
           (winio (split-window win -4)))
      (set-window-buffer winio (gdb-get-buffer-create 'gdb-inferior-io))
      (set-window-buffer win
       (cond
        (gud-last-last-frame (gud-find-file (car gud-last-last-frame)))
        ((gud-find-file gdb-main-file) (gud-find-file gdb-main-file))
        (t gud-comint-buffer)))
      (setq gdb-source-window win)))))

;; c++11 stuff
(make-face 'c++11-raw-string-face)
(set-face-foreground 'c++11-raw-string-face "#003000")
(set-face-background 'c++11-raw-string-face "#ddffdd")
(font-lock-add-keywords 'c++-mode '(("\\(R\\s\"\\(.*\\)([^)]*)\\2\\s\"\\)" 1 'c++11-raw-string-face t)))

(defun fmq-compilation-finish (buffer status)
  (let ((time (time-convert (time-subtract (current-time) (buffer-local-value 'compile-starts-at buffer)) 'integer)))
    (when (> time 20)
      (cond
       ((eq system-type 'gnu/linux)
        (call-process "notify-send" nil nil nil
                      "-i" "emacs"
                      (format "%s %s" status (car (buffer-local-value 'compilation-arguments buffer)))
                      (format "in %s %ds" (buffer-local-value 'compilation-directory buffer) time)))
       ((eq system-type 'darwin)
        (call-process "osascript" nil nil nil "-e" (format "display notification \"%s %s\" with title \"emacs\"" status (car (buffer-local-value 'compilation-arguments buffer)))))
      ))))

(setq compilation-process-setup-function
      (lambda () (make-local-variable 'compile-starts-at)
        (setq compile-starts-at (current-time))))

(setq compilation-finish-functions
      (append compilation-finish-functions
          '(fmq-compilation-finish)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode
(add-to-list 'magic-mode-alist '("^#!/usr/bin/env[^\n]*python[0-9.]*\n" . python-mode))
(add-hook 'python-mode-hook (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent-offset 4)
        (modify-syntax-entry ?_ "w" python-mode-syntax-table)))

(defun python-comint-filter (output)
  (replace-regexp-in-string "__PYTHON_EL_eval.+\n" "" output))
(add-to-list 'comint-preoutput-filter-functions #'python-comint-filter)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Haskel mode
;; (when (package-dir "haskell*")
;;   (require 'haskell-mode)
;;   (require 'haskell-interactive-mode)
;;   (require 'haskell-process)
;;   (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;;   (modify-syntax-entry ?_ "w" haskell-mode-syntax-table)

;;   (custom-set-variables
;;    '(haskell-process-suggest-remove-import-lines t)
;;    '(haskell-process-auto-import-loaded-modules t)
;;    '(haskell-process-log t)

;;    '(haskell-process-type 'cabal-repl))

;;   (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
;;   (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

;;   (require 'intero)
;;   (add-hook 'haskell-mode-hook 'intero-mode))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Ruby mode
;; ; loads ruby mode when a .rb file is opened.
;; (autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
;; (setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
;; (setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Scala mode
;; (when (package-dir "scala-mode2*")
;;   (require 'scala-mode2))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Lua mode
;; (when (package-dir "lua*")
;;   (require 'lua-mode)
;;   (modify-syntax-entry ?_ "w" lua-mode-syntax-table)
;;   (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL
;; (when (package-dir "sql-indent*")
;;   (require 'sql-indent)
;;   (defun sql-indent-region (beg end)
;;     "Indent the SQL statement in the region."
;;     (interactive "*r")
;;     (save-excursion
;;       (save-restriction
;;         (narrow-to-region beg end)
;;         (sql-indent-buffer)))))

;; (add-hook 'sql-mode-hook
;;           (lambda ()
;;             (sql-highlight-mysql-keywords)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Go
;; (add-hook 'go-mode-hook
;;   #'(lambda()
;;      (add-hook 'before-save-hook 'gofmt-before-save)
;;      (local-set-key '[f3]   'godef-jump)
;;      (local-set-key '[M-f3] 'pop-tag-mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set hooks
(cl-loop for mode in '(c-mode-hook c++-mode-hook cmake-mode-hook fortran-mode-hook go-mode-hook
                    qt-pro-mode-hook gud-mode-hook qml-mode-hook python-mode-hook haskell-mode-hook
                    bazel-build-mode-hook bazel-starlark-mode-hook bazel-module-mode-hook bazel-workspace-mode-hook
                    js-mode-hook objc-mode-hook web-mode-hook json-mode makefile-mode-hook makefile-bsdmake-mode-hook )
         do (add-hook mode 'development-mode-hook))

(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))
