;; -*- lexical-binding: t -*-
;; Bootstrap elpaca mode
(let ((boostrap-hash "1de28698b9619644ac6df45d64083334b77470b34c0b7e0557e6711d14800298")
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
	  (unless (file-exists-p bootstrap-dir) (make-directory bootstrap-dir t))
	  (with-temp-file bootstrap-file (insert content)))))))
  (load bootstrap-file nil 'nomessage)
  (elpaca elpaca-use-package
    (elpaca-use-package-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extend execution paths with fish paths
(let*
    ((fish-path (split-string (shell-command-to-string "/opt/homebrew/bin/fish -c 'printf \"%s\\n\" $PATH' 2>/dev/null") "\n"))
     (full-path (cl-remove-if #'string-empty-p (append fish-path (cl-remove-if (lambda (item) (member item fish-path)) exec-path)))))
  (setenv "PATH" (string-join full-path ":"))
  (setq exec-path full-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup packages
(defun set-standard-toplevel-value (var val)
  "Set standard value and customize value if it is not yet set."
  (put var 'standard-value `(,val))
  (unless (get var 'saved-value)
    (set-default-toplevel-value var val)))


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
  (put 'magit-log-mode 'magit-log-default-arguments (append (get 'magit-log-mode 'magit-log-default-arguments) '("--no-merges")))
  (mapc (lambda (args) (apply 'funcall #'set-standard-toplevel-value args))
        `((magit-process-finish-apply-ansi-colors t)
          (git-commit-summary-max-length 99))))

;; Org mode (required texinfo)
(use-package ob-http :ensure)
(use-package ob-html-chrome :ensure)
(use-package ob-mermaid :ensure)
(use-package ox-reveal :ensure t)
(use-package org
  :ensure t
  :init
  :bind* (("C-<return>" . mini-calc))
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
   '(org-babel-default-header-args:cpp '((:flags . "-std=c++20")))
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

  (require 'org-tempo)
  (setq org-structure-template-alist
        (append org-structure-template-alist
                '(("p" . "src python :results output")
                  ("m" . "src mermaid :file _.png")
                  ("pg" . "src sql :engine postgresql :dbuser postgres :dbhost localhost :dbport 5432"))))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (C . t) (haskell . t) (sqlite . t) (maxima . t) (latex . t) (dot . t) (R . t) (gnuplot . t) (scheme . t)
     (http . t) (html-chrome . t) (mermaid . t)))


  (add-hook 'org-babel-after-execute-hook (lambda () (condition-case nil (org-display-inline-images) (error nil)))))


(defun dired-open-file ()
  "In dired, open the file named on this line."
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (message "Opening %s..." file)
    (call-process "gnome-open" nil 0 nil file)
    (message "Opening %s done" file)))


(use-package openwith
  :ensure t
  :config
  (setq openwith-associations
        (cond
         ((eq system-type 'gnu/linux)
          '(("[^_]?\\.\\(ps\\|pdf\\|djvu\\|epub\\)\\'" "evince" (file))
            ("\\.\\(docx?\\|odt\\|pptx?\\|rtf\\|xlsx?\\)\\'" "libreoffice" (file))
            ("\\.\\(ai\\)\\'" "inkscape" (file))
            ("\\.\\(svg\\)\\'" "eog" (file))
            ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp4\\)\\'" "smplayer" (file))))
         ((eq system-type 'windows-nt)
          '("\\.\\(dll\\|pyd\\)\\'" "depends.exe" (file)))
         ((eq system-type 'darwin)
          '(("\\.\\(ps\\|pdf\\|djvu\\docx?\\|epub\\|tif\\|jp2\\|mov\\|drawio\\|pptx?\\|rtf\\|xlsx?\\)$" "open" (file))
            ("\\.\\(ai\\)\\'" "/Applications/Inkscape.app/Contents/MacOS/inkscape" (file))))))
  (openwith-mode t))


(use-package eldoc :defer t)
(use-package elfeed :ensure t
  :bind (("C-x w" . 'elfeed)))
(use-package sqlite3 :ensure t
  :config
  (require 'sqlite3))
(use-package web-mode :ensure t)
(use-package fish-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package cmake-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package typescript-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package flatbuffers-mode :ensure t)
(use-package livedown :ensure t (:host github :repo "shime/emacs-livedown" :branch "master" :main "livedown.el"))
(use-package helpful :ensure t
  :bind (("C-h f" . 'helpful-callable)
         ("C-h v" . 'helpful-variable)
         ("C-h c" . 'helpful-command)
         ("C-h k" . 'helpful-key)))
(use-package bitbake :ensure t)
(use-package cmm-mode
  :ensure t (:host github :repo "oxidase/cmm-mode" :main "cmm-mode.el") ; :branch "main"
  :config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions
(defun context-help ()
  """Show context help based in buffer mode."""
  (interactive)
  (cond
   ((member major-mode '(lisp-mode emacs-lisp-mode))
    (helpful-at-point))
   ((member major-mode '(c-mode c++-mode))
    (if (not (eldoc-print-current-symbol-info))
        (eldoc-doc-buffer t)
      (browse-url (format "https://duckduckgo.com/?q=%s+site%%3Acppreference.com" (downcase (current-word))))))
   ((member major-mode '(python-mode))
    (browse-url (format "https://docs.python.org/3/search.html?q=%s" (downcase (current-word)))))
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
(run-with-idle-timer 1.618 t 'blink-paren-first-line)


(defvar insert-date-format "%Y-%m-%d" "*Format for \\[insert-date].")
(defun insert-date ()
  "Insert the current date according to the variable \"insert-date-format\"."
  (interactive "*")
  (insert (format-time-string insert-date-format (current-time))))


(defun find-file-goto (filename)
  "Edit file FILENAME.
   Switch to a buffer visiting file FILENAME,
   creating one if none already exists."
  (interactive "FFind file: ")
  (let* ((name filename) line column target point)
    (cond
      ;; compiler error messages
      ;; set puuid (gsettings get org.gnome.Terminal.ProfilesList default | tr -d "'")
      ;; gsettings set org.gnome.Terminal.Legacy.Profile:/org/gnome/terminal/legacy/profiles:/:$puuid/ word-char-exceptions '@ms "-=&#:/.?@+~_%;"'
      ((string-match ":+\\([0-9]+\\):*\\([0-9]+\\)?[:,]?$" filename)
       (setq name (substring filename 0 (match-beginning 0)))
       (setq line (condition-case nil (string-to-number (match-string 1 filename)) (error nil)))
       (setq column (condition-case nil (string-to-number (match-string 2 filename)) (error nil)))
       (switch-to-buffer (find-file-noselect name))
       (when line (goto-line line))
       (when column (forward-char (- column (current-column)))))

      ((string-match "#L+\\([0-9]+\\)$" filename)
       (setq name (substring filename 0 (match-beginning 0)))
       (setq line (condition-case nil (string-to-number (match-string 1 filename)) (error nil)))
       (switch-to-buffer (find-file-noselect name))
       (when line (goto-line line)))

      ;; fallback to generic file opening
      (t (switch-to-buffer (find-file-noselect name))))))


(defun ansi-color ()
  "ANSI colorize buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (ansi-color-apply-face-function
         (lambda (beg end face)
           (when face
             (put-text-property beg end 'face face)))))
    (ansi-color-apply-on-region (point-min) (point-max))))


(defun scale-face-default-height (factor)
  "Scale font height by a factor."
  (set-face-attribute 'default nil :height (truncate (* factor (face-attribute 'default :height)))))


(defun mini-calc (expr &optional arg)
  "Calculate expression

If ARG is given, then insert the result to current-buffer"
  (interactive
   (list (read-from-minibuffer "Enter expression: "
                               (if (region-active-p)
                                   (buffer-substring-no-properties (region-beginning) (region-end))
                                 (thing-at-point 'word)))
	 current-prefix-arg))

  (let ((result (calc-eval expr)))
    (if arg
	(insert result)
      (message (format "Result: [%s] = %s" expr result)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Development mode hooks
(use-package eglot
  :defer t
  :config
  (custom-set-variables
   '(eglot-code-action-indications '(eldoc-hint mode-line))
   '(eglot-ignored-server-capabilities '(:inlayHintProvider))
   '(eglot-stay-out-of '("flymake"))))

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
      ((eq major-mode 'c-mode)
       (format "gcc -Wall -O0 -g %s -o %s" name stem))
      ((eq major-mode 'c++-mode)
       (cond
         ((string-equal ext "nxc") (format "nbc %s -O=%s.rxe; nxt_push %s.rxe" name stem stem))
         ((string-equal ext "cu") (format "nvcc -O0 -g %s -o %s" name stem))
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
  "Get compile command and cached it for next invocations."
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
  "Get run command and cached it for next invocations."
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


(define-minor-mode development-mode
  "A mode for development to running sessions"
  :init-value nil
  :lighter " Dev"
  :keymap (define-keymap
    "C-c C-c" #'do-compile
    "C-c C-v" #'do-run)

  ;; Update syntax table
  (modify-syntax-entry ?_ "w" (syntax-table))

  ;; Set buffer local variables
  (setq-local show-trailing-whitespace t)

  ;; Compile command
  (set (make-local-variable 'compiled-times) 0)
  (set 'compile-command (get-compile-command))

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

  ;; Enable LSP
  (eglot-ensure))


(setq development-modes '(asm-mode c-mode c++-mode ld-script-mode objc-mode python-mode fortran-mode
   fish-mode sh-mode yaml-mode conf-toml-mode go-mode haskell-mode
   js-mode typescript-mode json-mode web-mode
   cmake-mode makefile-mode makefile-bsdmake-mode dockerfile-mode
   bazel-build-mode bazel-starlark-mode bazel-module-mode bazel-workspace-mode))

(mapc (lambda (mode) (add-hook (intern (concat (symbol-name mode) "-hook")) 'development-mode)) development-modes)


(use-package dap-mode
  :ensure t
  :custom
  (dap-print-io nil)
  (dap-inhibit-io nil)
  (dap-auto-configure-mode t "Automatically configure dap.")
  (dap-auto-configure-features '(sessions expressions tooltip))

  :config
  (require 'dap-lldb)
  (setq dap-lldb-debug-program '("/opt/homebrew/opt/llvm/bin/lldb-dap" . ("/opt/homebrew/opt/llvm/bin/lldb-dap")))
  (setq dap-output-buffer-filter '("stdout" "stderr" "console"))

  (add-to-list 'savehist-additional-variables 'dap-lldb-mi-target-platform-history)
  (defun get-target-platform () (read-from-minibuffer "Select target for debug: " nil nil nil 'dap-lldb-mi-target-platform-history))
  (defun dap-gdb-lldb--populate-lldb-mi (conf)
    "Populate CONF with the required arguments."
    (plist-put conf :target (let ((target (plist-get conf :target))) (cond ((functionp target) (funcall target)) (t target))))
    (plist-put conf :executable (let ((exec (plist-get conf :executable))) (cond ((functionp exec) (funcall exec)) (t exec))))
    (plist-put conf :extra_commands (let ((cmds (plist-get conf :extra_commands))) (cond ((functionp cmds) (funcall cmds)) (t cmds))))
    (-> conf
        (dap--put-if-absent :type "lldb-mi")
        (dap--put-if-absent :name "LLDB Remote")
        (dap--put-if-absent :cwd (expand-file-name default-directory))
        (dap--put-if-absent :lldbmipath (expand-file-name "~/foss/lldb-mi/src/lldb-mi"))
        (dap--put-if-absent :dap-server-path `("node" ,(expand-file-name "~/foss/code-debug/out/src/lldb.js"))) ; to update: cd ~/foss/code-debug && npm run compile

        (dap--put-if-absent :printCalls nil)
        (dap--put-if-absent :showDevDebugOutput nil)

        ;; This may become unnecessary once https://github.com/WebFreak001/code-debug/issues/344 is resolved.
      (dap--put-if-absent :valuesFormatting "prettyPrinters")))
  (dap-register-debug-provider "lldb-mi" 'dap-gdb-lldb--populate-lldb-mi)
  (dap-register-debug-template
   "lldb-server platform"
   ;; target: connect+remote-linux://localhost:1234/main
   ;; remote: lldb-server platform --log-channels gdb-remote all --listen *:1234 --gdbserver-port 1337
   ;; lldb-mi: platform select ${platform:-remote-linux}
   ;;          platform connect ${remote_host:-localhost:1234}
   ;;          file-exec-and-symbols "${target:-main}
   (list :type "lldb-mi"
         :request "launch"
         :target #'get-target-platform
         :extra_commands '("b main") ; https://github.com/WebFreak001/code-debug/blob/eb9f5e4d/src/lldb.ts#L7
         :cwd "/tmp"
         :name "lldb-server platform remote"))
  (dap-register-debug-template
   "gdbserver"
   ;; target: main
   ;; remote: gdbserver :1234 main
   ;; lldb-mi: file-exec-and-symbols main
   ;;          interpreter-exec console "gdb-remote localhost:1234"
   (list :type "lldb-mi"
         :request "launch"
         :target #'(lambda () (expand-file-name (read-file-name "Select local binary: ")))
         :extra_commands #'(lambda () `(,(format "gdb-remote %s" (get-target-platform)) "b main")) ; code-debug/src/backend/mi2/mi2lldb.ts:45
         :stopAtStart t
         :stopAtEntry nil
         :name "gdbserver remote"))

  (advice-add 'delete-other-windows :around #'(lambda (func &rest args) (let ((ignore-window-parameters t)) (dap-ui-hide-many-windows) (apply func args))))

  (defun dap-debug-or-show ()
    (interactive)
    (if (dap--cur-session) (dap-ui-show-many-windows) (call-interactively  'dap-debug)))

  ;; https://emacs-lsp.github.io/dap-mode/page/how-to/#activate-minor-modes-when-stepping-through-code
  (define-minor-mode +dap-running-session-mode
    "A mode for adding keybindings to running sessions"
    :init-value nil
    :lighter " DAP"

    (when (member major-mode development-modes)
      (message "%s" (buffer-name) major-mode )
      (read-only-mode 1)
      (add-to-list 'minor-mode-overriding-map-alist
                   (cons '+dap-running-session-mode

(define-keymap
      "b" #'dap-breakpoint-toggle
      "B" #'dap-breakpoint-condition
      "C-B" #'dap-breakpoint-hit-condition
      "S-B" #'dap-breakpoint-log-message
      "c" #'dap-continue
      "d" #'dap-down-stack-frame
      "D" #'dap-disconnect
      "e" #'dap-eval
      "E" #'dap-eval-thing-at-point
      "S-e" #'dap-eval-region
      "f" #'dap-step-out
      "n" #'dap-next
      "r" #'dap-restart-frame
      "R" #'dap-debug-restart
      "s" #'dap-step-in
      "t" #'dap-switch-thread
      "T" #'dap-stop-thread
      "u" #'dap-up-stack-frame
      "q" #'dap-delete-all-sessions)

                         )))

    ;; The following code adds to the dap-terminated-hook so that this minor mode will be deactivated when the debugger finishes
    (when +dap-running-session-mode
      (let ((session-at-creation (dap--cur-active-session-or-die)))
        ;; DAP deactivation hook
        (add-hook 'dap-terminated-hook
                  (lambda (session)
                    (when (eq session session-at-creation)
                      (read-only-mode -1)
                      (dap-ui-hide-many-windows)
                      (+dap-running-session-mode -1)))))))

  ;; Activate this minor mode when dap is initialized
  (add-hook 'dap-session-created-hook '+dap-running-session-mode)
  ;(add-hook 'dap-session-created-hook 'dap-ui-repl)

  ;; Activate this minor mode when hitting a breakpoint in another file
  (add-hook 'dap-stopped-hook '+dap-running-session-mode)

  ;; Activate this minor mode when stepping into code in another file
  (add-hook 'dap-stack-frame-changed-hook
            (lambda (session) (when (and development-mode (dap--session-running session))
                                (+dap-running-session-mode 1)))))

;; Set context-dependent tabulation widths
(add-hook 'c++-mode-hook
  #'(lambda ()
     (when buffer-file-name
       (cond
        ((or (string-match "^/usr/include/c++" buffer-file-name)
             (string-match "^/usr/include/x86_64-linux-gnu/c++" buffer-file-name))
         (make-variable-buffer-local 'tab-width)
         (set-variable 'tab-width 8))))))


;; C++ additional face locks
(make-face 'c++11-raw-string-face)
(set-face-foreground 'c++11-raw-string-face "#003000")
(set-face-background 'c++11-raw-string-face "#ddffdd")
(font-lock-add-keywords
 'c++-mode
 '(("\\<\\(nullptr\\|static_assert\\)\\>" 1 font-lock-keyword-face t)
   ("\\<\\(atomic_cancel\\|atomic_commit\\|atomic_noexcept\\|reflexpr\\|synchronized\\)\\>" 1 font-lock-keyword-face t)
   ;; preprocessor constants
   ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
   ;; hexadecimal numbers
   ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
   ;; integer/float/scientific numbers
   ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
   ;; raw strings
   ("\\(R\\s\"\\(.*\\)([^)]*)\\2\\s\"\\)" 1 'c++11-raw-string-face t)))


;; end of compilation hook
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

;; Replace some template literals in compilation output
(defun replace-char-type-literals (start end)
  (save-excursion
    (goto-char start)
    (previous-line)
    (narrow-to-region (point) (point-max))
    (while (re-search-forward "sc::string_constant<\\([^,]+\\)\\(\\(, ([^)]+)[0-9]+\\)+\\)>" nil t)
      (progn
        (let* ((sc (save-match-data
                     (condition-case err
                         (let* ((match (match-string 0))
                                (type (match-string 1))
                                (ordinals (split-string (match-string 2) (format ", (%s)" type)))
                                (characters (mapcar (lambda (num) (let ((x (string-to-number num))) (if (> x 0) (string x) ""))) ordinals)))
                           (format  "\"%s\"_sc" (apply #'concat characters)))
                       (error
                        (progn (message "error occurred in replace-char-type-literals: %s\n  %s" (error-message-string err) match) nil))))))
        (when sc
          (replace-match sc t)
          (put-text-property (match-beginning 0) (match-end 0) 'font-lock-face `(:background  ,(face-attribute 'lazy-highlight :background)))))))))

;; ANSI colorization of compilation output
(add-hook
 'compilation-filter-hook
 (lambda ()
   (ignore-errors
     (when (eq major-mode 'compilation-mode)
       (replace-char-type-literals compilation-filter-start (point-max))
       (ansi-color-apply-on-region compilation-filter-start (point-max))))))

;; Set development environemnt variables
(setenv "PYTHONPYCACHEPREFIX" (expand-file-name "~/.cache/pycache"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global minor modes
(column-number-mode)
(delete-selection-mode)
(ido-mode 'buffer)
(line-number-mode)
(save-place-mode)
(savehist-mode)
(show-paren-mode)
(tool-bar-mode -1)

(put 'upcase-region 'disabled nil)       ;; allow conversion of a region to upper case.
(put 'downcase-region 'disabled nil)     ;; allow conversion of a region to lower case.

(setq auto-mode-alist
      (append
       '(("poetry.lock" . conf-toml-mode)
         ("\\.ldx$" . ld-script-mode))
       auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global hooks
(add-hook 'before-save-hook              ;; delete trailing workspaces on save
          'delete-trailing-whitespace)   ;; to remove (remove-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
(mapc (lambda (entry)
        (cl-destructuring-bind (key function &rest args) entry
          (if args (keymap-global-set key (lambda () (interactive) (apply function args)))
            (keymap-global-set key function))))
      '(("<f1>" context-help)
	("<f4>" query-replace-regexp)
	("<f5>" revert-buffer)
	("C-S-s" search-forward-regexp)
	("C-c ;" comment-region)
	("C-c '" uncomment-region)
	("C-`" ispell-word)
        ("C-x C-f" find-file-goto)
        ("C-<return>" mini-calc)
        ("S-s" isearch-forward-regexp)
        ("S-r" isearch-backward-regexp)
	;; Clipboard
	("C-<help>" clipboard-kill-ring-save)
	("S-<help>" clipboard-yank)
	("S-<delete>" clipboard-kill-region)
	;; Window movement
	("<home>" move-beginning-of-line)
	("<end>" move-end-of-line)
	("C-<up>" scroll-down-command)
	("C-<down>" scroll-up-command)
	("M-`" dap-debug-or-show)
	("M-?" goto-line)
	("M-<left>" move-beginning-of-line)
   	("M-<right>" move-end-of-line)
	("M-<up>" beginning-of-buffer)
	("M-<down>" end-of-buffer)
	("s-<left>" windmove-left)
   	("s-<right>" windmove-right)
	("s-<up>" windmove-up)
	("s-<down>" windmove-down)
        ;; Buffer default font height
        ("C--" scale-face-default-height 0.9)
        ("C-=" scale-face-default-height 1.1)
        ("C-0" set-face-attribute default nil :height 120)
	;; Buffer cycling
	("C-<tab>" cyclebuffer-forward)
	("C-S-<tab>" cyclebuffer-backward)
	;; Bootstrap org mode
	("C-c c" org-capture)
	;; Bootstrap magit mode
	("C-c s" magit-status)
	("C-c b" magit-blame)
	("C-c l" magit-log-buffer-file)
	("C-c g" gh-lines)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globally customized variables
(mapc (lambda (args) (apply 'funcall #'set-standard-toplevel-value args))
      `((default-frame-alist ((top . 0) (left . 1.0) (width . 0.92) (fullscreen . fullheight)))
        (backup-directory-alist ((".*/\\.git/.*")
                                 (".*" . ,(expand-file-name ".backups" user-emacs-directory))))
	(initial-scratch-message nil)
        (ring-bell-function nil)
        (compilation-ask-about-save nil)
        (gud-tooltip-mode t)
        (gdb-debuginfod-enable-setting t)
	(inhibit-startup-screen t)
	(indent-tabs-mode nil)
	(blink-cursor-mode nil)
	(find-file-visit-truename t)
	(grep-command "grep --exclude-dir=\"tmp\" -nHriIZ -e ")
	(use-short-answers t)
	(history-length t)
	(version-control t)
	(delete-old-versions t)
        (confirm-kill-processes nil)
	(undo-limit 24000000)
        (large-file-warning-threshold 200000000)
	(ispell-program-name "aspell")
	(ispell-check-comments t)
	(revert-without-query (".*"))
	(read-buffer-completion-ignore-case t)
        (js-indent-level 2)
        (css-indent-offset 2)
        (fill-column 120)
        (gud-highlight-current-line t)
        (auto-save-visited-file-name t)
	(scroll-error-top-bottom t)))

(add-to-list 'ispell-skip-region-alist '("[\\@]req\\([[:space:]]+[[:word:]+]\\)?[[:space:]]*{" . "}"))
(add-to-list 'ispell-skip-region-alist '("sha256:[[:xdigit:]]*" . "[^[:xdigit:]]"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System and user specific customized variables
(setq custom-system-file (expand-file-name (format "%s/custom.%s.el" user-emacs-directory (system-name))))
(unless (file-readable-p custom-system-file)
  (write-region (format ";; -*- mode: lisp -*-\n;; (system-name) is %s" (system-name)) nil custom-system-file))
(load custom-system-file)
(setq custom-file (expand-file-name (format "%s/custom.el" user-emacs-directory)))
(when (file-readable-p custom-file)
  (load custom-file))
