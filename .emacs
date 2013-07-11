(require 'cl)

;;{{{ Variables describing environment Emacs is running in

(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
(defvar running-on-windows (eq 'windows-nt system-type))
(defvar running-on-gnu/linux  (string-equal system-type "gnu/linux"))
(defvar running-on-darwin  (string-equal system-type "darwin"))
(defvar mule-present (featurep 'mule))
(defvar running-on-x (cond (running-xemacs (eq (console-type) 'x)) (t (eq window-system 'x))))

(defvar emacs-flavor
  (concat (if running-xemacs "xemacs" "gnuemacs") "."
          (int-to-string emacs-major-version) "."
          (int-to-string emacs-minor-version)))

;;}}}

;;{{{ load-path setup

(defun get-dir (name)
  (car (remove-if-not #'file-accessible-directory-p (append (file-expand-wildcards (concat custom-dir name))
                                                                              (file-expand-wildcards (concat custom-dir "*/" name))))))

;; Store customization information in file specific for emacs version
(setq custom-dir (expand-file-name "~/.emacs.d/"))
(setq custom-file (concat custom-dir "custom." emacs-flavor ".el"))
(progn (cd custom-dir) (normal-top-level-add-subdirs-to-load-path))
(setq recentf-save-file (concat custom-dir "/.recentf"))
(setq preview-dir (get-dir "/auctex*/preview"))
(setq cedet-dir (get-dir "/cedet-*"))
(setq python-dir (get-dir "/python-mode*"))
(setq ruby-dir (get-dir "/ruby-mode*"))
(setq git-dir (get-dir "/git-*"))
(setq feature-dir (get-dir "/cucumber.el/feature-*"))
(setq ects-dir (get-dir "/emacs-color-theme-solarized*"))

;; append custom pathes
(add-to-list 'load-path custom-dir)
(if python-dir (add-to-list 'load-path python-dir))
(setq load-path (append (list preview-dir git-dir ects-dir ruby-dir feature-dir) load-path))
(add-to-list 'load-path "/scratch/miha/local/share/emacs/site-lisp")


;; relocate other files so we don't clutter $HOME
(setq save-place-file (concat custom-dir "/save-places"))
(setq auto-save-list-file-prefix (concat custom-dir "/auto-save-list.d/"))

;;}}}

;; {{{ Setup ELPA repositories

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; }}}

;;{{{ Customization

;; Load customization information if it exists
(if (file-exists-p custom-file)
    (load custom-file))

(when (< emacs-major-version 24) 
  (pc-bindings-mode)
  (if (< emacs-major-version 22) (pc-selection-mode) (pc-selection-mode t)))

;; functional keys
(global-set-key [f4] 'query-replace-regexp)
(global-set-key [s-f4] 'search-forward-regexp)
(global-set-key [f5] 'revert-buffer)
;; (global-set-key [f6] 'other-window)
(global-set-key "\M-?" 'goto-line)
(global-set-key [C-x-p] 'bury-buffer)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c'" 'uncomment-region)
(global-set-key [C-f4] (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key [(mouse-4)] '(lambda () (interactive) (scroll-down (/ (window-height) 2))))
(global-set-key [(mouse-5)] '(lambda () (interactive) (scroll-up   (/ (window-height) 2))))
(global-set-key "\C-x\C-g" 'recentf-open-files)

;; shift+meta+<> keys
(global-set-key (vector (list 'super 'left))  'windmove-left)
(global-set-key (vector (list 'super 'right)) 'windmove-right)
(global-set-key (vector (list 'super 'up))    'windmove-up)
(global-set-key (vector (list 'super 'down))  'windmove-down)


(setq frame-title-format (list user-login-name " on " system-name " - %b - " invocation-name))

(when (> emacs-major-version 22) (savehist-mode 1))
(menu-bar-enable-clipboard)
(turn-off-auto-fill)
(setq fill-column 120)
(setq split-width-threshold 200)
(recentf-mode 1)                                     ;; Recent files in menu
(setq recentf-max-menu-items 125)
(delete-selection-mode 1)                            ;; hitting delete will delete the highlighted region
(setq undo-limit 20000000)
(setq revert-without-query '(".*"))
(transient-mark-mode 1)                              ;; When the mark is active, the region is highlighted.
;(setq inhibit-startup-screen t)                     ;; Silent boot
(setq initial-scratch-message nil)                   ;; Clear scratch buffer
(setq initial-major-mode 'text-mode)                 ;; text mode is default
(set-scroll-bar-mode 'right)                         ;; vertical scroll bars on the right side.
(global-font-lock-mode t)                            ;; Turn on font-lock in all modes that support it.
(setq font-lock-maximum-decoration t)                ;; use the maximum decoration available
(show-paren-mode t)                                  ;; Highlight matching parentheses.
(toggle-save-place-globally)                         ;; Save Emacs state for next session.
(setq default-major-mode 'text-mode)                 ;; Make text mode default major mode.
(setq shell-prompt-pattern "^[^#$%>\n]*[#$%>\)] *")  ;; My shell prompt ends on ")".
(setq visible-bell t)                                ;; Turn off beep.
(setq ring-bell-function 'ignore)                    ;; Turn the alarm totally off
(standard-display-8bit 128 255)                      ;; Do not expand unprintable characters to their octal values.
(setq fortran-comment-region "C MKR")                ;; Fortran comments prefix.
(setq-default tab-width 4)                           ;; Set Tab-Width.
(line-number-mode 1)                                 ;; Show line-number in the mode line.
(column-number-mode 1)                               ;; Show column-number in the mode line.
(fset 'yes-or-no-p 'y-or-n-p)                        ;; Will allow you to type just "y" instead of "yes".
(setq-default indent-tabs-mode nil)                  ;; Permanently force Emacs to indent with spaces.
(put 'upcase-region 'disabled nil)                   ;; Convert the region to upper case.
(put 'downcase-region 'disabled nil)                 ;; Convert the region to lower case.
(blink-cursor-mode -1)                               ;; Switch off blinking cursor mode.
(setq large-file-warning-threshold nil)              ;; Maximum size of file above which a confirmation is requested
(setq printer-name "pscs301")
(tool-bar-mode -1)
(setq vc-follow-symlinks t)
(setq grep-command "grep -nHriI -e ")
(if (not (assq 'user-size initial-frame-alist))      ;; Unless we've specified a number of lines, prevent the startup code from
    (setq tool-bar-originally-present nil))          ;; shrinking the frame because we got rid of the tool-bar. 
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq calendar-week-start-day 1)

;; Place Backup Files in Specific Directory
(setq make-backup-files t)                           ;; Enable backup files.
(setq version-control t)                             ;; Enable versioning with default values.
(setq delete-old-versions t)
(setq backup-directory-alist                         ;; Save all backup file in this directory.
      (list `(".*" . ,(concat custom-dir "/.emacs.backups"))))

(defvar insert-time-format "%T" "*Format for \\[insert-time].")
(defvar insert-date-format "%e %B %Y" "*Format for \\[insert-date].")

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;;}}}

;;{{{ Load local packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling
(require 'smooth-scrolling)
(setq smooth-scroll-margin 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired mode
(load-library "dired")

(setenv "LANG" "POSIX")

(defvar dired-sort-map (make-sparse-keymap))

;; sort keys
(define-key dired-mode-map "s" dired-sort-map)
(define-key dired-sort-map "s" (lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches "S"))))
(define-key dired-sort-map "x" (lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches "X"))))
(define-key dired-sort-map "t" (lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches "t"))))
(define-key dired-sort-map "n" (lambda () "sort by Name" (interactive) (dired-sort-other dired-listing-switches)))
(define-key dired-sort-map "d" (lambda () "sort by name grouping Dirs" (interactive) (dired-sort-other (concat dired-listing-switches " --group-directories-first"))))

;; humanized output
(setq dired-listing-switches-styles '("-alh" "-al"))
(setq dired-listing-switches-idx 0)
(setq dired-listing-switches (nth dired-listing-switches-idx dired-listing-switches-styles))
(define-key dired-mode-map "h" (lambda () (interactive) 
                                 (setq dired-listing-switches-idx (% (1+ dired-listing-switches-idx) (length dired-listing-switches-styles)))
                                 (setq dired-listing-switches (nth dired-listing-switches-idx dired-listing-switches-styles)) 
                                 (setq dired-actual-switches dired-listing-switches)
                                 (revert-buffer)))

;; additional faces
(defface dired-compressed-file
  '((t (:foreground "violet")))
  "*Face used for compressed files in dired buffers."
  :group 'dired :group 'font-lock-highlighting-faces)
(defvar dired-compressed-file 'dired-compressed-file)

(defface dired-image-file
  '((t (:foreground "magenta4")))
  "*Face used for image files in dired buffers."
  :group 'dired :group 'font-lock-highlighting-faces)
(defvar dired-image-file 'dired-image-file)

(defface dired-exe-file
  '((t (:foreground "lime green")))
  "*Face used for executable files in dired buffers."
  :group 'dired :group 'font-lock-highlighting-faces)
(defvar dired-exe-file 'dired-exe-file)

(defun mark-filename (re face)  (list re `(".+" (dired-move-to-filename) nil (0 ,face))))
(setq dired-font-lock-keywords (append dired-font-lock-keywords
   (list
    (mark-filename "[^ .]\\.\\([bgx]?[zZ]2?\\|[bg]?zip\\|rar\\)[*]?$" dired-compressed-file)
    (mark-filename "[^ .]\\.\\(jpg\\|png\\|eps\\)[*]?$" dired-image-file)
    (mark-filename dired-re-exe dired-exe-file))))

;; use a single buffer for dired mode
(require 'dired-single)
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it'sloaded."
  (require 'dired-sort-menu)
  (setq mouse-1-click-follows-link 200)
  (defun my-dired-single-buffer ()
    (interactive)
    (let ((name (dired-get-filename nil t)))
      (cond ((file-accessible-directory-p name) (joc-dired-single-buffer name))
            (t (joc-dired-single-buffer)))))
  (define-key dired-mode-map [return] 'my-dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'my-dired-single-buffer)
  (define-key dired-mode-map (read-kbd-macro "<backspace>") (function (lambda nil (interactive) (joc-dired-single-buffer "..")))))
(if (boundp 'dired-mode-map) (my-dired-init) (add-hook 'dired-load-hook 'my-dired-init))
(define-key dired-mode-map (read-kbd-macro "<f8>") 'dired-do-delete)

; use openwith minor mode
(when (>= emacs-major-version 23)
  (require 'openwith)
  (openwith-mode t)
  (setq openwith-associations
      '(("[^_]?\\.\\(ps\\|pdf\\|djvu\\)\\'" "okular" (file))
        ("\\.\\(docx?\\|ppt\\|rtf\\|xlsx?\\)\\'" "libreoffice" (file))
        ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp4\\)\\'" "smplayer" (file)))))

;;; Shell mode
(setq ansi-color-names-vector ["black" "red4" "green4" "yellow4" "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translator (google)
(when (> emacs-major-version 21)
(require 'google-translate)
(global-set-key "\C-xt" 
   (lambda ()
     (interactive)
     (let ((bnd (bounds-of-thing-at-point 'word))
           ;; TODO: add customization (C-u)
           (src google-translate-default-source-language)
           (dst google-translate-default-target-language)
           beg end)
       (cond 
        (mark-active (setq beg (region-beginning) end (region-end)))
        (bnd (setq beg (car bnd) end (cdr bnd))))
      (when (and beg end)
        (goto-char end)
        (setq orig (buffer-substring-no-properties beg end))
        (setq str orig)
        ;; remove TeX comamnds
        (setq str (replace-regexp-in-string "\\\\[a-zA-Z]+\\({[a-zA-Z]+}\\)?" "" str))
        (setq str (replace-regexp-in-string "[{}]" "" str))
        (insert (format "\n%s\n" (google-translate-translate-text src dst str)))))))
(setq google-translate-default-source-language "en")
(setq google-translate-default-target-language "ru"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git mode
(when git-dir
  (require 'git-emacs)
  (require 'git-status)
  (global-set-key "\C-xgs" 'git-status)
  (defun insert-find-executable()
    (interactive)
    (insert (shell-command-to-string "find . -executable -type f"))))

(when (setq dir (get-dir "magit*"))
  (setq load-path (cons dir load-path))
  (require 'magit)
  (require 'magit-blame))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Featrue mode
(when feature-dir
  (require 'feature-mode)
  (add-to-list 'auto-mode-alist '("\\.feature$" . feature-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matlab mode
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(custom-set-variables '(matlab-comment-region-s "% "))
(setq matlab-fill-code nil)
(custom-set-faces '(matlab-cellbreak-face ((t (:foreground "Firebrick" :weight bold)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; m4 mode
(load-library "m4-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote file editing via ssh
(require 'tramp) 
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Visible bookmarks in buffer.
(require 'bm)
;; M$ Visual Studio key setup.
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Po files
(autoload 'po-mode "start-po" "PO major mode" t)
(setq auto-mode-alist (cons '("\\.po[tx]?\\'\\|\\.po\\." . po-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subversion mode
(require 'psvn)
(setf svn-status-hide-unmodified t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup spell checker
(if (executable-find "aspell")
    (progn
      (require 'ispell)
      (setq-default ispell-program-name "aspell")
      (ispell-change-dictionary "american" t)
      (setq ispell-check-comments nil)
      (global-set-key (kbd "C-`") 'ispell-word)
      (add-hook 'text-mode-hook 'flyspell-mode)
      (add-hook 'org-mode-hook 'flyspell-mode))
  (message "Aspell not found."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript mode
(when (setq dir (get-dir "js3-mode*"))
  (setq load-path (cons dir load-path))
  (autoload 'js3-mode "js3" nil t))

(when (setq dir (get-dir "skewer-mode*"))
  (setq load-path (cons dir load-path))
  (load-library "skewer-mode")
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C and C++ modes
(require 'hide-comnt)
(require 'cc-langs)
(require 'cc-mode)
(require 'jam-mode)
(require 'cmake-mode)
(require 'gud)
(require 'gdb-mi)
(require 'qml-mode)

(setq gud-tooltip-mode t)

(setq compilation-ask-about-save nil)
;; (setq compilation-window-height 10)

(defun get-compile-command () 
  (when (and buffer-file-name compile-command)
    (let* ((bufname (file-name-nondirectory (buffer-file-name)))
           (filename (file-name-sans-extension bufname))
           (extension (file-name-extension bufname))
           (dirname (expand-file-name "."))
           (tex (if (and (boundp 'TeX-master) (stringp TeX-master)) TeX-master bufname)))
      (setf compile-command (replace-regexp-in-string "%%" "%" compile-command))
      (setf compile-command (replace-regexp-in-string "%f" bufname compile-command))
      (setf compile-command (replace-regexp-in-string "%d" dirname compile-command))
      (setf compile-command (replace-regexp-in-string "%n" filename compile-command))
      (setf compile-command (replace-regexp-in-string "%e" extension compile-command))
      (setf compile-command (replace-regexp-in-string "%t" tex compile-command)))))

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

(setq development-mode-hook
  ;;(print (mapcar (lambda (x) (car x)) (buffer-local-variables)))
  ;;(print (remove-if-not (lambda (x) (eq 'compile-command (car x))) (buffer-local-variables)))
  (function (lambda ()
    (let* ((bname (buffer-name))
           (fname (file-name-sans-extension bname))
           (ext (file-name-extension bname)))

      ;(gtags-mode 1)

      ;; add OpenFOAMcompile option
      (when (getenv "WM_COMPILE_OPTION")
        (add-to-list 'mode-line-format (concat (getenv "WM_COMPILE_OPTION") " ")))

      ;; compiler command, depends on the major-mode
      (local-set-key "\C-c\C-c"  'compile)
      (make-variable-buffer-local 'compile-command)
      (setq compile-command
         (cond
          ((eq major-mode 'c++-mode)
           (cond
            ((string-equal ext "nxc") "nbc %f -O=%n.rxe; nxt_push %n.rxe")
            (t "g++ -Wall -std=c++11 -O0 -g %f -o %n")))
          ((eq major-mode 'fortran-mode) "g77 -g %f -o %n")
          ((eq major-mode 'qt-pro-mode) "qmake && make")
          ((eq major-mode 'makefile-gmake-mode) "make")
          ((eq major-mode 'jam-mode) "bjam -d2")
          (t "make")))
      (get-compile-command)

      ;; run command, allow only commands in that starts with "./"
      (make-variable-buffer-local 'run-command)
      (local-set-key '[S-f5]  (lambda () (interactive) (shell-command run-command)))
      (cond
       ((eq major-mode 'python-mode)
        (setf run-command (format "python %s" (buffer-file-name))))
       ((eq major-mode 'qml-mode)
        (setf run-command (format "qmlscene %s &" (buffer-file-name)))
        (local-set-key '[S-f5]  (lambda () (interactive) (save-window-excursion (shell-command run-command)))))
       (t (setf run-command (format "./%s" (file-name-sans-extension (buffer-name))))))
      (put 'run-command 'safe-local-variable 'run-command-safe-variable)
      (defun run-command-safe-variable (var) (string-match 
         "^[ \t\n\r]*\./.+\\|qml\\(scene\\|viewer\\)\\|/usr/local/diana/bin/mpi\\(run\\|exec\\)[ \t\n\r]" var))
      )
    
    ;; settings depending on the mode
    (when (or (eq major-mode 'c++-mode) (eq major-mode 'fortran-mode)
              (eq major-mode 'jam-mode))
      ;; (flyspell-prog-mode)
      (local-set-key '[C-f8]   'flyspell-buffer)
      ;; source file keys
      (local-set-key '[f3]     'find-tag)
      (local-set-key '[C-f3]    (lambda () (interactive) (find-tag nil t)))
      (local-set-key '[C-S-f3] 'pop-tag-mark)
      ;; other settings
      (setq indent-tabs-mode nil))
    
    (when (eq major-mode 'fortran-mode)
      (local-set-key "\C-c'" 'fortran-comment-region))
    
    (when (or (eq major-mode 'c++-mode) (eq major-mode 'fortran-mode) (eq major-mode 'compilation-mode)
              (eq major-mode 'jam-mode) (eq major-mode 'makefile-gmake-mode) (eq major-mode 'python-mode)
              (eq major-mode 'qt-pro-mode))
      ;; compile keys
      (local-set-key '[f8]   'next-error)
      (local-set-key '[S-f8] 'previous-error)
      (local-set-key '[f7]   (lambda () (interactive) (compile (get-compile-command))))
      (local-set-key "\C-c\C-c" 'compile))
    
    (when (and (not running-on-windows)
               (or (eq major-mode 'c++-mode) (eq major-mode 'fortran-mode)
                   (eq major-mode 'gud-mode) (eq major-mode 'python-mode)))
      ;; (string-match "\\*gud-\\(.+\\)\\*" (buffer-name gud-comint-buffer))
      ;; debug functions
      (gud-def gud-frame "frame" "\C-g" "Select and print a stack frame.")
      
      ;; debug keys
;      (local-set-key '[f5]     (lambda () (interactive) 
;                                 (if (get-buffer-process gud-comint-buffer) 
;                                     (if gdb-active-process (gud-call "cont") (gud-call "run"))
;                                   (gud-refresh))))
      (local-set-key '[C-f5]   'gud-until)
      (local-set-key '[f9]     'gud-set-clear)
      (local-set-key '[S-f9]   'gud-break)
      (local-set-key '[C-f9]   'gud-remove)
      (local-set-key '[f10]    'gud-next)
      (local-set-key '[f11]    'gud-step)
      (local-set-key '[f12]    'gud-finish))
    )))

;; set global GDB properties and keys
(setf gdb-many-windows t)
(setf gdb-show-main t)
(setf gdb-show-threads-by-default t)
(global-set-key (kbd "s-`") (lambda () (interactive)(when (buffer-name gud-comint-buffer) (gdb-many-windows 1))))
(defun gdb-kill-buffers () (interactive)
  (set-process-query-on-exit-flag (get-buffer-process gud-comint-buffer) nil)
  (kill-buffer gud-comint-buffer)
  (loop for b in '(breakpoints threads memory disassembly stack locals registers update current-context)
        do (kill-buffer (funcall (intern (concatenate 'string "gdb-" (symbol-name b) "-buffer-name"))))))

;; Qt stuff
(require 'qt-pro)
(c-add-style "qt-gnu" '("gnu" 
    (c-access-key . "^\\(public\\|protected\\|private\\|signals\\|public slots\\|protected slots\\|private slots\\):")
    (c-basic-offset . 4)))
;; make new font for rest of qt keywords
(make-face 'qt-keywords-face)
(set-face-foreground 'qt-keywords-face "midnight blue")
;; qt keywords
(font-lock-add-keywords 'c++-mode '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)
                                    ("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face) 
                                    ("\\<Q_[A-Z][_A-Za-z]*" . 'qt-keywords-face)
                                    ("\\<foreach\\>" . 'qt-keywords-face)))
(setq c-default-style "qt-gnu")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode
(when python-dir
  (setq py-install-directory python-dir)
  (require 'python-mode)
  (setq py-load-pymacs-p t))
(setq python-indent 2)

;;pdb setup, note the python version
(setq pdb-path
  (cond
   ((string= system-name "miha-lt") '/usr/lib/python2.7/pdb.py)
   (t '/usr/lib/python2.7/pdb.py)))
(setq gud-pdb-command-name (symbol-name pdb-path))

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path (file-name-nondirectory buffer-file-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby mode
; loads ruby mode when a .rb file is opened.
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set hooks
(setq  c-mode-hook development-mode-hook
       c++-mode-hook development-mode-hook
       fortran-mode-hook development-mode-hook
       jam-mode-hook development-mode-hook
       makefile-gmake-mode-hook development-mode-hook
       qml-mode-hook development-mode-hook
       qt-pro-mode-hook development-mode-hook
       gud-mode-hook development-mode-hook)
(add-hook 'python-mode-hook development-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AucTeX
(when (setq dir (get-dir "auctex"))
  (load "preview-latex.el" nil t t)
  (setq preview-default-document-pt 12)
  ;;  TeX-style-path
  (setq LaTeX-enable-toolbar nil)
  (setq font-latex-title-fontify 'color) 
  (require 'tex)
  (require 'tex-site)
  (require 'latex)
  (require 'font-latex)
  (require 'okular-search)
  (setq TeX-command-default "XeLaTeX")
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-auto-local ".auctex-auto")
  (setq TeX-style-local ".auctex-auto")
  (setq-default TeX-master nil)
  (setq font-latex-title-fontify 'color)
  (setq font-latex-fontify-script nil)
  (setq ispell-parser 'tex)
  (custom-set-faces '(font-latex-sectioning-5-face ((((class color) (background light)) (:inherit nil :foreground "blue4")))))

  ; (setq TeX-source-correlate-method-active 'source-specials)

  (defun my-tex-init ()
    (TeX-fold-mode 1)
    (setq fill-column 100)

    (add-to-list 'TeX-expand-list 
       '("%u" (lambda () (concat "file://" (expand-file-name (funcall file (TeX-output-extension) t) (file-name-directory (TeX-master-file)))
                                 "#src:" (TeX-current-line) (TeX-current-file-name-master-relative)))))
    (setq TeX-view-program-list
          '(("Okular" "okular \"%u\" --unique")
            ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))
    (if running-on-gnu/linux (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular"))))
    (if running-on-darwin (setq TeX-view-program-selection '((output-pdf "Skim"))))

    (local-set-key (kbd "C-c C-b") (lambda () (interactive) (TeX-command "PDFLaTeX" 'TeX-master-file)))
    (local-set-key (kbd "C-c t") 'gt-region-or-thing-at-point)
    (local-set-key '[C-f8] 'flyspell-buffer)

    (add-to-list 'TeX-complete-list '("\\\\citep\\[[^]\n\r\\%]*\\]{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}"))
    (add-to-list 'TeX-complete-list '("\\\\citep{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}"))
    (add-to-list 'TeX-complete-list '("\\\\citep{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]*\\)" 2 LaTeX-bibitem-list))

    ;; add a list of commands
    (setq TeX-command-list (append TeX-command-list
     '(("XeLaTeX" "xelatex -synctex=1 %(mode) \"\\input\" %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run PDFLaTeX")
       ("XeLaTeX18" "xelatex -synctex=1 %(mode) --enable-write18 \"\\input\" %t" TeX-run-TeX nil (latex-mode doctex-mode))
       ("PDFLaTeX" "pdflatex -synctex=1 %(mode) \"\\input\" %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run PDFLaTeX")
       ("PDFLaTeX18" "pdflatex -synctex=1 %(mode) --enable-write18 \"\\input\" %t" TeX-run-TeX nil (latex-mode doctex-mode))
       ("Acroread" "acroread %s.pdf" TeX-run-command nil t)
       ("DVIPS" "dvips -o %s.ps %s.dvi" TeX-run-command nil t)
       ("Clean" "rm %s.log %s.aux %s.out %s.idx %s.dvi" TeX-run-command nil t)
       ("Clean All" "rm -f *.log *.aux *.out *.idx *.dvi *.bbl *.blg *.nav *.snm *.toc *~" TeX-run-command nil t)
       ("PS" "%l %(mode) \"\\input\" %t && dvips -o %s.ps %s.dvi" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX and DviPS")
       ("ViewPS" "gv -antialias -watch -scale 2 %f" TeX-run-discard t t  :help "View PS")
       ("ViewGGV" "ggv %f" TeX-run-discard t t  :help "View PS")
       ("PDF" "%l %(mode) \"\\input\" %t && dvips -Ppdf -o %s.ps %s.dvi && ps2pdf %s.ps" TeX-run-TeX nil (latex-mode doctex-mode))
       ("KPDF" "kpdf %s.pdf" TeX-run-discard t t)
       ("PSPhD" "%l %(mode) -jobname=%s \"\\documentclass{scrbook}\\usepackage{thesis-phd}\\begin{document}\\input{%t}\\end{document}\" 
                 && dvips -o %s.ps %s.dvi" TeX-run-TeX nil (latex-mode doctex-mode)))))
    ;; add a single item
    (add-to-list 'TeX-command-list
       (list "TikZ" "pdflatex %(mode) -jobname=%s \"\\documentclass{article}\\usepackage[utf8]{inputenc}\\usepackage[T2A]{fontenc}\\usepackage[active,tightpage]{preview}\\usepackage[eulergreek]{sansmath}\\usepackage{tikz}\\usepackage{pgfplots}\\usetikzlibrary{shapes,arrows,matrix,shadows,patterns,decorations.markings,calc}\\pgfplotsset{compat=newest,compat/show suggested version=false}\\begin{document}\\begin{preview}\\input{%t}\\end{preview}\\end{document}\"; convert -density 400 -quality 100 %s.pdf %s.jpg; rm -rf %s.aux %s.log" 'TeX-run-TeX nil '(latex-mode doctex-mode) :help "Tikz preview"))
    (add-to-list 'TeX-file-extensions "tikz")

    ;; compile keys
    (make-variable-buffer-local 'compile-command)
    (let* ((ext (file-name-extension (buffer-file-name))))
      (setq compile-command
         (cond 
          ((string-equal ext "tikz" ) "pdflatex -interaction=nonstopmode -jobname=%s \"\\documentclass{article}\\usepackage[pdftex]{graphics}\\usepackage[T1]{fontenc}\\usepackage[active,tightpage]{preview}\\usepackage{tikz}\\usepackage{pgfplots,calligra}\\usetikzlibrary{calc,shapes,arrows}\\begin{document}\\begin{preview}\\input{%f.tikz}\\end{preview}\\end{document}\"; convert -density 1000  %f.tikz.pdf %f.jpg")
          (t  "pdflatex  -interaction=nonstopmode \"\\input\" %t"))))
    (local-set-key '[f7]   (lambda () (interactive) (compile (get-compile-command)))))


  
  (put `TeX-insert-backslash `delete-selection nil)

  (add-hook 'tex-mode-hook    'my-tex-init)
  (add-hook 'TeX-mode-hook    'my-tex-init)
  (add-hook 'LaTeX-mode-hook  'my-tex-init)
  ;(add-hook 'LaTeX-mode-hook  'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook  'TeX-source-correlate-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode
(add-hook 'text-mode-hook '(lambda () (turn-off-auto-fill) (setq fill-column 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
(when (> emacs-major-version 21)
(when (setq dir (get-dir "/org*/lisp"))
  (setq load-path (cons dir load-path)))
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(when (file-exists-p (concat custom-dir "org-customize.el"))
  (load-file (concat custom-dir "org-customize.el")))

(when (setq dir (get-dir "/gnuplot*"))
  (setq load-path (cons dir load-path))
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t))

(when (setq dir (get-dir "/ESS/lisp"))
  (setq load-path (cons dir load-path))
  (require 'ess-site)))

(when (setq dir (get-dir "/scrum"))
  (setq load-path (cons dir load-path))
  (require 'scrum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MMM mode
(when (setq dir (get-dir "/mmm-mode"))
  (setq load-path (cons dir load-path))
  (require 'mmm-auto)

  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-[S]-Tab cycle buffer
(autoload `cyclebuffer-forward "cyclebuffer" "cycle forward" t)
(autoload `cyclebuffer-backward "cyclebuffer" "cycle backward" t)
(global-set-key (kbd "<C-tab>") 'cyclebuffer-forward)
(global-set-key (kbd "<C-S-iso-lefttab>") 'cyclebuffer-backward)
(org-defkey org-mode-map [(control tab)] 'cyclebuffer-forward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; help command
(autoload 'woman "woman"
  "Decode and browse a UN*X man page." t)
(defun custom-help () 
  (interactive) 
  (cond 
   ((or (eq major-mode 'lisp-mode) (eq major-mode 'emacs-lisp-mode))
    (let ((var (variable-at-point)))
      (if (eq var 0)
          (describe-function (function-called-at-point))
        (describe-variable var))))
   (t (when (> (length (current-word)) 1) (woman (current-word))))))
(global-set-key [f1] 'custom-help)

;;}}}

;;{{{ auto-mode-alist setup

;;Changing .h to use automatic C++ formatting (instead of standard C)
(setq auto-mode-alist
      (append
       '((".emacs$" . lisp-mode)
         ("\\.mdl$" . lisp-mode)
         ("\\.cg$" . lisp-mode)
         ("\\.el\\(.gz\\)?$" . lisp-mode)
         ("\\.tikz$" . LaTeX-mode)
         ("\\.tex$" . LaTeX-mode)
         ("\\.ipp" . c++-mode)
         ("\\.h$" . c++-mode)
         ("\\.c$" . c++-mode)
         ("\\.cxx$" . c++-mode)
         ("\\.moc$" . c++-mode)
         ("\\.cu$" . c++-mode)
         ("\\.cuh$" . c++-mode)
         ("\\.C$" . c++-mode)
         ("\\.H$" . c++-mode)
         ("\\.nxc$" . c++-mode)
         ("\\.pro$" . qt-pro-mode)
         ("\\.dps$" . pascal-mode)
         ("\\.qml$" . qml-mode)
         ("\\.pro$" . text-mode)
         ("\\.l$" . c-mode)
         ("\\.y$" . c-mode)
         ("\\.py$" . python-mode)
         ("\\.yaml$" . python-mode)
         ("\\.css$" . css-mode)
         ("\\.Xdefaults$" . xrdb-mode)
         ("\\.Xenvironment$" . xrdb-mode)
         ("\\.Xresources$" . xrdb-mode)
         ("*.\\.ad$" . xrdb-mode)
         ("\\.fetchmailrc$" . fetchmail-mode)
         ("\\.xml$" . nxml-mode)
         ("\\.xsl$" . nxml-mode)
         ("\\.tei$" . nxml-mode)
         ("\\.php$" . php-mode)
         ("\\.dcl$" . dtd-mode)
         ("\\.dec$" . dtd-mode)
         ("\\.dtd$" . dtd-mode)
         ("\\.ele$" . dtd-mode)
         ("\\.ent$" . dtd-mode)
         ("\\.mod$" . dtd-mode)
         ("\\.m\\'" . matlab-mode)
         ("\\.m4\\'" . m4-mode)
         ("\\.mc\\'" . m4-mode)
         ("\\.sql$" . sql-mode)
         ("\\.js\\'" . js3-mode)
         ("Jamfile.v2" . jam-mode) 
         ("\\.jam$" . jam-mode)
         ("\\.s?html$" . sgml-mode)
         ("\\.mc" . m4-mode) 
         ("\\.m4" . m4-mode)
         ("\.i$" . c++-mode)
         ("\.cc$" . c++-mode)
         ("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)
         ) auto-mode-alist))

;;}}}


;;{{{ OS specific

(cond 
 (running-on-windows
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; NT Emacs specific settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (prefer-coding-system 'utf-8)          ; TODO fix codepage
  (codepage-setup 1251)
  (set-w32-system-coding-system 'cp1251-dos)
  (set-clipboard-coding-system 'cp1251-dos)
  (setq w32-standard-fontset-spec
  "-*-Courier New-normal-r-*-*-*-110-*-*-c-*-fontset-courier,
         ascii:-*-Courier New-normal-r-*-*-*-110-*-*-c-*-iso8859-1,
         latin-iso8859-1:-*-Courier New-normal-r-*-*-*-110-*-*-c-*-iso8859-1,
         latin-iso8859-2:-*-Courier New-normal-r-*-*-*-110-*-*-c-*-iso8859-2,
         latin-iso8859-3:-*-Courier New-normal-r-*-*-*-110-*-*-c-*-iso8859-3,
         latin-iso8859-4:-*-Courier New-normal-r-*-*-*-110-*-*-c-*-iso8859-4,
         latin-iso8859-9:-*-Courier New-normal-r-*-*-*-110-*-*-c-*-iso8859-9,
         cyrillic-iso8859-5:-*-Courier New-normal-r-*-*-*-110-*-*-c-*-iso8859-5,
         greek-iso8859-7:-*-Courier New-normal-r-*-*-*-110-*-*-c-*-iso8859-7")
  (setq w32-enable-italics t)
  (create-fontset-from-fontset-spec w32-standard-fontset-spec t)
  (setq default-frame-alist '((font . "fontset-courier")
                              (top . 0) (left . 180)
                              (width . 134) (height . 42)
                              (cursor-color . "black")
                              (cursor-type . box)
                              (foreground-color . "black")
                              (background-color . "white"))))

 (running-on-gnu/linux
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Unix specific settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq default-frame-alist 
        '(
         ;;    (background-color     . "LightCyan1")
         ;;    (cursor-color         . "purple")
         ;;    (cursor-type          . box)
         ;;    (foreground-color     . "grey10")
         ;;    (vertical-scroll-bars . left)
         ;;    (active-alpha         . 0.875)
         ;;    (inactive-alpha       . 0.75)
         ;;    (top . 25) (left . 50) (width . 89) (height . 50)
         ;; (font . "-*-fixed-medium-r-*-*-*-*-*-*-*-*-*-*")
          ))

  (prefer-coding-system 'utf-8)
  (global-set-key [S-delete] 'clipboard-kill-region)
  (global-set-key [S-insert] 'clipboard-yank)
  (global-set-key [C-insert] 'clipboard-kill-ring-save)
  (define-key global-map [(control delete)]  'kill-region)

  ;; host specific
  (cond
    ;;
    ((string= system-name "miha-lt")
     (setq default-frame-alist '(
          (top . 0) (left . 200) (width . 160) (height . 44)
          (font . "-*-DejaVu Sans Mono-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")
          ;;(font . "-*-Inconsolata-*-*-*-*-18-*-*-*-m-0-iso10646-1")
          )))
    ;;
    ((string= system-name "mkrasnyk-luxoft")
     (setq default-frame-alist '(
          (top . 0) (left . 80) (width . 224) (height . 58)
          (font . "-*-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
          )))
    ;;
    (t (message (format "unknown host name %s" system-name)))))

 (t (message "unknown OS")))

;;}}}

;;{{{ Some own functions 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load ido mode first
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (> emacs-major-version 21)(ido-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open file and go to line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-file-goto (filename)
  "Edit file FILENAME.
   Switch to a buffer visiting file FILENAME,
   creating one if none already exists."
  (interactive "FFind file: ")
  (let* ((name filename) line column)
    (when (string-match ":+\\([0-9]+\\):*\\([0-9]+\\)?:?$" filename)
      (setq name (substring filename 0 (match-beginning 0)))
      (setq line (condition-case nil (string-to-number (match-string 1 filename)) (error nil)))
      (setq column (condition-case nil (string-to-number (match-string 2 filename)) (error nil))))
    (switch-to-buffer (find-file-noselect name))
    (when line (goto-line line))
    (when column (forward-char (- column (current-column))))))
(global-set-key [(control x) (control f)] 'find-file-goto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parenthesis blinking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun blink-paren-first-line () ; from blink-matching-open
  (interactive)
  ;; if mode is c++ or lisp and 
  (when (or (eq major-mode 'c++-mode) (eq major-mode 'lisp-mode))
    ;; create local variable with opening paren pos
    (unless (local-variable-p 'paren-first-line-pos) 
      (make-local-variable 'paren-first-line-pos) 
      (setf paren-first-line-pos -1))
    ;; if the last caharacter is closing parenthesis
    (if (eq (syntax-class (syntax-after (1- (point)))) 5)
     ;; get the first line and print it
     (let* (blinpkos beginpos parenpos
            open-paren-line-string curent-message)
       ;; get corresponding opening parenthesis
       (condition-case () (setq blinkpos (scan-sexps (point) -1)) (error (setq blinkpos nil paren-first-line-pos -1)))
       ;; if found -> print the first line
       (when blinkpos
         (save-excursion 
           ;; get the line
           (goto-char blinkpos)
           (setq beginpos (line-beginning-position))
           (setq open-paren-line-string (buffer-substring beginpos (line-end-position))))
         ;; get the parenthesis position in the line 
         (setq parenpos (- blinkpos beginpos))
         ;; highlight the parenthesis
         (put-text-property parenpos (1+ parenpos) 'face '(background-color . "turquoise" ) open-paren-line-string)
         ;; print to the message line
         (unless (= paren-first-line-pos blinkpos)
           (setf paren-first-line-pos blinkpos)
           (message "%s" (first (split-string open-paren-line-string "\n"))))))
     ;; if a non-paren symbol -> remove message locking
     (setf paren-first-line-pos -1))))
(run-with-idle-timer 0.1 t 'blink-paren-first-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CR insert/remove
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun unix2dos ()
  ;; This function really does work now, changed `replace-string()'
  ;; to `replace-regexp()' which does the business !! *PP*
  "Convert this entire buffer from UNIX text file format to MS-DOS."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "$"  "\015" )
    (goto-char (point-max))
    (insert "\n\C-z")))

(defun dos2unix ()
  "Convert this entire buffer from MS-DOS text file format to UNIX."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\r$" "" nil)
    (goto-char (1- (point-max)))
    (if (looking-at "\C-z")
        (delete-char 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert current date and time functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-time ()
  "Insert the current time according to the variable \"insert-time-format\"."
  (interactive "*")
  (insert (format-time-string insert-time-format (current-time))))

(defun insert-date ()
  "Insert the current date according to the variable \"insert-date-format\"."
  (interactive "*")
  (insert (format-time-string insert-date-format (current-time))))

(defun insert-time-and-date ()
  "Insert the current date according to the variable \"insert-date-format\",
   then a space, then the current time according to the
   variable \"insert-time-format\"."
  (interactive "*")
  (progn (insert-date) (insert " ") (insert-time)))

(defun ascii ()
  (interactive)
  ;(switch-to-buffer "*scratch*")(clone-buffer "*ASCII*" t)(erase-buffer)(text-mode)
  (switch-to-buffer "*ASCII*")(fundamental-mode)
  (erase-buffer)
  (loop
   for i from 0 to #x2fff
   do (when (and (not (zerop i)) (zerop (mod i 256))) (insert "-------------\n"))
   (insert (format " %4x %6d %c  \n" i i i)))
  (beginning-of-buffer))

(defun display-fonts ()
  "Sorted display of all the fonts Emacs knows about."
  (interactive)
  (with-output-to-temp-buffer "*Fonts*"
    (save-excursion
      (set-buffer standard-output)
      (mapcar (lambda (font) (insert font "\n"))
	      (sort (x-list-fonts "*") 'string-lessp)))
    (print-help-return-message)))

(defun clean-zotero-bib () 
  (interactive)
  (replace-regexp "^.*url[ \t\n]*=.*$" "" nil (point-min) (point-max))
  (replace-regexp "^.*abstract[ \t\n]*=.*$" "" nil (point-min) (point-max))
  (replace-regexp "^.*annote[ \t\n]*=.*$" "" nil (point-min) (point-max))
  (replace-regexp "^.*doi[ \t\n]*=.*$" "" nil (point-min) (point-max))
  (delete-matching-lines "^[ \t\n]*$" (point-min) (point-max)))

;;;}}}


;; {{{ Calculator

(require 'calc-ext)
(setq calc-language 'c)

;; usefull mini calculator
(defun mini-calc (expr &optional arg)
  "Calculate expression

If ARG is given, then insert the result to current-buffer"
  (interactive
   (list (read-from-minibuffer "Enter expression: ")
	 current-prefix-arg))

  (let ((result (calc-eval expr)))
    (if arg
	(insert result)
      (message (format "Result: [%s] = %s" expr result)))))
(global-set-key (kbd "C-=") 'mini-calc)

;; {{{ Customization

(custom-set-variables
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "semiverbatim")))
 '(font-latex-fontify-sectioning 1.0)
 '(matlab-comment-region-s "% "))

(custom-set-faces
 '(font-latex-sectioning-5-face ((((class color) (background light)) (:inherit nil :foreground "blue4")))))

;; }}}

;; {{{ TODO

(setq ongoing-char-choice '("Special characters" 
                              ("" 
                               ("ccedil" #xe7) 
                               ("copyright" #xa9) 
                               ("degree" #xb0) 
                               ("dot" #xb7) 
                               ("eacute" #xe9) 
                               ("half" "&#xbd;") 
                               ("omacr" "&#x14d;") 
                               ("oouml" #xe4) 
                               ("uuml" #xfc) 
                               ("euro" #x20ac) 
                               ("cents" #xa2) 
                               ("egrave" #xe8) 
                               ("lsquo" #x2018) 
                               ("rsquo" #x2019) 
                               ("ldquo" #x201c) 
                               ("rdquo" #x201d) 
                               ("mdash" #x2014))))

(defun ong-special-chars-menu () "Insert a special character from a menu" (interactive) 
  (let ((value (car (x-popup-menu (list '(10 10) (selected-window)) ongoing-char-choice)))) 
    (cond ((integerp value) (ucs-insert value)) ((stringp value) (insert value)) ('t ))))

(defun insert-random-int-values ()
  (interactive)
  (insert (apply #'concatenate 'string (loop for i from 0 to (% (abs (random)) 20) collect (format "%d, " (% (random) 100))))))

(defun insert-subdir-executable ()
  (interactive)
  (insert (shell-command-to-string "find . -executable -type f")))

;; }}}

