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

(defun package-dir (name)
  (let* ((dir1 (file-expand-wildcards (concat custom-dir name)))
         (dir2 (file-expand-wildcards (concat custom-dir "elpa/" name)))
         (dirs (remove-if-not #'file-accessible-directory-p (append dir1 dir2)))
         (dirp (car dirs)))
    (message "Checking for %s pacakge: %s" name (if dirp dirp "not found"))
    (when dirp (add-to-list 'load-path dirp))
    dirp))

;; Store customization information in file specific for emacs version
(setq custom-dir (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (concat custom-dir "/lisp"))
(setq custom-file (concat custom-dir "custom." emacs-flavor ".el"))
(setq recentf-save-file (concat custom-dir "/.recentf"))

;; Save Emacs state for next session.
(require 'saveplace)
(custom-set-variables '(save-place t) `(save-place-file ,(concat custom-dir "save-places")))

;;}}}

;; {{{ Setup ELPA repositories

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Guarantee all packages are installed on start
(defvar packages-list '(auctex bm dired-single google-translate js2-mode
                        magit openwith qml-mode smooth-scrolling mew w3m
                        cedet helm sql-indent org kanban gh-md ggtags
                        tdd-status-mode-line ess feature-mode yaml-mode
                        web-mode htmlize markdown-mode markdown-mode+
                        auto-complete auto-complete-c-headers ag emojify
                        jade-mode hide-lines lua-mode keychain-environment)
  "List of packages needs to be installed at launch")
(defun has-package-not-installed ()
   (loop for p in packages-list
         when (not (package-installed-p p)) do (return t)
         finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

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
(global-set-key "\M-?" 'goto-line)
(global-set-key [C-x-p] 'bury-buffer)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c'" 'uncomment-region)
(global-set-key [C-f4] (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key [(mouse-4)] '(lambda () (interactive) (scroll-down (/ (window-height) 2))))
(global-set-key [(mouse-5)] '(lambda () (interactive) (scroll-up   (/ (window-height) 2))))
(global-set-key "\C-x\C-g" 'recentf-open-files)

;; windmove modifier+<> keys
(defun windmove-default-keybindings (&optional modifier)
  "Set up keybindings for `windmove'.
Keybindings are of the form MODIFIER-{left,right,up,down}.
Default MODIFIER is 'shift."
  (interactive)
  (unless modifier (setq modifier 'shift))
  (global-set-key (vector (list modifier 'left))  'windmove-left)
  (global-set-key (vector (list modifier 'right)) 'windmove-right)
  (global-set-key (vector (list modifier 'up))    'windmove-up)
  (global-set-key (vector (list modifier 'down))  'windmove-down))
(cond
 (running-on-windows
  ;; #IfWinActive ahk_class Emacs ; AutoHotKey mapping
  ;;    LWin::AppsKey
  ;; #IfWinActive
  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper)
  (windmove-default-keybindings 'hyper))
 (t
  (windmove-default-keybindings 'super)))

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
(global-eldoc-mode -1)
(transient-mark-mode 1)                              ;; When the mark is active, the region is highlighted.
(setq inhibit-startup-screen t)                      ;; Silent boot
(setq initial-scratch-message nil)                   ;; Clear scratch buffer
(setq initial-major-mode 'text-mode)                 ;; text mode is default
(set-scroll-bar-mode 'right)                         ;; vertical scroll bars on the right side.
(setq scroll-error-top-bottom t)                     ;; scroll to top or bottom
(global-font-lock-mode t)                            ;; Turn on font-lock in all modes that support it.
(setq font-lock-maximum-decoration t)                ;; use the maximum decoration available
(show-paren-mode t)                                  ;; Highlight matching parentheses.
(setq default-major-mode 'text-mode)                 ;; Make text mode default major mode.
(setq shell-prompt-pattern "^[^#$%>\n]*[#$%>\)] *")  ;; My shell prompt ends on ")".
(setq shell-command-switch "-lc")                    ;; Shell is interactive
(setq visible-bell t)                                ;; Turn off beep.
(setq ring-bell-function 'ignore)                    ;; Turn the alarm totally off
(standard-display-8bit 128 255)                      ;; Do not expand unprintable characters to their octal values.
(setq fortran-comment-region "C MKR")                ;; Fortran comments prefix.
(line-number-mode 1)                                 ;; Show line-number in the mode line.
(column-number-mode 1)                               ;; Show column-number in the mode line.
(fset 'yes-or-no-p 'y-or-n-p)                        ;; Will allow you to type just "y" instead of "yes".
(setq-default tab-width 2)                           ;; Set Tab-Width.
(setq-default indent-tabs-mode nil)                  ;; Permanently force Emacs to indent with spaces.
(put 'upcase-region 'disabled nil)                   ;; Convert the region to upper case.
(put 'downcase-region 'disabled nil)                 ;; Convert the region to lower case.
(blink-cursor-mode -1)                               ;; Switch off blinking cursor mode.
(setq large-file-warning-threshold nil)              ;; Maximum size of file above which a confirmation is requested
;(add-hook 'before-save-hook 'delete-trailing-whitespace)  ;; configuration required
(setq mode-require-final-newline t)
;; (setq delete-trailing-lines nil)
(setq printer-name "HP-ENVY-4520-series")            ;; lpstat -p -d
(tool-bar-mode -1)
(setq vc-follow-symlinks t)
(setq grep-command "grep --exclude-dir=\".svn\" --exclude=TAGS -nHriI -e ")
(setq tags-case-fold-search nil)
(if (not (assq 'user-size initial-frame-alist))      ;; Unless we've specified a number of lines, prevent the startup code from
    (setq tool-bar-originally-present nil))          ;; shrinking the frame because we got rid of the tool-bar.
;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(defadvice ediff-quit (around advice-ediff-quit activate)
      (cl-flet ((yes-or-no-p (&rest args) t) (y-or-n-p (&rest args) t))
        ad-do-it))

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
  (cl-flet ((process-list ())) ad-do-it))

;; ANSI colorization of a buffer
(require 'ansi-color)
(defun ansi-colorize-buffer ()
  (interactive)
  (when buffer-read-only (toggle-read-only))
  (ansi-color-apply-on-region (point-min) (point-max)))
(setq ansi-color-drop-regexp "\\[\\([ABCDsuK]\\|[12][JK]\\|=[0-9]+[hI]\\|[0-9;]*[HfGg]\\|\\?[0-9]+[hl]\\)")
;(add-hook 'compilation-filter-hook 'ansi-colorize-buffer)

;; calendar settings
(defface calendar-kw `((t (:foreground "black") (:background "pale green")))  "Face for a calendar week number column")
(setq calendar-week-start-day 1)
(setq calendar-intermonth-text '(propertize (format "%2d" (car (calendar-iso-from-absolute (calendar-absolute-from-gregorian (list month day year))))) 'font-lock-face 'calendar-kw))
(setq calendar-intermonth-header (propertize "KW" 'font-lock-face 'calendar-kw))

;; Start Emacs as a server
(server-start)

;;}}}

;;{{{ Load local packages

(when (package-dir "hide-lines*")
  (require 'hide-lines))

(when (package-dir "keychain-environment*")
  (require 'keychain-environment))

(when (package-dir "gradle-mode*")
  (require 'gradle-mode))

(when (package-dir "go-mode*")
  (require 'go-mode)
  (when (file-exists-p (concat custom-dir "/lisp/go-autocomplete.el"))
    (require 'go-autocomplete)
    (require 'auto-complete-config)))

(when (package-dir "go-direx*")
  (require 'go-direx)
  (define-key go-mode-map (kbd "C-c C-j") 'go-direx-pop-to-buffer))

(when (package-dir "yaml-mode*")
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling
(when (package-dir "smooth-scrolling*")
  (require 'smooth-scrolling)
  (setq smooth-scroll-margin 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fontlocking modes
(require 'doc-mode)

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
;(setq dired-listing-switches-styles
;      (if (string-match ".tm.ro." system-name) '("-alh" "-al")
;        '("-alh --group-directories-first" "-al --group-directories-first")))
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
(when (package-dir "dired-single*")
  (require 'dired-single)
  (defun my-dired-init ()
    "Bunch of stuff to run for dired, either immediately or when it'sloaded."
    (require 'dired-sort-menu)
    (setq mouse-1-click-follows-link 200)
    ;; TODO: add call stack
    (add-hook 'before-change-major-mode-hook
              '(lambda () (when (eq major-mode 'dired-mode)
                            (print 'aaa)
                            (print (previous-buffer))
                            (make-local-variable 'dired-stack)
                            (setq dired-stack '('hello)))))
    ;; global stack works, but local stack can not be saved
    (defvar dired-single-stack '())
    (defun dired-single-buffer-down ()
      (interactive)
      (let ((name (dired-get-filename nil t)))
        (cond
         ((file-accessible-directory-p name)
          (push (file-name-nondirectory name) dired-single-stack)
          (dired-single-buffer name))
         (t (dired-single-buffer)))))
    (defun dired-single-buffer-up ()
      (interactive)
      (let ((name (pop dired-single-stack)) pos)
        (dired-single-buffer "..")
        (when name
          (setq pos (search-forward-regexp (concat name "$") nil t))
          (if pos
              (goto-char (- pos (length name)))
            (setq dired-single-stack '())))))
    (define-key dired-mode-map [return] 'dired-single-buffer-down)
    (define-key dired-mode-map [mouse-1] 'dired-single-buffer-down)
    (define-key dired-mode-map (read-kbd-macro "<backspace>") 'dired-single-buffer-up))
  (if (boundp 'dired-mode-map) (my-dired-init) (add-hook 'dired-load-hook 'my-dired-init))
  (define-key dired-mode-map (read-kbd-macro "<f8>") 'dired-do-delete))

; use openwith minor mode
(when (package-dir "openwith*")
  (require 'openwith)
  (openwith-mode t)
  (setq openwith-associations
      '(("[^_]?\\.\\(ps\\|pdf\\|djvu\\|epub\\)\\'" "okular" (file))
        ("\\.\\(docx?\\|odt\\|pptx?\\|rtf\\|xlsx?\\)\\'" "libreoffice" (file))
        ("\\.\\(ai\\)\\'" "inkscape" (file))
        ("\\.\\(dll\\|pyd\\)\\'" "depends.exe" (file))
        ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp4\\)\\'" "smplayer" (file)))))

;;; Shell mode
(setq ansi-color-names-vector ["black" "red4" "green4" "yellow4" "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translator (google)
(when (package-dir "google-translate*")
  (require 'google-translate)
  (global-set-key "\C-xt"
     (lambda ()
       (interactive)
       (let ((bnd (bounds-of-thing-at-point 'word))
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
           (insert (format "\n%s\n" (google-translate-translate src dst str)))))))
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "ru"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git mode
(when (package-dir "magit*")
  (require 'magit)
  (require 'magit-blame)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-save-repository-buffers nil) ; ???
  (custom-set-variables '(git-commit-summary-max-length 70))

  ;; Some specific function to show/edit branch descriptions
  (defun magit-show-description ()
    "Print descriptions"
    (interactive)
    (magit-section-action info (info)
      (branch
       (message (magit-git-string "config" (format "branch.%s.description" info))))))

  (defun magit-edit-description ()
    "Edit descriptions"
    (interactive) ; (list (magit-read-rev "Edit branch description" (magit-get-current-branch))))
    (magit-section-action info (info)
      (branch
       (let* ((config (format "branch.%s.description" info))
              (current (magit-git-string "config" config))
              (description (read-string (format "Descrition of [%s]: " info) current)))
         (magit-git-string "config" config description)))))

  (eval-after-load "magit-mode"
    '(define-key magit-mode-map [C-tab] nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matlab mode
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(custom-set-variables '(matlab-comment-region-s "% "))
(setq matlab-fill-code nil)
(custom-set-faces '(matlab-cellbreak-face ((t (:foreground "Firebrick" :weight bold)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ESS
(when (package-dir "ess*")
  (require 'ess-site)
  (setq ess-ask-for-ess-directory nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gnuplot
(when (package-dir "/gnuplot*")
  (autoload 'gnuplot-mode "gnuplot" "gnuplot major mode" t)
  (autoload 'gnuplot-make-buffer "gnuplot" "open a buffer in gnuplot-mode" t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; m4 mode
(load-library "m4-mode")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote file editing via ssh
(require 'tramp)
(setq tramp-default-method "ssh")

(global-set-key "\C-c\C-t" 'tramp-cleanup-all-connections)

(when (package-dir "magit-tramp*")
  (require 'magit-tramp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gentoo ebuild mode
(when (package-dir "ebuild-mode*")
  (require 'ebuild-mode)
  (require 'eselect-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Visible bookmarks in buffer.
(when (package-dir "bm-*")
  (require 'bm)
  ;; M$ Visual Studio key setup.
  (global-set-key (kbd "<C-f2>") 'bm-toggle)
  (global-set-key (kbd "<f2>")   'bm-next)
  (global-set-key (kbd "<S-f2>") 'bm-previous))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Po files
(autoload 'po-mode "start-po" "PO major mode" t)
(setq auto-mode-alist (cons '("\\.po[tx]?\\'\\|\\.po\\." . po-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subversion mode
(require 'psvn)
(setf svn-status-hide-unmodified t)
(defadvice svn-status-commit (after modify-default-commit-message activate)
  "Insert an svn message template"
  (insert "\n\nReviewers: \nFindings: \nRisk: medium\nKlocwork: ok\nJira key is NDSAL-\n"))

(defun get-svn-parent-directory (dir)
  (cond
   ((or (not dir) (string= (file-name-directory (directory-file-name dir)) dir))
    nil)
   ((file-accessible-directory-p (concat (file-name-as-directory dir) ".svn"))
    dir)
   (t
    (get-svn-parent-directory (file-name-directory (directory-file-name dir))))))

(defun insert-svn-info-message()
  (interactive)
  (let* ((dir (get-svn-parent-directory (buffer-file-name)))
        (info (shell-command-to-string (concat "svn info " dir)))
        (root (if (string-match "^Repository Root: \\([^ ]+\\)$" info) (match-string 1 info) "unknown"))
        (fullurl (if (string-match "^URL: \\([^ ]+\\)$" info) (match-string 1 info) "unknown"))
        (url (concat "^" (substring fullurl (length root))))
        (rev (if (string-match "^Revision: \\([0-9]+\\)$" info) (match-string 1 info) "unknown"))
        (head (format "Merge with %s rev %s" url rev)))
    (insert (format "%s\n\n%s" head info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup spell checker
(when running-on-windows
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
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
(when (package-dir "js2-mode*")
  (load-library "js2-mode")
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
  (modify-syntax-entry ?_ "w" js2-mode-syntax-table)
  (add-hook 'js2-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace))))

(when (package-dir "feature-mode*")
  (load-library "feature-mode")
  (modify-syntax-entry ?_ "w" feature-mode-syntax-table)
  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))

(when (package-dir "jade-mode*")
  (load-library "jade-mode")
  (add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode)))

(when (package-dir "qml-mode*")
  (require 'qml-mode)
  (add-hook 'qml-mode-hook (lambda () (modify-syntax-entry ?' "|"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C and C++ modes
(require 'cc-langs)
(require 'cc-mode)
(require 'jam-mode)
(require 'cmake-mode)
(require 'gud)
(require 'gdb-mi)

(setq gud-tooltip-mode t)

(setq compilation-ask-about-save nil)
;; (setq compilation-window-height 10)

(defun get-shell-command (command)
  (if buffer-file-name
    (let* ((cmd command)
           (bufname (file-name-nondirectory (buffer-file-name)))
           (filename (file-name-sans-extension bufname))
           (extension (file-name-extension bufname))
           (dirname (expand-file-name "."))
           (tex (if (and (boundp 'TeX-master) (stringp TeX-master)) TeX-master bufname)))
      (setf cmd (replace-regexp-in-string "%%" "%" cmd))
      (setf cmd (replace-regexp-in-string "%f" bufname cmd))
      (setf cmd (replace-regexp-in-string "%d" dirname cmd))
      (setf cmd (replace-regexp-in-string "%n" filename cmd))
      (setf cmd (replace-regexp-in-string "%e" extension cmd))
      (setf cmd (replace-regexp-in-string "%t" tex cmd))
      cmd)
    command))

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

      ;;(ggtags-mode 1)
      (c-toggle-auto-newline -1)                           ;; Turn off auto-newline feature
      (defun c-font-lock-invalid-string () t)              ;; Turn off invalid string highlight
      (c-set-offset 'substatement-open 0)                  ;; project brace indent style

      ;; compiler command, depends on the major-mode
      (make-variable-buffer-local 'compile-command)
      (setq compile-command
       (get-shell-command
         (cond
          ((eq major-mode 'c++-mode)
           (cond
            ((string-equal ext "nxc") "nbc %f -O=%n.rxe; nxt_push %n.rxe")
            (t "g++ -Wall -std=c++11 -O0 -g %f -o %n")))
          ((eq major-mode 'fortran-mode) "g77 -g %f -o %n")
          ((eq major-mode 'qt-pro-mode) "qmake && make")
          ((eq major-mode 'makefile-gmake-mode) "make")
          ((eq major-mode 'jam-mode) "bjam -d2")
          ((eq major-mode 'go-mode) "go build -v && go test -v && go vet")
          (t "make"))))

      ;; run command, allow only commands in that starts with "./"
      (make-variable-buffer-local 'run-command)
      (local-set-key '[S-f5]  (lambda () (interactive) (shell-command (get-shell-command run-command))))
      (setq run-command
       (get-shell-command
         (cond
          ((eq major-mode 'python-mode) "python3 %f")
          ((eq major-mode 'qml-mode)
           (local-set-key '[S-f5]  (lambda () (interactive) (save-window-excursion (shell-command run-command))))
           "qmlscene %f &")
          (running-on-windows "%n")
          (t "./%n"))))
      (put 'run-command 'safe-local-variable 'run-command-safe-variable)
      (defun run-command-safe-variable (var) (or
             (string-match "^[ \t\n\r]*\\(qml\\(scene\\|viewer\\)\\|optirun\\)[ \t\n\r]*\./.+" var)
             (string-match "/usr/bin/curl.+" var))))

    ;; settings depending on the mode
    (when (or (eq major-mode 'c++-mode) (eq major-mode 'fortran-mode)
              (eq major-mode 'jam-mode))
      (setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face))
      (flyspell-prog-mode)
      (modify-syntax-entry ?_ "w" c++-mode-syntax-table)
      (local-set-key '[C-f8]   'flyspell-buffer)
      ;; other settings
      (setq indent-tabs-mode nil))

    ;; TAGS lookup
    (when (or (eq major-mode 'c++-mode) (eq major-mode 'fortran-mode)
              (eq major-mode 'jam-mode) (eq major-mode 'gud-mode))
      (local-set-key '[f3]     (lambda () (interactive) (find-tag (word-at-point))))
      (local-set-key '[C-f3]   (lambda () (interactive) (find-tag nil t)))
      (local-set-key '[M-f3]   'pop-tag-mark))

    (when (eq major-mode 'fortran-mode)
      (local-set-key "\C-c'" 'fortran-comment-region))

    (when (or (eq major-mode 'c++-mode) (eq major-mode 'fortran-mode) (eq major-mode 'compilation-mode)
              (eq major-mode 'jam-mode) (eq major-mode 'makefile-gmake-mode) (eq major-mode 'python-mode)
              (eq major-mode 'qt-pro-mode)  (eq major-mode 'go-mode))
      (setq show-trailing-whitespace t)
      (local-set-key [C-S-mouse-1] (lambda (event) (interactive "e") (posn-set-point (elt event 1)) (find-tag (word-at-point))))
      ;; compile keys
      (local-set-key "\C-c\C-c"  'compile)
      (local-set-key '[f8]   'next-error)
      (local-set-key '[S-f8] 'previous-error)
      (local-set-key '[f7]   (lambda () (interactive) (compile (get-shell-command compile-command))))
      (local-set-key "\C-c\C-c" 'compile))

    (when (and (not running-on-windows)
               (or (eq major-mode 'c++-mode) (eq major-mode 'fortran-mode)
                   (eq major-mode 'gud-mode) (eq major-mode 'python-mode)
                   (eq major-mode 'go-mode)))
      ;; (string-match "\\*gud-\\(.+\\)\\*" (buffer-name gud-comint-buffer))
      ;; debug functions
      (gud-def gud-frame "frame" "\C-g" "Select and print a stack frame.")

      ;; debug keys
      (local-set-key '[C-f5]   'gud-until)
      (local-set-key '[f9]     'gud-set-clear)
      (local-set-key '[S-f9]   'gud-break)
      (local-set-key '[C-f9]   'gud-remove)
      (local-set-key '[f10]    'gud-next)
      (local-set-key '[f11]    'gud-step)
      (local-set-key '[f12]    'gud-finish))

    ;; auto complete
    (when (and (boundp 'ac-sources) (listp ac-sources))
      (add-to-list 'ac-sources 'ac-source-semantic-raw))
    )))
(modify-syntax-entry ?_ "w" c++-mode-syntax-table)
(add-hook 'c++-mode-hook
      '(lambda()
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

;; set context-dependent tabulation widths
(add-hook 'c++-mode-hook
  '(lambda ()
     (cond
      ((string-match "^/usr/include/c++" buffer-file-name)
       (make-variable-buffer-local 'tab-width)
       (set-variable 'tab-width 8)))))

;; set global GDB properties and keys
(setf gdb-show-threads-by-default t)
(setf gdb-show-main t)
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
         (if gud-last-last-frame
             (gud-find-file (car gud-last-last-frame))
           (gud-find-file gdb-main-file)))
      (setq gdb-source-window win)))))

;; Qt stuff
(require 'qt-pro)
(c-add-style "qt-gnu" '("gnu"
    (c-access-key . "^\\(public\\|protected\\|private\\|signals\\|public slots\\|protected slots\\|private slots\\):")
    (c-basic-offset . 4)
    (c-set-offset 'substatement-open '0)))
;; make new font for rest of qt keywords
(make-face 'qt-keywords-face)
(set-face-foreground 'qt-keywords-face "midnight blue")
;; qt keywords
(font-lock-add-keywords 'c++-mode '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)
                                    ("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)
                                    ("\\<Q_[A-Z][_A-Za-z]*" . 'qt-keywords-face)
                                    ("\\<foreach\\>" . 'qt-keywords-face)))
(setq c-default-style "qt-gnu")
;; c++11 stuff
(make-face 'c++11-raw-string-face)
(set-face-foreground 'c++11-raw-string-face "#003000")
(set-face-background 'c++11-raw-string-face "#ddffdd")
(font-lock-add-keywords 'c++-mode '(("\\(R\\s\"\\(.*\\)([^)]*)\\2\\s\"\\)" 1 'c++11-raw-string-face t)))

(require 'sd)
(add-to-list 'auto-mode-alist '("\\.sd$" . sd-mode))

(defun create-tags-file (directory)
  "Create TAGS file recursively"
  (interactive "DCreate TAGS recursively: ")
  (async-shell-command (format "find %s | grep '.*\\.\\(c\\|cc\\|cpp\\|cxx\\|h\\|hh\\|hxx\\|hpp\\)$' | etags -" directory)))


(when (package-dir "ag*")
  (require 'ag)
  (custom-set-variables '(ag-ignore-list '("TAGS")) '(ag-highlight-search t))
  (global-set-key (kbd "<s-f3>") (lambda () (interactive) (ag/search (word-at-point) (ag/project-root default-directory)))))

(when (package-dir "emojify*")
  (require 'emojify)
  (custom-set-variables '(emojify-emoji-styles (quote (github unicode))))
  (custom-set-variables '(emojify-program-contexts (quote (comments))))
  (add-hook 'after-init-hook #'global-emojify-mode))

;; (when (package-dir "ggtags*")
;;   (require 'ggtags)
;;   (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
;;   (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
;;   (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
;;   (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
;;   (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
;;   (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode
(when (package-dir "python*")
  (require 'python-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby mode
; loads ruby mode when a .rb file is opened.
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala mode
(when (package-dir "scala-mode2*")
  (require 'scala-mode2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lua mode
(when (package-dir "lua*")
  (require 'lua-mode)
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SQL
(when (package-dir "sql-indent*")
  (require 'sql-indent)
  (defun sql-indent-region (beg end)
    "Indent the SQL statement in the region."
    (interactive "*r")
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (sql-indent-buffer)))))

(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-mysql-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go
(add-hook 'go-mode-hook
  '(lambda()
     (add-hook 'before-save-hook 'gofmt-before-save)
     (local-set-key '[f3]   'godef-jump)
     (local-set-key '[M-f3] 'pop-tag-mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set hooks
(loop for mode in '(c-mode-hook c++-mode-hook fortran-mode-hook jam-mode-hook go-mode-hook
                    qt-pro-mode-hook gud-mode-hook qml-mode-hook python-mode-hook)
      do (add-hook mode development-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AucTeX
(when (package-dir "auctex*")
  (require 'tex-site)
  ;(load "preview-latex.el" nil t t)
  (setq preview-default-document-pt 12)
  ;;  TeX-style-path
  (setq LaTeX-enable-toolbar nil)
  (setq font-latex-title-fontify 'color)
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
     '(("XeLaTeX" "xelatex -output-driver=\"xdvipdfmx -v\"  -synctex=1 %(mode) \"\\input\" %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run PDFLaTeX")
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
  (add-hook 'latex-mode-hook  'my-tex-init)
  ;(add-hook 'LaTeX-mode-hook  'TeX-PDF-mode)
  (add-hook 'LaTeX-mode-hook  'TeX-source-correlate-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode
(add-hook 'text-mode-hook '(lambda () (turn-off-auto-fill) (setq fill-column 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
(when (package-dir "org-*")
  (require 'org)
  (require 'ob-core)
  (org-defkey org-mode-map [(control tab)] 'cyclebuffer-forward)
  (org-defkey org-mode-map [(control return)] 'mini-calc)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((sh . t) (python . t) (C . t) (haskell . t) (sqlite  . t) (maxima . t)
                                 (latex . t) (plantuml . t) (dot . t) (ruby . t) (R . t)))
  (add-hook 'org-babel-after-execute-hook (lambda () (condition-case nil (org-display-inline-images) (error nil))))
  (setq org-babel-results-keyword "results")                           ;; Make babel results blocks lowercase
  (setq org-confirm-babel-evaluate nil)                                ;; Do not prompt to confirm evaluation
  (plist-put org-format-latex-options :scale 1.5)
  (setq org-src-fontify-natively t)
  (custom-set-faces
   '(org-block ((t (:background "#F8F8FF"))))
   '(org-block-begin-line ((t (:underline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF"))))
   '(org-block-end-line ((t (:overline "#A7A6AA" :foreground "#008ED1" :background "#EAEAFF")))))
  (setq org-support-shift-select 'always)
  (setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))
  ;(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental-mode))) ;; Use fundamental mode when editing plantuml blocks with C-c '
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-hook 'org-mode-hook '(lambda () (local-set-key (kbd "<H-tab>") 'pcomplete)))
  (setq org-format-latex-options '(:foreground default :background default
      :scale 2 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0
      :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

  (defadvice org-mode-flyspell-verify
    (after my-org-mode-flyspell-verify activate)
    "Don't spell check src blocks."
    (setq ad-return-value
          (and ad-return-value
               (not (eq (org-element-type (org-element-at-point)) 'src-block))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MMM mode
(when (package-dir "/mmm-mode")
  (require 'mmm-auto)
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php))

(when (package-dir "/web-mode*")
  (require 'web-mode)
	(setq web-mode-markup-indent-offset 2)
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-[S]-Tab cycle buffer
(autoload `cyclebuffer-forward "cyclebuffer" "cycle forward" t)
(autoload `cyclebuffer-backward "cyclebuffer" "cycle backward" t)
(global-set-key (kbd "<C-tab>") 'cyclebuffer-forward)
(global-set-key (kbd "<C-S-iso-lefttab>") 'cyclebuffer-backward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; help command
(autoload 'woman "woman"
  "Decode and browse a UN*X man page." t)
(defun custom-help ()
  (interactive)
  (cond
   ;; emacs lisp help
   ((or (eq major-mode 'lisp-mode) (eq major-mode 'emacs-lisp-mode))
    (let ((var (variable-at-point)))
      (if (eq var 0) (describe-function (function-called-at-point)) (describe-variable var))))
   ;; Qt assistant
   ((and (eq major-mode 'c++-mode) (string= (substring (current-word) 0 1) "Q"))
   ;;  (unless (get-process "assistant")
   ;;    (start-process-shell-command "assistant" nil "assistant" "-enableRemoteControl"))
   ;;  (process-send-string "assistant" (concat "setSource qthelp://org.qt-project.qtmultimedia/qdoc/" (downcase (current-word)) ".html\r"))
   ;;  (process-send-string "assistant" "syncContents\r"))
    (browse-url (format "http://qt-project.org/doc/qt-5/%s.html" (downcase (current-word)))))
   ;; try to find a man page
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
         ("\\.pr[oif]$" . qt-pro-mode)
         ("\\.dps$" . pascal-mode)
         ("\\.qml\\(types\\)?$" . qml-mode)
         ("\\.pro$" . text-mode)
         ("\\.l$" . c-mode)
         ("\\.y$" . c-mode)
         ("\\.glsl$" . c++-mode)
         ("\\.py$" . python-mode)
         ("\\.yaml$" . python-mode)
         ("\\.css$" . css-mode)
         ("\\.Xdefaults$" . xrdb-mode)
         ("\\.Xenvironment$" . xrdb-mode)
         ("\\.Xresources$" . xrdb-mode)
         ("*.\\.ad$" . xrdb-mode)
         ("\\.fetchmailrc$" . fetchmail-mode)
         ("\\.xml$" . web-mode)
         ("\\.xsl$" . web-mode)
         ("\\.tei$" . web-mode)
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
         ("\\.ds\\'" . java-mode)
         ("Jamfile.v2" . jam-mode)
         ("\\.jam$" . jam-mode)
         ("\\.mc" . m4-mode)
         ("\\.m4" . m4-mode)
         ("\.i$" . c++-mode)
         ("\.cc$" . c++-mode)
         ("\.ebuild$" . ebuild-mode)
         ("\.gradle$" . gradle-mode)
         ("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)
         ("[Mm]akefile\\.inc$" . makefile-mode)
         ("\.go$" . go-mode)
         ("swdd.*\\.txt$" . doc-mode)
         ) auto-mode-alist))

;;}}}

;;{{{ OS specific

(defun configuration-height-lines ()
  (cond
   ((and (= (display-mm-width) 677) (= (display-mm-height) 381)) 62) ;; win, laptop only
   ((and (< (abs (- (display-mm-width) 508)) 3) (= (display-mm-height) 260)) 53) ;; VirtualBox, window mode
   ((and (< (abs (- (display-mm-width) 508)) 3) (= (display-mm-height) 285)) 59) ;; VirtualBox, fullscreen mode
   (t (message (format "unknown screen with physical size %dx%d" (display-mm-width) (display-mm-height))) 42)))

(cond
 (running-on-windows
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; NT Emacs specific settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (prefer-coding-system 'utf-8)
  (when (package-dir "w32-browser*")
    (require 'w32-browser)
    (define-key dired-mode-map (kbd "<M-return>") 'dired-w32explore))

  (cond
    ;;
   ((string-match "^mykr" user-login-name)
     (setq default-frame-alist '((top . 0) (left . 200) (width . 208) (fullscreen . fullheight)
          (font . "-*-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")))
     (setenv "PATH" (concat "C:\\MinGW\\bin;" (getenv "PATH")))
     (remove-hook 'after-init-hook 'w32-check-shell-configuration)
     (setq shell-command-switch "/c")
     ;(setq shell-file-name "c:/Windows/System32/cmd.exe")
     ;(setq w32-quote-process-args nil)
     (setq org-babel-sqlite3-command "D:\\tools\\sqlite3.exe")
     )))

 (running-on-gnu/linux
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Unix specific settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
          (top . 0) (left . 200) (width . 154) (fullscreen . fullheight)
          (font . "-*-DejaVu Sans Mono-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")
          ;;(font . "-*-Inconsolata-*-*-*-*-18-*-*-*-m-0-iso10646-1")
          ))
     ;; erc settings
     (require 'erc)
     (setq erc-join-buffer 'bury)
     (add-hook 'erc-after-connect '(lambda (server nick)(cond ((string-match "oftc\\.net" server) (erc-join-channel "#osrm")))))
     ;; erc logging
     (setq erc-log-channels-directory (concat custom-dir "erc/logs"))
     (setq erc-save-buffer-on-part t)
     (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
       (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode)) t)))
     (erc :server "irc.oftc.net" :port 6667)
     (set-process-query-on-exit-flag (get-process "erc-irc.oftc.net-6667") nil))
     ;; erc connect on timer
     ;(run-with-timer 0 300 (lambda ()
     ;   (unless (erc-server-buffer-live-p) (erc :server "irc.oftc.net" :port 6667)
     ;           (set-process-query-on-exit-flag (get-process "erc-irc.oftc.net-6667") nil)))))

    ((string-match "VirtualBox" system-name)
     (setq default-frame-alist `(
          (top . 0) (left . 120) (width . 224) (fullscreen . fullheight)
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
    (when (string-match ":+\\([0-9]+\\):*\\([0-9]+\\)?[:,]?$" filename)
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
(defun blink-paren-first-line ()
  (interactive)
  ;; if mode is c++ or lisp and
  (when (and (member major-mode '(c++-mode lisp-mode qml-mode))
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
                (save-excursion (save-restriction (save-match-data (goto-char (1- open)) (re-search-backward  "[^ \t\r\n]" nil t) (print (point)) (line-beginning-position))))))
          (setq a (buffer-substring  sol open) b (buffer-substring  open (1+ open)) c (buffer-substring  (1+ open) eol)))
        (setq a (replace-regexp-in-string "[\n]+\\s-*" " " a)) ;; remove internal newlines
        (setq msg (concat a b c))
        ;; (message (format "%d %d %d %d |%s|%s|%s" sol open eol non-space a b c))
        (put-text-property (length a) (1+ (length a)) 'face '(background-color . "turquoise" ) msg)
        (message "%s" msg)))))
(run-with-idle-timer 0.1 t 'blink-paren-first-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CR insert/remove
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun unix2dos ()
  ;; This function really does work now, changed `replace-string()'
  ;; to `replace-regexp()' which does the business !! *PP*
  "Convert this entire buffer from UNIX text file format to MS-DOS."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "$"  "\015")
    (goto-char (point-max))))

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

(defun highlight-timestamps ()
  (interactive "*")
  (highlight-lines-matching-regexp "[0-9]\{9\}\.[0-9]\{9\}"))

(defun ascii (arg_from arg_to arg_format)
  (interactive (let ((from (string-to-char (read-string "From: " "A")))
                     (to (string-to-char (read-string "To: " "Z")))
                     (format (read-string "Format: " " %4x %6d %c  \n")))
                     (list from to format)))
  ;(switch-to-buffer "*scratch*")(clone-buffer "*ASCII*" t)(erase-buffer)(text-mode)
  (let () ; (n (count-occurrences "%" arg_format)))
    (switch-to-buffer "*ASCII*") (fundamental-mode)
    (erase-buffer)
    (loop
     for i from arg_from to arg_to
     do (insert (format arg_format  i i i i i i)))
    (beginning-of-buffer)))

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

;; }}}

;; {{{ Calculator

(require 'calc-ext)
(setq calc-language 'c)

;; usefull mini calculator
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
(global-set-key [(control return)]  'mini-calc)

;; }}}

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
