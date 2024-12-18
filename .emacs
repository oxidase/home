;; dev packages: autoconf automake texinfo libgtk-3-dev libxpm-dev libjpeg-dev libgif-dev libtiff5-dev libgnutls28-dev librsvg2-dev libmagickwand-dev libxml2-dev libcairo2-dev libsystemd-dev libselinux1-dev libgpm-dev libgconf2-dev
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
         (dir2 (file-expand-wildcards (concat custom-dir "quelpa/**/" name)))
         (dir3 (file-expand-wildcards (concat custom-dir "elpa/" name)))
         (dirs (remove-if-not #'file-accessible-directory-p (append dir1 dir2 dir3)))
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

(unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))

(require 'quelpa)
(when (>= emacs-major-version 28)
  (quelpa '(eff :repo "oxidase/eff" :fetcher github)))

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; Guarantee all packages are installed on start
(defvar packages-list
  '(adoc-mode ag arxiv-mode back-button bazel bitbake bm debian-el dired-single docker
              dockerfile-mode elfeed ess fish-mode geiser google-c-style gptel impatient-mode lsp-mode magit
              ob-html-chrome ob-http ob-mermaid openwith ox-reveal
              protobuf-mode rainbow-mode simple-httpd string-inflection use-package web-mode yaml-mode)
  "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
  (unless package--initialized
    (package-initialize))
  (cl-loop for p in packages-list
         when (not (package-installed-p p)) do (cl-return t)
         finally (cl-return nil)))
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

;; {{{ straight packages

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)
  (straight-use-package 'use-package))

(use-package lean4-mode
  :straight (lean4-mode
	     :type git
	     :host github
	     :repo "leanprover/lean4-mode"
	     :files ("*.el" "data"))
  ;; to defer loading the package until required
  :commands (lean4-mode))

(use-package dap-mode
  :straight (dap-mode
	     :type git
	     :host github
	     :repo "oxidase/dap-mode"
       :no-byte-compile t
	     :files ("*.el" "icons"))
  ;; to defer loading the package until required
  :commands (dap-mode))


(use-package systemd
  :straight (systemd-mode
	     :type git
	     :host github
	     :repo "holomorph/systemd-mode"
       :no-byte-compile t
	     :files ("*.el" "*.txt"))
  ;; to defer loading the package until required
  :commands (systemd-mode))

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
(global-set-key [?\s-s] 'search-forward-regexp)
(global-set-key [f5] 'revert-buffer)
(global-set-key "\M-?" 'goto-line)
(global-set-key [C-x-p] 'bury-buffer)
(global-set-key "\C-c;" 'comment-region)
(global-set-key "\C-c'" 'uncomment-region)
(global-set-key [C-f4] (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key [(mouse-4)] #'(lambda () (interactive) (scroll-down (/ (window-height) 2))))
(global-set-key [(mouse-5)] #'(lambda () (interactive) (scroll-up   (/ (window-height) 2))))
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

(when (> emacs-major-version 22) (setq history-length 1000) (savehist-mode 1))
;(setenv "LANG" "en_US.UTF-8")
;(setenv "LC_ALL" "en_US.UTF-8")
;(setenv "LC_CTYPE" "en_US.UTF-8")
(set-face-attribute 'region nil :background "#b8d8ff")
(menu-bar-enable-clipboard)
(turn-off-auto-fill)
(setq fill-column 120)
(setq split-width-threshold 200)
(recentf-mode 1)                                     ;; Recent files in menu
(setq recentf-max-saved-items nil)
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
(setq visible-bell t)                                ;; Turn off beep.
(setq ring-bell-function 'ignore)                    ;; Turn the alarm totally off
(standard-display-8bit 128 255)                      ;; Do not expand unprintable characters to their octal values.
(setq fortran-comment-region "C MKR")                ;; Fortran comments prefix.
(line-number-mode 1)                                 ;; Show line-number in the mode line.
(column-number-mode 1)                               ;; Show column-number in the mode line.
(fset 'yes-or-no-p 'y-or-n-p)                        ;; Will allow you to type just "y" instead of "yes".
(setq-default tab-width 2)                           ;; Set Tab-Width.
(setq sh-basic-offset 2)                             ;; Set tab-width in shell scripts
(setq-default indent-tabs-mode nil)                  ;; Permanently force Emacs to indent with spaces.
(put 'upcase-region 'disabled nil)                   ;; Convert the region to upper case.
(put 'downcase-region 'disabled nil)                 ;; Convert the region to lower case.
(blink-cursor-mode -1)                               ;; Switch off blinking cursor mode.
(setq large-file-warning-threshold nil)              ;; Maximum size of file above which a confirmation is requested
(add-hook 'before-save-hook 'delete-trailing-whitespace)  ;; configuration required (remove-hook 'before-save-hook 'delete-trailing-whitespace)
(setq find-file-visit-truename t)                    ;; Follow symbolic links when file is visited
(setq mode-require-final-newline t)
(setq delete-trailing-lines nil)
(setq printer-name "HP-ENVY-4520-series")            ;; lpstat -p -d
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq vc-follow-symlinks t)
(setq grep-command "grep --exclude-dir=\".svn\" --exclude=TAGS -nHriIZ -e ")
(setq tags-case-fold-search nil)
(if (not (assq 'user-size initial-frame-alist))      ;; Unless we've specified a number of lines, prevent the startup code from
    (setq tool-bar-originally-present nil))          ;; shrinking the frame because we got rid of the tool-bar.
;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(defadvice ediff-quit (around advice-ediff-quit activate)
      (cl-flet ((yes-or-no-p (&rest args) t) (y-or-n-p (&rest args) t))
        ad-do-it))

(add-hook 'comint-exec-hook                          ;; Don't ask about killing process buffers on exit
          #'(lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;; Place Backup Files in Specific Directory
(setq make-backup-files t)                           ;; Enable backup files.
(setq version-control t)                             ;; Enable versioning with default values.
(setq delete-old-versions t)
(setq backup-directory-alist                         ;; Save all backup file in this directory.
      (list `(".*" . ,(concat custom-dir "/.emacs.backups"))))

(defvar insert-time-format "%T" "*Format for \\[insert-time].")
(defvar insert-date-format "%e %B %Y %T" "*Format for \\[insert-date].")

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))


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

;; ANSI colorization of a buffer
(require 'ansi-color)
(defun ansi-color ()
  (interactive)
  (let ((inhibit-read-only t)
        (ansi-color-apply-face-function
         (lambda (beg end face)
           (when face
             (put-text-property beg end 'face face)))))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook
 'compilation-filter-hook
 (lambda ()
   (ignore-errors
     (when (eq major-mode 'compilation-mode)
       (replace-char-type-literals compilation-filter-start (point-max))
       (ansi-color-apply-on-region compilation-filter-start (point-max))))))

(add-hook
 'compilation-filter-hook
 (lambda ()
   (ignore-errors
     (when (eq major-mode 'compilation-mode)
))))


;; calendar settings
(defface calendar-kw `((t (:foreground "black") (:background "pale green")))  "Face for a calendar week number column")
(setq calendar-week-start-day 1)
(setq calendar-intermonth-text '(propertize (format "%2d" (car (calendar-iso-from-absolute (calendar-absolute-from-gregorian (list month day year))))) 'font-lock-face 'calendar-kw))
(setq calendar-intermonth-header (propertize "KW" 'font-lock-face 'calendar-kw))

;; Thema
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;(load-theme 'solarized-dark t)

;; Start Emacs as a server
(load "server")
(unless (server-running-p) (server-start))

;;}}}

;;{{{ Load local packages

(when (package-dir "adoc-mode*")
  (require 'adoc-mode))

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

(when (package-dir "somafm*")
  (require 'somafm))

(when (package-dir "volume*")
  (autoload 'volume "volume" "Tweak your sound card volume." t))

(when (package-dir "elfeed*")
  (require 'elfeed)
  (global-set-key (kbd "C-x w") (lambda () (interactive) (elfeed) (elfeed-update)))
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "http://arxiv.org/rss/cs.CV"
          ;;"http://arxiv.org/rss/cs.RO"
          ;;"http://arxiv.org/rss/cs.SY"
          ;;"http://arxiv.org/rss/eess.SP"
          "https://openai.com/blog/rss.xml"
          "https://towardsdatascience.com/feed"
          "https://news.mit.edu/rss/topic/artificial-intelligence2"
          "https://bair.berkeley.edu/blog/feed.xml"
          "https://machinelearning.apple.com/rss.xml"
          "https://www.sciencedaily.com/rss/computers_math/artificial_intelligence.xml"
          "https://www.deepmind.com/blog/rss.xml"
          "https://ai2people.com/feed/"
          "https://machinelearningmastery.com/blog/feed/"
          "https://aws.amazon.com/blogs/machine-learning/feed/"
          "https://www.reddit.com/r/Cyberpunk/.rss"
          "https://www.reddit.com/r/cpp/.rss"
          "https://www.reddit.com/r/robotics/.rss"
          "https://www.reddit.com/r/SelfDrivingCars/.rss"
          "https://www.reddit.com/r/Driverless/.rss"
          "https://xkcd.com/rss.xml"
          "https://hnrss.org/newest"
          ;; "https://www.wired.com/feed/rss"
          ;; "https://www.wired.com/feed/category/science/latest/rss"
          ;; "https://www.wired.com/feed/category/security/latest/rss"
          ;; "https://www.wired.com/feed/category/transportation/latest/rss"
          ;; "https://www.wired.com/feed/category/ideas/latest/rss"
          ;; "https://www.wired.com/feed/category/gear/latest/rss"
          ;; "https://habr.com/ru/rss/best/daily/?fl=ru"
          ))

  (defun elfeed-show-quick-url-note ()
    "Fastest way to capture entry link to org agenda from elfeed show mode"
    (interactive)
    (let* ((link (elfeed-entry-link elfeed-show-entry))
           (title (replace-regexp-in-string "[][]" "" (elfeed-entry-title elfeed-show-entry)))
           (tags `(,(when (string-match-p "arxiv.org" link) ":ARXIV:")))
           (note (format "[[%s][%s]] %s" link title (mapconcat 'identity (remove-if 'not tags) " "))))
      (kill-new note)
      (org-capture nil "n")
      (yank)
      (org-capture-finalize)))

  (define-key elfeed-show-mode-map "v" 'elfeed-show-quick-url-note))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth scrolling
(when (package-dir "smooth-scrolling*")
  (require 'smooth-scrolling)
  (setq smooth-scroll-margin 2))

(global-set-key (kbd "<C-up>") 'scroll-down-command)
(global-set-key (kbd "<C-down>") 'scroll-up-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fontlocking modes
(require 'doc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired mode
(load-library "dired")

(when running-on-darwin
  (setq dired-use-ls-dired nil))

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
(define-key dired-mode-map "z" (lambda () (interactive)
                                 (let* ((name (or (dired-get-filename nil t) default-directory))
                                        (localname (or (file-remote-p name 'localname) name)))
                                   (kill-new localname)
                                   (x-set-selection nil localname))))

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
              #'(lambda () (when (eq major-mode 'dired-mode)
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
  (setq pdf-app "evince")
  (setq openwith-associations
        (cond
         (running-on-gnu/linux
          '(("[^_]?\\.\\(ps\\|pdf\\|djvu\\|epub\\)\\'" "evince" (file))
            ("\\.\\(docx?\\|odt\\|pptx?\\|rtf\\|xlsx?\\)\\'" "libreoffice" (file))
            ("\\.\\(ai\\)\\'" "inkscape" (file))
            ("\\.\\(svg\\)\\'" "eog" (file))
            ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mp4\\)\\'" "smplayer" (file))))
         (running-on-windows
          '("\\.\\(dll\\|pyd\\)\\'" "depends.exe" (file)))
         (running-on-darwin
          '(("\\.\\(ps\\|pdf\\|djvu\\docx?\\|epub\\|tif\\|jp2\\|mov\\|drawio\\|pptx?\\|rtf\\|xlsx?\\)$" "open" (file))
            ("\\.\\(ai\\)\\'" "/Applications/Inkscape.app/Contents/MacOS/inkscape" (file))))
         )))

;; open file in alternative editor
(defvar alternate-editor "gedit"
  "Editor to use when visiting a buffer outside of emacs.")

(defun open-in-alternate-editor (&optional arg)
  "Open buffer in alternative editor.  If buffer is unsaved,
bring it up in a temporary file.  With prefix argument, ask for
the editor to use."
  (interactive "P")
  (let ((edit (executable-find (if arg
                                   (read-from-minibuffer
                                    "Enter editor to use: "
                                    alternate-editor)
                                 alternate-editor)))
        (file (or (buffer-file-name)
                  (make-temp-file "unsaved-emacs-buffer-")))
        (buff (unless (buffer-file-name)
                (save-restriction (widen) (buffer-string)))))
    (when (null edit)
      (error "Can't find alternate editor"))
    (unless (buffer-file-name)
      (with-temp-file file
        (insert buff)))
    (start-process "Alternate Editor" nil edit file (format "+%d" (line-number-at-pos)))))

;;; Shell mode
(setq ansi-color-names-vector ["black" "red4" "green4" "yellow4" "blue3" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'fish-mode-hook (lambda () (modify-syntax-entry ?_ "w" fish-mode-syntax-table)))
(add-hook 'sh-mode-hook (lambda () (modify-syntax-entry ?_ "w" sh-mode-syntax-table)))

;; Extend execution paths with fish paths
(let*
    ((fish-path (split-string (shell-command-to-string "/opt/homebrew/bin/fish -c 'printf \"%s\\n\" $PATH' 2>/dev/null") "\n"))
     (full-path (append fish-path (cl-remove-if (lambda (item) (member item fish-path)) exec-path))))
  (setenv "PATH" (string-join full-path ":"))
  (setq exec-path full-path))

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
           (google-translate-translate src dst str 'kill-ring)
           (insert (format " "))
           (insert (car kill-ring-yank-pointer))))))
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "ru"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git mode
(when (package-dir "magit*")
  (require 'magit)
  (require 'magit-blame)
  (setq magit-refs-pad-commit-counts t)
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-save-repository-buffers nil)
  (custom-set-variables '(git-commit-summary-max-length 80))
  (global-set-key "\C-cs" 'magit-status)
  (global-set-key "\C-cb" 'magit-blame)
  (global-set-key "\C-cl" 'magit-log-buffer-file)
  (global-set-key "\C-cg" 'gh-lines)

  ;; in .dir-locals.el
  ;; ((json-mode . ((js-indent-level . 2)))
  ;;  (magit-refs-mode . ((eval . (remove-hook 'magit-refs-sections-hook 'magit-insert-remote-branches)))))

  (defun gh-lines (ref)
    "Open the current line in GitHub"
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

  (defun bb-lines (ref)
    "Open the current line in Bitbucket"
    (interactive (list (magit-read-starting-point "Show line for" nil (magit-get-current-branch))))
    (let* ((remotes (magit-list-remotes))
           (remote (cond
                    ((null remotes) "origin")
                    ((member "upstream" remotes) "upstream")
                    ((member "origin" remotes) "origin")
                    ((t (car remotes)))))
           (remote-url
            (replace-regexp-in-string "/+$" "" ; remove trailing slashes
            (replace-regexp-in-string ":[0-9]+/" "/" ; remove port number
            (replace-regexp-in-string "://[^@]+@" "://" ; remove user name
             (magit-get "remote" remote "url")))))
           (proj-url (replace-regexp-in-string ".git" "/browse" (replace-regexp-in-string "scm/werk" "projects/WERK/repos" remote-url)))
           (from (line-number-at-pos (if (and transient-mark-mode mark-active) (region-beginning) (point)))) ; or (format-mode-line "%l")
           (to (line-number-at-pos (if (and transient-mark-mode mark-active) (- (region-end) (if (= (current-column) 0) 1 0)) (point))))
           (lines (if (>= from to) (format "%d" from) (format "%d-%d" from to)))
           (gh-url (format "%s/%s?at=%s#%s" proj-url (magit-file-relative-name) (substring (magit-rev-parse ref) 0 8) lines)))
      (kill-new gh-url)
      (browse-url gh-url)))

  ;; Add magit process buffer ANSI colorization
  (defun magit-color-buffer (proc &rest args)
    (interactive)
    (with-current-buffer (process-buffer proc)
      (read-only-mode -1)
      (ansi-color-apply-on-region (point-min) (point-max))
      (read-only-mode 1)))
  (advice-add 'magit-process-filter :after #'magit-color-buffer)

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
(when (package-dir "matlab-mode*")
  (autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
  (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
)
;; (custom-set-variables '(matlab-comment-region-s "% "))
;; (setq matlab-fill-code nil)
;; (custom-set-faces '(matlab-cellbreak-face ((t (:foreground "Firebrick" :weight bold)))))

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
(setq tramp-debug-buffer t)
(setq tramp-verbose 10)
(setq tramp-histfile-override "/dev/null")
(setq tramp-remote-process-environment (append tramp-remote-process-environment '("LC_ALL=C.UTF-8" "LANG=C.UTF-8")))

(global-set-key "\C-c\C-t" 'tramp-cleanup-all-connections)

(when (package-dir "magit-tramp*")
  (require 'magit-tramp))

(when (package-dir "protobuf-mode*")
  (require 'protobuf-mode))

(when (package-dir "docker-*")
  (require 'docker))

(when (package-dir "twittering-mode-*")
  (require 'twittering-mode))

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
;; setup spell checker
(when running-on-windows
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
(if (executable-find "aspell")
    (progn
      (require 'ispell)
      (setq-default ispell-program-name "aspell")
      (ispell-change-dictionary "american" t)
      (setq ispell-check-comments t)
      (global-set-key (kbd "C-`") 'ispell-word)
      (add-hook 'text-mode-hook 'flyspell-mode)
      (add-hook 'c-mode-hook 'flyspell-mode)
      (add-hook 'python-mode-hook 'flyspell-mode)
      (add-hook 'org-mode-hook 'flyspell-mode))
  (message "Aspell not found."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other modes
(add-hook 'js-json-mode-hook (lambda () (setq js-indent-level 2)))
(setq auto-mode-alist (append auto-mode-alist '(("\\.json$" . json-mode) ("\\.geojson$" . json-mode) ("\\.manifest$" . json-mode))))


(when (package-dir "feature-mode*")
  (load-library "feature-mode")
  (modify-syntax-entry ?_ "w" feature-mode-syntax-table)
  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))

(when (package-dir "jade-mode*")
  (load-library "jade-mode")
  (add-to-list 'auto-mode-alist '("\\.jade\\'" . jade-mode)))

(when (package-dir "bitbake*")
  (require 'bitbake))

(when (package-dir "polymode*")
  (require 'polymode)
  (require 'poly-lock)

  (when (package-dir "bitbake*")
    (define-innermode poly-bitbake-root-innermode
                      :mode nil
                      :fallback-mode 'host
                      :head-mode 'host
                      :tail-mode 'host)
    (define-innermode poly-bitbake-python-innermode poly-bitbake-root-innermode
                      "Displayed python { } innermode."
                      :mode 'python-mode
                      :head-matcher "^python"
                      :tail-matcher "^}$"
                      :head-mode 'host
                      :tail-mode 'host
                      :allow-nested nil)))

(when (package-dir "qml-mode*")
  (require 'qml-mode)
  (add-hook 'qml-mode-hook (lambda () (modify-syntax-entry ?' "|"))))

(when (package-dir "bazel-*")
  (require 'bazel)
  (add-to-list 'auto-mode-alist '("BUILD\\(\\.[^/]+\\)?$" . bazel-build-mode))
  (add-to-list 'auto-mode-alist '("\\.BUILD$" . bazel-build-mode))
  (add-to-list 'auto-mode-alist '("WORKSPACE.bzlmod$" . bazel-workspace-mode))
  (modify-syntax-entry ?_ "w" bazel-mode-syntax-table)
  (modify-syntax-entry ?_ "w" bazel-build-mode-syntax-table)
  (modify-syntax-entry ?_ "w" bazel-starlark-mode-syntax-table)
  (modify-syntax-entry ?_ "w" bazel-workspace-mode-syntax-table))

(when (package-dir "eff-*")
  (require 'eff))

(when (package-dir "string-inflection*")
  ;; cycle between snake case, camel case, etc.
  (require 'string-inflection)
  (global-set-key (kbd "C-c i") 'string-inflection-cycle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C and C++ modes
(require 'cc-langs)
(require 'cc-mode)
(require 'jam-mode)
(require 'cmake-mode)
(require 'gud)
(require 'gdb-mi)
(require 'osl-mode)
(require 'glsl-mode)

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
      (t (format "%s%s" (if running-on-windows "" "./") (file-name-base (get-buffer-file-name))))))
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

  (when (and (not running-on-windows)
             (or (eq major-mode 'c++-mode) (eq major-mode 'fortran-mode)
                 (eq major-mode 'gud-mode) (eq major-mode 'python-mode)
                 (eq major-mode 'go-mode) (eq major-mode 'objc-mode)))
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
  (custom-set-variables '(ag-ignore-list '("TAGS" "*.bin" "bundle.js" "*.ipynb" "*.html" "*node_modules*")) '(ag-highlight-search t))
  (global-set-key (kbd "<s-f3>") (lambda () (interactive) (ag/search (word-at-point) (ag/project-root default-directory))))

  ;; use ag-project-root in local variables of polyrepos
  (advice-add 'ag/project-root :around (lambda (orig file-path)
                                         (if (boundp 'ag-project-root)
                                             ag-project-root
                                           (funcall orig file-path)))))


(when (package-dir "emojify*")
  (require 'emojify)
  ;(add-hook 'after-init-hook #'global-emojify-mode)
  (custom-set-variables '(emojify-emoji-styles (quote (github unicode))))
  (custom-set-variables '(emojify-program-contexts (quote (comments)))))

(when (package-dir "rosemacs*")
  (require 'rosemacs)
  (global-set-key "\C-x\C-r" ros-keymap))

(defun fmq-compilation-finish (buffer status)
  (let ((time (time-convert (time-subtract (current-time) (buffer-local-value 'compile-starts-at buffer)) 'integer)))
    (when (> time 20)
      (call-process "notify-send" nil nil nil
                    "-i" "emacs"
                    (format "%s %s" status (car (buffer-local-value 'compilation-arguments buffer)))
                    (format "in %s %ds" (buffer-local-value 'compilation-directory buffer) time)))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskel mode
(when (package-dir "haskell*")
  (require 'haskell-mode)
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (modify-syntax-entry ?_ "w" haskell-mode-syntax-table)

  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t)

   '(haskell-process-type 'cabal-repl))

  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

  (require 'intero)
  (add-hook 'haskell-mode-hook 'intero-mode))

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
  (modify-syntax-entry ?_ "w" lua-mode-syntax-table)
  (add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode)))

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

(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-mysql-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go
(add-hook 'go-mode-hook
  #'(lambda()
     (add-hook 'before-save-hook 'gofmt-before-save)
     (local-set-key '[f3]   'godef-jump)
     (local-set-key '[M-f3] 'pop-tag-mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set hooks
(cl-loop for mode in '(c-mode-hook c++-mode-hook fortran-mode-hook jam-mode-hook go-mode-hook
                    qt-pro-mode-hook gud-mode-hook qml-mode-hook python-mode-hook haskell-mode-hook
                    bazel-build-mode-hook bazel-starlark-mode-hook bazel-module-mode-hook bazel-workspace-mode-hook
                    js-mode-hook objc-mode-hook web-mode-hook json-mode makefile-mode-hook makefile-bsdmake-mode-hook)
         do (add-hook mode 'development-mode-hook))

(add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))

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
          '(("Evince" "evince \"%u\"")
            ("Okular" "okular \"%u\" --unique")
            ("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))
    (if running-on-gnu/linux (setq TeX-view-program-selection '((output-pdf "Evince") (output-dvi "Okular"))))
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
(add-hook 'text-mode-hook #'(lambda () (turn-off-auto-fill) (setq fill-column 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
(when (or (package-dir "org-*") (fboundp 'org-mode))
  (require 'org)
  (require 'ob-core)
  (require 'org-agenda)
  (require 'org-tempo)
  (require 'ox-reveal)
  (org-defkey org-mode-map [(control tab)] 'cyclebuffer-forward)
  (org-defkey org-mode-map [(control return)] 'mini-calc)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((http . t) (html-chrome . t) (python . t) (C . t) (haskell . t) (sqlite  . t) (maxima . t)
                                 (latex . t) (plantuml . t) (dot . t) (ruby . t) (R . t) (gnuplot . t) (scheme . t) (mermaid . t)))
  (add-hook 'org-babel-after-execute-hook (lambda () (condition-case nil (org-display-inline-images) (error nil))))
  (add-hook 'scheme-mode-hook 'geiser-mode)
  (add-hook 'org-mode-hook #'(lambda ()
                             ;(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
                             (local-set-key (kbd "<H-tab>") 'pcomplete)))

  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)


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
   '(geiser-default-implementation 'guile)
   '(org-log-done 'time)
   '(org-support-shift-select 'always)
   '(org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))
   '(org-todo-keyword-faces '(("TODO" . "deep pink")
                              ("DONE" . "sea green")
                              ("IDEA" . "IndianRed2")
                              ("INPROGRESS" . "DodgerBlue3")
                              ("REPORT" . "blue")
                              ("BUG" . "red")
                              ("KNOWNCAUSE" . "purple")
                              ("FIXED" . "SpringGreen3")
                              ("CANCELED" . "grey")))

   '(org-format-latex-options
     '(:foreground default
       :background default
       :scale 2
       :html-foreground "Black"
       :html-background "Transparent"
       :html-scale 1.0
       :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

   '(org-reveal-mathjax-url "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")

   '(org-reveal-external-plugins
     '((CopyCode . "https://cdn.jsdelivr.net/npm/reveal.js-copycode@1.2.0/plugin/copycode/copycode.js")
       (Clipboard . "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.6/clipboard.min.js")
       (RevealMath.KaTeX . "%splugin/math/math.js")
       (RevealMath.MathJax2 . "%splugin/math/math.js")
       (RevealMath.MathJax3 . "%splugin/math/math.js")))


   '(org-directory "~/share/org")
   '(org-tag-alist '(("BIO" . ?b) ("COMP" . ?c) ("EMACS" . ?e) ("LOC" . ?l)))
   '(org-capture-templates '(("n" "note" entry (file+datetree "~/notes/notes.org") "* %?\nEntered on %U\n  %i"))))

  (add-to-list 'org-structure-template-alist '("p" . "src python :results output"))
  (add-to-list 'org-structure-template-alist '("m" . "src mermaid :file _.png"))
  (add-to-list 'org-structure-template-alist '("pg" . "src sql :engine postgresql :dbuser postgres :dbhost localhost :dbport 5432"))
  (add-to-list 'org-structure-template-alist '("sb" . "src sql :engine postgresql :dbuser postgres :dbpassword postgres :dbhost localhost :dbport 54322"))

  (setq ob-mermaid-cli-path (or (executable-find "mmdc") (progn (message ".emacs: no mermaid CLI mmdc found in exec paths") nil)))

  ;(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental-mode))) ;; Use fundamental mode when editing plantuml blocks with C-c '

  (defun org-babel-kill-session ()
    "Kill session for current code block."
    (interactive)
    (unless (org-in-src-block-p)
      (error "You must be in a src-block to run this command"))
    (save-window-excursion
      (org-babel-switch-to-session)
      (kill-buffer)))

  ;; (defadvice org-mode-flyspell-verify
  ;;   (after my-org-mode-flyspell-verify activate)
  ;;   "Don't spell check src blocks."
  ;;   (setq ad-return-value
  ;;         (and ad-return-value
  ;;                (not (eq (org-element-type (org-element-at-point)) 'src-block)))))

  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MMM mode
(when (package-dir "/mmm-mode")
  (require 'mmm-auto)
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'html-mode "\\.php\\'" 'html-php))

(when (package-dir "/web-mode*")
  (require 'web-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (modify-syntax-entry ?_ "w" web-mode-syntax-table)
              (local-set-key (kbd "C-c C-o") 'browse-url-of-buffer)))
  ;; (add-to-list 'web-mode-comment-formats '("jsx" . "//"))
	(setq web-mode-markup-indent-offset 2))

;; (use-package rainbow-mode
;;   :hook (web-mode emacs-lisp-mode lisp-mode))

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
       '((".emacs$" . emacs-lisp-mode)
         ("\\.mdl$" . lisp-mode)
         ("\\.cg$" . lisp-mode)
         ("\\.el\\(.gz\\)?$" . emacs-lisp-mode)
         ("\\.tikz$" . LaTeX-mode)
         ("\\.tex$" . LaTeX-mode)
         ("\\.\\(ipp\\|c\\|i\\|h\\|cc\\|cxx\\|moc\\|cul\\|cuh\\|C\\|H\\|nxc\\|glsl\\|inl\\)$" . c++-mode)
         ("\\.\\(mm\\)$" . objc-mode)
         ("\\.pr[oif]$" . qt-pro-mode)
         ("\\.dps$" . pascal-mode)
         ("\\.qml\\(types\\)?$" . qml-mode)
         ("\\.pro$" . text-mode)
         ("\\.[ly]$" . c-mode)
         ("\\.pyi?$" . python-mode)
         ("\\.py.\\(j2\\|tpl\\)$" . python-mode)
         ("\\.css$" . css-mode)
         ("\\.Xdefaults$" . xrdb-mode)
         ("\\.Xenvironment$" . xrdb-mode)
         ("\\.Xresources$" . xrdb-mode)
         ("*.\\.ad$" . xrdb-mode)
         ("\\.fetchmailrc$" . fetchmail-mode)
         ("\\.osm$" . web-mode)
         ("\\.phtml$" . web-mode)
         ("\\.tpl\\.php$" . web-mode)
         ("\\.[gj]sp$" . web-mode)
         ("\\.as[cp]x$" . web-mode)
         ("\\.erb$" . web-mode)
         ("\\.mustache$" . web-mode)
         ("\\.html?$" . web-mode)
         ("\\.tsx$" . web-mode)
         ("\\.tmpl$" . web-mode)
         ("\\.launch$" . web-mode)
         ("\\.xsl$" . web-mode)
         ("\\.tei$" . web-mode)
         ("\\.[kx]ml$" . text-mode)
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
         ("\\.m[4c]$" . m4-mode)
         ("\.ebuild$" . ebuild-mode)
         ("\.gradle$" . gradle-mode)
         ("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode)
         ("[Mm]akefile\\..+$" . makefile-mode)
         ("\.go$" . go-mode)
         ("swdd.*\\.txt$" . doc-mode)
         ("\\.[mc]?[jt]sx?$" . web-mode)
         ("poetry.lock$" . conf-toml-mode)
         ) auto-mode-alist))

;;}}}

;;{{{ OS specific
(let* ((display (car (x-display-list))))
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
     ((string-match "^miha" user-login-name)
      (setq default-frame-alist '((top . 0) (left . 200) (width . 168) (fullscreen . fullheight)
                                  (font . "-*-DejaVu Sans Mono-normal-normal-normal-*-16-*-*-*-m-0-iso8859-1")))
      (setenv "PATH" (concat "C:\\MinGW\\bin;" (getenv "PATH")))
      (remove-hook 'after-init-hook 'w32-check-shell-configuration)
      (setq shell-command-switch "/c")
                                        ;(setq shell-file-name "c:/Windows/System32/cmd.exe")
                                        ;(setq w32-quote-process-args nil)
      (setq org-babel-sqlite3-command "D:\\tools\\sqlite3.exe")
      )))

   ;; Darwin specific settings
   (running-on-darwin
    (global-set-key [home] 'move-beginning-of-line)
    (global-set-key [end] 'move-end-of-line)
    (global-set-key [C-help] 'clipboard-kill-ring-save)
    (global-set-key [S-help] 'clipboard-yank)
    (global-set-key [C-help] 'clipboard-kill-ring-save)
    (global-set-key [S-delete] 'clipboard-kill-region)
    (define-key global-map [(control delete)] 'kill-region)
    (setq default-frame-alist
          (cond
           ((eq (display-mm-width display) 417) '((top . 0) (left . 100) (width . 198) (fullscreen . fullheight)))
           (t  '((top . 0) (left . 100) (width . 230) (fullscreen . fullheight))))))

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
     ((string-match "^miha-.*" system-name)
      (setq default-frame-alist '(
                                  (top . 0) (left . 200) (width . 216) (fullscreen . fullheight)
                                  (font . "-*-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
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
     ;; (run-with-timer 0 300 (lambda ()
     ;;   (unless (erc-server-buffer-live-p) (erc :server "irc.oftc.net" :port 6667)
     ;; (set-process-query-on-exit-flag (get-process "erc-irc.oftc.net-6667") nil)))))

     ((string-match "VirtualBox" system-name)
      (setq default-frame-alist `(
             (top . 0) (left . 120) (width . 224) (fullscreen . fullheight)
             (font . "-*-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
          )))
     ;;
     ((string-match ".*krasny.*" user-login-name)
      (cond
       ((not (and display (display-graphic-p))))
       ((eq (display-mm-width display) 494) ;; (x-display-pixel-width) (x-display-pixel-height)
        (setq default-frame-alist '(
             (top . 0) (left . 00) (width . 110) (fullscreen . fullheight)
             (font . "-*-Liberation Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))))
       ((and (< 508 (display-mm-width display)) (< (display-mm-width display) 524))
        (setq default-frame-alist '(
             (top . 0) (left . 00) (width . 226) (fullscreen . fullheight)
             (font . "-*-Liberation Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))))
       ((eq (display-mm-width display) 542)
        (setq default-frame-alist '(
             (top . 0) (left . 00) (width . 242) (fullscreen . fullheight)
             (font . "-*-Liberation Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))))
       ((eq (display-mm-width display) 677)
        (setq default-frame-alist '(
             (top . 0) (left . 100) (width . 222) (fullscreen . fullheight)
             (font . "-*-Liberation Mono-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1"))))
       ((eq (display-mm-width display) 1185)
        (setq default-frame-alist '(
             (top . 0) (left . 2000) (width . 306) (fullscreen . fullheight)
             (font . "-*-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))))
       ((eq (display-mm-width display) 1524)
        (setq default-frame-alist '(
             (top . 0) (left . 200) (width . 330) (fullscreen . fullheight)
             (font . "-*-DejaVu Sans Mono-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1"))))
       (t
        (setq default-frame-alist '(
             (top . 0) (left . 2000) (width . 306) (fullscreen . fullheight)
             (font . "-*-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"))))
         ))))
     ;;
    (t (message (format "unknown host name %s" system-name)))

 (t (message "unknown OS"))))

;;}}}

;;{{{ Host specific

(if (file-readable-p (concat custom-dir (system-name)))
    (load (concat custom-dir (system-name)))
  (write-region ";; -*- mode: lisp -*-" nil (concat custom-dir (system-name))))

;;}}}

;;{{{ Some own functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load ido mode first
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (> emacs-major-version 21)(ido-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open file and go to line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-file-goto (filename &optional cwd)
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

      ;; bazel target
      ((and cwd (string-match "/\\(/.*\\):\\([][[:alnum:]!%-@^_` \"#$&'()*-+,;<=>?{|}~/.]+\\):?$" filename))
       (setq name (concat cwd (match-string 1 filename) "/BUILD"))
       (setq target (match-string 2 filename))
       (switch-to-buffer (find-file-noselect name))
       (setq point (point))
       (condition-case nil
           (progn
             (goto-line 1)
             (re-search-forward (concat "name = \"" target "\""))
             (goto-char (match-beginning 0)))
         (error (goto-char point))))

      ;; open a file
      (t (switch-to-buffer (find-file-noselect name))))
    ))
(global-set-key [(control x) (control f)] 'find-file-goto)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parenthesis blinking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun blink-paren-first-line ()
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
(run-with-idle-timer 0.5 t 'blink-paren-first-line)

;; ;; Make parentheses more obvious.
;; (require 'paren-face)
;; (require 'highlight-parentheses)
;; (set-face-attribute 'parenthesis nil :inherit 'font-lock-keyword-face)
;; (set-face-attribute 'show-paren-match nil :background nil  :inverse-video t)
;; (set-face-attribute 'show-paren-mismatch nil :inherit 'warning)
;; (set-face-attribute 'hl-paren-face nil :inverse-video t)

;; ;; Adapts ‘highlight-parentheses-mode’ colours to theme.
;; (let ((c (face-attribute 'font-lock-keyword-face :foreground)))
;;   (setf hl-paren-colors
;;         (list
;;          (color-lighten-name c 10)
;;          (color-lighten-name c 20)
;;          (color-lighten-name c 30)
;;          (color-lighten-name c 40))))

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
    (cl-loop
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
    (help-print-return-message)))

(defun clean-zotero-bib ()
  (interactive)
  (replace-regexp "^.*url[ \t\n]*=.*$" "" nil (point-min) (point-max))
  (replace-regexp "^.*abstract[ \t\n]*=.*$" "" nil (point-min) (point-max))
  (replace-regexp "^.*annote[ \t\n]*=.*$" "" nil (point-min) (point-max))
  (replace-regexp "^.*doi[ \t\n]*=.*$" "" nil (point-min) (point-max))
  (delete-matching-lines "^[ \t\n]*$" (point-min) (point-max)))

(defun htmlfontify-region ()
  """HTML-fontify a region and open in a browser."""
  (interactive)
  (let* ((filename (make-temp-file (concat (buffer-name) "-") nil ".html"))
         (regionp (region-active-p))
         (beg (and regionp (region-beginning)))
         (end (and regionp (region-end)))
         (buf (current-buffer)))
    (with-temp-buffer
      (switch-to-buffer (current-buffer) nil t)
      (insert-buffer-substring buf beg end)
      (set-buffer (htmlfontify-buffer))
      (write-region nil nil filename)
      (kill-this-buffer)
      (browse-url-of-file filename))))

(defun markdown-html-filter (buffer)
  """
M-x httpd-start RET
M-x impatient-mode RET
M-x imp-set-user-filter RET markdown-html RET
M-x browse-url-default-macosx-browser RET http://localhost:8080/imp RET
"""
  (princ
   (with-current-buffer buffer
     (format "<!DOCTYPE html><html><title>Impatient Markdown!</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
   (current-buffer)))

(defun visit-markdown-html ()
  """Visit markdown buffers in a browser."""
  (interactive)
  (require 'simple-httpd)
  (setq httpd-port 8191)
  (httpd-start)
  (impatient-mode)
  (imp-set-user-filter 'markdown-html-filter)
  (browse-url-default-macosx-browser (format "http://localhost:%d/imp" httpd-port)))

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
  (insert (apply #'concatenate 'string (cl-loop for i from 0 to (% (abs (random)) 20) collect (format "%d, " (% (random) 100))))))

(defun insert-subdir-executable ()
  (interactive)
  (insert (shell-command-to-string "find . -executable -type f")))

;; }}}


;; TODO
(defun compile-goto-error (&optional event)
  "Visit the source for the error message at point.
Use this command in a compilation log buffer."
  (interactive (list last-input-event))
  (if event (posn-set-point (event-end event)))
  (or (compilation-buffer-p (current-buffer))
      (error "Not in a compilation buffer"))
  (compilation--ensure-parse (point))
  (if (get-text-property (point) 'compilation-directory)
      (dired-other-window
       (car (get-text-property (point) 'compilation-directory)))
    (setq compilation-current-error (point))
    (next-error-internal)))

(defun reverse-region (beg end)
 "Reverse characters between BEG and END."
 (interactive "r")
 (let ((region (buffer-substring beg end)))
   (delete-region beg end)
   (insert (nreverse region))))


(defun insert-random-uuid()
  (interactive)
  (let ((xstr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                              (user-uid)
                              (emacs-pid)
                              (system-name)
                              (user-full-name)
                              (current-time)
                              (emacs-uptime)
                              (garbage-collect)
                              (buffer-string)
                              (random)
                              (recent-keys)))))
      (insert (format "%s-%s-4%s-%s%s-%s"
                      (substring xstr 0 8)
                      (substring xstr 8 12)
                      (substring xstr 13 16)
                      (format "%x" (+ 8 (random 4)))
                      (substring xstr 17 20)
                      (substring xstr 20 32))))
    )


;; TODO DAP debugger
(when t
  (require 'dap-lldb)
  (setq dap-lldb-debugged-program-function #'(lambda () ))
  (defun dap-lldb--populate-start-file-args (conf)
    "Populate CONF with the required arguments."
    (print conf)
    (message "Expanded default directory %s %s" (expand-file-name default-directory) (plist-get conf :cwd))
                                        ;(interactive (list (if (< run-times 2) (read-string "Run command: " run-command) run-command)))
                                        ;(setq-local run-times (if (string= run-command command) (1+ run-times) 0))
                                        ;(setq-local run-command command)

    (plist-put conf :cwd (expand-file-name default-directory))
    (-> conf
        (dap--put-if-absent :dap-server-path dap-lldb-debug-program)
        (dap--put-if-absent :type "lldb-vscode")
        (dap--put-if-absent :program  (expand-file-name (read-file-name "Select file to debug: " (buffer-file-name))))
      (dap--put-if-absent :name "LLDB Debug")))

  (dap-register-debug-template "LLDB"
                               (list :type "lldb-vscode"
                                     :cwd nil
                                     :request "launch"
                                     :program nil
                                     :stopOnEntry t
                                     :name "LLDB::Run"))

  ;; (dap-register-debug-template "LLDB"
  ;;                              (list :type "lldb-vscode"
  ;;                                    :request "launch"
  ;;                                    :name "LLDB::Run"
  ;;                                    :stopAtEntry "main"
  ;;                                    :lldbmipath (locate-file "lldb-mi" exec-path)
  ;;                                    :cwd "/Users/michael/tmp"
  ;;                                    :debugger_args nil
  ;;                                    :env nil
  ;;                                    :target nil))
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (setq dap-print-io 1))

