;;; sd.el --- SD major mode

;; Copyright (C) 2007  Free Software Foundation, Inc.

;; Author: Todd Neal <tolchz@gmail.com>
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;
;; Commentary:
;;
;; Version 1.0 (7 January 2007)
;;
;; Based off simple.el
;;
;; Add the following to your .emacs to install
;; (require 'sd)
;; (add-to-list 'auto-mode-alist '("\\.sd$" . sd-mode))

;; (defface sd-list
;;   '((((class color) (background light))
;;      :foreground "red" :weight bold :slant italic)
;;     (((class color) (background dark))
;;      :foreground "red" :weight bold :slant italic))
;; "Face for sd lists")
    ;:group 'org-faces)

(defvar sd-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?\\ "_" st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?$ "." st)
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `sd-mode'.")

(defvar sd-font-lock-keywords
  `(("%\\w+\\(\\[\\]\\)?" (0 font-lock-variable-name-face t))
    ("$\\w+\\(\\[\\]\\)?" (0 font-lock-type-face t))
    ("@@[^@]*@@" (0 highlight))
    ("\\<[A-Z][A-Z0-9_]+\\>" (0 font-lock-function-name-face))
    ("\\(gosub\\|proc\\)[ \t]+\\(\\w+\\)" (2 font-lock-function-name-face))
    ;; (,(concatenate 'string "\\<" (regexp-opt '("DEST" "RESET")) "_[A-Z0-9_]+\\>") (0 font-lock-function-name-face))
    (,(concatenate 'string "^[ \t]*"
       (regexp-opt '("set" "global" "if" "else" "endif" "for" "next" "wait" "send"
                     "quit" "proc" "endproc" "gosub" "call" "switch" "case" "endcase" "endswitch"
                     "file" "write" "writeln" "message" "goto" "dialogpage" "listpage" "statusbar"
                     "subregion" "region" "endregion" "return" "exit" "mapdraw" "log" "udp" "menu"
                     "directory" "exists" "show" "while" "endwhile" "dialogbook" "mappanel" "dialog")))
     (0 font-lock-keyword-face))
    (,(regexp-opt '("singleton" "nowindow" "skipunknownids" "skipunknownids" "nocode") `words)
     (0 font-lock-keyword-face)))
  "Keyword highlighting specification for `sd-mode'.")

;;;###autoload
(define-derived-mode sd-mode fundamental-mode "SD"
	"A major mode for editing SD files."
	:syntax-table sd-mode-syntax-table
	(set (make-local-variable 'comment-start) "# ")
	(set (make-local-variable 'comment-start-skip) "#+\\s-*")
	(set (make-local-variable 'font-lock-defaults) '(sd-font-lock-keywords)))

(provide 'sd)
;;; sd.el ends here
