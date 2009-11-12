;; Alist of possible indentations and start of statement they would close.
(defvar caml+twt-indent-list nil
  "Internal use.")
;; Length of the above
(defvar caml+twt-indent-list-length nil
  "Internal use.")
;; Current index into the alist.
(defvar caml+twt-indent-index nil
  "Internal use.")

;;behaves like vim's autoindent
(defun caml+twt-indent-line-1 ()
  (let ((target 
	 (save-excursion
	   (beginning-of-line)
	   (if (bobp)
	       ;;If this is the first line then it should be set to 0
	       0
	     (forward-line -1)
	     ;;Begin navigating to find the first previous non blank line...
	     (while (and (not (bobp)) (looking-at "[:blank:]*\n"))
	       (forward-line -1))
	     (current-indentation)
	     )
	   )))
	 (beginning-of-line)
	 (delete-horizontal-space)
	 (indent-to target))
  )

;;Ripped of python.el 
(defun caml+twt-indentation-levels ()
  (list (+ 2 (current-indentation)) 
	(current-indentation))
)

(defun caml+twt-indent-line ()
  (interactive)
  (if (eq last-command this-command)
      (progn (setq caml+twt-indent-index (% (1+ caml+twt-indent-index)
					     caml+twt-indent-list-length))
	     (beginning-of-line)
	     (delete-horizontal-space)
	     (indent-to (nth caml+twt-indent-index caml+twt-indent-list))
	     )
    (caml+twt-indent-line-1)
    (setq caml+twt-indent-list (caml+twt-indentation-levels)
	  caml+twt-indent-list-length (length caml+twt-indent-list)
	  caml+twt-indent-index (1- caml+twt-indent-list-length))))


(define-derived-mode caml+twt-mode tuareg-mode "tuareg+twt mode"
 (setq indent-line-function 'caml+twt-indent-line)
 (setq indent-tabs-mode 'nil)
 (run-hooks 'caml+twt-mode-hook)
 )

(defun caml+twt-help () (interactive)
  (describe-function 'caml+twt-mode))

;;error translating with macros?
;;Maybe defadvice
;;http://www.bookshelf.jp/texi/onlisp/onlisp_8.html#SEC53
;;
;;

(provide 'caml+twt-mode)
