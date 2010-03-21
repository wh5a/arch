;;;; tramp usage:
;;;; use the cvs version to speed it up!
;; C-x C-f /remotehost:filename  RET (or /method:user@remotehost:filename)
;; C-x C-f /su::/etc/hosts RET
(setq tramp-default-method "ssh")

(setq-default indent-tabs-mode nil)

(setq compilation-skip-threshold 2)

(setq inhibit-splash-screen t)

(push "~/Emacs" load-path)

(transient-mark-mode t)

(setq js-indent-level 2)
(setq lisp-indent-offset 2)

(ido-mode t)
(setq
  ido-ignore-buffers ;; ignore these guys
  '("\\` " "^\*Mess" "^\*Help" "^\*Back" "^\*Completion" "^\*Ido")
  ido-create-new-buffer 'always
  ido-max-work-file-list      50   ; remember many
;  ido-use-filename-at-point t
  )

(require 'recentf)
(recentf-mode 1)

;; Ignore notices
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; disable tool-bar
(tool-bar-mode 0)

; http://www.tsdh.de/cgi-bin/wiki.pl/doc-view.el
(require 'doc-view)

;;;; Enhance redo
(require 'redo)
(global-set-key "\M-z" 'redo)

(setq font-lock-maximum-decoration t)

;; This is the default since 24 on UNIX/GTK
;(set-scroll-bar-mode 'right)
;; Cursor blinking by default since 24?
(blink-cursor-mode 0)
; The default cursor (a box) blocks the char under it for a few secs. Rumor says it's some GTK bug. Remove the next lines when the bug's fixed.
(setq-default cursor-type '(hbar . 3))
(setq-default cursor-in-non-selected-windows 'hollow)


(mouse-avoidance-mode 'animate)

(setq column-number-mode t)

(setq x-stretch-cursor t)

;;;; Auto fill
;; M-q to reformat paragraph
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq minibuffer-max-depth nil)

;; I use the following code. It makes C-a go to the beginning of the
;; command line, unless it is already there, in which case it goes to the
;; beginning of the line. So if you are at the end of the command line
;; and want to go to the real beginning of line, hit C-a twice:
(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))
(add-hook 'eshell-mode-hook
          '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))

;; WindMove enables buffer moving using Shift+Arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;;; elide the copyright headers
(require 'elide-head)
;; force eliding on all files. We could also do this to certain file types as we do above for hs-minor-mode.
;; C-u M-x elide-head, to undo eliding.
(add-hook 'find-file-hook 'elide-head)

;; C-h S to look up a symbol in the info doc.
(require 'info-look)

;; eldoc, for automatically displaying the argument list as you type
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
;; http://nflath.com/2010/01/c-eldoc-speedups/
;; http://github.com/nflath/c-eldoc/raw/master/c-eldoc.el
(require 'c-eldoc)
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; Enable clear command in eshell buffer                                                                           
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;;; goto-line is already bound to "M-g g" or "M-g M-g"
;(global-set-key "\C-c\C-g" 'goto-line)

(setq require-final-newline t)          ; assures the newline at eof

;; strip trailing whitespace, including excess newlines.                                           
(setq-default nuke-trailing-whitespace-p t)

; In X resource
;(set-default-font "Inconsolata-12")

; resize the fonts: C-x C--, C-x C-=
; Already loaded by default
;(require 'face-remap)

(require 'inf-haskell)
(setq inferior-haskell-find-project-root nil)
(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.hs$"  . haskell-mode)
                ("\\.hi$"  . haskell-mode)
                ("\\.ds$"  . haskell-mode) ; DDC
                ("\\.lhs$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for editing literate Haskell scripts." t)
(add-hook 'haskell-mode-hook
   (lambda ()
     (turn-on-haskell-decl-scan)
     ; Navigate between function definitions
     (define-key haskell-mode-map "\M-n" 'haskell-ds-forward-decl)
     (define-key haskell-mode-map "\M-p" 'haskell-ds-backward-decl)
     ; This mode has been modified by me to make use of scion
     (turn-on-haskell-doc-mode)
     ;(turn-on-haskell-indent)
     ;(turn-on-haskell-simple-indent)
     ;; Try out a said-to-be-better indentation-mode: http://groups.google.com/group/fa.haskell/browse_thread/thread/ce8910712d8cdfa6/
     ; which is included in haskell-mode since 2.5, so there's no need to download it separately.
     (turn-on-haskell-indentation)
     ;(yas/minor-mode)
     ))

(setq haskell-literate-default 'tex)
(require 'haskell-cabal)

; I believe we can customize delimiters, check out the doc.
(require 'mic-paren)
(paren-activate)
(setq paren-sexp-mode t)
(setq paren-match-face 'highlight)
; By default (i.e. in lisp), when the matching '(' is out of screen, we show the text after '('.
; In C, it makes more sense to show the text before '{'.
(add-hook 'c-mode-common-hook
          (function (lambda ()
                       (paren-toggle-open-paren-context 1))))

;;;; NOTE: yasnippet and auto-complete are different, but their ideas are related and can work together.
;; yasnippet recognizes user-defined keywords and expands into templates.
;; Such keywords have to be memorized usually.
;; auto-complete recognize the word being typed, and expands into matching candidates.
;; Such candidates can come from various sources, including yasnippet keywords!

;; yasnippet is available in the AUR as emacs-yasnippet
(add-to-list 'load-path "/usr/share/emacs/site-lisp/yas")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "/usr/share/emacs/site-lisp/yas/snippets")
;; Haskell support http://groups.google.com/group/fa.haskell/browse_thread/thread/739d9c8314fe7727
(load-file "~/Emacs/haskell-snippets/haskell-snippets.el")
;; OCaml support http://blog.mestan.fr/2009/02/22/ocaml-completion-reloaded/
(yas/load-directory "~/Emacs/snippets")

;; auto-complete: http://www.emacswiki.org/emacs/AutoComplete
;; For more configuration tips, see http://www.emacswiki.org/emacs/init-auto-complete.el
;; auto-complete-yasnippet is now bundled into auto-complete-config
(add-to-list 'load-path "~/Emacs/auto-complete")
(when (require 'auto-complete-config)
  ; Load the default configs and do some further tweaking
  (ac-config-default)
  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)

  ;; default sources of candidates
  (setq-default ac-sources (cons 'ac-source-yasnippet ac-sources))

  ;; A new way of adding keywords is through dictionary:
  ; Any user defined keywords go here
  ;(add-to-list 'ac-user-dictionary "foobar@example.com")
  ; mode-specific keywords
  (add-to-list 'ac-dictionary-directories "~/Emacs/auto-complete/dict")

  ;; I'm using the extension from http://madscientist.jp/~ikegami/diary/20090215.html
  ;; Except that the module names are produced by ghc-mod http://www.mew.org/~kazu/proj/ghc-mod/en/
  ;; There's also http://www.emacswiki.org/emacs/auto-complete-extension.el, that can do hoogle search
  (require 'auto-complete-haskell)
  ;; Somehow the hook doesn't enable auto-complete-mode for Haskell although it should
  ; ac-modes lists all modes with auto-complete enabled
  (setq ac-modes
      (append '(scheme-mode haskell-mode literate-haskell-mode tuareg-mode js-mode)
              ac-modes))
  )

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

(require 'csurf)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; AUR provides emacs-tuareg-mode
;; Many cool features like completion, help, type info, check out the mode help!
(setq auto-mode-alist (cons '("\\.ml[iylp]?\\'" . tuareg-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\\.ml[iylp]?$" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
; For tuareg-browse-manual. Requires an executable file "netscape".
(setq tuareg-manual-url
      "http://caml.inria.fr/pub/docs/manual-ocaml/index.html")
; For tuareg-browse-library
(setq tuareg-library-path "/usr/lib/ocaml/")
;; http://www.cocan.org/tips/single_line_comment_syntax
;; Comment from the point to the end of line or, if the point is at the end
;; of a line and not following a comment, insert one.
;; If the current line already ends with a comment, remove it.
(defun caml-toggle-comment-endofline (u)
  (interactive "P")
  (if (eq u nil)
      (let ((init (point)) beg end end_comment)
        (progn
          (end-of-line)
          (setq end (point))
          (if (looking-back (regexp-quote comment-end))
              (progn ; remove the ending comment (naive)
                (beginning-of-line)
                (uncomment-region (point) end)
                (setq end (- end (string-width comment-start)
                             (string-width comment-end)))
                (goto-char (min init end)))
            (if (eq init end)
                (indent-for-comment)
              (progn
                (beginning-of-line)
                (search-forward-regexp "[^ \t]" end t)
                (backward-char 1); now at the 1st non-space of the line
                (setq beg (point))
                (if (eq beg nil)
                    ;; line is composed of spaces => new comment
                    (indent-for-comment)
                  (progn
                    (comment-region (max init beg) end)
                    (goto-char init))
                  ))
              ))
          ))
    (progn ; C-u prefix
      (insert "(**  *)")
      (forward-char 4)
      )
    ))
(add-hook 'tuareg-mode-hook
   (lambda ()
      (define-key tuareg-mode-map "\C-c," 'caml-toggle-comment-endofline)
      (define-key tuareg-interactive-mode-map "\C-c,"
        'caml-toggle-comment-endofline)
      (yas/minor-mode)
      ))
; The Whitespace Thing, only if the first line is:
;(*pp ocaml+twt*)
(autoload 'caml+twt-mode "caml+twt" "Major mode for editing Caml+twt code" t)
(defun start-mlmode ()
    (when
      (save-excursion
        (progn
          (goto-char (point-min))
          (looking-at "(\\*pp ocaml\\+twt\\*)[:blank:]*")))
      (caml+twt-mode))
      (remove-hook 'find-file-hook 'start-mlmode 1))
(add-hook 'tuareg-load-hook (lambda ()
                              (add-hook 'find-file-hook
                                        'start-mlmode 1)))
(add-hook 'caml+twt-mode-hook
  (lambda ()
    ; A hack to force the execution of tuareg-fontify
    (setq major-mode 'tuareg-mode)))

; CIL: M-x cil-debug
(load-file "~/cil/tips/debugging.el")
(push "~/cil/TAGS" tags-table-list)
; case sensitive
(setq tags-case-fold-search nil)

;;;; Automatically keeps track of your recent positions
;; http://code.google.com/p/dea/source/browse/trunk/my-lisps/recent-jump.el
;; (setq rj-ring-length 10000)
(require 'recent-jump)
(recent-jump-mode)
(global-set-key (kbd "C-,") 'recent-jump-backward)
(global-set-key (kbd "C-.") 'recent-jump-forward)

;; SLIME
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(require 'slime)
(slime-setup '(slime-fancy slime-asdf slime-autodoc))
;(setq inferior-lisp-program "/path/to/lisp-executable")
(setq slime-lisp-implementations
      '((sbcl ("sbcl") :coding-system utf-8-unix)))
(setf slime-default-lisp 'sbcl)
(setq slime-startup-animation nil)

;(load "/usr/share/emacs/site-lisp/nxhtml/autostart.el")

;; Associate external applications with files so that you can open them via C-x C-f, with RET in dired, etc.
(require 'openwith)
(openwith-mode t)
; M-x customize-group RET openwith RET, or do it the old way like below
(setq openwith-associations
 '(("\\.pdf\\'" "acroread" (file))
   ("\\.mp3\\'" "mplayer" (file))
   ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file))
   ))

;; Edit texts areas in Google Chrome https://chrome.google.com/extensions/detail/ljobjlafonikaiipfkggjbhkghgicgoh
;; My own fork http://github.com/wh5a/emacs_chrome
(push "~/Chrome/emacs_chrome/servers" load-path)
(require 'edit-server)
(if (daemonp) (edit-server-start))
(defun edit-server-restart ()
   (interactive)
   (edit-server-stop)
   (edit-server-start)
   )

;; http://www.emacswiki.org/emacs/ImenuMode
;; Awesome function that makes use of imenu and ido to navigate in the current buffer
(defun ido-imenu ()
    "Will update the imenu index and then use ido to select a symbol to navigate to"
    (interactive)
    (imenu--make-index-alist)
    (let ((name-and-pos '())
          (symbol-names '()))
      (flet ((addsymbols (symbol-list)
                         (when (listp symbol-list)
                           (dolist (symbol symbol-list)
                             (let ((name nil) (position nil))
                               (cond
                                ((and (listp symbol) (imenu--subalist-p symbol))
                                 (addsymbols symbol))
   
                                ((listp symbol)
                                 (setq name (car symbol))
                                 (setq position (cdr symbol)))
   
                                ((stringp symbol)
                                 (setq name symbol)
                                 (setq position (get-text-property 1 'org-imenu-marker symbol))))
   
                               (unless (or (null position) (null name))
                                 (add-to-list 'symbol-names name)
                                 (add-to-list 'name-and-pos (cons name position))))))))
        (addsymbols imenu--index-alist))
      (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
             (position (cdr (assoc selected-symbol name-and-pos))))
        (cond
         ((overlayp position)
          (goto-char (overlay-start position)))
         (t
          (goto-char position))))))

;; Icicles not compatible with ido. To use it, uncomment these lines.
; (push "~/Emacs/icicles" load-path)
;(require 'icicles)
;(icy-mode)

;; M-x anything, to open anything (controlled by anything-sources) by category.
(push "~/Emacs/anything" load-path)
(require 'anything-config)
;; Better find-tag
(require 'anything-yaetags)
(global-set-key (kbd "M-.") 'anything-yaetags-find-tag)
;(require 'anything-etags)

;; http://emacser.com/process-sentinel.htm
(defun kill-buffer-when-shell-command-exit ()
  "Close current buffer when `shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
    (when process
      (set-process-sentinel process
                            (lambda (proc change)
                              (when (string-match "\\(finished\\|exited\\)" change)
                                (kill-buffer (process-buffer proc))))))))
;; 退出gdb的时候关闭gdb对应的buffer
(add-hook 'gdb-mode-hook 'kill-buffer-when-shell-command-exit)
;; 退出term的时候关闭term对应的buffer
(add-hook 'term-mode-hook 'kill-buffer-when-shell-command-exit)
(add-hook 'shell-mode-hook 'kill-buffer-when-shell-command-exit)

;; AUR: emacs-egg-git, much nicer UI than magit
; M-x egg-status, or C-x v s
(require 'egg)
;; magit is less friendly, but has some unique functionalities
; M-x magit-status
(autoload 'magit-status "magit" nil t)
; http://github.com/jimhourihan/egit/raw/master/egit.el
; Sort-of like gitk, depends on emacs-git-mode (AUR)
; M-x egit. Look at egit.el and search for egit-mode-map (line 914) for keybindings. Or look at the EGit menu.
(require 'egit)

;; AUR: emacs-markdown-mode-git
(autoload 'markdown-mode "markdown-mode/markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\.md" . markdown-mode) auto-mode-alist))

;; http://stackoverflow.com/questions/2171890/emacs-how-to-evaluate-the-smallest-s-expression-the-cursor-is-in-or-the-followi/2172519#2172519
(defun eval-surrounding-sexp (levels)
  (interactive "p")
  (save-excursion
    (up-list (abs levels))
    (eval-last-sexp nil)))

;; Similar to the kill-ring keybindings, but deal with clipboard
(global-set-key (kbd "C-c C-w") 'clipboard-kill-region)
(global-set-key (kbd "C-c C-y") 'clipboard-yank)
(global-set-key (kbd "C-c M-w") 'clipboard-kill-ring-save)

;; doing a pg-up followed by a pg-down should return to the original place.
(require 'pager)
(global-set-key "\C-v" 'pager-page-down)
(global-set-key "\ev" 'pager-page-up)

;; add the function point is inside to the mode-line.
(setq which-func-modes t)
(which-func-mode 1)

;; Remember buffer positions per-window, not per buffer
(require 'winpoint)
(winpoint-mode t)

;; `C-c <-` and `C-c ->` to cycle through window configurations
(winner-mode 1)

;; http://emacs-fu.blogspot.com/2010/02/dealing-with-many-buffers-ibuffer.html
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
  (quote (("default"
            ("Emacs"
              (or
                (mode . emacs-lisp-mode)
                (mode . lisp-interaction-mode)))
            ("Haskell"
              (or
                (mode . haskell-mode)
                (mode . literate-haskell-mode)
                (mode . inferior-haskell-mode)
                (mode . haskell-cabal-mode)))
            ("OCaml"
              (or
                (mode . tuareg-mode)
                (mode . caml+twt-mode)
                (mode . tuareg-interactive-mode)))
            ("Scheme"
              (or
                (mode . scheme-mode)
                (mode . inferior-scheme-mode)))
            ("Web"
              (or
                (mode . css-mode)
                (mode . javascript-mode)
                (mode . js-mode)
                (mode . html-mode)))
            ("Dired"
              (mode . dired-mode))
            ;; ("MyProject1"
            ;;   (filename . "src/myproject1/"))
            ;; ("MyProject2"
            ;;   (filename . "src/myproject2/"))
            ))))
(setq
  ibuffer-expert t
  ; Don't show empty groups
  ibuffer-show-empty-filter-groups nil
  ibuffer-display-summary nil)
(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")
    ; Override the ibuffer-find-file function to use ido
    (define-key ibuffer-mode-map (kbd "C-x C-f")
      (lambda ()
        (interactive)
        (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                   (if (buffer-live-p buf)
                                     (with-current-buffer buf
                                       default-directory)
                                     default-directory))))
          (ido-find-file-in-dir default-directory))))
    ))

;; My helper functions for resizing windows, buggy for windows more than 2 on a row/col.
(defun resize-left ()
  (interactive)
  (let ((other-window (windmove-find-other-window 'left)))
    (if other-window
      (enlarge-window-horizontally 1)
      (shrink-window-horizontally 1))))
(defun resize-right ()
  (interactive)
  (let ((other-window (windmove-find-other-window 'right)))
    (if other-window
      (enlarge-window-horizontally 1)
      (shrink-window-horizontally 1))))
(defun resize-up ()
  (interactive)
  (let ((other-window (windmove-find-other-window 'up)))
    (if other-window
      (enlarge-window 1)
      (shrink-window 1))))
(defun resize-down ()
  (interactive)
  (let* ((down-window (windmove-find-other-window 'down))
         (other-window (not (window-minibuffer-p down-window))))
    (if other-window
      (enlarge-window 1)
      (shrink-window 1))))
(global-set-key (kbd "S-C-<left>") 'resize-left)
(global-set-key (kbd "S-C-<right>") 'resize-right)
(global-set-key (kbd "S-C-<down>") 'resize-down)
(global-set-key (kbd "S-C-<up>") 'resize-up)

;; Add the ability to move between frames as well as windows
;; http://www.emacswiki.org/emacs/framemove.el
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; http://emacser.com/highlight-tail.htm  Draw a colourful "tail" while you write
; A little too distracting.
;(require 'highlight-tail)
;(highlight-tail-mode)

;; Highlight "TODO:"s  http://emacs-fu.blogspot.com/2008/12/highlighting-todo-fixme-and-friends.html
;; http://stackoverflow.com/questions/2242572/emacs-todo-indicator-at-left-side
;
; (add-hook 'find-file-hooks
;   (lambda ()
;     (font-lock-add-keywords nil
;       '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))
;
;; A more complete solution: http://www.emacswiki.org/emacs/FixmeMode
; Supports auto highlighting, and navigating between TODOs
(require 'fixme-mode)
;; Enabled for modes listed in 'fixme-modes
(fixme-mode 1)

;; Popup a terminal in the working directory
(defun popup-term ()
  (interactive)
  (start-process "terminal" nil "urxvtc")
  )

;; A more intuitive way of replace-string operation
; Mark a region, or use the current word, or use the incrementally searched string, and then activate iedit-mode by pressing C-;
; Use TAB and S-TAB to move around
; Edit one occurrence and have all others edited the same way.
(require 'iedit)
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

;; The 'devel' branch of scion: http://github.com/nominolo/scion/tree/devel
; Read README.markdown for usage, or look at the keybindings of scion-mode
(require 'scion)
(add-hook 'haskell-mode-hook
   (lambda ()
     (scion-mode 1)
     ;; Whenever a file is saved, immediately type check it and
     ;; highlight errors/warnings in the source.
     (scion-flycheck-on-save 1)))
; quiet mode
(setq scion-log-events nil)
; use ido-mode completion
(setq scion-completing-read-function 'ido-completing-read)

;; lightweight version of Desktop.el that only save the files you have open
;; http://github.com/nflath/save-visited-files/raw/master/save-visited-files.el
;; save is done automatically, you can also do `M-x save-visited-files-save` and `M-x save-visited-files-restore`
(require 'save-visited-files)
(turn-on-save-visited-files-mode)


;;;; This should be placed at the end!! So that all files will be properly opened.
(save-visited-files-restore)
