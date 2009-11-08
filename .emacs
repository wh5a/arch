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

(ido-mode t)
(setq ido-create-new-buffer 'always)

(require 'recentf)
(recentf-mode 1)

;; disable tool-bar
(tool-bar-mode 0)

; http://www.tsdh.de/cgi-bin/wiki.pl/doc-view.el
(require 'doc-view)

;;;; Enhance redo
(require 'redo)
(global-set-key "\M-z" 'redo)

(setq font-lock-maximum-decoration t)

(set-scroll-bar-mode 'right)

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

;; Enable clear command in eshell buffer                                                                           
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;;; goto-line is already bound to "M-g g" or "M-g M-g"
;(global-set-key "\C-c\C-g" 'goto-line)

;;;; C-. to save a point   C-, to jump between them                                                             
(global-set-key [(control ?\.)] 'ska-point-to-register)
(global-set-key [(control ?\,)] 'ska-jump-to-register)
(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.                                                                   
Use ska-jump-to-register to jump back to the stored                                                             
position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8)
  (message "Position saved"))
(defun ska-jump-to-register()
  "Switches between current cursorposition and position                                                         
that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
        (jump-to-register 8)
        (set-register 8 tmp)))

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
                ("\\.lhs$" . literate-haskell-mode))))
(autoload 'haskell-mode "haskell-mode"
  "Major mode for editing Haskell scripts." t)
(autoload 'literate-haskell-mode "haskell-mode"
  "Major mode for editing literate Haskell scripts." t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; Try out a said-to-be-better indentation-mode: http://groups.google.com/group/fa.haskell/browse_thread/thread/ce8910712d8cdfa6/
; which is included in haskell-mode since 2.5, so there's no need to download it separately.
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
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
;; Haskell support for yasnippet
;; http://groups.google.com/group/fa.haskell/browse_thread/thread/739d9c8314fe7727
(load-file "~/Emacs/haskell-snippets/haskell-snippets.el")

;; auto-complete: http://www.emacswiki.org/emacs/AutoComplete
;; For more configuration tips, see http://www.emacswiki.org/emacs/init-auto-complete.el
;; auto-complete-yasnippet is now bundled into auto-complete-config
(add-to-list 'load-path "~/Emacs/auto-complete")
(when (require 'auto-complete-config)
  
  ;; I didn't include every possible extension.
  
  ;; Turn on the mode globally for modes included in 'ac-modes
  (global-auto-complete-mode t)

  (define-key ac-complete-mode-map "\C-n" 'ac-next)
  (define-key ac-complete-mode-map "\C-p" 'ac-previous)

  ;; default sources of candidates
  (set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))
  
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq ac-sources
                    '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer ac-source-symbols))))

  ;; I'm using the extension from http://madscientist.jp/~ikegami/diary/20090215.html
  ;; There's also http://www.emacswiki.org/emacs/auto-complete-extension.el, that can do hoogle search
  (require 'auto-complete-haskell)
  ;; Somehow the hook doesn't enable auto-complete-mode for Haskell although it should
  (setq ac-modes
      (append '(scheme-mode haskell-mode literate-haskell-mode)
              ac-modes))
  )

(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(setq auto-mode-alist (append '(("/PKGBUILD$" . pkgbuild-mode)) auto-mode-alist))

(require 'csurf)

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

; AUR provides emacs-tuareg-mode
(setq auto-mode-alist (cons '("\\.ml[iylp]?\\'" . tuareg-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\\.ml[iylp]?$" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
