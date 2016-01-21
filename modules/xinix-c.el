;;; google-c-style.el --- Google's C/C++ style for c-mode
;; Keywords: c, tools
;; google-c-style.el is Copyright (C) 2008 Google Inc. All Rights Reserved.
;;
;; It is free software; you can redistribute it and/or modify it under the
;; terms of either:
;;
;; a) the GNU General Public License as published by the Free Software
;; Foundation; either version 1, or (at your option) any later version, or
;;
;; b) the "Artistic License".
;; 
;;; Commentary:
;; 
;; Provides the google C/C++ coding style. You may wish to add
;; `google-set-c-style' to your `c-mode-common-hook' after requiring this
;; file. For example:
;;
;;    (add-hook 'c-mode-common-hook 'google-set-c-style)
;;
;; If you want the RETURN key to go to the next line and space over
;; to the right place, add this to your .emacs right after the load-file:
;;
;;    (add-hook 'c-mode-common-hook 'google-make-newline-indent)
;; 
;;; Code:
;; 
;; For some reason 1) c-backward-syntactic-ws is a macro and 2)  under Emacs 22
;; bytecode cannot call (unexpanded) macros at run time:
(eval-when-compile (require 'cc-defs))

;; Wrapper function needed for Emacs 21 and XEmacs (Emacs 22 offers the more
;; elegant solution of composing a list of lineup functions or quantities with
;; operators such as "add")
(defun google-c-lineup-expression-plus-4 (langelem)
  "Indents to the beginning of the current C expression plus 4 spaces.

This implements title \"Function Declarations and Definitions\"
of the Google C++ Style Guide for the case where the previous
line ends with an open parenthese.

\"Current C expression\", as per the Google Style Guide and as
clarified by subsequent discussions, means the whole expression
regardless of the number of nested parentheses, but excluding
non-expression material such as \"if(\" and \"for(\" control
structures.

Suitable for inclusion in `c-offsets-alist'."
  (save-excursion
    (back-to-indentation)
    ;; Go to beginning of *previous* line:
    (c-backward-syntactic-ws)
    (back-to-indentation)
    (cond
     ;; We are making a reasonable assumption that if there is a control
     ;; structure to indent past, it has to be at the beginning of the line.
     ;; [xinix]+catch
     ((looking-at "\\(\\(if\\|for\\|while\\|catch\\)\\s *(\\)")
      (goto-char (match-end 1)))
     ;; For constructor initializer lists, the reference point for line-up is
     ;; the token after the initial colon.
     ((looking-at ":\\s *")
      (goto-char (match-end 0))))
    (vector (+ 4 (current-column)))))

(defconst google-c-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (c-basic-offset . 8)		       ; [xinix]: from 2 to 8 then to 2
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . t)
    (comment-column . 40)
    (c-indent-comment-alist . ((other . (space . 2))))
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((arglist-intro google-c-lineup-expression-plus-4)
                        (func-decl-cont . ++)
                        ;; (member-init-intro . ++) ; [xinix]-
                        (member-init-intro . *) ; [xinix]+
                        ;; member-init-cont	-- Subsequent member initialization list lines.
                        (member-init-cont . 0) ; [xinix]+
                        ;; (inher-intro . /) ; [xinix]+-
                        (inher-intro . ++) ; [xinix]-+
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (case-label . +) 
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . /)
                        ;; (friend . /) ; [xinix]+-
                        (innamespace . 0))))
  "Google C/C++ Programming Style.")

(defun google-set-c-style ()
  "Set the current buffer's c-style to Google C/C++ Programming
  Style. Meant to be added to `c-mode-common-hook'."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (setq global-hl-line-mode t)					; [xinix]+
  (c-toggle-auto-hungry-state t)				; [xinix]+
  (c-toggle-auto-state nil)					; [xinix]+
  (setq tab-width 8)						; [xinix]+
  ;; [xinix]+
  ;; (setq tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 
  ;; 			  42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80 
  ;; 			  82 84 86 88 90 92 94 96 98 100 102 104 106 108 110 112 114 116 118 120
  ;; 			  122 124 126 128 130 132 134 136 138 140 142 144 146 148 150 152 154 156 158 160))
  (setq tab-stop-list '(8 16 24 32  40 
                          48 56 64 72 80 
                          88 96 104 112 120
                          128 136 144 152 160))
  (setq comment-start "/* " comment-end " */")			; [xinix]+
  (setq global-hl-line-mode t)					; [xinix]+
  (setq comment-multi-line t)					; [xinix]+
  (setq comment-style 'extra-line)				; [xinix]+
  (define-key c-mode-base-map [(control \`)] 'hs-toggle-hiding)	; [xinix]+
  (define-key c-mode-base-map [(f7)] 'compile)			; [xinix]+
  (define-key c-mode-base-map [(meta \`)] 'c-indent-command)	; [xinix]+
  ;; (define-key c-mode-base-map [(return)] 'newline-and-indent)	; [xinix]+
  ;; (define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
  (define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu) ; [xinix]+
  ;; 预处理设置
  (setq c-macro-shrink-window-flag t)	; [xinix]+
  (setq c-macro-preprocessor "cpp")	; [xinix]+
  (setq c-macro-cppflags " ")		; [xinix]+
  (setq c-macro-prompt-flag t)		; [xinix]+
  (setq hs-minor-mode t)		; [xinix]+
  (setq abbrev-mode t)			; [xinix]+
  (c-add-style "Google" google-c-style t))

(defun google-make-newline-indent ()
  "Sets up preferred newline behavior. Not set by default. Meant
  to be added to `c-mode-common-hook'."
  (interactive)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map [ret] 'newline-and-indent))

(require 'xinix-programming)
;; (xinix-require-packages '(cc-mode
;;                           xcscope
;;                           c-eldoc
;;                           ))
;; 
;; ;; (add-hook 'c-mode-common-hook 'cscope-minor-mode)
;; (cscope-setup)


(defun xinix-c-mode-common-defaults ()
  )
(setq xinix-c-mode-common-hook 'xinix-c-mode-common-defaults)
;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
;; (add-hook 'c-mode-common-hook (lambda ()
;;                                 (run-hooks 'google-set-c-style
;;                                            'google-make-newline-indent
;;                                            'eldoc-mod
;;                                            'xinix-c-mode-common-hook)))
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook 'eldoc-mode)

(defun xinix-makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))
(setq xinix-makefile-mode-hook 'xinix-makefile-mode-defaults)
(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'xinix-makefile-mode-hook)))

(add-to-list 'auto-mode-alist (cons "\\.[hH]$" #'c++-mode))
(add-to-list 'auto-mode-alist (cons "\\.[hHcC][pP][pP]$" #'c++-mode))
(add-to-list 'auto-mode-alist (cons "\\.[cC][cC]$" #'c++-mode))
(add-to-list 'auto-mode-alist (cons "\\.[hHcC][xX][xX]$" #'c++-mode))
(add-to-list 'auto-mode-alist (cons "\\.[cC]$" #'c++-mode))

(provide 'xinix-c)

;;; xinix-c.el ends here
