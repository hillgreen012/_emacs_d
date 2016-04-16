(xinix-require-packages '(elpy flycheck py-autopep8))

;; (xinix-require-package 'anaconda-mode)
;; (when (boundp 'company-backends)
;;   (xinix-require-package 'company-anaconda)
;;   (add-to-list 'company-backends 'company-anaconda))

(require 'electric)
(require 'xinix-programming)

;; Copy pasted from ruby-mode.el
(defun xinix-python--encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun xinix-python--detect-encoding ()
  (let ((coding-system
         (or save-buffer-coding-system
             buffer-file-coding-system)))
    (if coding-system
        (symbol-name
         (or (coding-system-get coding-system 'mime-charset)
             (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun xinix-python--insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

(defun xinix-python-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (xinix-python--encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (xinix-python--detect-encoding)))
        (when coding-system
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
                 ;; update existing encoding comment if necessary
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (xinix-python--insert-coding-comment coding-system)))
          (when (buffer-modified-p)
            (basic-save-buffer-1)))))))


(when (and (not (string= system-type "windows-nt"))
           (fboundp 'exec-path-from-shell-copy-env))
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(defun xinix-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  ;; (anaconda-mode)
  ;; (eldoc-mode)
  (elpy-use-ipython)
  (elpy-enable)
  (setq elpy-modules
        (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook
            'flycheck-mode)
  ;; (add-hook 'elpy-mode-hook
  ;;           'py-autopep8)
  (add-hook 'elpy-mode-hook
            'py-autopep8-enable-on-save)
  (add-hook 'elpy-mode-hook 'xinix-python-mode-set-encoding)
  
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local)
  (add-hook 'after-save-hook 'xinix-python-mode-set-encoding nil 'local)
  )

(setq xinix-python-mode-hook 'xinix-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'xinix-python-mode-hook)))

(provide 'xinix-python)

;;; xinix-python.el ends here
