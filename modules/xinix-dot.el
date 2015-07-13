(require 'xinix-programming)
(xinix-require-packages '(graphviz-dot-mode))

(require 'graphviz-dot-mode)

(add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode))
;; (add-to-list 'interpreter-mode-alist '("node" . graphviz-dot-mode))

(eval-after-load 'graphviz-dot-mode
  '(progn
     (defun xinix-dot-mode-defaults ()
       ;; electric-layout-mode doesn't play nice with smartparens
       (setq mode-name "DOT"))

     (setq xinix-dot-mode-hook 'xinix-dot-mode-defaults)

     (add-hook 'graphviz-dot-mode-hook (lambda () (run-hooks 'xinix-dot-mode-hook)))))

(provide 'xinix-dot)


