(require 'xinix-programming)
(xinix-require-packages '(plantuml-mode))

(setq plantuml-jar-path
      (expand-file-name "~/.emacs.d/vendor/org/plantuml.jar"))
;; (require 'plantuml-mode-autoloads)
(require 'plantuml-mode)

(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
;; (add-to-list 'interpreter-mode-alist '("node" . graphviz-dot-mode))


(eval-after-load 'plantuml-mode
  '(progn
     (defun xinix-plantuml-mode-defaults ()
       ;; electric-layout-mode doesn't play nice with smartparens
       (setq mode-name "PlantUML"))

     (setq xinix-plantuml-mode-hook 'xinix-plantuml-mode-defaults)

     (add-hook 'plantuml-mode-hook
               (lambda ()
                 (run-hooks 'plantuml-mode-hook)))))

(provide 'xinix-plantuml)


