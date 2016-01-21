;;; xinix-rust.el --- Emacs Xinix: A nice setup for Rust (and Rails) devs.

;;; Code:

(require 'xinix-programming)

(xinix-require-packages '(rust-mode))

;; Rake files are rust, too, as are gemspecs, rackup files, and gemfiles.
(add-to-list 'auto-mode-alist '("\\.rust\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; We never want to edit Rubinius bytecode
;; (add-to-list 'completion-ignored-extensions ".rbc")

(eval-after-load 'rust-mode
  '(progn
     (defun xinix-rust-mode-defaults ()
       )

     (setq xinix-rust-mode-hook 'xinix-rust-mode-defaults)

     (add-hook 'rust-mode-hook (lambda ()
                                 (run-hooks 'xinix-rust-mode-hook)))))

(provide 'xinix-rust)
;;; xinix-rust.el ends here
