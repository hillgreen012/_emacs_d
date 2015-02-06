;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: ztree  ;;
;;                 ;;
;; GROUP: Files    ;;
;;;;;;;;;;;;;;;;;;;;;
;; since neotree works with files and directories, let's consider it in
;; group Files
(require 'neotree)
;; (setq projectile-switch-project-action 'neotree-projectile-action)
(defun neotree-ffip-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root."))))
;; 如果使用 find-file-in-project (ffip), 使用下面的代码就可以随时切换根到项目目录
(define-key neotree-mode-map (kbd "C-c C-p") 'neotree-ffip-project-dir)
;; 如果你使用 popwin, 当 NeoTree 和 popwin 同时打开, 会在一旁出现另外一个新的
;; NeoTree buffer (#50). 可以使用下面代码解决。
;; (when neo-persist-show
;;   (add-hook 'popwin:before-popup-hook
;;             (lambda () (setq neo-persist-show nil)))
;;   (add-hook 'popwin:after-popup-hook
;;             (lambda () (setq neo-persist-show t))))


(provide 'xinix-files)

;;; xinix-files.el ends here
