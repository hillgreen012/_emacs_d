;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: nyan-mode                    ;;
;;                                       ;;
;; GROUOP: Environment -> Frames -> Nyan ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; only turn on if a window system is available
;; this prevents error under terminal that does not support X
(require 'nyan-mode)
;; (case window-system                
;;   ((x w32) (nyan-mode)))
(nyan-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: golden-ratio                         ;;
;;                                               ;;
;; GROUP: Environment -> Windows -> Golden Ratio ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'golden-ratio)

(defun pl/helm-alive-p ()
  (if (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))
(add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
;; do not enable golden-raio in thses modes
(setq golden-ratio-exclude-modes '("ediff-mode"
                                   "gud-mode"
                                   "gdb-locals-mode"
                                   "gdb-registers-mode"
                                   "gdb-breakpoints-mode"
                                   "gdb-threads-mode"
                                   "gdb-frames-mode"
                                   "gdb-inferior-io-mode"
                                   "gud-mode"
                                   "gdb-inferior-io-mode"
                                   "gdb-disassembly-mode"
                                   "gdb-memory-mode"
                                   "magit-log-mode"
                                   "magit-reflog-mode"
                                   "magit-status-mode"
                                   "IELM"
                                   "eshell-mode"
                                   "dired-mode"
                                   "helm-mode"
                                   "neotree-mode"))

(golden-ratio-mode -1)


;; Dejavu Sans Mono 9 --- WenQuanYi Micro Hei Mono 12
;; Consolas 11 --- Microsoft Yahei 16
;; Liberation Mono 12 --- WenQuanYi Micro Mono 15
(set-language-environment "UTF-8")
(set-locale-environment "UTF-8")
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (set-fontset-font "fontset-default" 'unicode "WenQuanYi Micro Hei Mono 12"))))
  (set-fontset-font "fontset-default" 'unicode "WenQuanYi Micro Hei Mono 12"))
;; (set-fontset-font "fontset-default" 'unicode "WenQuanYi Micro Hei Mono 12")
;; (set-default-font "Dejavu Sans Mono 9")

(provide 'xinix-environment)

;;; xinix-environment.el ends here
