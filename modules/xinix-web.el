;;; xinix-web.el --- Emacs Xinix: web template support
;;
;; Copyright © 2011-2014 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://www.batsov.com/emacs-xinix
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for web-mode.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(xinix-require-packages '(web-mode))
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist
             '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

(xinix-require-packages '(web-beautify))
(require 'web-beautify)

;; make web-mode play nice with smartparens
(setq web-mode-enable-auto-pairing nil)

(sp-with-modes '(web-mode)
  (sp-local-pair "%" "%"
                 :unless '(sp-in-string-p)
                 :post-handlers '(((lambda (&rest _ignored)
                                     (just-one-space)
                                     (save-excursion (insert " ")))
                                   "SPC" "=" "#")))
  (sp-local-pair "<% "  " %>" :insert "C-c %")
  (sp-local-pair "<%= " " %>" :insert "C-c =")
  (sp-local-pair "<%# " " %>" :insert "C-c #")
  (sp-local-tag "%" "<% "  " %>")
  (sp-local-tag "=" "<%= " " %>")
  (sp-local-tag "#" "<%# " " %>"))

(eval-after-load 'web-mode
  '(progn
     (defun xinix-web-mode-defaults ())
     (setq xinix-web-mode-hook 'xinix-web-mode-defaults)

     (add-hook 'web-mode-hook (lambda ()
                                (run-hooks 'xinix-web-mode-hook)))))

(xinix-require-package 'jinja2-mode)
(require 'jinja2-mode)
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . jinja2-mode))
(eval-after-load 'jinja2-mode
  '(progn
     (defun xinix-jinja2-mode-defaults ()
       (progn
         (setq sgml-basic-offset 2)
         ))
     (setq xinix-jinjia2-mode-hook 'xinix-jinjia2-mode-defaults)

     (add-hook 'jinja2-mode-hook (lambda ()
                                   (run-hooks 'xinix-jinja2-mode-hook)))))

(provide 'xinix-web)

;;; xinix-web.el ends here
