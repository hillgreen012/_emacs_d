;;; xinix-css.el --- Emacs Xinix: css support
;;
;; Copyright © 2011-2014 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://www.batsov.com/emacs-xinix
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for css-mode.

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

(eval-after-load 'css-mode
  '(progn
     (xinix-require-packages '(rainbow-mode))

     (setq css-indent-offset 2)

     (defun xinix-css-mode-defaults ()
       (rainbow-mode +1)
       (run-hooks 'xinix-prog-mode-hook))

     (setq xinix-css-mode-hook 'xinix-css-mode-defaults)

     (add-hook 'css-mode-hook (lambda ()
                                (run-hooks 'xinix-css-mode-hook)))))

(provide 'xinix-css)
;;; xinix-css.el ends here
