;;; xinix-scss.el --- Emacs Xinix: scss support
;;
;; Copyright © 2011-2014 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://www.batsov.com/emacs-xinix
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for scss-mode.

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

(require 'xinix-css)
(xinix-require-packages '(scss-mode))

;; turn off annoying auto-compile on save
(setq scss-compile-at-save nil)

(defun xinix-scss-mode-defaults ()
  (xinix-css-mode-defaults))

(setq xinix-scss-mode-hook 'xinix-scss-mode-defaults)

(add-hook 'scss-mode-hook (lambda () (run-hooks 'xinix-scss-mode-hook)))

(provide 'xinix-scss)
;;; xinix-scss.el ends here
