;;; xinix-scala.el --- Emacs Xinix: scala-mode configuration.
;;
;; Copyright © 2011-2014 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/xinix
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic support for the Scala programming language

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

(require 'xinix-programming)
(xinix-require-packages '(scala-mode2))

(defun xinix-scala-mode-defaults ()
  (subword-mode +1))

(setq xinix-scala-mode-hook 'xinix-scala-mode-defaults)

(add-hook 'scala-mode-hook (lambda ()
                             (run-hooks 'xinix-scala-mode-hook)))
(provide 'xinix-scala)

;;; xinix-scala.el ends here
