;;; xinix-haskell.el --- Emacs Xinix: Nice config for Haskell programming.
;;
;; Copyright © 2011-2014 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/xinix
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Nice config for Haskell programming.

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
(xinix-require-packages '(haskell-mode))

(eval-after-load 'haskell-mode
  '(progn
     (defun xinix-haskell-mode-defaults ()
       (subword-mode +1)
       (turn-on-haskell-doc-mode)
       (turn-on-haskell-indentation)
       (interactive-haskell-mode +1))

     (setq xinix-haskell-mode-hook 'xinix-haskell-mode-defaults)

     (add-hook 'haskell-mode-hook (lambda ()
                                    (run-hooks 'xinix-haskell-mode-hook)))))

(provide 'xinix-haskell)

;;; xinix-haskell.el ends here
