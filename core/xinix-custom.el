;;; xinix-custom.el --- Emacs Xinix: Xinix's customizable variables.
;;
;; Copyright © 2011-2014 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/xinix
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Refinements of the core editing experience in Emacs.

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

;; customize
(defgroup xinix nil
  "Emacs Xinix configuration."
  :prefix "xinix-"
  :group 'convenience)

(defcustom xinix-auto-save t
  "Non-nil values enable Xinix's auto save."
  :type 'boolean
  :group 'xinix)

(defcustom xinix-guru t
  "Non-nil values enable `guru-mode'."
  :type 'boolean
  :group 'xinix)

(defcustom xinix-whitespace nil
  "Non-nil values enable Xinix's whitespace visualization."
  :type 'boolean
  :group 'xinix)

(defcustom xinix-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `xinix-whitespace' is also enabled."
  :type 'boolean
  :group 'xinix)

(defcustom xinix-flyspell t
  "Non-nil values enable Xinix's flyspell support."
  :type 'boolean
  :group 'xinix)

(defcustom xinix-user-init-file (expand-file-name "personal/"
                                                    user-emacs-directory)
  "Path to your personal customization file.
Xinix recommends you only put personal customizations in the
personal folder.  This variable allows you to specify a specific
folder as the one that should be visited when running
`xinix-find-user-init-file'.  This can be easily set to the desired buffer
in lisp by putting `(setq xinix-user-init-file load-file-name)'
in the desired elisp file."
  :type 'string
  :group 'xinix)

(defcustom xinix-indent-sensitive-modes
  '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list
  :group 'xinix)

(defcustom xinix-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here."
  :type 'list
  :group 'xinix)

(defcustom xinix-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur."
  :type 'number
  :group 'xinix)

(defcustom xinix-theme 'zenburn
  "The default color theme, change this in your /personal/preload config."
  :type 'symbol
  :group 'xinix)

(provide 'xinix-custom)

;;; xinix-custom.el ends here
