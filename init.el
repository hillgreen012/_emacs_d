;;; init.el --- Xinix's configuration entry point.
;;
;; Copyright (c) 2011 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: http://batsov.com/xinix
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Xinix.

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
(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "Xinix is powering up... Be patient, Master %s!" current-user)

(when (version< emacs-version "24.1")
  (error "Xinix requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

(defvar xinix-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Xinix distribution.")

(defvar xinix-core-dir (expand-file-name "core" xinix-dir)
  "The home of Xinix's core functionality.")

(defvar xinix-modules-dir (expand-file-name  "modules" xinix-dir)
  "This directory houses all of the built-in Xinix modules.")

(defvar xinix-personal-dir (expand-file-name "personal" xinix-dir)
  "This directory is for your personal configuration.

Users of Emacs Xinix are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Xinix.")

(defvar xinix-personal-preload-dir (expand-file-name "preload" xinix-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Xinix.")

(defvar xinix-vendor-dir (expand-file-name "vendor" xinix-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")

(defvar xinix-savefile-dir (expand-file-name "savefile" xinix-dir)
  "This folder stores all the automatically generated save/history-files.")

(defvar xinix-modules-file (expand-file-name "xinix-modules.el" xinix-dir)
  "This files contains a list of modules that will be loaded by Xinix.")

(unless (file-exists-p xinix-savefile-dir)
  (make-directory xinix-savefile-dir))

(defun xinix-add-subfolders-to-load-path (parent-dir)
 "Add all level PARENT-DIR subdirs to the `load-path'."
 (dolist (f (directory-files parent-dir))
   (let ((name (expand-file-name f parent-dir)))
     (when (and (file-directory-p name)
                (not (string-prefix-p "." f)))
       (add-to-list 'load-path name)
       (xinix-add-subfolders-to-load-path name)))))

;; add Xinix's directories to Emacs's `load-path'
(add-to-list 'load-path xinix-core-dir)
(add-to-list 'load-path xinix-modules-dir)
(add-to-list 'load-path xinix-vendor-dir)
(xinix-add-subfolders-to-load-path xinix-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `xinix-personal-preload-dir'
(when (file-exists-p xinix-personal-preload-dir)
  (message "Loading personal configuration files in %s..." xinix-personal-preload-dir)
  (mapc 'load (directory-files xinix-personal-preload-dir 't "^[^#].*el$")))

(message "Loading Xinix's core...")

;; the core stuff
(require 'xinix-packages)
(require 'xinix-custom)  ;; Needs to be loaded before core, editor and ui
(require 'xinix-ui)
(require 'xinix-core)
(require 'xinix-mode)
(require 'xinix-editor)
(require 'xinix-global-keybindings)

;; OSX specific settings
(when (eq system-type 'darwin)
  (require 'xinix-osx))

(message "Loading Xinix's modules...")

;; the modules
(when (file-exists-p xinix-modules-file)
  (load xinix-modules-file))

;; config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" xinix-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p xinix-personal-dir)
  (message "Loading personal configuration files in %s..." xinix-personal-dir)
  (mapc 'load (directory-files xinix-personal-dir 't "^[^#].*el$")))

(message "Xinix is ready to do thy bidding, Master %s!" current-user)

(xinix-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'xinix-tip-of-the-day))

;;; init.el ends here
