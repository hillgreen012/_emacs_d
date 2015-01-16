;;; xinix-mode.el --- Emacs Xinix: minor mode
;;
;; Copyright © 2011-2014 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/xinix
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A minor mode defining a local keymap, plus a menu.

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
(require 'easymenu)

(defvar xinix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'xinix-open-with)
    (define-key map (kbd "C-c g") 'xinix-google)
    (define-key map (kbd "C-c G") 'xinix-github)
    (define-key map (kbd "C-c y") 'xinix-youtube)
    (define-key map (kbd "C-c U") 'xinix-duckduckgo)
    ;; mimic popular IDEs binding, note that it doesn't work in a terminal session
    (define-key map [(shift return)] 'xinix-smart-open-line)
    (define-key map (kbd "M-o") 'xinix-smart-open-line)
    (define-key map [(control shift return)] 'xinix-smart-open-line-above)
    (define-key map [(control shift up)]  'move-text-up)
    (define-key map [(control shift down)]  'move-text-down)
    (define-key map [(meta shift up)]  'move-text-up)
    (define-key map [(meta shift down)]  'move-text-down)
    (define-key map (kbd "C-c n") 'xinix-cleanup-buffer-or-region)
    (define-key map (kbd "C-c f")  'xinix-recentf-ido-find-file)
    (define-key map (kbd "C-M-z") 'xinix-indent-defun)
    (define-key map (kbd "C-c u") 'xinix-view-url)
    (define-key map (kbd "C-c e") 'xinix-eval-and-replace)
    (define-key map (kbd "C-c s") 'xinix-swap-windows)
    (define-key map (kbd "C-c D") 'xinix-delete-file-and-buffer)
    (define-key map (kbd "C-c d") 'xinix-duplicate-current-line-or-region)
    (define-key map (kbd "C-c M-d") 'xinix-duplicate-and-comment-current-line-or-region)
    (define-key map (kbd "C-c r") 'xinix-rename-buffer-and-file)
    (define-key map (kbd "C-c t") 'xinix-visit-term-buffer)
    (define-key map (kbd "C-c k") 'xinix-kill-other-buffers)
    (define-key map (kbd "C-c TAB") 'xinix-indent-rigidly-and-copy-to-clipboard)
    (define-key map (kbd "C-c I") 'xinix-find-user-init-file)
    (define-key map (kbd "C-c S") 'xinix-find-shell-init-file)
    (define-key map (kbd "C-c i") 'xinix-goto-symbol)
    ;; extra prefix for projectile
    (define-key map (kbd "s-p") 'projectile-command-map)
    ;; make some use of the Super key
    (define-key map (kbd "s-g") 'god-local-mode)
    (define-key map (kbd "s-r") 'xinix-recentf-ido-find-file)
    (define-key map (kbd "s-j") 'xinix-top-join-line)
    (define-key map (kbd "s-k") 'xinix-kill-whole-line)
    (define-key map (kbd "s-m m") 'magit-status)
    (define-key map (kbd "s-m l") 'magit-log)
    (define-key map (kbd "s-m f") 'magit-file-log)
    (define-key map (kbd "s-m b") 'magit-blame-mode)
    (define-key map (kbd "s-o") 'xinix-smart-open-line-above)

    map)
  "Keymap for Xinix mode.")

(defun xinix-mode-add-menu ()
  "Add a menu entry for `xinix-mode' under Tools."
  (easy-menu-add-item nil '("Tools")
                      '("Xinix"
                        ("Files"
                         ["Open with..." xinix-open-with]
                         ["Delete file and buffer" xinix-delete-file-and-buffer]
                         ["Rename buffer and file" xinix-rename-buffer-and-file]
                         ["Copy file name to clipboard" xinix-copy-file-name-to-clipboard])

                        ("Buffers"
                         ["Clean up buffer or region" xinix-cleanup-buffer-or-region]
                         ["Kill other buffers" xinix-kill-other-buffers])

                        ("Editing"
                         ["Insert empty line" xinix-insert-empty-line]
                         ["Move line up" xinix-move-line-up]
                         ["Move line down" xinix-move-line-down]
                         ["Duplicate line or region" xinix-duplicate-current-line-or-region]
                         ["Indent rigidly and copy to clipboard" xinix-indent-rigidly-and-copy-to-clipboard]
                         ["Insert date" xinix-insert-date]
                         ["Eval and replace" xinix-eval-and-replace]
                         )

                        ("Windows"
                         ["Swap windows" xinix-swap-windows])

                        ("General"
                         ["Visit term buffer" xinix-visit-term-buffer]
                         ["Search in Google" xinix-google]
                         ["View URL" xinix-view-url]))
                      "Search Files (Grep)...")

  (easy-menu-add-item nil '("Tools") '("--") "Search Files (Grep)..."))

(defun xinix-mode-remove-menu ()
  "Remove `xinix-mode' menu entry."
  (easy-menu-remove-item nil '("Tools") "Xinix")
  (easy-menu-remove-item nil '("Tools") "--"))

;; define minor mode
(define-minor-mode xinix-mode
  "Minor mode to consolidate Emacs Xinix extensions.

\\{xinix-mode-map}"
  :lighter " Pre"
  :keymap xinix-mode-map
  (if xinix-mode
      ;; on start
      (xinix-mode-add-menu)
    ;; on stop
    (xinix-mode-remove-menu)))

(define-globalized-minor-mode xinix-global-mode xinix-mode xinix-on)

(defun xinix-on ()
  "Turn on `xinix-mode'."
  (xinix-mode +1))

(defun xinix-off ()
  "Turn off `xinix-mode'."
  (xinix-mode -1))

(provide 'xinix-mode)
;;; xinix-mode.el ends here
