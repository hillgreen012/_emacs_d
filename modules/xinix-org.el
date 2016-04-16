;;; xinix-org.el --- Emacs Xinix: org-mode configuration.
;;
;; Copyright © 2011-2014 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/xinix
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for org-mode.

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

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (xinix-require-packages '(org org-plus-contrib))

(xinix-require-package 'htmlize)
(require 'htmlize)
;; (require 'org-install)
(require 'org-publish)
;; (require 'ox-publish)
(require 'org-latex)
;; (require 'ox-latex)

(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

(defun org-insert-worg-styles ()
  "Insert some `worg' styles."
  (interactive)
  (progn
    (insert "#+STYLE: <link rel='stylesheet' type='text/css' title='Standard' href='worg.css' />\n")
    (insert "#+STYLE: <link rel='stylesheet' type='text/css' title='Zenburn' href='worg-zenburn.css' />\n")
    (insert "#+STYLE: <link rel='stylesheet' type='text/css' title='Classic' href='worg-classic.css' />\n")))

(defun org-insert-src-block (language)
  "Insert a `LANGUAGE' type source code block in org-mode."
  (interactive
   (let ((language-set
          '("asymtote" "awk" "calc" "C" "clojure" "comint" "css" "ditaa"
            "dot" "emacs-lisp" "eval" "exp" "fortran" "gnuplot" "haskell"
            "io" "java" "js" "keys" "latex" "ledger" "lilypond" "lisp"
            "lob" "matlab" "maxima" "mscgen" "ocaml" "octave" "org" "perl"
            "picolisp" "plantuml" "python" "ref" "R" "ruby" "sass" "scala"
            "scheme" "screen" "sh" "shen" "sql" "sqlite" "table" "tangle")))
     (list (ido-completing-read "Language: " language-set))))
  (progn
    (insert (format "#+BEGIN_SRC %s\n" language))
    (newline)
    (insert "#+END_SRC")
    (previous-line 1)
    (org-edit-src-code)))

(defun org-insert-rich-src-block (name language switches headers)
  "Insert `LANGUAGE' source code block in org-mode with `NAME', `SWITCHES' and `HEADERS' properties."
  (interactive
   (let ((language-set
          '("asymtote" "awk" "calc" "C" "clojure" "comint" "css" "ditaa"
            "dot" "emacs-lisp" "eval" "exp" "fortran" "gnuplot" "haskell"
            "io" "java" "js" "keys" "latex" "ledger" "lilypond" "lisp"
            "lob" "matlab" "maxima" "mscgen" "ocaml" "octave" "org" "perl"
            "picolisp" "plantuml" "python" "ref" "R" "ruby" "sass" "scala"
            "scheme" "screen" "sh" "shen" "sql" "sqlite" "table" "tangle")))
     (list
      (read-string "Name: ")
      (ido-completing-read "Language: " language-set)
      (read-string "Switches{-n -r -l}: ")
      (read-string "Headers{:results :file :file-desc :dir :exports :tangle :mkdirp :comments :padline :no-expand :session :noweb :noweb-ref :noweb-sep :cache :sep :hlines :colnames :rownames :shebang :tangle-mode :eval :wrap :post :prologue :epilogue :var}: "))))
  (progn
    (unless (string-equal name "")
      (insert (format "#+NAME: %s\n" name)))
    (insert (format "#+BEGIN_SRC %s %s %s\n" language switches headers))
    (newline)
    (insert "#+END_SRC")
    (previous-line 1)
    (org-edit-src-code)))

(defun xinix-org-mode-defaults ()
  (let ((oldmap
         (cdr (assoc 'xinix-mode minor-mode-map-alist)))
        (newmap
         (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-c +") nil)
    (define-key newmap (kbd "C-c -") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(xinix-mode . ,newmap) minor-mode-overriding-map-alist))
  (turn-on-font-lock)
  (iimage-mode -1)
  ;; Make Org use ido-completing-read for most of its completing prompts.
  (ido-mode)
  ;; 
  (setq ps-paper-type 'a4
        ps-font-size 12.0
        ps-print-header nil
        ps-landscape-mode nil)
  ;;解决在编辑中文时不会自动折行的问题
  (setq truncate-lines nil)
  (set-face-attribute 'org-level-1 nil :height 2.0 :bold t)
  (set-face-attribute 'org-level-2 nil :height 1.9 :bold t)
  (set-face-attribute 'org-level-3 nil :height 1.8 :bold t)
  (set-face-attribute 'org-level-4 nil :height 1.7 :bold t)
  (set-face-attribute 'org-level-5 nil :height 1.6 :bold t)
  (set-face-attribute 'org-level-6 nil :height 1.5 :bold t)
  (set-face-attribute 'org-level-7 nil :height 1.4 :bold t)
  (set-face-attribute 'org-level-8 nil :height 1.3 :bold t)
  ;; (set-face-attribute 'org-level-9 nil :height 1.2 :bold t)
  ;; (set-face-attribute 'org-level-10 nil :height 1.1 :bold t)
  (define-key org-mode-map (kbd "×") (kbd "*"))
  (define-key org-mode-map (kbd "——") (kbd "-"))
  (setq org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")
          (sequence "TODO" "DONE")
          (sequence "TOFUCK" "FUCKING" "FUCKED")))
  (setq org-plantuml-jar-path
        (expand-file-name "~/.emacs.d/vendor/org/plantuml.jar"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((asymptote . t)
     (awk . t)
     (calc . t)
     (C . t)
     (clojure . t)
     (comint . t)
     (css . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (eval . t)
     (exp . t)
     (fortran . t)
     (gnuplot . t)
     (haskell . t)
     (io . t)
     (java . t)
     (js . t)
     (keys . t)
     (latex . t)
     (ledger . t)
     (lilypond . t)
     (lisp . t)
     (lob . t)
     (matlab . t)
     (maxima . t)
     (mscgen . t)
     (ocaml . t)
     (octave . t)
     (org . t)
     (perl . t)
     (picolisp . t)
     (plantuml . t)
     (python . t)
     (ref . t)
     (R . t)
     (ruby . t)
     (sass . t)
     (scala . t)
     (scheme . t)
     (screen . t)
     (sh . t)
     (shen . t)
     (sql . t)
     (sqlite . t)
     (table . t)
     (tangle . t)))
  ;; turn on flyspell-mode by default
  ;; (setq-default flyspell-mode t)
  ;; C-TAB for expanding
  (local-set-key (kbd "C-<tab>") 'yas/expand-from-trigger-key)
  ;; keybinding for editing source code blocks
  (local-set-key (kbd "C-c x e") 'org-edit-src-code)
  ;; keybinding for inserting code blocks
  (local-set-key (kbd "C-c x i") 'org-insert-src-block)
  (local-set-key (kbd "C-c x w") 'org-insert-worg-styles)
  (setq-default
   org-startup-indented t
   org-src-fontify-natively t
   org-export-with-toc t
   org-export-with-section-numbers 8
   org-use-sub-superscripts nil
   org-confirm-babel-evaluate nil
   org-log-done 'note
   org-inline-image-overlays nil
   ;; Make Org use ido-completing-read for most of its completing prompts.
   org-outline-path-complete-in-steps nil
   org-completion-use-ido t
   org-export-html-style-include-default t
   org-export-html-style
   "
<style type=\"text/css\">
<!--/*--><![CDATA[/*><!--*/
html {
        font-family: \"Courier Pitch 10\", Courier, monospace;
        font-size: 100%;
        font: 10.5pt Consola, \"Bitstream Vera Sans\", Courier New, helvetica;
        /* background: rgba(200,200,200,0.50); */
}
.src {
        background-color: #404;
        /* background-color: white; */
        line-height: 1.2;
        font-family: \"Courier 10 Pitch\";
        font-size: 90%;
        color: wheat;
        -webkit-box-shadow: 0px 0px 2px rgba(0,0,0,0.23);
        /* -webkit-box-shadow: #333 3px 3px 4px; */
        -moz-box-shadow: 0px 0px 2px rgba(0,0,0,0.23);
        -o-box-shadow: #033 3px 2px 4px;
        box-shadow: #333 3px 2px 4px;
        -moz-box-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
}
/*]]>*/-->
</style>
"
   org-publish-project-alist
   '(("org-static"
      :base-directory "~/.emacs.d/vendor/org/static/"
      :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
      :publishing-directory "./"
      :recursive t
      :publishing-function org-publish-attachment)
     ("org" :components ("org-static")))))

(defun org-mode-article-modes ()
  (reftex-mode t)
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all)))
(add-hook 'org-mode-hook
          (lambda ()
            (if (member "REFTEX" org-todo-keywords-1)
                (org-mode-article-modes))))

(setq xinix-org-mode-hook
      'xinix-org-mode-defaults)

(add-hook 'org-mode-hook
          (lambda ()
            (run-hooks 'xinix-org-mode-hook)))

(provide 'xinix-org)

;;; xinix-org.el ends here
