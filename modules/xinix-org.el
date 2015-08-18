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

(xinix-require-package 'htmlize)
(require 'htmlize)
(require 'org-publish)
(require 'org-latex)

(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

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
  (let ((org-latex-to-pdf '("xelatex -interaction nonstopmode %f"
                            "xelatex -interaction nonstopmode %f"
                            ("xelatex %f")
                            )))
    (if (boundp 'org-latex-to-pdf-process)
        (setq org-latex-to-pdf-process
              org-latex-to-pdf)
      (setq org-latex-pdf-process
            org-latex-to-pdf)))
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
  (local-set-key (kbd "C-c s e") 'org-edit-src-code)
  ;; keybinding for inserting code blocks
  (local-set-key (kbd "C-c s i") 'org-insert-src-block)
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
        background-color: #000;
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
   org-export-latex-listings t
   org-publish-project-alist
   '(("org-static"
      :base-directory "~/.emacs.d/vendor/org/static/"
      :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
      :publishing-directory "./"
      :recursive t
      :publishing-function org-publish-attachment)
     ("org" :components ("org-static")))))

;; Auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(defun org-mode-article-modes ()
  (reftex-mode t)
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
       (reftex-parse-all)))
(add-hook 'org-mode-hook
          (lambda ()
            (if (member "REFTEX" org-todo-keywords-1)
                (org-mode-article-modes))))
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))

(add-to-list 'org-export-latex-classes
             '("cn-article"
               "\\documentclass[12pt,a4paper]{article}
\\usepackage{graphicx}
\\usepackage{xcolor}
\\usepackage{xeCJK}
\\usepackage{lmodern}
\\usepackage{verbatim}
\\usepackage{fixltx2e}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{tikz}
\\usepackage{wrapfig}
\\usepackage{soul}
\\usepackage{textcomp}
\\usepackage{listings}
\\usepackage{geometry}
\\usepackage{algorithm}
\\usepackage{algorithmic}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{latexsym}
\\usepackage{natbib}
\\usepackage{fancyhdr}
\\usepackage[xetex,colorlinks=true,CJKbookmarks=true,linkcolor=blue,urlcolor=blue,menucolor=blue]{hyperref}
\\usepackage{fontspec,xunicode,xltxtra}
\\setmainfont[BoldFont=Ubuntu Mono Bold]{Ubuntu Mono}  
\\setsansfont[BoldFont=Ubuntu Mono Bold]{Ubuntu Mono}  
\\setmonofont{Ubuntu Mono}  
\\newcommand\\fontnamemono{Ubuntu Mono}%等宽字体
\\newfontinstance\\MONO{\\fontnamemono}
\\newcommand{\\mono}[1]{{\\MONO #1}}
\\setCJKmainfont[Scale=0.9]{Ubuntu Mono}%中文字体
\\setCJKmonofont[Scale=0.9]{Ubuntu Mono}
\\hypersetup{unicode=true}
\\geometry{a4paper, textwidth=6.5in, textheight=10in, marginparsep=7pt, marginparwidth=.6in}
\\definecolor{foreground}{RGB}{220,220,204}%浅灰
\\definecolor{background}{RGB}{62,62,62}%浅黑
\\definecolor{preprocess}{RGB}{250,187,249}%浅紫
\\definecolor{var}{RGB}{239,224,174}%浅肉色
\\definecolor{string}{RGB}{154,150,230}%浅紫色
\\definecolor{type}{RGB}{225,225,116}%浅黄
\\definecolor{function}{RGB}{140,206,211}%浅天蓝
\\definecolor{keyword}{RGB}{239,224,174}%浅肉色
\\definecolor{comment}{RGB}{180,98,4}%深褐色
\\definecolor{doc}{RGB}{175,215,175}%浅铅绿
\\definecolor{comdil}{RGB}{111,128,111}%深灰
\\definecolor{constant}{RGB}{220,162,170}%粉红
\\definecolor{buildin}{RGB}{127,159,127}%深铅绿
\\punctstyle{kaiming}
\\title{}
\\fancyfoot[C]{\\bfseries\\thepage}
\\chead{\\MakeUppercase\\sectionmark}
\\pagestyle{fancy}
\\tolerance=1000
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; 使用Listings宏包格式化源代码(只是把代码框用listing环境框起来，还需要额外的设置)
(setq org-export-latex-listings t)
;; Options for \lset command（reference to listing Manual)
(setq org-export-latex-listings-options
      '(
        ("basicstyle" "\\color{foreground}\\small\\mono")           ; 源代码字体样式
        ("keywordstyle" "\\color{function}\\bfseries\\small\\mono") ; 关键词字体样式
        ("identifierstyle" "\\color{doc}\\small\\mono")
        ("commentstyle" "\\color{comment}\\small\\itshape")         ; 批注样式
        ("stringstyle" "\\color{string}\\small")                    ; 字符串样式
        ("showstringspaces" "false")                                ; 字符串空格显示
        ("numbers" "left")                                          ; 行号显示
        ("numberstyle" "\\color{preprocess}")                       ; 行号样式
        ("stepnumber" "1")                                          ; 行号递增
        ("backgroundcolor" "\\color{background}")                   ; 代码框背景色
        ("tabsize" "4")                                             ; TAB等效空格数
        ("captionpos" "t")                                          ; 标题位置 top or buttom(t|b)
        ("breaklines" "true")                                       ; 自动断行
        ("breakatwhitespace" "true")                                ; 只在空格分行
        ("showspaces" "false")                                      ; 显示空格
        ("columns" "flexible")                                      ; 列样式
        ("frame" "single")                                          ; 代码框：阴影盒
        ("frameround" "tttt")                                       ; 代码框： 圆角
        ("framesep" "0pt")
        ("framerule" "8pt")
        ("rulecolor" "\\color{background}")
        ("fillcolor" "\\color{white}")
        ("rulesepcolor" "\\color{comdil}")
        ("framexleftmargin" "10mm")
        ))
;; Make Org use ido-completing-read for most of its completing prompts.
(setq org-completion-use-ido t)
(add-to-list 'org-export-latex-classes
             ;; beamer class, for presentations
             '("beamer"
               "\\documentclass[11pt,professionalfonts]{beamer}
\\mode
\\usetheme{{{{Warsaw}}}}
%\\usecolortheme{{{{beamercolortheme}}}}

\\beamertemplateballitem
\\setbeameroption{show notes}
\\usepackage{graphicx}
\\usepackage{tikz}
\\usepackage{xcolor}
\\usepackage{xeCJK}
\\usepackage{amsmath}
\\usepackage{lmodern}
\\usepackage{fontspec,xunicode,xltxtra}
\\usepackage{polyglossia}
\\setmainfont{Times New Roman}
\\setCJKmainfont{DejaVu Sans YuanTi}
\\setCJKmonofont{DejaVu Sans YuanTi Mono}
\\usepackage{verbatim}
\\usepackage{listings}
\\institute{{{{beamerinstitute}}}}
\\subject{{{{beamersubject}}}}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}"
                "\\begin{frame}[fragile]\\frametitle{%s}"
                "\\end{frame}")))

(setq xinix-org-mode-hook
      'xinix-org-mode-defaults)

(add-hook 'org-mode-hook
          (lambda ()
            (run-hooks 'xinix-org-mode-hook)))

(provide 'xinix-org)

;;; xinix-org.el ends here
