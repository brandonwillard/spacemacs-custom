;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("texw_" "#+TITLE: ${1:title}\n#+AUTHOR: Brandon T. Willard\n#+DATE: `(format-time-string \"%Y-%m-%d\")`\n#+EMAIL: brandonwillard@gmail.com\n\n#+STARTUP: hideblocks indent hidestars\n#+OPTIONS: ^:nil toc:nil d:(not \"logbook\" \"todo\" \"notes\") tex:t |:t broken-links:t\n#+SELECT_TAGS: export\n#+EXCLUDE_TAGS: noexport\n\n#+PROPERTY: header-args :session $2 :exports both :eval never-export :results output drawer replace\n#+PROPERTY: header-args:text :eval never\n\n#+HTML_HEAD: <link rel=\"stylesheet\" type=\"text/css\" href=\"../extra/custom.css\" />\n#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" href=\"../extra/custom.css\" />\n\n#+INCLUDE: latex-setup.org\n#+INCLUDE: org-setup.org\n\n#+NAME: python-org-imports\n#+BEGIN_SRC python :exports none :results silent :noweb-ref python-imports\nfrom tabulate import tabulate\n#+END_SRC\n\n#+BEGIN_abstract\n  $4\n#+END_abstract\n\n* Introduction\n\n  $0\n\n* Conclusion\n\n\n#+BIBLIOGRAPHY: $3\n#+BIBLIOGRAPHYSTYLE: plainnat\n" "texw" nil nil
                        ((yas-indent-line 'nil)
                         (yas-wrap-around-region 'nil)
                         (org-adapt-indentation 'nil))
                        "/home/bwillard/.emacs.d/private/snippets/org-mode/texw" nil nil)
                       ("org_todo_" "#+TITLE: ${1:title} TODOs\n#+DATE: `(format-time-string \"%Y-%m-%d\")`\n#+AUTHOR: Brandon T. Willard\n#+EMAIL: brandonwillard@gmail.com\n#+LANGUAGE: en\n#+SELECT_TAGS: export\n#+EXCLUDE_TAGS: noexport\n\n#+STARTUP: hideblocks indent hidestars\n\n#+OPTIONS: ':nil *:t -:t ::t <:t H:3 \\n:nil ^:t arch:headline author:t\n#+OPTIONS: broken-links:nil c:nil creator:nil d:results date:t e:t\n#+OPTIONS: email:nil f:t inline:t num:t p:nil pri:nil prop:nil stat:t tags:t\n#+OPTIONS: tasks:t tex:t timestamp:t title:t toc:t todo:t |:t\n\n#+PROPERTY: header-args :session :eval never-export :results output drawer replace\n\n* TODO ${2:todo-item}\n:PROPERTIES:\n:header-args: :session ${3:todo-session} :eval never-export :results output drawer replace :dir ${4:todo-dir}\n:END:\n\n#+NAME: demo-src-block\n#+BEGIN_SRC python :tangle demo_src_file.py :eval never\n# This source is written to disk when tangled with \\`(org-babel-tangle)\\`\nprint('Example file')\n#+END_SRC\n\n$0" "org-todo" nil nil
                        ((yas-indent-line 'nil)
                         (yas-wrap-around-region 'nil)
                         (org-adapt-indentation 'nil))
                        "/home/bwillard/.emacs.d/private/snippets/org-mode/org-todo" nil nil)
                       ("src_" "src_${1:python}[:eval ${3:never}]\\{$0\\}" "org-src-inline" nil nil nil "/home/bwillard/.emacs.d/private/snippets/org-mode/org-src-inline.yasnippet" nil nil)
                       ("#+B" "${2:#+NAME: $3}\n#+BEGIN_SRC $1 ${4::eval ${5:never}}\n`yas-selected-text`$0\n#+END_SRC" "org-src-block"
                        (or
                         (=
                          (current-column)
                          5)
                         (=
                          (current-column)
                          0))
                        nil
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/home/bwillard/.emacs.d/private/snippets/org-mode/org-src-block.yasnippet" nil nil)
                       ("ipy_" "#+BEGIN_SRC ipython :session :exports both :results raw drawer :async t\n$0\n#+END_SRC\n" "ipython" nil nil nil "/home/bwillard/.emacs.d/private/snippets/org-mode/ipython" nil nil)
                       ("date" "`(format-time-string \"%Y-%m-%d\")`$0" "date" nil nil nil "/home/bwillard/.emacs.d/private/snippets/org-mode/insert-date" nil nil)))


;;; Do not edit! File generated at Sat Apr 10 16:43:35 2021
