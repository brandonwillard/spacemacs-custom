;;; config.el --- org-extras layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Brandon T. Willard <brandonwillard@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Variables:

(defvar org-latex-pdf-output-dir
  nil
  "Output directory for PDF files")

(defvar org-inline-src-keyword
  '(spacemacs//org-element-inline-src-block-parser
    (1 '(face org-target invisible org-link) t)
    (2 '(face org-target invisible org-link) t)
    (3 'org-code t))
  "Font-lock keyword for inline org-babel src statements")

(spacemacs|add-toggle org-highlight-inline-src
  :documentation
  "Enable highlighting for inline src statements (i.e. 'src_<lang>[...]{...}')"
  :status (let ((org-mode-keywords (assq 'org-mode font-lock-keywords-alist)))
            (member (list (list org-inline-src-keyword)) org-mode-keywords))
  :on (font-lock-add-keywords 'org-mode
                              (list org-inline-src-keyword))
  :off (font-lock-remove-keywords 'org-mode
                                  (list org-inline-src-keyword)))

(defvar org-babel-inline-src-rx
  '(seq "src_"
        (one-or-more word)
        (zero-or-one (seq "["
                          (zero-or-more not-newline)
                          "]"))
        "{"
        (zero-or-more not-newline)
        "}")
  "Elisp rx form for matching inline org src statements.")

(spacemacs|add-toggle org-inline-src-in-links
  :documentation
  "Allow inline org src statements inside link descriptions."
  :status (advice-member-p 'spacemacs//org-make-link-regexps 'org-make-link-regexps)
  :on (advice-add 'org-make-link-regexps :after 'spacemacs//org-make-link-regexps)
  :off (advice-remove 'org-make-link-regexps 'spacemacs//org-make-link-regexps))

(defvar org-latex-listings-wrapper
  'tcolorbox
  "Wrapper for listings (e.g. tcolorbox)")

(defvar org-latex-tcolorbox-listing-env
  "\\newtcblisting[auto counter,number within=section]{oxtcblisting}[1]{%
\tframe hidden,
\tlisting only,
\tlisting engine=minted,
\tminted options={fontsize=\\scriptsize},
\tbreakable,
\tenhanced,
\ttitle after break={\\raggedleft\\lstlistingname\\ \\thetcbcounter~ -- continued},
\tlisting remove caption=false,
\tarc=0pt,
\touter arc=0pt,
\tboxrule=0pt,
\tcoltitle=black,
\tcolbacktitle=white,
\tcenter title,
\t#1
}"
  ;; , boxed title style={empty, size=minimal}, attach boxed title to bottom center={yshift=-10pt}
  "Default tcolorbox listings environment")

(defvar ob-python-execute-echo
  nil
  "Echo input in Python inferior process")

(defvar org-projectile-resources-dir
  "figures"
  "Name of the resources directory for a projectile project used to store
figures and whatnot.")

(defvar org-projectile-output-dir
  "output"
  "Name of the output directory for a projectile project used to store
compiled results (e.g. PDFs).")
