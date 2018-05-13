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

(defvar org-latex-listings-wrapper
  'tcolorbox
  "Wrapper for listings (e.g. tcolorbox)")

(defvar org-latex-tcolorbox-listing-env
  "\\newtcblisting[auto counter,number within=section]{oxtcblisting}[1]{%
\tlisting only,
\tlisting engine=minted,
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
