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

(defvar org-latex-tcolorbox-default-options
  "arc=0pt, outer arc=0pt, boxrule=0pt, coltitle=black, colbacktitle=white"
  ;; , boxed title style={empty, size=minimal}, attach boxed title to bottom center={yshift=-10pt}
  "Default tcblistings options")

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
