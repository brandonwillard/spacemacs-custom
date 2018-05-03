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
