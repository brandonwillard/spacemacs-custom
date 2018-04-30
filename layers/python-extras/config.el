;;; config.el --- python-extras layer packages file for Spacemacs.
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

;; TODO: Consider adding a toggle for auto-pyvenv functionality
;; Use `spacemacs|add-toggle'.

(defvar spacemacs--python-help-setup-code
      "
try:
   get_ipython().inspector.pinfo(%1$s, detail_level=1)
except Exception:
   help(%1$s)
"
      "Code used to extract help information from a comint REPL.")

(defvar spacemacs--pyvenv-virtual-env-name-prev nil
  "Name of the previously active virtual env; nil otherwise")
