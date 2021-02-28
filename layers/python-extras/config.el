;;; config.el --- python-extras layer packages file for Spacemacs.
;;
;; Author: Brandon T. Willard
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Variables:

(defvar spacemacs--python-help-setup-code
      "
try:
   get_ipython().inspector.pinfo(%1$s, detail_level=0)
except Exception:
   help(%1$s)
"
      "Code used to extract help information from a comint REPL.")
