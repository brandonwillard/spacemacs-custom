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

(defvar spacemacs--python-help-setup-code
      "
try:
   get_ipython().inspector.pinfo(%1$s, detail_level=0)
except Exception:
   help(%1$s)
"
      "Code used to extract help information from a comint REPL.")

(defvar spacemacs--pyvenv-virtual-env-name-prev
  nil
  "Name of the previously active virtual env; nil otherwise")

(defvar spacemacs--pyvenv-last-scope
  nil
  "The last scope (e.g. buffer, project) considered by `pyvenv-track-virtualenv'.")

(spacemacs|add-toggle pyvenv-track-buffer-changes
  :documentation
  "Activate pyvenv tracking only on buffer changes."
  :status (advice-member-p 'spacemacs//pyvenv-track-buffer-virtualenv
                           #'pyvenv-track-virtualenv)
  :on (advice-add 'pyvenv-track-virtualenv :around
                  #'spacemacs//pyvenv-track-buffer-virtualenv)
  :off (advice-remove 'pyvenv-track-virtualenv
                      #'spacemacs//pyvenv-track-buffer-virtualenv))

(spacemacs|add-toggle pyvenv-track-projectile-changes
  :documentation
  "Activate pyvenv tracking only on projectile project changes."
  :status (advice-member-p 'spacemacs//pyvenv-track-projectile-virtualenv
                           #'pyvenv-track-virtualenv)
  :on (advice-add 'pyvenv-track-virtualenv :around
                  #'spacemacs//pyvenv-track-projectile-virtualenv)
  :off (advice-remove 'pyvenv-track-virtualenv
                      #'spacemacs//pyvenv-track-projectile-virtualenv))
