;;; packages.el --- python-extras layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Brandon T. Willard <brandonwillard@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst python-extras-packages
  '(
    (python :location built-in)
    company
    evil-text-object-python
    editorconfig
    ;; conda
    f
    s
    persp-mode
    hy-mode
    (python-btw
     :location (recipe :fetcher github
                       :repo "brandonwillard/python-btw"))
    (pyvenv-extras
     :location (recipe :fetcher github
                       :repo "brandonwillard/pyvenv-extras"))
    (company-anaconda :excluded t)
    (anaconda-mode :excluded t)
    (pylookup :excluded t)
    (importmagic :excluded t)
    (live-py-mode :excluded t)
    (pyenv-mode :excluded t)
    (pippel :excluded t)
    (pipenv :excluded t)
    flycheck
    projectile))

(defun python-extras/init-f ()
  (use-package f :defer t))

(defun python-extras/init-s ()
  (use-package s :defer t))

(defun python-extras/init-evil-text-object-python ()
  (use-package evil-text-object-python
    :init (add-hook 'python-base-mode-hook #'evil-text-object-python-add-bindings)))

(defun python-extras/post-init-editorconfig ()

  ;; (spacemacs|add-toggle editorconfig-sets-pyvenv
  ;;   :documentation
  ;;   "Let editorconfig check for a `pyvenv-workon' value and set it, if found."
  ;;   :status (advice-member-p 'spacemacs//editorconfig-set-pyvenv 'editorconfig-set-pyvenv)
  ;;   :on (advice-add 'editorconfig-set-pyvenv :around 'spacemacs//editorconfig-set-pyvenv)
  ;;   :off (advice-remove 'editorconfig-set-pyvenv 'spacemacs//editorconfig-set-pyvenv))
  ;;
  ;; (spacemacs/toggle-editorconfig-sets-pyvenv-on)

  (add-hook 'editorconfig-custom-hooks #'spacemacs//editorconfig-set-pyvenv))

(defun python-extras/post-init-projectile ())

(defun python-extras/post-init-persp-mode ())

(defun python-extras/post-init-hy-mode ()
  ;; Use projectile-specific Hy REPLs.
  (when (configuration-layer/package-used-p 'projectile)
    (declare-function hy-shell-get-process-name "hy-mode.el")
    (advice-add #'hy-shell-get-process-name :around
                #'spacemacs//hy-shell-get-process-name)))

(defun python-extras/pre-init-python ()
  (spacemacs|use-package-add-hook python
    :post-config
    (progn
      (setq python-shell-completion-native-output-timeout 3.0)
      (setq python-pdbtrack-activate nil)

      ;; This calls `ipython --version', which is costly, so disable it.
      (advice-add #'spacemacs//python-setup-shell :override (lambda (&rest r) nil))

      (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
      (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")

      ;; `python-mode-local-vars-hook' is supposed to do this, but I think
      ;; `hack-local-variables-hook' isn't being called (or is locally
      ;; overwritten, causing the global spacemacs
      ;; `spacemacs//run-local-vars-mode-hook' to be skipped).
      (spacemacs//python-setup-company))))

(defun python-extras/post-init-company ()

  (defun python--private-lessp (x y)
    (cond
     ((and (string-prefix-p "_" x)
           (not (string-prefix-p "_" y))) nil)
     ((and (string-prefix-p "_" y)
           (not (string-prefix-p "_" x))) t)
     (t (string-lessp x y))))

  (defun python-extras/company-transform-python (candidates)
    "De-prioritize internal/private Python variables (e.g. '_blah') in completion list ordering.

See `company-transformers'."
    ;; TODO: Find a replacement for `seq-sort-by'.
    ;; (seq-sort-by #'company-strip-prefix #'python--private-lessp
    ;;              candidates)
    (seq-sort-by #'identity #'python--private-lessp candidates))

  (defun python-extras/python-company-conf ()
    (add-to-list 'company-transformers #'python-extras/company-transform-python
                 t))

  (spacemacs/add-to-hooks 'python-extras/python-company-conf
                          '(python-base-mode-hook inferior-python-mode-hook))

  ;; Disable company idle/automatic completion.
  (advice-add #'spacemacs//init-company-vars-inferior-python-mode
              :after #'(lambda (&rest _)
                         (setq-local company-idle-delay nil))))

(defun python-extras/init-pyvenv-extras ()
  (use-package pyvenv-extras
    :config
    (progn
      (pyvenv-projectile-tracking-mode +1)

      (defun spacemacs//persp-after-switch-set-venv (orig-func frame-or-window)
        (when (eq python-auto-set-local-pyvenv-virtualenv 'on-project-switch)
          (funcall orig-func frame-or-window)))

      (advice-add #'pyvenv-extras//persp-after-switch-set-venv :around
                  #'spacemacs//persp-after-switch-set-venv)

      (pyvenv-persp-tracking-mode +1)

      (advice-add #'spacemacs/projectile-shell-pop :around #'pyvenv-extras//run-in-pyvenv-wrapper)

      (pyvenv-extras-mode +1))))

(defun python-extras/init-python-btw ()
  (use-package python-btw
    :config
    (progn
      (python-btw-mode +1))))

;;; packages.el ends here
