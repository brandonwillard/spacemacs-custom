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
    pyvenv
    persp-mode
    hy-mode
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
    :init (add-hook 'python-mode-hook #'evil-text-object-python-add-bindings)))

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

(defun python-extras/post-init-persp-mode ()
  (when (configuration-layer/package-used-p 'pyvenv)
    (add-to-list 'persp-activated-functions #'spacemacs//persp-after-switch-set-venv)))

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

      (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
      (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")

      ;; `python-mode-local-vars-hook' is supposed to do this, but I think
      ;; `hack-local-variables-hook' isn't being called (or is locally
      ;; overwritten, causing the global spacemacs
      ;; `spacemacs//run-local-vars-mode-hook' to be skipped).
      (spacemacs//python-setup-company)

      (advice-add #'python-shell-send-string :override
                  #'spacemacs//python-shell-send-string)

      (when (configuration-layer/package-used-p 'projectile)
        (advice-add #'python-shell-get-process-name :around
                    #'spacemacs//python-shell-get-process-name))

      (when (configuration-layer/package-used-p 'lsp-python)
        (defun spacemacs//python-help-for-region-or-symbol (&rest r)
          (interactive)
          (lsp-ui-doc--make-request)))

      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "hh" #'spacemacs//python-help-for-region-or-symbol
        "sr" #'spacemacs//python-shell-send-region-echo
        "sl" #'spacemacs//python-shell-send-line-echo))))

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
    (seq-sort-by #'company-strip-prefix #'python--private-lessp
                 candidates))

  (defun python-extras/python-company-conf ()
    (add-to-list 'company-transformers #'python-extras/company-transform-python
                 t))

  (spacemacs/add-to-hooks 'python-extras/python-company-conf
                          '(python-mode-hook inferior-python-mode-hook))

  ;; Disable company idle/automatic completion.
  (advice-add #'spacemacs//init-company-vars-inferior-python-mode
              :after #'(lambda (&rest _)
                         (setq-local company-idle-delay nil))))

(defun python-extras/post-init-pyvenv ()

  (defun python-extras//filter-venvwrapper-supported-anaconda-hooks (pyvenv-res &rest r)
    "If we're using Anaconda envs, do not run virtualenvwrapper hooks."
    (and pyvenv-res
         (not (s-contains? (concat (f-path-separator) "anaconda")
                           pyvenv-res
                           t))))

  (advice-add #'pyvenv-virtualenvwrapper-supported
              :filter-return #'python-extras//filter-venvwrapper-supported-anaconda-hooks)

  (spacemacs/toggle-pyvenv-track-buffer-changes-off)
  (spacemacs/toggle-pyvenv-track-projectile-changes-on)

  ;; If `pyvenv-workon' buffer-local variables is set, activate the corresponding
  ;; venv when entering the buffer.
  ;; (pyvenv-tracking-mode +1)

  ;; (defun python-extras//track-previous-pyvenv (&res _)
  ;;   ...)
  ;; (advice-add #'pyvenv-activate :before #'python-extras//track-previous-pyvenv)

  ;; Enable automatic projectile-based venv activation before the following activities.
  (advice-add 'spacemacs/python-start-or-switch-repl :before #'spacemacs//check-and-activate-projectile-pyvenv)
  (advice-add 'spacemacs/projectile-shell-pop :before #'spacemacs//check-and-activate-projectile-pyvenv)

  (add-hook 'pyvenv-post-activate-hooks #'spacemacs//pyvenv-conda-activate-additions)
  (add-hook 'pyvenv-post-deactivate-hooks #'spacemacs//pyvenv-conda-deactivate-additions)
  (add-hook 'term-exec-hook #'spacemacs//pyvenv-conda-env-shell-init))

(defun python-extras/post-init-flycheck ()
  ;; (flycheck-add-next-checker 'python-flake8 'python-pylint)

  (defun python-extras/flycheck-virtualenv-executable-find (executable &rest find-any)
    "Find an EXECUTABLE in the current virtualenv (exclusively) if any."
    (if (bound-and-true-p python-mode)
        (if (bound-and-true-p python-shell-virtualenv-root)
            (let ((exec-path (nth 0 (python-shell-calculate-exec-path))))
              (executable-find executable))
          (when find-any
            (executable-find executable)))
      (executable-find executable)))

  (setq flycheck-executable-find
        #'python-extras/flycheck-virtualenv-executable-find)

  (add-hook 'python-mode-hook
            #'(lambda () (add-to-list 'flycheck-disabled-checkers 'python-pylint))))

;;; packages.el ends here
