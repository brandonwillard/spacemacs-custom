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
    :init (add-hook 'python-mode-hook 'evil-text-object-python-add-bindings)))

(defun python-extras/post-init-editorconfig ()
  (add-hook 'editorconfig-custom-hooks #'spacemacs//editorconfig-set-pyvenv))

(defun python-extras/post-init-projectile ())

(defun python-extras/post-init-persp-mode ()
  (when (configuration-layer/package-used-p 'pyvenv)
    (add-to-list 'persp-activated-functions 'spacemacs//persp-after-switch-set-venv)))

(defun python-extras/pre-init-python ()
  (spacemacs|use-package-add-hook python
    :post-config
    (progn
      (setq python-shell-completion-native-output-timeout 3.0)
      (setq python-pdbtrack-activate nil)
      (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
      (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")

      (advice-add 'python-shell-get-process-name :around
                  #'spacemacs//python-shell-get-process-name)

      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "hh" 'spacemacs//python-help-for-region-or-symbol
        "sr" 'spacemacs//python-shell-send-region-echo
        "sl" 'spacemacs//python-shell-send-line-echo))))

(defun python-extras/post-init-company ()

  (defun python-extras/company-transform-python (candidates)
    "De-prioritize internal variables (i.e. '_blah') in completion list ordering.

See `company-transformers'.

From URL `https://emacs.stackexchange.com/a/12403'"
    (let ((deleted))
      (mapcar #'(lambda (c)
                  (if (or (string-prefix-p "_" c) (string-prefix-p "._" c))
                      (progn
                        (add-to-list 'deleted c)
                        (setq candidates (delete c candidates)))))
              candidates)
      (append candidates (nreverse deleted))))

  (defun python-extras/python-company-conf()
    (setq-local company-transformers
                (append company-transformers '(python-extras/company-transform-python))))

  (add-hook 'python-mode-hook 'python-extras/python-company-conf t)

  ;; Disable company idle/automatic completion.
  (advice-add 'spacemacs//init-company-vars-inferior-python-mode
              :after
              #'(lambda (&rest _)
                  (setq-local company-idle-delay nil))))

(defun python-extras/post-init-pyvenv ()

  (defun python-extras//filter-venvwrapper-supported-anaconda-hooks (pyvenv-res &rest r)
    "If we're using Anaconda envs, do not run virtualenvwrapper hooks."
    (and pyvenv-res
         (not (s-contains? (concat (f-path-separator) "anaconda")
                           pyvenv-res
                           t))))

  (advice-add 'pyvenv-virtualenvwrapper-supported
              :filter-return 'python-extras//filter-venvwrapper-supported-anaconda-hooks)

  ;; If `pyvenv-workon' buffer-local variables is set, activate the corresponding
  ;; venv when entering the buffer.
  ;; (pyvenv-tracking-mode +1)

  ;; (defun python-extras//track-previous-pyvenv (&res _)
  ;;   ...)
  ;; (advice-add 'pyvenv-activate :before 'python-extras//track-previous-pyvenv)

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
