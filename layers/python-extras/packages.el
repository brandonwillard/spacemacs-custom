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

      ;; This calls `ipython --version', which is costly, so disable it.
      (advice-add #'spacemacs//python-setup-shell :override (lambda (&rest r) nil))

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
    ;; TODO: Find a replacement for `seq-sort-by'.
    ;; (seq-sort-by #'company-strip-prefix #'python--private-lessp
    ;;              candidates)
    (seq-sort-by #'identity #'python--private-lessp candidates))

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
  (defun spacemacs//run-in-pyvenv-wrapper (oldfun &rest args)
    (spacemacs//run-in-pyvenv
     (apply oldfun args)))

  (advice-add #'spacemacs/python-start-or-switch-repl :around #'spacemacs//run-in-pyvenv-wrapper)
  (advice-add #'spacemacs/projectile-shell-pop :around #'spacemacs//run-in-pyvenv-wrapper)

  ;; TODO: `pyvenv-restart-python' checks `pyvenv-virtual-env-name' and
  ;; `pyvenv-virtual-env' *within* each inferior Python buffer, so we need to [re]set
  ;; those values there (e.g. using the caller's venv values).
  (defun btw//pyvenv-restart-python (&rest _)
    "Restart Python inferior processes (with venv awareness and not cursor jumps)."
    (interactive)
    (save-window-excursion
      (dolist (buf (persp-buffer-list))
        (set-buffer buf)
        (spacemacs//run-in-pyvenv
         (when (and (eq major-mode 'inferior-python-mode)
                    (get-buffer-process buf))
           (let ((cmd (combine-and-quote-strings (process-command
                                                  (get-buffer-process buf))))
                 (dedicated (if (string-match "\\[.*\\]$" (buffer-name buf))
                                t
                              nil))
                 (show nil))
             (delete-process (get-buffer-process buf))
             (insert "\n\n"
                     "###\n"
                     (format "### Restarting in virtualenv %s (%s)\n"
                             pyvenv-virtual-env-name
                             pyvenv-workon
                             ;; pyvenv-virtual-env
                             )
                     "###\n"
                     "\n\n")
             (run-python cmd dedicated show)))))))

  (advice-add #'pyvenv-restart-python :override #'btw//pyvenv-restart-python)

  (defun spacemacs//set-project-root (func &rest args)
    "Run the wrapped function in the project root directory."
    (let ((default-directory (expand-file-name (or (projectile-project-root) default-directory))))
      (apply func args)))

  (advice-add #'python-shell-make-comint :around #'spacemacs//set-project-root)

  (add-hook 'pyvenv-post-activate-hooks #'spacemacs//pyvenv-conda-activate-additions)
  (add-hook 'pyvenv-post-deactivate-hooks #'spacemacs//pyvenv-conda-deactivate-additions)
  (with-eval-after-load 'vterm
    (defun spacemacs//vterm-init-pyvenv ()
      (spacemacs//pyvenv-conda-env-term-init #'vterm-send-string))
    (add-hook 'vterm-mode-hook #'spacemacs//vterm-init-pyvenv))
  (defun spacemacs//term-init-pyvenv ()
    (spacemacs//pyvenv-conda-env-term-init
     #'(lambda (command-string) (term-send-string (current-buffer) command-string))))
  (add-hook 'term-exec-hook #'spacemacs//term-init-pyvenv))

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
