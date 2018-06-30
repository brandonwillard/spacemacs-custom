;;; funcs.el --- python-extras layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Brandon T. Willard <brandonwillard@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(when (configuration-layer/package-used-p 'projectile)
  ;; See `hy-shell-buffer-name' and `hy-shell-internal-buffer-name'.
  ;; `hy-shell-get-process'
  (defun spacemacs//project-process-name (proc-name)
    (let ((proj-name (with-demoted-errors "Error: %S" (projectile-project-name))))
      (if (and proj-name
               (not (s-suffix? (format "(%s)" proj-name) proc-name)))
          (format "%s(%s)" proc-name proj-name)
        proc-name)))
  (defun spacemacs//hy--shell-format-process-name (orig-func proc-name)
    (funcall orig-func (spacemacs//project-process-name proc-name)))
  (defun spacemacs//python-shell-get-process-name (orig-func dedicated)
    (spacemacs//project-process-name (funcall orig-func dedicated))))

(when (configuration-layer/package-used-p 'pyvenv)
  (defun spacemacs//pyvenv-conda-activate-additions ()
    (setq spacemacs--pyvenv-virtual-env-name-prev pyvenv-virtual-env-name)
    (when (and (bound-and-true-p pyvenv-virtual-env)
               (bound-and-true-p pyvenv-virtual-env-name))
      (setenv "CONDA_PREFIX"
              (string-remove-suffix "/" pyvenv-virtual-env))
      (setenv "CONDA_DEFAULT_ENV" pyvenv-virtual-env-name)))

  (defun spacemacs//pyvenv-conda-deactivate-additions ()
    (setenv "CONDA_PREFIX" nil)
    (setenv "CONDA_DEFAULT_ENV" nil))

  (defun spacemacs//pyvenv-conda-env-shell-init (&rest process)
    "Activate the current env in a newly opened shell PROCESS.

Inspired by https://github.com/necaris/conda.el/blob/master/conda.el#L339"
    (when-let* ((pyvenv-env-name (or (bound-and-true-p pyvenv-virtual-env-name)
                                     (bound-and-true-p pyvenv-workon)))
                (activate-command (if (eq system-type 'windows-nt)
                                      '("activate")
                                        ;'("source" "activate")
                                    '("conda" "activate")))
                (full-command (append activate-command
                                      `(,pyvenv-env-name "\n")))
                (command-string (combine-and-quote-strings full-command))
                (buffer-or-process (if (not process)
                                       (current-buffer)
                                     process)))
      (progn
        (message "sending %s to %S" command-string
                 buffer-or-process)
        (term-send-string buffer-or-process command-string))))

  (defun* spacemacs//pyvenv-mode-set-local-virtualenv (&optional (caller-name ""))
    "If the buffer-local and global values differ, [re]activate the env."
    ;; TODO: When `pyvenv-virtual-env-name' is initially set here, and not with a
    ;; `setq' or `setq-default', all subsequent `setq' calls are applied to
    ;; buffer-local values (if any).  This is a problem for the comparison below,
    ;; since there will never be a non-nil `default-toplevel-value' and we'll
    ;; needlessly [re]activate the venv.
    ;; To work around this, we create a separate global variable that tracks
    ;; the--potentially--local one.
    ;; We could also use `(getenv "CONDA_DEFAULT_ENV")' (or another env var),
    ;; but that's too conda/implementation-specific.
    (when (and (boundp 'pyvenv-virtual-env-name)
               (local-variable-p 'pyvenv-virtual-env-name)
               (not (string-equal pyvenv-virtual-env-name
                        (or (ignore-errors (default-toplevel-value 'pyvenv-virtual-env-name))
                            spacemacs--pyvenv-virtual-env-name-prev))))
      (message "(%s) setting local venv %s" caller-name pyvenv-virtual-env-name)
      (pyvenv-workon pyvenv-virtual-env-name)
      ;; TODO: Remove; it's just debugging.
      (message "(%s) done setting local venv %s" caller-name pyvenv-virtual-env-name)))

  (when (configuration-layer/package-used-p 'persp-mode)
    (defun spacemacs//persp-after-switch-set-venv (frame-or-window)
      ;; `persp-activate' calls `persp-restore-window-conf', which
      ;; switches/restores the window config for the perspective.  If we don't
      ;; work within buffer in new window config, then we're simply using the
      ;; old perspective's buffer.
      (when (eq python-auto-set-local-pyvenv-virtualenv 'on-project-switch)
        (with-current-buffer (window-buffer)
          (spacemacs//pyvenv-mode-set-local-virtualenv "persp-switch")))))

  (when (configuration-layer/package-used-p 'editorconfig)
    (defun spacemacs//editorconfig-set-pyvenv (props)
      "Set Anaconda virtual env variables `pyvenv-workon' and
      `python-shell-virtualenv-root' from entry in editorconfig file.

      The config file entry should be the env name, and `pyenv-workon-home' should be set."
      (let ((env-name (gethash 'conda_env_name props)))
        (when (and env-name
                   (and (not (local-variable-p 'python-shell-virtualenv-root))
                        (not (local-variable-p 'pyvenv-virtual-env-name))))
          ;; We're setting this locally, but the variable is used globally, so we can
          ;; compare the two for a hackish means of determining buffer-specific envs.
          ;; See `spacemacs//pyvenv-mode-set-local-virtualenv'.
          (message "(editorconfig) setting pyvenv-virtual-env-name to %s" env-name)
          (setq-local pyvenv-virtual-env-name env-name)
          ;; Activate the venv, if not already or currently in a different one.
          (spacemacs//pyvenv-mode-set-local-virtualenv "editorconfig")
          (when-let* ((workon-env (getenv "WORKON_HOME"))
                      (venv-root (f-join (getenv "WORKON_HOME") env-name)))
            ;; This is generally useful when using inferior shells.
            (message "(editorconfig) setting python-shell-virtualenv-root to %s" venv-root)
            (setq-local python-shell-virtualenv-root venv-root)))))))

(defun spacemacs//python-shell-send-string (string &optional process msg)
  "Send STRING to inferior IPython PROCESS.  Uses %cpaste for multiline input.

When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive (list (read-string "Python command: ")
                     nil
                     t))
  (let* ((process (or process
                      (python-shell-get-process-or-error msg)))
         (process-executable (car (process-command process))))
    (if (string-match ".\n+." string) ;Multiline.
        (if (or (s-contains? process-executable "jupyter"
                             t)
                (s-contains? process-executable "ipython"
                             t))
            (comint-send-string process
                                (format "get_ipython().run_line_magic('cpaste', '-q')\n%s\n--\n" string))
          (let* ((temp-file-name (python-shell--save-temp-file string))
                 (file-name (or (buffer-file-name)
                                temp-file-name)))
            (python-shell-send-file file-name process
                                    temp-file-name t)))
      (comint-send-string process string)
      (when (or (not (string-match "\n\\'" string))
                (string-match "\n[ \t].*\n?\\'" string))
        (comint-send-string process "\n")))))

(defun spacemacs//python-shell-append-to-output (string)
  "Append STRING to `comint' display output."
  (let ((buffer (current-buffer))
        (py-buffer (process-buffer (python-shell-get-process))))
    (unless (eq buffer py-buffer)
      (save-mark-and-excursion
        (with-current-buffer py-buffer
          (let ((oldpoint (point)))
            (goto-char (process-mark (python-shell-get-process)))
            ;; (insert (propertize string 'read-only t))
            (insert string)
            (set-marker (process-mark (python-shell-get-process)) (point))
            (goto-char oldpoint))
          ))
      )))

(defun spacemacs//python-shell-send-string-echo (string &optional process msg)
  (spacemacs//python-shell-append-to-output string)
  (python-shell-send-string string process msg))

(defun spacemacs//add-missing-newline (line)
  (if (s-ends-with? "\n" line)
      line
    (concat line "\n")))

(defun spacemacs//python-shell-send-line-echo ()
  "Send and echo a literal line to the `comint' buffer.
Ignores beginning white-space."
  (interactive)
  (let (start end line)
    (save-excursion
      (end-of-line)
      ;; or `forward-line'?
      (setq end (point))
      (beginning-of-line-text)
      (setq start (point)))
    (setq line (buffer-substring-no-properties start end))
    (spacemacs//python-shell-send-string-echo (spacemacs//add-missing-newline line))))

(defun spacemacs//python-shell-send-syntax-line-echo ()
  "Send and echo a \"syntactical\" line to the `comint' buffer."
  (interactive)
  (let (start end line)
    (save-excursion
      (python-nav-end-of-statement)
      (setq end (point))
      (python-nav-beginning-of-statement)
      (setq start (point)))
    (setq line (buffer-substring-no-properties start end))
    (spacemacs//python-shell-send-string-echo line)))

(defun spacemacs//python-shell-send-region-echo (start end &optional send-main msg)
  (interactive
    (list (region-beginning) (region-end) current-prefix-arg t))
  (let* ((substring (buffer-substring-no-properties start end))
          ;; (string (python-shell-buffer-substring start end (not send-main)))
          (process (python-shell-get-process-or-error msg))
          (original-string substring))
    (spacemacs//python-shell-send-string-echo substring process msg)))

(defun spacemacs//python-help--display-for-string (proc string)
  "Originally from `python-x.el'"
  (let ((buffer (get-buffer-create "*help[Python]*"))
        (output (python-shell-send-string-no-output
                 (format spacemacs--python-help-setup-code string) proc)))
    (unless (s-blank? output)
      (with-current-buffer buffer
        ;; (special-mode)
        (help-mode)
        (buffer-disable-undo)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert output)
          (ansi-color-apply-on-region (point-min) (point-max))
          (whitespace-cleanup)
          (goto-char (point-min)))
        (set-buffer-modified-p nil)
        (setq truncate-lines nil)
        (setq word-wrap t)
        ;; (setq font-lock-defaults python-font-lock-keywords)
        (setq python-help--parent-proc proc))
      (display-buffer buffer))))

(defun spacemacs//python-help-for-region-or-symbol (string)
  "Display documentation for the current region or symbol at point. If a prefix
  argument is given, prompt for a statement to inspect.
  Originally from `python-x.el'"
  (interactive (let* ((string (if (use-region-p)
                                  (buffer-substring-no-properties (region-beginning)
                                                                  (region-end))
                                (python-info-current-symbol))))
                  (list (if current-prefix-arg
                            (read-string "Help for: " string t)
                          string))))
  (unless (s-blank? string)
    (spacemacs//python-help--display-for-string (python-shell-get-process)
                                      string)))
