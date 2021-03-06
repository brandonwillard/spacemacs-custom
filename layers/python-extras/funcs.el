;;; funcs.el --- python-extras layer packages file for Spacemacs.
;;
;; Author: Brandon T. Willard
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(when (configuration-layer/package-used-p 'projectile)
  ;; See `hy-shell-buffer-name' and `hy-shell-internal-buffer-name'.
  ;; `hy-shell-get-process'
  (defun spacemacs//hy-shell-get-process-name (orig-func &optional internal)
    (pyvenv-extras//project-process-name (funcall orig-func internal))))

(when (configuration-layer/package-used-p 'pyvenv)
  (when (configuration-layer/package-used-p 'editorconfig)
    (defun spacemacs//editorconfig-set-pyvenv (props)
      "Set Anaconda virtual env variables `pyvenv-workon' and
      `python-shell-virtualenv-root' from entry in editorconfig file.

      The config file entry should be the env name, and `pyenv-workon-home' should be set."
      (spacemacs//run-in-pyvenv
       (let ((env-name (gethash 'conda_env_name props)))
         (when (and env-name
                    (and (not (local-variable-p 'python-shell-virtualenv-root))
                         (not (local-variable-p 'pyvenv-workon))))
           ;; We're setting this locally, but the variable is used globally, so we can
           ;; compare the two for a hackish means of determining buffer-specific envs.
           ;; See `spacemacs//pyvenv-mode-set-local-virtualenv'.
           (message "(editorconfig) setting pyvenv-workon to %s" env-name)
           (setq-local pyvenv-workon env-name)
           ;; Activate the venv, if not already or currently in a different one.
           (spacemacs//pyvenv-mode-set-local-virtualenv "editorconfig")
           (-when-let* ((workon-env (getenv "WORKON_HOME"))
                        (venv-root (f-join (getenv "WORKON_HOME") env-name)))
             ;; This is generally useful when using inferior shells.
             (message "(editorconfig) setting python-shell-virtualenv-root to %s" venv-root)
             (setq-local python-shell-virtualenv-root venv-root))))))))

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
  "Send the selected region of text to the Python process.  A new line will be appended if one
isn't already present in the text region."
  (interactive
    (list (region-beginning) (region-end) current-prefix-arg t))
  (let* ((substring (buffer-substring-no-properties start end))
         (process (python-shell-get-process-or-error msg))
         (send-string (if (s-ends-with? "\n" substring)
                          substring
                        (s-append "\n" substring))))
    (spacemacs//python-shell-send-string-echo send-string process msg)))

(defun spacemacs//python-help--display-for-string (proc string)
  "Originally from `python-x.el'"
  (let ((buffer (help-buffer))
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
