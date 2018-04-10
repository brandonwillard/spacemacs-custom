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
  (defun spacemacs//python-shell-get-process-name (orig-func dedicated)
    "Append project name to shell process name.
Makes shells specific to the active project."
    ;; FYI: Could just use a python-mode hook that sets `python-shell-buffer-name'.
    (let ((proc-name (funcall orig-func dedicated))
          (proj-name (with-demoted-errors "Error: %S" (projectile-project-name))))
      (if (and proj-name
               (not (s-suffix? (format "(%s)" proj-name) proc-name)))
          (format "%s(%s)" proc-name proj-name)
        proc-name))))

(when (configuration-layer/package-used-p 'pyvenv)
  (defun spacemacs//pyvenv-conda-activate-additions ()
    (when (and (boundp 'pyvenv-virtual-env)
               (boundp 'pyvenv-virtual-env-name))
      (setenv "CONDA_PREFIX"
              (string-remove-suffix "/" pyvenv-virtual-env))
      (setenv "CONDA_DEFAULT_ENV" pyvenv-virtual-env-name)))

  (defun spacemacs//pyvenv-conda-deactivate-additions ()
    (setenv "CONDA_PREFIX" nil)
    (setenv "CONDA_DEFAULT_ENV" nil))

  (defun spacemacs//pyvenv-conda-env-shell-init (&rest process)
    "Activate the current env in a newly opened shell PROCESS.

From https://github.com/necaris/conda.el/blob/master/conda.el#L339"
    (when (boundp 'pyvenv-virtual-env-name)
      (let* ((activate-command (if (eq system-type 'windows-nt)
                                   '("activate")
                                        ;'("source" "activate")
                                 '("conda" "activate")
                                 ))
             (full-command (append activate-command `(,pyvenv-virtual-env-name "\n")))
             (command-string (combine-and-quote-strings full-command))
             (buffer-or-process (if (not process)
                                    (current-buffer)
                                  process)))
        (progn (message "sending %s to %S" command-string buffer-or-process)
               (term-send-string buffer-or-process command-string))))))

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
    (spacemacs//python-shell-send-string-echo substring process)))

(defun spacemacs//python-help--display-for-string (proc string)
  "Originally from `python-x.el'"
  (let ((buffer (get-buffer-create "*help[Python]*"))
        (output (python-shell-send-string-no-output
                  (format python-help-setup-code string) proc)))
    (unless (s-blank? output)
      (with-current-buffer buffer
        ;; (special-mode)
        (help-mode)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert output)
          (ansi-color-apply-on-region (point-min) (point-max))
          (whitespace-cleanup)
          (goto-char (point-min)))
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
