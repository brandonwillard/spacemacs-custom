;;; funcs.el --- org-extras layer packages file for Spacemacs.
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


(when (configuration-layer/package-used-p 'projectile)
  (defun spacemacs//ob-ipython-project-dirs-setup ()
    (let* ((projectile-require-project-root nil)
           (proj-root (projectile-project-root))
           (proj-figures-dir (when proj-root
                               (f-join proj-root "figures"))))
      (when (and proj-figures-dir
                 (f-exists? proj-figures-dir))
        (setq-local ob-ipython-resources-dir proj-figures-dir))))

  (defun spacemacs//org-export-output-project-file-name (orig-fun extension &optional subtreep pub-dir)
    "Export to a project's corresponding source directory as determined by EXTENSION."
    (let* ((projectile-require-project-root nil)
           (proj-root (projectile-project-root))
           (lang-src-dir (when proj-root
                           (f-join proj-root
                                   "src"
                                   (s-chop-prefix "." extension))))
           (pub-dir (if (and lang-src-dir
                             (f-exists? lang-src-dir))
                        lang-src-dir
                      pub-dir)))
      (message "%s" pub-dir)
      (funcall orig-fun extension subtreep pub-dir)))

  (defun spacemacs//org-latex-pdf-process (file-name)
    "XXX: This will err-out because of org-export's assumption that the output should b
in the local directory"
    (let* ((projectile-require-project-root nil)
           (proj-root (projectile-project-root))
           (pdf-out-dir (when proj-root
                          (f-join proj-root "output")))
           (pub-dir (if (and pdf-out-dir
                             (f-exists? pdf-out-dir))
                        pdf-out-dir
                      (f-dirname file-name)))
           (compile-cmd (s-join " "
                                (list "max_print_line=1000 error_line=254 half_error_line=238 openout_any=a"
                                      "latexmk -pdf -bibtex -pdflatex='pdflatex -shell-escape -file-line-error -interaction=nonstopmode -synctex=1'"
                                      (concat "-jobname="
                                              (f-join pub-dir
                                                      (f-base file-name)))
                                      file-name))))
      (message "%s" compile-cmd)
      (compile compile-cmd))))

(defun spacemacs//org-babel-load-session:python (session body params)
  "Load BODY into SESSION using python-shell-send-string-echo."
  (declare-function python-shell-send-string "python.el")
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:python session params))
          (python-shell-send (if (and ob-python-execute-echo
                                      (fboundp 'python-shell-send-string-echo))
                                 'python-shell-send-string-echo
                               'python-shell-send-string)))
      (with-current-buffer buffer
        (funcall python-shell-send
                 (org-babel-chomp body)
                 (get-buffer-process (current-buffer))))
      buffer)))
(defun spacemacs//ob-ipython--create-repl (name)
  "Use `python-shell-get-process-name' for buffer processes."
  (let ((python-shell-completion-native-enable nil)
        (cmd (s-join " "
                     (ob-ipython--kernel-repl-cmd name)))
        (process-name (if (string= "default" name)
                          (python-shell-get-process-name nil)
                        (format "%s:%s"
                                (python-shell-get-process-name nil)
                                name))))
    (get-buffer-process (python-shell-make-comint cmd process-name
                                                  nil))
    (format "*%s*" process-name)))
(defun spacemacs//ob-ipython--process-response (ret file result-type)
  "Don't append 'Out[...]:\n' junk to value-type output!"
  (let ((result (cdr (assoc :result ret)))
        (output (cdr (assoc :output ret))))
    (if (eq result-type 'output)
        output
      (car (->> (-map (-partial 'ob-ipython--render file)
                      (list (cdr (assoc :value result))
                            (cdr (assoc :display result))))
                (remove-if-not nil))))))
(defun spacemacs//ob-jupyter-console-repl-refresh ()
  " Manually--and hackishly--'refresh' a Jupyter console session with a
      remote kernel (opening one if not present) and display results echoed from
      a remote kernel.

      XXX: Requires 'c.ZMQTerminalInteractiveShell.include_other_output = True' in
      the jupyter console config.  Could add this to `ob-ipython' console initiation
      just to be sure."
  (with-demoted-errors "Error: %S"
    ;; FIXME: Does not seem to find the correct/any session!
    (let ((session-buffer (org-babel-initiate-session)))
      ;; (let* ((info (or info (org-babel-get-src-block-info)))
      ;;        (lang (nth 0 info))
      ;;        (params (nth 2 info))
      ;;        (session (cdr (assq :session params))))
      (when session-buffer
        (save-mark-and-excursion (with-current-buffer session-buffer
                                   (python-shell-send-string "pass"
                                                             (python-shell-get-process))))))))
(defun spacemacs//ob-ipython--render (file-or-nil values)
  "Display `value' output without prepended prompt."
  (let ((org (lambda (value)
               value))
        (png (lambda (value)
               (let ((file (or file-or-nil
                               (ob-ipython--generate-file-name ".png"))))
                 (ob-ipython--write-base64-string file value)
                 (format "[[file:%s]]" file))))
        (svg (lambda (value)
               (let ((file (or file-or-nil
                               (ob-ipython--generate-file-name ".svg"))))
                 (ob-ipython--write-string-to-file file value)
                 (format "[[file:%s]]" file))))
        (html (lambda (value)))
        (txt (lambda (value)
               (when (s-present? value)
                 (s-replace "'" "" value)))))
    (or (-when-let (val (cdr (assoc 'text/org values)))
          (funcall org val))
        (-when-let (val (cdr (assoc 'image/png values)))
          (funcall png val))
        (-when-let (val (cdr (assoc 'image/svg+xml values)))
          (funcall svg val))
        (-when-let (val (cdr (assoc 'text/plain values)))
          (funcall txt val)))
    ))
(defun spacemacs//ob-ipython--dump-error (err-msg)
  "No-op used to get rid of separate trace buffer"
  ;; Drop into console instead?
  ;; (with-demoted-errors "Error: %S" (spacemacs//ob-jupyter-console-repl-refresh))
  (error "There was a fatal error trying to process the request."))
(defun spacemacs//org-babel-python-session-buffer (orig-func session)
  "Make org-babel's default python session buffer naming follow `python-mode'."
  (if (eq session :default)
      (format "*%s*"
              (python-shell-get-process-name nil))
    (funcall orig-func session)))
