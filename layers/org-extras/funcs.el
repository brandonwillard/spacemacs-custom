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
    "Set `ob-ipython-resources-dir' to a directory in the current projectile project.

Uses `org-projectile-resources-dir' and the projectile project's root directory."
    (let* ((projectile-require-project-root nil)
           (proj-root (projectile-project-root))
           (proj-figures-dir (when proj-root
                               (f-join proj-root org-projectile-resources-dir))))
      (when (and proj-figures-dir
                 (f-exists? proj-figures-dir))
        (setq-local ob-ipython-resources-dir proj-figures-dir))))

  (defun spacemacs//org-export-output-project-file-name (orig-fun extension &optional subtreep pub-dir)
    "Export to a project's corresponding source directory as determined by EXTENSION.

Uses `projectile-src-directory' plus the EXTENSION to determine the exact output sub-directory."
    (let* ((projectile-require-project-root nil)
           (proj-root (projectile-project-root))
           (lang-src-dir (when proj-root
                           (f-join proj-root
                                   (projectile-src-directory (projectile-project-type))
                                   (s-chop-prefix "." extension))))
           (pub-dir (if (and lang-src-dir
                             (f-exists? lang-src-dir))
                        lang-src-dir
                      pub-dir)))
      (message "%s" pub-dir)
      (funcall orig-fun extension subtreep pub-dir)))

  (defun spacemacs//org-latex-pdf-process (file-name)
    "XXX: This will err-out because of org-export's assumption that the output
should be in the local directory"
    (let* ((projectile-require-project-root nil)
           (proj-root (projectile-project-root))
           (pdf-out-dir (when proj-root
                          (f-join proj-root org-projectile-output-dir)))
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

(defun spacemacs//org-babel-execute-from-here (&optional arg)
  "Execute source code blocks from the subtree at the current point upward.
Call `org-babel-execute-src-block' on every source block in
the current subtree upward."
  (interactive "P")
  (save-restriction
    (save-excursion
      (narrow-to-region (point-min)
                        (progn (org-end-of-subtree t t)
	                             (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
	                             (point)))
      (org-babel-execute-buffer arg)
      (widen))))
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
(defun spacemacs//org-latex-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information.

This is mostly the standard `ox-latex' with only the following differences:
  1. Float placement options for src blocks (e.g. listings) are now used.
  2. Generic custom language environments can be declared using the key language/symbol `all'.
"
  (when (org-string-nw-p (org-element-property :value src-block))
    (let* ((lang (org-element-property :language src-block))
           (caption (org-element-property :caption src-block))
           (caption-above-p (org-latex--caption-above-p src-block info))
           (label (org-element-property :name src-block))
           (custom-env (or (and lang
                                (cadr (assq (intern lang) org-latex-custom-lang-environments)))
                           nil
                           ;; TODO: Allow an `all' entry that applies to all languages
                           ;; (assoc-default (intern lang) org-latex-custom-lang-environments
                           ;;                nil (assq 'all org-latex-custom-lang-environments))
                           )
                       )
           (num-start (org-export-get-loc src-block info))
           (retain-labels (org-element-property :retain-labels src-block))
           (attributes (org-export-read-attribute :attr_latex src-block))
           (placement (let ((place (plist-get attributes :placement)))
                        (cond
                         (place (format "%s" place))
                         (t (plist-get info :latex-default-figure-position)))))
           (float (plist-get attributes :float))
           (listings (plist-get info :latex-listings)))
      (cond
       ;; Case 1.  No source fontification.
       ((not listings)
        (let* ((caption-str (org-latex--caption/label-string src-block
                                                             info))
               (float-env (cond
                           ((string= "multicolumn" float)
                            (format "\\begin{figure*}[%s]\n%s%%s\n%s\\end{figure*}"
                                    placement
                                    (if caption-above-p caption-str "")
                                    (if caption-above-p "" caption-str)))
                           (caption (concat (if caption-above-p caption-str "")
                                            "%s"
                                            (if caption-above-p
                                                ""
                                              (concat "\n" caption-str))))
                           (t "%s"))))
          (format float-env
                  (concat (format "\\begin{verbatim}\n%s\\end{verbatim}"
                                  (org-export-format-code-default src-block
                                                                  info))))))
       ;; Case 2.  Custom environment.

       (custom-env (let ((caption-str (org-latex--caption/label-string src-block
                                                                       info))
                         (formatted-src (org-export-format-code-default src-block
                                                                        info)))
                     (if (string-match-p "\\`[a-zA-Z0-9]+\\'" custom-env)
                         (format "\\begin{%s}\n%s\\end{%s}\n"
                                 custom-env
                                 (concat (and caption-above-p caption-str)
                                         formatted-src
                                         (and (not caption-above-p)
                                              caption-str))
                                 custom-env)
                       (format-spec custom-env
                                    `((?s . ,(or formatted-src ""))
                                      (?c . ,(or caption ""))
                                      (?f . ,(or float ""))
                                      (?l . ,(or (org-latex--label src-block info) ""))
                                      (?o . ,(or (plist-get attributes :options)
                                                 "")))))))
       ;; Case 3.  Use minted package.

       ((eq listings 'minted)
        (let* ((caption-str (org-latex--caption/label-string src-block
                                                             info))
               (float-env (cond
                           ((string= "multicolumn" float)
                            (format "\\begin{listing*}[%s]\n%s%%s\n%s\\end{listing*}"
                                    placement
                                    (if caption-above-p caption-str "")
                                    (if caption-above-p "" caption-str)))
                           (caption (format "\\begin{listing}[%s]\n%s%%s\n%s\\end{listing}"
                                            placement
                                            (if caption-above-p caption-str "")
                                            (if caption-above-p "" caption-str)))
                           ((string= "t" float)
                            (concat (format "\\begin{listing}[%s]\n" placement)
                                    "%s\n\\end{listing}"))
                           (t "%s")))
               (options (plist-get info :latex-minted-options))
               (body (format "\\begin{minted}[%s]{%s}\n%s\\end{minted}"
                             ;; Options.

                             (concat (org-latex--make-option-string (if (or (not num-start)
                                                                            (assoc "linenos" options))
                                                                        options
                                                                      (append `(("linenos")
                                                                                ("firstnumber" ,(number-to-string (1+ num-start))))
                                                                              options)))
                                     (let ((local-options (plist-get attributes :options)))
                                       (and local-options
                                            (concat "," local-options))))
                             ;; Language.
                             (or (cadr (assq (intern lang)
                                             (plist-get info :latex-minted-langs)))
                                 (downcase lang))
                             ;; Source code.
                             (let* ((code-info (org-export-unravel-code src-block))
                                    (max-width (apply 'max
                                                      (mapcar 'length
                                                              (org-split-string (car code-info)
                                                                                "\n")))))
                               (org-export-format-code (car code-info)
                                                       (lambda (loc _num ref)
                                                         (concat loc
                                                                 (when ref
                                                                   ;; Ensure references are flushed to the right,
			                                                             ;; separated with 6 spaces from the widest line
			                                                             ;; of code.
                                                                   (concat (make-string (+ (- max-width
                                                                                              (length loc))
                                                                                           6)
                                                                                        ?\
                                                                                        s)
                                                                           (format "(%s)" ref)))))
                                                       nil
                                                       (and retain-labels
                                                            (cdr code-info)))))))
          ;; Return value.

          (format float-env body)))
       ;; Case 4.  Use listings package.

       (t (let ((lst-lang (or (cadr (assq (intern lang)
                                          (plist-get info :latex-listings-langs)))
                              lang))
                (caption-str (when caption
                               (let ((main (org-export-get-caption src-block))
                                     (secondary (org-export-get-caption src-block t)))
                                 (if (not secondary)
                                     (format "{%s}"
                                             (org-export-data main info))
                                   (format "{[%s]%s}"
                                           (org-export-data secondary info)
                                           (org-export-data main info))))))
                (lst-opt (plist-get info :latex-listings-options)))
            (concat
             ;; Options. (format
	           "\\lstset{%s}\n"
	           (concat (org-latex--make-option-string (append lst-opt
                                                            (cond
                                                             ((and (not float)
                                                                   (plist-member attributes :float)) nil)
                                                             ((string= "multicolumn" float) '(("float" "*")))
                                                             ((and float
                                                                   (not (assoc "float" lst-opt))) `(("float" ,placement))))
                                                            `(("language" ,lst-lang))
                                                            (if label
                                                                `(("label" ,(org-latex--label src-block info)))
                                                              '(("label" " ")))
                                                            (if caption-str
                                                                `(("caption" ,caption-str))
                                                              '(("caption" " ")))
                                                            `(("captionpos" ,(if caption-above-p "t" "b")))
                                                            (cond
                                                             ((assoc "numbers" lst-opt) nil)
                                                             ((not num-start) '(("numbers" "none")))
                                                             (t `(("firstnumber" ,(number-to-string (1+ num-start)))
                                                                  ("numbers" "left"))))))
                     (let ((local-options (plist-get attributes :options)))
                       (and local-options
                            (concat "," local-options)))))
            ;; Source code.

            (format
	           "\\begin{lstlisting}\n%s\\end{lstlisting}"
	           (let* ((code-info (org-export-unravel-code src-block))
                    (max-width (apply 'max
                                      (mapcar 'length
                                              (org-split-string (car code-info)
                                                                "\n")))))
               (org-export-format-code (car code-info)
                                       (lambda (loc _num ref)
                                         (concat loc
                                                 (when ref
                                                   ;; Ensure references are flushed to the right,
		                                               ;; separated with 6 spaces from the widest line of
		                                               ;; code
                                                   (concat (make-string (+ (- max-width
                                                                              (length loc))
                                                                           6)
                                                                        ?\
                                                                        s)
                                                           (format "(%s)" ref)))))
                                       nil
                                       (and retain-labels
                                            (cdr code-info)))))))))))
