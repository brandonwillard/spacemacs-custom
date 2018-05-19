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

(when (configuration-layer/package-used-p 'org-ref)
  (defun spacemacs//org-ref-find-bibliography ()
  "Find the bibliography files in the current buffer.

This function sets and returns `org-ref-bibliography-files' obtained from
#+BIBLIOGRAPHY options."
    (prog1
        ;; When called from within a bibtex file, assume we want it; otherwise,
        ;; check the current file for a bibliography source.
        (setq org-ref-bibliography-files (or (and (f-ext? buffer-file-name "bib")
                                                  (list buffer-file-name))
                                             (plist-get (org-export-get-environment)
                                                        :bibliography)
                                             org-ref-default-bibliography))

        ;; TODO: Obtain items within latex tokens '\bibliography' and '\addbibresource'.
        ;; Try `org-map-tree' and follow the example of `org-latex-math-block-tree-filter'
        ;; (and/or `org-element-latex-fragment-parser').

        ;; Set reftex-default-bibliography so we can search.
        (setq-local reftex-default-bibliography org-ref-bibliography-files)))

  (defun spacemacs//org-ref-parse-bib-latex-entries (tree backend info)
    "Add an export block with the bibliography at the location of the last bibliography
keyword."
    (let ((last-bib-elem))
      (org-element-map tree
          '(keyword)
        (lambda (element)
          (if (string-equal (org-element-property :key element) "BIBLIOGRAPHY")
              (setq last-bib-elem element)))
        info)
      (if last-bib-elem
          (let* ((bib-style (plist-get info :bibliographystyle))
                 (bib-value (org-ref-bibliography-format
                             (string-join (plist-get info :bibliography) ",")
                             ;; (org-element-property :value last-bib-elem)
                             nil backend))
                 (parent (org-element-property :parent last-bib-elem))
                 (parent-end (org-element-property :end parent))
                 (new-bib-elem))
            (if (and bib-style (or (eq backend 'beamer) (eq backend 'latex)))
                (setq bib-value (concat (format "\\bibliographystyle{%s}\n" bib-style) bib-value)))
            (setq new-bib-elem (org-element-create 'export-block
                                                   (list :type (string-inflection-upcase-function (symbol-name backend))
                                                         :value bib-value
                                                         :begin parent-end
                                                         :end (+ parent-end (seq-length bib-value)))))
            (org-element-set-element last-bib-elem new-bib-elem)))
      tree)))

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
  (defun spacemacs//org-compile-file (source process ext &optional err-msg log-buf spec)
    "Same as `org-compile-file' but considers the variable `org-projectile-output-dir'
for the output directory."
    (let* ((projectile-require-project-root nil)
           (proj-root (projectile-project-root))
           (base-name (file-name-base source))
           (full-name (file-truename source))
           (out-dir (or (f-join proj-root org-projectile-output-dir)
                        (file-name-directory source)
                        "./"))
           (output (expand-file-name (concat base-name "." ext)
                                     out-dir))
           (time (current-time))
           (err-msg (if (stringp err-msg)
                        (concat ".  " err-msg)
                      "")))
      (save-window-excursion (pcase process
                               ((pred functionp)
                                (funcall process
                                         (shell-quote-argument source)))
                               ((pred consp)
                                (let ((log-buf (and log-buf
                                                    (get-buffer-create log-buf)))
                                      (spec (append spec
                                                    `((?b . ,(shell-quote-argument base-name))
                                                      (?f . ,(shell-quote-argument source))
                                                      (?F . ,(shell-quote-argument full-name))
                                                      (?o . ,(shell-quote-argument out-dir))
                                                      (?O . ,(shell-quote-argument output))))))
                                  (dolist (command process)
                                    (shell-command (format-spec command spec)
                                                   log-buf))
                                  (when log-buf
                                    (with-current-buffer log-buf
                                      (compilation-mode)))))
                               (_ (error "No valid command to process %S%s"
                                         source err-msg))))
      ;; Check for process failure.  Output file is expected to be
      ;; located in the same directory as SOURCE.
      (unless (org-file-newer-than-p output time)
        (error (format "File %S wasn't produced%s" output
                       err-msg)))
      output)))

(defun spacemacs//org-make-link-regexps ()
    "Update the link regular expressions.
  This should be called after the variable `org-link-types' has changed."
    (let ((types-re (regexp-opt (org-link-types)
                                        t)))
      (setq org-bracket-link-regexp
            (rx-to-string `(seq "[["
                     (submatch
                      (one-or-more
                       (not
                        (any ?[ ?]))))
                     "]"
                     (zero-or-one
                      (submatch "["
                                (submatch
                                 (one-or-more
                                  ;; Simply add an exception for inline babel src statements.
                                  (or
                                   ;; Org inline src block/statement
                                   (zero-or-one ,org-babel-inline-src-rx)
                                   ;; This is the original condition.
                                   (not
                                    (any ?[ ?])))))
                                "]"))
                     "]"))
            org-any-link-re
            (concat "\\(" org-bracket-link-regexp "\\)\\|\\("
                    org-angle-link-re "\\)\\|\\("
                    org-plain-link-re "\\)"))))
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
          (funcall txt val)))))
(defun spacemacs//ob-ipython--dump-error (err-msg)
  "No-op used to get rid of the separate trace buffer"
  ;; Drop into console instead?
  ;; (with-demoted-errors "Error: %S" (spacemacs//ob-jupyter-console-repl-refresh))
  (error "There was a fatal error trying to process the request."))
(defun spacemacs//org-babel-python-session-buffer (orig-func session)
  "Make org-babel's default python session buffer naming follow `python-mode'."
  (if (eq session :default)
      (format "*%s*"
              (python-shell-get-process-name nil))
    (funcall orig-func session)))
(defun spacemacs//org-latex-src-block (oldfun src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information.

This is mostly the standard `ox-latex' with only the following differences:
  1. Float placement options for src blocks (e.g. listings) are now used.
  2. Optional tcolorbox listings environment for minted.
"
  (when (org-string-nw-p (org-element-property :value src-block))
    (let* ((lang (org-element-property :language src-block))
           (caption (org-export-data (org-export-get-caption src-block) info)
                    ;; TODO: There's also a caption option in attr; prefer that one?
                    ;; (org-element-property :caption src-block)
                    )
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
           (listings (plist-get info :latex-listings))
           (listings-options (mapconcat #'identity
                                        ;; Only add [default] placement option when float is specified.
                                        (remove nil `(,(and float placement)
                                                      ,(plist-get attributes :listing-options)))
                                        ",")))
      (cond
       ((eq org-latex-listings-wrapper 'tcolorbox)
        (let* ((listings-env-name (or (plist-get attributes :listings-env-name) "oxtcblisting"))
               (body (format "\\begin{%s}{minted language=%s, %s}\n%s\n\\end{%1$s}"
                             ;;
                             listings-env-name
                             ;; Language.
                             (or (cadr (assq (intern lang)
                                             (plist-get info :latex-minted-langs)))
                                 (downcase lang))
                             ;; Options.
                             (mapconcat #'identity
                                        (remove nil
                                                `(,(if (org-string-nw-p caption)
                                                       (format "title={\\lstlistingname\\ \\thetcbcounter: {%s}}" caption))
                                                       ;; (format "listing options={caption={%s}}" caption))
                                                  ,(if (org-string-nw-p label)
                                                       (format "label type=listing, label={%s}" label))
                                                  ,(if (string= "t" float)
                                                       (format "float, floatplacement=%s" placement)
                                                     "nofloat")
                                                  ;; ,org-latex-tcolorbox-default-options
                                                  ,(plist-get attributes :options)))
                                        ",")
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
          body))
       ((eq listings 'minted)
        (let* ((caption-str (org-latex--caption/label-string src-block
                                                             info))
               (listings-env-name (or (plist-get attributes :listings-env-name) "listing"))
               (float-env (cond
                           ((string= "multicolumn" float)
                            (format "\\begin{%s*}[%s]\n%s%%s\n%s\\end{%1$s*}"
                                    listings-env-name
                                    listings-options
                                    (if caption-above-p caption-str "")
                                    (if caption-above-p "" caption-str)))
                           (caption (format "\\begin{%s}[%s]\n%s%%s\n%s\\end{%1$s}"
                                            listings-env-name
                                            listings-options
                                            (if caption-above-p caption-str "")
                                            (if caption-above-p "" caption-str)))
                           ((string= "t" float)
                            (format "\\begin{%s}[%s]\n%%s\n\\end{%1$s}" listings-env-name listings-options))
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
       (t (funcall oldfun src-block _contents info))))))
