;;; packages.el --- org-extras layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: bwillard <bwillard@lappy-x1>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst org-extras-packages
  '(
    org
    org-agenda
    org-projectile
    f
    s
    dash
    ob-async
    ob-ipython))

(defun org-extras/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      (setq org-capture-templates
            '(("t" "Tasks" entry
              (file+headline org-default-notes-file "Tasks"))))

      (setq org-highlight-latex-and-related '(latex script entities))

      ;; Most often, we'll use inline src statements (e.g. src_python{...}) to
      ;; simply display formatted text.
      (setq org-babel-default-inline-header-args
            '((:exports . "code")
              (:eval . "never")
              (:results . "none")))

      (setq org-edit-src-content-indentation 0
            org-src-tab-acts-natively t
            org-src-window-setup 'current-window
            org-src-fontify-natively t
            org-support-shift-select 'always)

      (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

      (advice-add 'org-babel-python-session-buffer :around
                  #'spacemacs//org-babel-python-session-buffer)

      (when (configuration-layer/package-used-p 'projectile)
        (advice-add 'org-export-output-file-name :around
                    #'spacemacs//org-export-output-project-file-name))

      ;; TODO: Does this work?
      ;; (declare-function python-shell-calculate-command "ext:python" nil)
      ;; (setq org-babel-python-command (python-shell-calculate-command))
      (setq org-babel-python-command "ipython --simple-prompt -i")

      (advice-add 'org-babel-load-session:python :override
                  #'spacemacs//org-babel-load-session:python)

      (setq org-latex-listings 'minted
            org-latex-prefer-user-labels t
            org-latex-packages-alist '(("" "minted")))

      (when (configuration-layer/package-used-p 'projectile)
        (setq org-latex-pdf-process #'spacemacs//org-latex-pdf-process))

      ;; (add-to-list 'org-babel-load-languages '(R . t))
      ;; (add-to-list 'org-babel-load-languages '(sql. t))
      ;; (add-to-list 'org-babel-load-languages '(shell . t))
      (add-to-list 'org-babel-load-languages '(emacs-lisp . t)))))

(defun org-extras/pre-init-ob-ipython ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-ipython
        :init (add-to-list 'org-babel-load-languages '(ipython . t))
        :config
        (progn
          ;; Only initialize `ob-ipython-mode' when we edit a src block.
          (add-hook 'org-src-mode-hook
                    #'(lambda ()
                        (when (derived-mode-p python-mode)
                          (ob-ipython-mode))))
          (spacemacs|add-company-backends :backends company-ob-ipython
                                          :modes ob-ipython-mode)

          (when (configuration-layer/package-used-p 'projectile)
            (add-hook 'ob-ipython-mode-hook #'spacemacs//ob-ipython-project-dirs-setup))

          (advice-add 'ob-ipython--create-repl :override #'spacemacs//ob-ipython--create-repl)
          (advice-add 'ob-ipython--process-response
                      :override #'spacemacs//ob-ipython--process-response)
          (advice-add 'ob-ipython--render :override #'spacemacs//ob-ipython--render)
          (advice-add 'ob-ipython--dump-error :override #'spacemacs//ob-ipython--dump-error)))))

(defun org-extras/init-ob-ipython ())

(defun org-extras/pre-init-org-agenda ()
  (spacemacs|use-package-add-hook org-projectile
    :post-config (let ((existing-todos (-filter 'f-exists-p
                                                (org-projectile-todo-files))))
                   (setq org-agenda-files (append org-agenda-files existing-todos)))))

(defun org-extras/post-init-org-projectile ()
  (setq org-projectile-capture-template "* TODO %?\n  %u\n  %a"))

(defun org-extras/init-ob-async ()
  (use-package ob-async))

(defun org-extras/post-init-dash ())

(defun org-extras/post-init-f ())

(defun org-extras/post-init-s ())

;;; packages.el ends here
