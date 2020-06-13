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
    ob-hy
    ;; ob-ipython
    ))

(defun org-extras/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (progn
      (add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append)

      (add-hook 'org-mode-hook #'spacemacs//set-nobreak-predicate)

      (add-hook 'org-export-before-processing-hook #'spacemacs//org-remove-headlines)

      ;; Create a function that can wrap the process-name function and
      ;; prepend a session (if present).
      (spacemacs//session-and-process-name org-babel-python-buffers nil)
      (advice-add 'python-shell-get-process-name :around
                  #'spacemacs//org-babel-python-buffers-process-name)

      (advice-add 'org-babel-python-session-buffer :around
                  #'spacemacs//org-babel-python-session-buffer)

      (when (configuration-layer/package-used-p 'projectile)
        (advice-add 'org-compile-file :override #'spacemacs//org-compile-file)
        (advice-add 'org-export-output-file-name :around
                    #'spacemacs//org-export-output-project-file-name))

      (when (and (configuration-layer/layer-usedp 'python-extras) (fboundp #'spacemacs//set-project-root))
        (advice-add #'org-babel-python-initiate-session :around #'spacemacs//run-in-pyvenv-wrapper))

      ;; Enable graphics output
      (advice-add #'org-babel-execute:python :around
                  #'org-btw//ob-python-generate-plots)

      ;; TODO: Does this work?
      ;; (declare-function python-shell-calculate-command "ext:python" nil)
      ;; (setq org-babel-python-command (python-shell-calculate-command))
      (setq org-babel-python-command "ipython --simple-prompt -i")

      (advice-add 'org-babel-load-session:python :override
                  #'spacemacs//org-babel-load-session:python)

      ;; (with-eval-after-load 'ox-latex
      ;;   (advice-add 'org-latex-src-block :around #'spacemacs//org-latex-src-block))

      (spacemacs/toggle-org-highlight-inline-src-on)

      (spacemacs/toggle-org-inline-src-in-links-on)

      (setq org-latex-listings 'minted
            org-latex-prefer-user-labels t
            org-latex-packages-alist '(("" "minted")
                                       ("minted, listings, breakable, skins" "tcolorbox")))

      ;; (defun spacemacs//org-export-latex-add-tcolorbox (body backend info)
      ;;   "Add a custom tcolorbox listing environment to the latex-header-extra options."
      ;;   (when (or (eq backend 'latex)
      ;;             (eq 'latex (org-export-backend-parent (org-export-get-backend backend))))
      ;;     (concat org-latex-tcolorbox-listing-env "\n" body)
      ;;     ;; (plist-put options :latex-header-extra `(,org-latex-tcolorbox-listing-env))
      ;;     ))
      ;;
      ;; ;; (add-to-list 'org-export-filter-options-functions 'spacemacs//org-export-latex-add-tcolorbox)
      ;; (add-to-list 'org-export-filter-body-functions 'spacemacs//org-export-latex-add-tcolorbox)

      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "bh" #'spacemacs/org-babel-execute-from-here)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "bD" #'org-babel-remove-result-one-or-many)

      (add-to-list 'org-babel-load-languages '(emacs-lisp . t)))))

(defun org-extras/pre-init-ob-hy ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-hy
      :init (progn
              (add-to-list 'org-babel-load-languages '(hy . t))
              ;; Create a function that can wrap the process-name function and
              ;; prepend a session (if present).
              (spacemacs//session-and-process-name org-babel-hy-buffers t)
              (advice-add 'hy-shell-get-process-name :around
                          #'spacemacs//org-babel-hy-buffers-process-name)
              (advice-add 'org-babel-hy-session-buffer :around
                          #'spacemacs//org-babel-hy-session-buffer)))))
(defun org-extras/init-ob-hy ())

(defun org-extras/pre-init-ob-ipython ()
  (spacemacs|use-package-add-hook org
    :post-config
    (use-package ob-ipython
        :init (add-to-list 'org-babel-load-languages '(ipython . t))
        :config
        (progn
          ;; Only initialize `ob-ipython-mode' when we edit a src block.
          ;; (add-hook 'org-src-mode-hook
          ;;           #'(lambda ()
          ;;               (when (derived-mode-p 'python-mode)
          ;;                 (ob-ipython-mode))))
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

(defun org-extras/pre-init-org-agenda ())

(defun org-extras/post-init-org-projectile ()
  (when (configuration-layer/package-used-p 'org-agenda)
    (with-eval-after-load 'org-agenda
      (require 'org-projectile)
      (let ((existing-todos (seq-filter 'f-exists-p
                                        (org-projectile-todo-files))))
        ;; Add TODO files from existing projects.
        (setq org-agenda-files (append org-agenda-files existing-todos))))))

(defun org-extras/init-ob-async ()
  (use-package ob-async))

(defun org-extras/post-init-dash ())

(defun org-extras/post-init-f ())

(defun org-extras/post-init-s ())

;;; packages.el ends here
