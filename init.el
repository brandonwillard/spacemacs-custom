;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-configuration-layers
   '(;; rust
     ;; kubernetes
     eww
     gnus
     ess
     ;; ocaml
     ;; elixir
     ;; javascript
     clojure
     ;; go
     csv
     restructuredtext
     ;; (javascript :packages (not tern))
     (lsp :variables
          lsp-ui-doc-enable nil
          lsp-ui-sideline-enable nil
          lsp-ui-remap-xref-keybindings t
          ;; :packages (not flycheck-lsp)
          )
     html
     markdown
     (latex :variables
            latex-build-command "Make")
     bibtex
     graphviz
     ;; (ess :variables
     ;;      ess-disable-underscore-assign t
     ;;      :packages (not ess-R-object-popup))
     (python :variables
             ;; NOTE: These can also be .dir-local/project specific.
             python-test-runner 'pytest
             python-backend 'lsp
             python-formatter 'black
             python-format-on-save nil
             :packages (not live-py-mode))
     (python-extras :variables
                    python-auto-set-local-pyvenv-virtualenv 'on-project-switch)
     (hy :variables hy-shell-spy-delim "\n--spy-output--\n")
     yaml
     sql
     ;; noweb
     ;; (c-c++ :variables
     ;;        c-c++-backend 'lsp-clangd
     ;;        ;; company-c-headers-path-user '("../include" "./include" "." "../../include"
     ;;        ;;                               "../inc" "../../inc")
     ;;        c-c++-enable-clang-support t
     ;;        c-c++-default-mode-for-headers 'c++-mode)
     helm
     (auto-completion :variables
                      ;; auto-completion-enable-sort-by-usage t
                      spacemacs-default-company-backends '(company-files company-capf company-yasnippet)
                      ;; ((company-semantic company-dabbrev-code company-gtags company-etags company-keywords) company-files company-dabbrev)
                      auto-completion-return-key-behavior nil
                      auto-completion-idle-delay nil
                      auto-completion-tab-key-behavior nil
                      auto-completion-complete-with-key-sequence "C-y"
                      auto-completion-private-snippets-directory nil
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip 'manual)
     emacs-lisp
     git
     ;; (spacemacs-purpose :packages (not window-purpose))
     ;; XXX: The `github' layer requires `forge', which requires `closql' and interacts poorly
     ;; with recent Emacs master branch changes to `eieio' (seen during the
     ;; loading of `window-purpose' class objects)
     ;; (github :variables magit-gh-pulls-pull-detail-limit 10)
     scheme
     racket
     pdf
     (org :variables
          org-enable-org-journal-support t
          org-enable-github-support t
          org-projectile-file "TODOs.org")
     org-extras

     (shell :variables
            shell-default-shell 'vterm
            shell-default-height 30
            shell-default-position 'bottom)

     spell-checking
     (syntax-checking :variables
                      syntax-checking-enable-tooltips nil)

     ;; slack
     (erc :variables
          erc-server-list
          '(("chat.freenode.net"
             :port "6697"
             :ssl t
             :nick "brandonwillard"
             ))
          erc-enable-notifications t
          erc-enable-sasl-auth t)

     ;; FIXME: We get `semantic-idle-scheduler-function' errors in `polymode' modes.
     ;; (semantic :enabled-for emacs-lisp common-lisp python)
     common-lisp)

   ;; FYI: You can use MELPA recipes here (i.e. https://github.com/melpa/melpa#recipe-format).
   dotspacemacs-additional-packages '(;; elisp file manipulation library
                                      f
                                      ;; elisp string manipulation library
                                      s
                                      ;; elisp list manipulation library
                                      dash
                                      dash-functional
                                      debbugs
                                      ;; embrace
                                      evil-embrace
                                      ;; ox-jira
                                      ;; ox-confluence
                                      ox-rst
                                      (ox-ipynb :location (recipe :fetcher github
                                                                  :repo "jkitchin/ox-ipynb"))
                                      (ox-gfm :location (recipe :fetcher github
                                                                :repo "larstvei/ox-gfm"))
                                      plantuml-mode
                                      org-gcal
                                      dockerfile-mode
                                      evil-extra-operator

                                      kubernetes-tramp

                                      cython-mode
                                      jupyter

                                      (multi-vterm :location (recipe :fetcher github
                                                                     :repo "suonlight/multi-vterm"))

                                      (ob-racket :location (recipe :fetcher github
                                                                   :repo "wallyqs/ob-racket"))

                                      (org-btw :location "~/projects/code/emacs/org-btw")
                                      (hy-mode :location "~/projects/code/emacs/hy-mode")
                                      (org-ref :location "~/projects/code/emacs/org-ref")
                                      (ob-hy :location "~/projects/code/emacs/ob-hy")

                                      sphinx-doc
                                      yasnippet-snippets
                                      ;; Use a newer version of python.el.
                                      (python :location elpa :min-version "0.26.1"))
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(hl-todo company-emoji emoji-cheat-sheet-plus emojify)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading t
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   ;; dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-mode-line-theme 'spacemacs
   dotspacemacs-default-font '("Noto Mono"
                               :size 13.0
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key (concat "C-" dotspacemacs-leader-key)
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key (concat "C-" dotspacemacs-major-mode-leader-key)
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 5
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 100
   dotspacemacs-inactive-transparency 100
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers '(:relative nil
                               :enabled-for-modes prog-mode
                                                  text-mode
                               :disabled-for-modes dired-mode
                                                   doc-view-mode
                                                   markdown-mode
                                                   pdf-view-mode
                                                   xwidget-webkit-mode
                                                   edebug-mode
                                                   debugger-mode
                               :size-limit-kb 1000)
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-enable-server t
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   dotspacemacs-import-env-vars-shell-file-name shell-file-name
   dotspacemacs-switch-to-buffer-prefers-purpose nil))

(defun dotspacemacs/user-init ()

  ;; (modify-syntax-entry ?_ "w" (standard-syntax-table))

  (setq history-delete-duplicates t)

  (setq auto-save-timeout nil)
  (setq init-file-debug nil)
  (setq debug-on-error t)
  (setq debug-on-quit nil)

  ;; Helps with delays while handling very long lines.
  (setq-default bidi-display-reordering nil)
  (setq debugger-stack-frame-as-list t)
  (setq edebug-print-circle t)
  (setq edebug-print-level 20)
  (setq print-circle t)

  ;; This will help avoid errors with old `ert'-based code
  (when (> emacs-major-version 26)
    (defalias 'ert--print-backtrace 'backtrace-to-string))

  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

  (setq custom-file (concat user-emacs-directory "private/custom-settings.el"))

  ;; Viper is loaded/installed automatically, but we want it disabled.
  (setq package-load-list '(all (viper nil)))

  (defun btw/add-valid-paths-to-list (target-list object-list &optional append)
    (dolist (file (seq-take-while #'file-exists-p object-list))
      (add-to-list target-list (expand-file-name file) append)))

  (btw/add-valid-paths-to-list 'load-path
                               ;; TODO: Use `dotspacemacs-directory'?
                               '("~/.spacemacs.d"
                                 ;; "~/projects/code/emacs/org-btw"
                                 ;; "~/projects/code/emacs/ob-hy"
                                 ;; "~/projects/code/emacs/hy-mode"
                                 ;; "~/projects/code/emacs/org-ref"
                                 ))

  (btw/add-valid-paths-to-list 'Info-default-directory-list
                               '("/usr/share/info/emacs-27" "/usr/local/share/info"
                                 "/usr/share/info"))

  ;; Fix for anaconda env interaction with pyvenv.
  (if (and
       (not (getenv "ANACONDA_HOME"))
       (setq conda-home
             (seq-find #'file-exists-p
                       (list (or (getenv "ANACONDA_HOME")
                                 "~/apps/anaconda3")
                             "~/anaconda3"))))
      (progn
        (setenv "ANACONDA_HOME" conda-home)
        (setenv "WORKON_HOME" (concat conda-home "/" "envs"))

        ;; Just to be sure (and because we're seeing some new problems with PATH var
        ;; output from `shell-command-to-string' under let-bound `shell-file-name'),
        ;; let's make sure the conda binaries are available.
        (add-to-list 'exec-path (expand-file-name (concat conda-home "/" "bin"))))
    (and (not (getenv "ANACONDA_HOME"))
         (display-warning 'conda-home "Couldn't find anaconda home directory!")))

  (add-to-list 'exec-path (expand-file-name (concat user-home-directory
                                                    ".cask" "/" "bin")))

  ;; Hack for `exec-path' not being used by `shell-command-to-string'.
  ;; We're basically setting `process-environment', which is used by those shell commands.
  ;; (seq-filter (lambda (var) (s-starts-with-p "PATH=" var)) process-environment)
  ;; (setenv "PATH" (mapconcat #'identity (delete-dups exec-path) ":"))

  ;; Looks like `spacemacs/loadenv' is cutting off DBUS_SESSION_BUS_ADDRESS at the "="!
  ;; FYI: This now happens in `spacemacs/load-spacemacs-env'.
  ;; (-when-let* (((string-equal (getenv "DBUS_SESSION_BUS_ADDRESS") "unix:path"))
  ;;             (bus-path (format "/run/user/%s/bus" (user-uid)))
  ;;             ((file-exists-p bus-path)))
  ;;   (setenv "DBUS_SESSION_BUS_ADDRESS" (concat "unix:path=" bus-path)))

  (setq browse-url-browser-function '((".*slack.*" . browse-url-chrome)
                                      (".*youtube.*" . browse-url-chrome)
                                      ("." . eww-browse-url)))
  ;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)

  ;; Be more permissive about the accepted forms of version strings
  (add-to-list 'version-regexp-alist '("^[-._+ ]?Devi$" . -4)))

(defun dotspacemacs/user-config ()

  ;; Just a helper function for whatever.
  (defun hash-table-to-alist (hash-table)
    (json-read-from-string (json-encode hash-table)))

  ;; TODO: Hack fix; consider fixing, and then removing, this.
  (defun spacemacs/symbol-highlight-transient-state/body ())

  ;; TODO: Hack fix for `purpose-mode''s incorrect use of
  ;; `window--display-buffer'.
  ;;   (defun btw--window--display-buffer
  ;;       (old-func BUFFER WINDOW TYPE &optional ALIST DEDICATED)
  ;;     "This wrapper is a fix for `window--display-buffer' calls that use the
  ;; now-removed DEDICATED parameter."
  ;;     (funcall old-func BUFFER WINDOW TYPE ALIST))
  ;;
  ;;   (advice-add #'window--display-buffer :around #'btw--window--display-buffer)

  (setq default-input-method "TeX")
  (setq comment-empty-lines t)
  (setq evil-move-beyond-eol t)
  (setq evil-search-wrap nil)
  (add-hook 'display-line-numbers-mode-hook
            (lambda ()
              (setq display-line-numbers-width 4)))

  (require 'mode-local)

  (setq-mode-local text-mode scroll-margin 10)
  (setq-mode-local prog-mode scroll-margin 10)
  (setq-mode-local makefile-mode indent-tabs-mode t)

  ;; Stop terminals from re-centering!
  (setq scroll-step 1)
  (setq auto-window-vscroll nil)
  (setq scroll-conservatively 10000)

  (defun btw//lightweight-debug-settings ()
    (setq-local truncate-lines t)
    (setq-local print-level 4)
    (setq-local print-length 12)
    (setq-local eval-expression-print-level 4)
    (setq-local eval-expression-print-length 12)
    ;; `global-hl-line-mode' can slow down tracebacks considerably.
    (spacemacs/disable-hl-line-mode))

  (add-hook 'edebug-mode-hook #'btw//lightweight-debug-settings)
  (add-hook 'debugger-mode-hook #'btw//lightweight-debug-settings)

  (add-to-list 'debug-ignored-errors 'search-failed)
  (add-to-list 'debug-ignored-errors 'quit)
  (add-to-list 'debug-ignored-errors "^Before first heading$")
  (add-to-list 'debug-ignored-errors "^Nothing to complete$")
  (add-to-list 'debug-ignored-errors "^No such page: ")
  (add-to-list 'debug-ignored-errors "^Beginning of history$")
  (add-to-list 'debug-ignored-errors "^End of history$")
  (add-to-list 'debug-ignored-errors "^No surrounding delimiters found$")
  (add-to-list 'debug-ignored-errors "^Invalid search bound (wrong side of point)$")
  (add-to-list 'debug-ignored-errors "^Selecting deleted buffer$")
  (add-to-list 'debug-ignored-errors
               "^Company: backend \(:?.*?\) error \"Nothing to complete\"")
  (add-to-list 'debug-ignored-errors 'lsp-timed-out-error)
  (add-to-list 'debug-ignored-errors
               "^Candidates function ‘helm-ag--do-ag-candidate-process’ should run a process")
  (add-to-list 'debug-ignored-errors
               "^Current buffer has no process")
  (add-to-list 'debug-ignored-errors
               "Attempt to delete minibuffer or sole ordinary window")

  (setq-default sentence-end-double-space t)

  ;; XXX: This will stop completion with TAB (but also fix annoying noops when
  ;; attempting to indent lines).
  (setq tab-always-indent t)
  (setq fill-indent-according-to-mode t)

  (spacemacs/toggle-aggressive-indent-on)

  (setq compilation-scroll-output #'first-error)

  ;; Change default spacemacs keybinding.
  (define-key minibuffer-local-map (kbd "C-n") #'next-history-element)
  (define-key minibuffer-local-map (kbd "C-p") #'previous-history-element)
  (spacemacs/set-leader-keys "nd" 'narrow-to-defun)
  (spacemacs/set-leader-keys "kx" 'sp-split-sexp)
  (unbind-key (kbd "nf") spacemacs-default-map)

  (use-package cython-mode
    :defer t
    :interpreter ("cython" . cython-mode)
    :mode (("\\.pyx\\'" . cython-mode)
           ("\\.pxd\\'" . cython-mode)
           ("\\.pxi\\'" . cython-mode))
    :config (progn
              (spacemacs/set-leader-keys-for-major-mode 'cython-mode
                "cc" #'cython-compile)))

  (use-package debbugs :defer t)

  (use-package kubernetes-tramp
    :defer t
    :config (setq tramp-remote-shell-executable "sh"))

  ;; (use-package helpful)

  (use-package multi-vterm)

  (use-package jupyter
    :defer t
    :init (progn
            (spacemacs/set-leader-keys
              "aja" 'jupyter-repl-associate-buffer
              "ajc" 'jupyter-connect-repl
              "ajr" 'jupyter-run-repl
              "ajs" 'jupyter-server-list-kernels)
            (spacemacs/set-leader-keys-for-major-mode 'jupyter-repl-mode
              "i" 'jupyter-inspect-at-point
              "l" 'jupyter-load-file
              "s" 'jupyter-repl-scratch-buffer
              "I" 'jupyter-repl-interrupt-kernel
              "R" 'jupyter-repl-restart-kernel))
    :config (progn
              (when (eq dotspacemacs-editing-style 'vim)
                (evil-define-key 'insert jupyter-repl-mode-map
                  (kbd "C-j") 'jupyter-repl-history-next
                  (kbd "C-k") 'jupyter-repl-history-previous
                  (kbd "C-l") 'jupyter-repl-clear-cells
                  (kbd "C-R") 'isearch-forward
                  (kbd "C-r") 'isearch-backward)
                (evil-define-key 'normal jupyter-repl-mode-map
                  (kbd "C-l") 'jupyter-repl-clear-cells
                  (kbd "M-j") 'jupyter-repl-forward-cell
                  (kbd "M-k") 'jupyter-repl-backward-cell
                  (kbd "C-s") 'jupyter-repl-scratch-buffer))

              (add-hook 'jupyter-repl-mode-hook #'spacemacs/disable-vi-tilde-fringe)

              (when (fboundp 'purpose-set-extension-configuration)
                ;; NOTE: To delete this configuration...
                ;; (purpose-del-extension-configuration :jupyter)
                (purpose-set-extension-configuration
                 :jupyter (purpose-conf :mode-purposes
                                        '((jupyter-repl-mode . repl)
                                          (jupyter-repl-interaction-mode . repl)))))

              (spacemacs|add-company-backends :backends company-capf :modes jupyter-repl-mode)))

  (use-package ox-ipynb
    :disabled t
    :defer t
    :after (org jupyter)
    :init (progn
            (defun jupyter/ox-ipynb-emacs-jupyter ()
              (cl-loop
               for (kernel . (_dir . spec)) in (jupyter-available-kernelspecs)
               for lang = (plist-get spec :language)
               for display-name = (plist-get spec :display_name)
               do (cl-pushnew (cons (intern (concat "jupyter-" lang))
                                    (cons (intern "kernelspec") (list
                                                                 (cons (intern "display_name") display-name)
                                                                 (cons (intern "language") lang)
                                                                 (cons (intern "name") kernel)
                                                                 )))
                              ox-ipynb-kernelspecs :test #'equal))

              (cl-loop
               for (kernel . (_dir . spec)) in (jupyter-available-kernelspecs)
               for lang = (plist-get spec :language)
               for display-name = (plist-get spec :display_name)
               do (cl-pushnew (cons (intern (concat "jupyter-" lang))
                                    (cons (intern "language_info") (list
                                                                    (cons (intern "name") lang)
                                                                    (cons (intern "version") (nth 1 (split-string display-name)))
                                                                    )))
                              ox-ipynb-language-infos :test #'equal)))

            (add-hook 'org-mode-hook #'jupyter/ox-ipynb-emacs-jupyter)))

  (use-package ox-rst
    :after (org))

  ;; (use-package ox-jira
  ;;   :after (org))

  (use-package ox-pelican
    :commands (org-pelican-publish-to-pelican)
    :after (ox-gfm))

  (use-package org-btw-python
    :after (org)
    :commands (org-btw//ob-python-generate-plots))

  (use-package org-ref+
    :after (org-ref)
    :config (progn
              (org-ref+-mode +1)))

  (use-package ox-latex+
    :after (ox-latex))

  (use-package ox-sphinx
    :commands (org-sphinx-publish-to-rst)
    :after (org))

  (use-package org-gcal
    :disabled t
    :after (org)
    :config (progn
              (-when-let* ((client-info (cdr (car (json-read-file
                                                   (f-join dotspacemacs-directory
                                                           "private"
                                                           "org-gcal-brandonwillard-gmail.json")))))
                           (client-id (alist-get 'client_id client-info))
                           (client-secret (alist-get 'client_secret client-info)))
                ;; TODO: Use `plstore'/authstore
                ;; (add-to-list 'auth-sources "~/.authinfo.json.gpg")
                (setq org-gcal-client-id client-id
                      org-gcal-client-secret client-secret
                      org-gcal-file-alist '(("brandonwillard@gmail.com" .
                                             (f-join dotspacemacs-directory
                                                     "private"
                                                     "brandonwillard-gcal.org"))))
                ;; (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
                (with-eval-after-load 'org-agenda
                  ;; (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
                  ;; TODO: Map values and `add-to-list'.
                  (add-to-list 'org-agenda-files
                               (f-join dotspacemacs-directory
                                       "private"
                                       "brandonwillard-gcal.org"))))))

  (use-package plantuml-mode
    :init (setq plantuml-jar-path (expand-file-name "~/apps/plantuml.jar"))
    :config (progn
              (defun btw/plantuml-custom-graphvizdot-location (cmd-list)
                (append cmd-list (list "-graphvizdot" (executable-find "dot"))))
              (advice-add #'plantuml-render-command :filter-return
                          #'btw/plantuml-custom-graphvizdot-location)))

  (use-package embrace
    :init (progn
            (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
            (add-hook 'org-mode-hook 'embrace-org-mode-hook))
    :functions embrace-add-pair-regexp
    :config (progn
              (defun btw/embrace-emacs-lisp-mode-hook ()
                ;; (assq-delete-all ?f embrace--pairs-list)
                (defun embrace-with-function-elisp ()
                  (let ((fname (read-string "Function: ")))
                    (cons (format "(%s "
                                  (or fname "")) ")")))
                (embrace-add-pair-regexp ?f
                                         "(\\(\\sw\\|\\s_\\)+?\\s-+?"
                                         ")"
                                         'embrace-with-function-elisp
                                         (embrace-build-help "(function " ")")
                                         nil))
              (advice-add 'embrace-emacs-lisp-mode-hook
                          :after #'btw/embrace-emacs-lisp-mode-hook)
              (add-hook 'emacs-lisp-mode-hook 'embrace-emacs-lisp-mode-hook)))

  (use-package evil-embrace
    :init (evil-embrace-enable-evil-surround-integration))

  (use-package dockerfile-mode
    :mode ("Dockerfile\\'" . dockerfile-mode))

  (use-package sphinx-doc
    :defer t
    :commands (sphinx-doc sphinx-doc-mode)
    :init (progn
            (spacemacs/set-leader-keys-for-major-mode 'python-mode
              "rd" #'sphinx-doc)
            (defun btw/setup-sphinx-doc ()
              (sphinx-doc-mode t))
            (add-hook 'python-mode-hook #'btw/setup-sphinx-doc)))

  ;; This is also loaded by the `auto-complete' layer, but the directory
  ;; addition isn't present.
  ;; (with-eval-after-load yasnippet-snippets
  ;;   (add-to-list 'yas-snippet-dirs yasnippet-snippets-dir))

  (use-package yasnippet-snippets
    :after (yasnippet)
    :init (progn
            (add-to-list 'yas-snippet-dirs yasnippet-snippets-dir)))

  ;; (with-eval-after-load 'go-mode
  ;;   (setq godoc-command "godoc")
  ;;   (setq godoc-and-godef-command "godoc"))

  (when (fboundp 'kubernetes-overview)
    (spacemacs|define-custom-layout "@Kubernetes"
      :binding "K"
      :body (progn (kubernetes-overview))))

  (with-eval-after-load 'kubernetes
    (setq kubernetes-poll-frequency 30)
    (setq kubernetes-clean-up-interactive-exec-buffers nil))

  (with-eval-after-load 'vterm
    ;; (add-hook 'vterm-mode-hook
    ;;           (lambda ()
    ;;             (setq-local evil-insert-state-cursor 'box)
    ;;             (evil-insert-state)))
    (defun evil-collection-vterm-escape-stay ()
      "Go back to normal state but don't move cursor backwards.
Moving cursor backwards is the default vim behavior but
it is not appropriate in some cases like terminals."
      (setq-local evil-move-cursor-back nil))

    (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)
    (setq vterm-keymap-exceptions nil)
    (define-key vterm-mode-map [return] #'vterm-send-return)
    (evil-define-key 'insert vterm-mode-map
      (kbd "C-e") #'vterm--self-insert
      (kbd "C-z") #'vterm--self-insert
      (kbd "C-f") #'vterm--self-insert
      (kbd "C-a") #'vterm--self-insert
      (kbd "C-v") #'vterm--self-insert
      (kbd "C-b") #'vterm--self-insert
      (kbd "C-w") #'vterm--self-insert
      (kbd "C-u") #'vterm--self-insert
      (kbd "C-d") #'vterm--self-insert
      (kbd "C-n") #'vterm--self-insert
      (kbd "C-m") #'vterm--self-insert
      (kbd "C-p") #'vterm--self-insert
      (kbd "C-j") #'vterm--self-insert
      (kbd "C-k") #'vterm--self-insert
      (kbd "C-r") #'vterm--self-insert
      (kbd "C-t") #'vterm--self-insert
      (kbd "C-g") #'vterm--self-insert
      (kbd "C-c") #'vterm--self-insert
      (kbd "C-SPC") #'vterm--self-insert
      (kbd "<delete>") #'vterm-send-delete)
    (evil-define-key 'normal vterm-mode-map
      (kbd "[[") #'vterm-previous-prompt
      (kbd "]]") #'vterm-next-prompt
      (kbd "p") #'vterm-yank
      (kbd "u") #'vterm-undo
      (kbd "C-d") #'vterm--self-insert
      (kbd (concat dotspacemacs-major-mode-leader-key "c")) #'multi-vterm
      (kbd (concat dotspacemacs-major-mode-leader-key "n")) #'multi-vterm-next
      (kbd (concat dotspacemacs-major-mode-leader-key "p")) #'multi-vterm-prev
      (kbd "i") #'evil-insert-resume
      (kbd "o") #'evil-insert-resume
      (kbd "p") #'vterm-yank
      (kbd "P") #'vterm-yank
      ;; (kbd "<return>") #'evil-insert-resume
      ))

  (with-eval-after-load 'utop
    (setq utop-command "opam config exec -- utop -emacs")
    ;; (let ((camlp5-loc (string-trim (shell-command-to-string "camlp5 -where"))))
    ;;   (if camlp5-loc
    ;;       (setq utop-command (format "opam config exec -- utop -emacs -I %s camlp5o.pam" camlp5-loc))
    ;;     (setq utop-command "opam config exec -- utop -emacs")))
    (define-key utop-mode-map (kbd "C-j") 'comint-next-input)
    (define-key utop-mode-map (kbd "C-k") 'comint-previous-input)
    (define-key utop-mode-map (kbd "C-r") 'comint-history-isearch-backward)
    (define-key utop-mode-map (kbd "C-l") 'spacemacs/comint-clear-buffer))

  (with-eval-after-load 'racket-mode
    (when (fboundp 'purpose-set-extension-configuration)
      (purpose-set-extension-configuration
       :racket (purpose-conf :mode-purposes
                             '((racket-repl-mode . repl))))))

  (with-eval-after-load 'geiser
    (when (fboundp 'purpose-set-extension-configuration)
      (purpose-set-extension-configuration
       :scheme (purpose-conf :mode-purposes
                             '((geiser-repl-mode . repl)))))

    ;; Indent special macros/functions
    (cl-loop
     for x in '(rewrite-rules fresh match*)
     do (put x 'scheme-indent-function 1))

    (setq-default geiser-default-implementation 'racket))

  (with-eval-after-load 'overseer

    ;; Only ask to save file in the current `projectile' project.
    (setq overseer--save-buffers-predicate compilation-save-buffers-predicate)

    (defun btw/overseer-compilation-run (cmdlist buffer-name)
      "Run CMDLIST in BUFFER-NAME and returns the compilation buffer."

      (save-some-buffers (not compilation-ask-about-save) overseer--save-buffers-predicate)

      (let* ((overseer--buffer-name buffer-name)
             (compilation-filter-start (point-min)))
        (with-current-buffer
            (compilation-start (mapconcat 'concat cmdlist " ")
                               'overseer-buffer-mode
                               (lambda (_b) overseer--buffer-name))
          (set (make-local-variable 'compilation-error-regexp-alist)
               (cons 'overseer compilation-error-regexp-alist))
          ;; TODO: Add an entry to `compilation-error-regexp-alist-alist'!
          (add-hook 'compilation-filter-hook 'overseer--handle-ansi-color nil t)
          (add-hook 'compilation-filter-hook 'overseer--remove-header nil t))))

    ;; (advice-add #'overseer-compilation-run :override #'btw/overseer-compilation-run)

    (defun btw/overseer--current-buffer-test-file-p ()
      (string-match (rx (seq "-test" (optional "s") "\.el" eol))
                    (or (buffer-file-name) "")))

    (advice-add #'overseer--current-buffer-test-file-p :override
                #'btw/overseer--current-buffer-test-file-p))

  (with-eval-after-load 'semantic
    (setq semanticdb-search-system-databases nil)
    ;; (add-to-list 'semanticdb-project-root-functions #'projectile-project-root)
    )

  (with-eval-after-load 'evil-surround
    (setq-default evil-surround-pairs-alist
                  '((?\( . ("(" . ")"))
                    (?\[ . ("[" . "]"))
                    (?\{ . ("{" . "}"))

                    (?\) . ("( " . " )"))
                    (?\] . ("[ " . " ]"))
                    (?\} . ("{ " . " }"))

                    (?# . ("#{" . "}"))
                    (?b . ("(" . ")"))
                    (?B . ("{" . "}"))
                    (?> . ("<" . ">"))
                    (?t . evil-surround-read-tag)
                    (?< . evil-surround-read-tag)
                    (?f . evil-surround-function))))

  (with-eval-after-load 'erc
    (add-to-list 'erc-modules 'notifications)
    (setq erc-track-when-inactive t)
    (setq erc-track-remove-disconnected-buffers t)
    (setq erc-format-query-as-channel-p t
          erc-track-priority-faces-only 'all
          erc-track-faces-priority-list '(erc-error-face
                                          erc-current-nick-face
                                          erc-keyword-face
                                          erc-nick-msg-face
                                          erc-direct-msg-face
                                          erc-dangerous-host-face
                                          erc-notice-face
                                          erc-prompt-face))
    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477"))
    (setq erc-track-exclude-server-buffer t)
    (setq erc-track-visibility 'selected-visible)
    (setq erc-autojoin-channels-alist '((".*" "#hy")))
    (setq erc-auto-query 'window)
    (setq erc-track-enable-keybindings nil))

  (with-eval-after-load 'magit
    ;; Prevent `magit' from restoring unrelated window configurations (very
    ;; annoying when doing work with `magit' windows open).
    (setq magit-inhibit-save-previous-winconf t)
    (setq magit-repository-directories '(("~/" . 1)
                                         ("~/projects/code" . 3)
                                         ("~/projects/papers" . 3)
                                         ("~/projects/citybase" . 3))))

  (with-eval-after-load 'magithub-dash
    (setq magithub-dashboard-show-read-notifications nil))

  (with-eval-after-load 'editorconfig
    (add-to-list 'editorconfig-exclude-modes 'help-mode)
    (add-to-list 'editorconfig-exclude-modes 'edebug-mode)
    (add-to-list 'editorconfig-exclude-modes 'debugger-mode))

  (with-eval-after-load 'flycheck
    ;; TODO: Consider adding logic to `flycheck-python-find-module' that only
    ;; matches checker modules in the virtualenv (if any).
    ;; (add-to-list 'flycheck-disabled-checkers 'python-flake8)
    ;; (add-hook 'python-mode-hook
    ;;           #'(lambda () (add-to-list 'flycheck-disabled-checkers 'python-pylint)))
    (setq flycheck-indication-mode 'right-fringe))

  (with-eval-after-load 'python

    (defun btw//python-adjust-adaptive-fill-regexp ()
      (setq-local adaptive-fill-regexp
                  (s-replace "%" "" adaptive-fill-regexp)))

    (add-hook 'python-mode-hook #'btw//python-adjust-adaptive-fill-regexp)

    (when (fboundp 'purpose-set-extension-configuration)
      ;; NOTE: To delete this configuration...
      ;; (purpose-del-extension-configuration :python)
      (purpose-set-extension-configuration
       :python (purpose-conf :mode-purposes
                             '((python-mode . python)
                               (inferior-python-mode . repl))
                             ;; :regexp-purposes
                             ;; '(("^test-.*\\.py$" . test))
                             )))
    (setq-default python-eldoc-get-doc nil))
    (setq pytest-cmd-flags "-r A --verbose"))

  (with-eval-after-load 'hy-mode
    (spacemacs|use-package-add-hook evil-cleverparens
      :pre-init
      (add-to-list 'evil-lisp-safe-structural-editing-modes 'hy-mode))
    (when (fboundp 'purpose-set-extension-configuration)
      ;; NOTE: To delete this configuration...
      ;; (purpose-del-extension-configuration :hy)
      (purpose-set-extension-configuration
       :hy (purpose-conf :mode-purposes
                         '((hy-mode . hy)
                           (inferior-hy-mode . repl))
                         ;; :regexp-purposes
                         ;; '(("^test-.*\\.hy$" . test))
                         ))))

  (with-eval-after-load 'pyvenv
    ;; Set buffer local `pyvenv-workon' values for automatic activation.
    (when (fboundp 'pyvenv-tracking-mode)
      (setq pyvenv-tracking-ask-before-change t)
      ;; Make these buffer local so that virtualenvs don't creep into other
      ;; project buffers.
      (pyvenv-tracking-mode +1)))

  (with-eval-after-load 'vim-powerline-theme
    ;; Egh, doesn't really work.
    (setq winum-auto-setup-mode-line t)
    (setq powerline-default-separator 'bar))

  (with-eval-after-load 'bibtex
    (defun btw/find-project-bib-file (dir)
      "Finds an bib file within a projectile project."
      (let* ((project-root (with-demoted-errors "Error: %S" (projectile-project-root)))
             (file-name (f-glob "src/tex/*.bib" project-root)))
        (when (f-exists? file-name)
          file-name))))

  (with-eval-after-load 'org-ref
    (setq org-ref-bibliography-entry-format
          (add-to-list 'org-ref-bibliography-entry-format
                       '("misc" . "%a, %t, <i>%j</i>, %p (%y). <a href=\"%U\">link</a>.")))

    ;; Stop this library from making bad default choices
    (advice-add #'org-ref-show-link-messages :override (lambda ()))
    (advice-add #'org-ref-mouse-messages-on :override (lambda ()))
    (advice-add #'org-ref-mouse-message :override (lambda ()))

    (setq org-ref-pdf-directory "~/projects/papers/references"
          org-ref-bibliography-notes "~/projects/papers/references/notes.org"
          org-ref-prefer-bracket-links t))

  (with-eval-after-load 'lsp-mode

    ;; Temporary fix (until a PR takes care of this)
    ;; (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
    ;;   "bd" #'lsp-describe-session)
    (setq lsp-auto-guess-root t)
    (setq lsp-enable-snippet t)
    (setq lsp-document-sync-method lsp--sync-incremental)
    (setq lsp-enable-on-type-formatting nil)
    (setq lsp-before-save-edits nil)
    (setq lsp-eldoc-enable-hover nil)
    ;; Was `lsp-hover'
    (setq lsp-signature-auto-activate nil)
    (setq lsp-eldoc-hook '(lsp-document-highlight))
    (setq lsp-eldoc-render-all nil))

  (with-eval-after-load 'lsp-pyls
    (defun btw/lsp-pyls-library-folders-fn (_workspace)
      "Find workspaces based on virtualenvs.  Function which returns the
 folders that are considered to be not projects but library files.  "
      (if (fboundp 'spacemacs//run-in-pyvenv)
          (spacemacs//run-in-pyvenv
           (if python-shell-virtualenv-root
               (list python-shell-virtualenv-root)
             ;; TODO: Could just make this variable buffer-local.
             lsp-clients-python-library-directories))
        lsp-clients-python-library-directories))
    (let ((client
           (make-lsp-client :new-connection (lsp-stdio-connection
                                             ;; (lambda () lsp-pyls-server-command)
                                             "pyls")
                            :priority -1
                            :major-modes '(python-mode)
                            :server-id 'pyls
                            :initialized-fn (lambda (workspace)
                                              (with-lsp-workspace workspace
                                                (lsp--set-configuration (lsp-configuration-section "pyls"))))
                            :library-folders-fn #'btw/lsp-pyls-library-folders-fn)))
      (puthash (lsp--client-server-id client) client lsp-clients))
    ;; (setq lsp-pyls-plugins-pylint-enabled nil)
    )

  (with-eval-after-load 'lsp-ui
    (setq lsp-enable-symbol-highlighting nil)
    (setq lsp-ui-peek-enable nil)
    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-sideline-delay nil)
    (setq lsp-ui-sideline-show-hover nil)
    (setq lsp-ui-sideline-enable nil))

  (with-eval-after-load 'org
    ;; TODO: Consider this...
    ;; (org-babel-make-language-alias "python" "ipython")

    (defun btw//org-show-entry (&rest r)
      "Expand collapsed blocks when using goto-char."
      (when (eq major-mode 'org-mode)
        (save-excursion
          (org-reveal))))

    (advice-add 'goto-line :after #'btw//org-show-entry)
    (advice-add 'forward-line :after #'btw//org-show-entry)

    ;; This fixes the broken behavior when used within drawers.
    (defun btw//org-babel-result-end ()
      "Return the point at the end of the current set of results."
      (cond
       ((looking-at-p "^[ \t]*$")
        (point))                        ;no result
       ((looking-at-p (format "^[ \t]*%s[ \t]*$" org-bracket-link-regexp))
        (line-beginning-position 2))
       (t (let* ((element (org-element-at-point))
                 (elements (cons element (if (eq (org-element-type element) 'paragraph)
                                             (list (org-element-property :parent element))))))
            (if-let ((element (seq-find (lambda (x)
                                          (memq (org-element-type x)
                                                ;; Possible results types.

                                                '(drawer example-block export-block fixed-width
                                                         item plain-list src-block table)))
                                        elements)))
                (save-excursion
                  (goto-char (min (point-max) ;for narrowed buffers
                                  (org-element-property :end element)))
                  (skip-chars-backward " \r\t\n")
                  (line-beginning-position 2))
              (point))))))

    (advice-add #'org-babel-result-end :override #'btw//org-babel-result-end)

    (defun btw//org-pcompletions-hook ()
      "Enable `org-mode' completions in `company'."
      (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

    (add-hook 'org-mode-hook #'btw//org-pcompletions-hook)

    (spacemacs|add-company-backends :backends company-yasnippet
                                    :append-hook t
                                    :modes org-mode)

    (defvaralias 'org-plantuml-jar-path 'plantuml-jar-path)
    ;; (setq org-plantuml-jar-path plantuml-jar-path)

    (add-to-list 'org-babel-load-languages '(plantuml . t))
    (add-to-list 'org-babel-load-languages '(dot . t))
    (add-to-list 'org-babel-load-languages '(scheme . t))
    (add-to-list 'org-babel-load-languages '(jupyter . t) t)
    (add-to-list 'org-babel-load-languages '(latex . t) t)

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
             (TeX-process-asynchronous nil))
        ;; Runs `compile', which starts an asych process.  We need it to finish
        ;; *before* this function finishes; otherwise, `org-mode' will read any
        ;; existing output immediately and the update times will not differ,
        ;; making it believe that the compilation failed.
        ;; `TeX-command-buffer' is the output buffer.
        (TeX-command latex-build-command
                     (lambda (&rest _)
                       (f-filename (f-swap-ext file-name "pdf")))
                     -1)))

    (setq org-capture-templates
          '(("t" "Tasks" entry
             (file+headline org-default-notes-file "Tasks"))))

    (setq org-highlight-latex-and-related '(native))

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

    (setq org-latex-pdf-process #'spacemacs//org-latex-pdf-process)

    ;; What to allow before and after markup
    ;; See https://emacs.stackexchange.com/a/13828
    ;; (setcar (nthcdr 1 org-emphasis-regexp-components)
    ;;         (concat (nth 0 org-emphasis-regexp-components) "s"))
    (setq org-link-file-path-type 'relative)
    (setq org-confirm-babel-evaluate nil)
    (setq org-default-notes-file (f-join user-home-directory "Documents" "notes.org")))

  (with-eval-after-load 'org-eldoc
    ;; Prevent a stupid eldoc loop when the cursor is on the source in a Python
    ;; block.
    (puthash "python" nil org-eldoc-local-functions-cache))

  (with-eval-after-load 'tex

    ;; (add-to-list 'preview-default-option-list "amsmath")
    ;; (add-to-list 'preview-default-option-list "amsfonts")
    ;; (add-to-list 'preview-default-option-list "amssymb")
    ;; (add-to-list 'preview-default-option-list "mathtools")
    ;; (add-to-list 'preview-default-option-list "amsthm")
    ;; (add-to-list 'preview-default-preamble "\PreviewEnvironment{align}" t)

    (defvar TeX-command-list)
    (add-to-list 'TeX-command-list
                 '("Make" "make %o" TeX-run-command nil t))
    ;; XXX: Must have this set in the spacemacs tex layer!
    (setq TeX-command-default "Make"))

  (with-eval-after-load 'projectile
    ;; Don't bother us with unsaved files in other projects when we try to compile.
    ;; TODO: This should probably be set in `projectile--run-project-cmd'.
    (setq compilation-save-buffers-predicate
          (lambda ()
            (when-let ((project-root (projectile-project-root)))
              (projectile-project-buffer-p (current-buffer) project-root))))

    (defun btw/projectile-switch-project-by-name (project-to-switch &optional arg)
      "This version *doesn't* pre-load the dir-locals."
      (unless (projectile-project-p project-to-switch)
        (projectile-remove-known-project project-to-switch)
        (error "Directory %s is not a project" project-to-switch))
      (let ((switch-project-action (if arg
                                       'projectile-commander
                                     projectile-switch-project-action)))
        (run-hooks 'projectile-before-switch-project-hook)
        (let* ((default-directory project-to-switch)
               (projectile-project-name (funcall projectile-project-name-function
                                                 project-to-switch)))
          ;; (with-temp-buffer
          ;;   (hack-dir-local-variables-non-file-buffer))
          (funcall switch-project-action)
          ;; Default to VCS when/if `helm-projectile' is aborted.
          (if (and (string-prefix-p "helm"
                                    (symbol-name switch-project-action))
                   (eq helm-exit-status 1))
              (projectile-vc project-to-switch)))
        (run-hooks 'projectile-after-switch-project-hook)))

    (advice-add #'projectile-switch-project-by-name
                :override #'btw/projectile-switch-project-by-name)

    (setq projectile-project-search-path
          (mapcar #'expand-file-name
                  '("~/projects/code/python" "~/projects/code/emacs" "~/projects/citybase" "~/projects/papers")))
    (setq projectile-indexing-method 'hybrid)

    (setq projectile-globally-unignored-files '("TODO.org" "TODOs.org"))
    (setq projectile-globally-ignored-directories
          (delete-dups (append projectile-globally-ignored-directories
                               (list ".ropeproject" ".cache" "__pycache__"
                                     ".pytest_cache" ".mypy_cache"
                                     "src/tex/.build" "src/tex/_minted" "src/tex/_minted-.build"
                                     ;; Directory pattern.
                                     ;; (rx "_minted" (* any))
                                     ;; (rx (+ (or alnum digit blank "." "/" "-" "_"))
                                     ;;     "/_minted" (* any))
                                     ))))
    (setq projectile-tags-command "/usr/bin/ctags -Re -f \"%s\" %s \"%s\"")
    (setq projectile-tags-file-name ".TAGS")
    (setq projectile-use-git-grep nil))

  (with-eval-after-load 'org-projectile
    (setq org-projectile-capture-template "* TODO %?
  %u
  %a"))

  (with-eval-after-load 'hideshow

    (defun btw//hs-show-block-rec ()
      "Unfold/show all blocks at point."
      (while (hs-already-hidden-p)
        (save-mark-and-excursion
          (hs-show-block))))

    ;; Add a recursive unfold to `evil''s mappings for `hideshow'
    (when-let ((opt (assoc-if (lambda (x) (memq 'hs-minor-mode x)) evil-fold-list)))
      (setf (cdr opt) (plist-put (cdr opt) :open-rec #'btw//hs-show-block-rec)))

    ;; (cursor-sensor-mode +1)
    ;;
    ;; (defun btw//hs-show-on-jump (window prev-pos motion)
    ;;   ;; TODO
    ;;   (when (eq motion 'entered)
    ;;     (let ((cursor-sensor-inhibit t))
    ;;       (message "%s" (point))
    ;;       (save-mark-and-excursion
    ;;         (hs-show-block))
    ;;       (message "%s" (point)))))
    ;;
    ;; (defun btw//hs-show-on-jump-overlay (ov)
    ;;   (overlay-put ov 'cursor-sensor-functions '(btw//hs-show-on-jump))
    ;;   ;; (when (eq 'code (overlay-get ov 'hs))
    ;;   ;;   (overlay-put ov 'cursor-sensor-functions '(btw//hs-show-on-jump)))
    ;;   )
    ;;
    ;; (setq hs-set-up-overlay #'btw//hs-show-on-jump-overlay)

    (setq hs-allow-nesting t)

    ;; Let's not lose the cursor position when folding.
    (defun btw//apply-in-save-mark-excursion (oldfun &rest r)
      (save-mark-and-excursion
        (apply oldfun r)))

    (advice-add 'hs-hide-block :around #'btw//apply-in-save-mark-excursion)
    (advice-add 'hs-show-block :around #'btw//apply-in-save-mark-excursion)

    ;; TODO The meaning of "z[r|m]" is "level-folding" in Vim, but `evil-commands' has
    ;; no notion of this.  For `hideshow' we can use `hs-show-level' and `hs-hide-level'
    ;; to better approximate level-folding, but we would still have to work that into
    ;; evil's framework via `evil-fold-list' (e.g. new level-folding properties--perhaps
    ;; with fall-backs, too).
    ;; (evil-global-set-key 'normal "zr" 'hs-show-level)
    (evil-global-set-key 'normal "zm" 'hs-hide-level))

  (evil-global-set-key 'normal "zR" 'evil-open-folds)
  (evil-global-set-key 'normal "zM" 'evil-close-folds)

  (with-eval-after-load 'evil-lisp-state
    ;; Let's get rid of that annoying mode switching.
    (dolist (x evil-lisp-state-commands)
      (let ((key (car x))
            (cmd (cdr x)))
        (eval
         `(progn
            (if evil-lisp-state-global
                (define-key evil-lisp-state-map ,(kbd key) (quote ,cmd))
              (define-key evil-lisp-state-major-mode-map ,(kbd key) (quote ,cmd)))))))

    ;; (spacemacs/set-leader-keys "k" evil-lisp-state-map)
    )

  (with-eval-after-load 'persp-mode
    (setq persp-autokill-buffer-on-remove t)
    ;; Add all opened buffers (filter certain ones below).
    (setq persp-add-buffer-on-after-change-major-mode t)

    (defun btw/persp-assign-projectile-root (persp persp-hash)
      "Add a variable to the perspective tracking the projectile project name
 (if any).

 This is run before the buffer is created, so we need to get the project name
 from this perspective's path.  We assume the perspective's name is the project
 path (which it is per Spacemacs)"
      (let* ((persp-name (safe-persp-name persp))
             (persp-projectile-dir (when (and (f-dir? persp-name)
                                              (funcall projectile-project-name-function
                                                       persp-name))
                                     persp-name)))
        (set-persp-parameter 'projectile-project-root
                             persp-projectile-dir
                             persp)))

    (add-hook 'persp-created-functions #'btw/persp-assign-projectile-root)

    (defun btw/persp-projectile-project-root (oldfun &rest r)
      "Use the perp project name and regular `projectile-project-root' as a
 fallback."
      (let* ((persp-name (spacemacs//current-layout-name))
             (persp-projectile-dir (when (and (f-dir? persp-name)
                                              (funcall projectile-project-name-function
                                                       persp-name))
                                     persp-name)))
        ;; If the persp name is a directory and is mapped to a projectile project,
        ;; return the directory; otherwise, use the fallback.
        (or persp-projectile-dir
            (persp-parameter 'projectile-project-root)
            (apply oldfun r))))

    (advice-add #'projectile-project-root :around #'btw/persp-projectile-project-root)

    (defun btw/persp-projectile-project-name (oldfun &rest r)
      "Query the persp layout for the projectile project name and use projectile
 for the fallback."
      (let* ((persp-name (spacemacs//current-layout-name))
             (persp-projectile-name (if (f-dir? persp-name)
                                        (funcall projectile-project-name-function
                                                 persp-name)
                                      (persp-parameter 'projectile-project-root))))
        (or persp-projectile-name (apply oldfun r))))

    (advice-add #'projectile-project-name :around #'btw/persp-projectile-project-name)

    ;; (setq persp-set-ido-hooks nil)

    (defun btw/persp-restrict-ido-buffers (&rest r)
      "Remove `nil' buffer names from the returned results.
This fixes some `helm' issues."
      (setq ido-temp-list (remove nil ido-temp-list)))

    (advice-add #'persp-restrict-ido-buffers :after #'btw/persp-restrict-ido-buffers)

    ;; (add-hook 'persp-common-buffer-filter-functions
    ;;           ;; there is also `persp-add-buffer-on-after-change-major-mode-filter-functions'
    ;;           #'(lambda (b) (string-prefix-p "*" (buffer-name b))))

    ;; Restore eshell buffers.
    ;; See https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-inferior-python-save-load-el
    ;; for more examples (e.g. Python inferior shells).
    (persp-def-buffer-save/load
     :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
     :save-vars '(major-mode default-directory))

    (defun btw/projectile-shell-pop ()
      "Open a term buffer at projectile project root for the current perspective."
      (let* ((current-persp-name (spacemacs//current-layout-name))
             (persp-idx (seq-find #'identity
                                  (seq-map-indexed (lambda (name i)
                                                     (if (eq name current-persp-name)
                                                         i
                                                       nil))
                                                   (persp-names-current-frame-fast-ordered))))
             (shell (if (eq 'multi-term shell-default-shell)
                        'multiterm
                      shell-default-shell))
             (shell-pop-func (intern (format "spacemacs/shell-pop-%S" shell))))
        (funcall shell-pop-func persp-idx)))

    (advice-add #'spacemacs/projectile-shell-pop :override #'btw/projectile-shell-pop))

  (with-eval-after-load 'comint
    ;; Make terminals and REPLs read-only.
    ;; https://emacs.stackexchange.com/a/2897
    (setq-default comint-prompt-read-only t)
    (setq-default comint-use-prompt-regexp nil)
    (setq-default inhibit-field-text-motion nil)
    (setq-default comint-move-point-for-output t)

    (defun btw/comint-maybe-goto-prompt ()
      (when (and (derived-mode-p 'comint-mode)
                 ;; `term-mode' has a different idea of pmark.
                 (not (derived-mode-p 'term-mode))
                 (not (comint-after-pmark-p)))
        (comint-goto-process-mark)
        (move-end-of-line nil)))

    (add-hook 'evil-insert-state-entry-hook
              #'btw/comint-maybe-goto-prompt)
    (add-hook 'evil-hybrid-state-entry-hook
              #'btw/comint-maybe-goto-prompt)

    (defun btw/comint-preoutput-turn-buffer-read-only (text)
      (propertize text 'read-only t))

    (add-hook 'comint-output-filter-functions
              #'btw/comint-preoutput-turn-buffer-read-only
              'append))

  (with-eval-after-load 'evil

    (defun btw//evil-open-on-movement (fn &rest r)
      "Expand collapsed blocks after FN moves the point."
      (let ((start-point (point)))
        (prog1
            (apply fn r)
          (unless (eq start-point (point))
            (ignore-errors
              (evil-open-fold-rec))))))

    ;; XXX: These are too aggressive.
    ;; (advice-add #'goto-line :after #'btw//evil-open-on-movement)
    ;; (advice-add #'goto-char :after #'btw//evil-open-on-movement)
    ;; (advice-add #'forward-line :after #'btw//evil-open-on-movement)
    ;; (advice-add #'forward-char :after #'btw//evil-open-on-movement)

    (advice-add #'evil-goto-line :around #'btw//evil-open-on-movement)
    (advice-add #'spacemacs/jump-to-definition :around #'btw//evil-open-on-movement)
    (advice-add #'primitive-undo :around #'btw//evil-open-on-movement)
    (advice-add #'helm-ag--find-file-action :around #'btw//evil-open-on-movement)

    (add-hook 'xref-after-jump-hook #'evil-open-fold-rec)

    ;; Attempt to open folds when jumping into a folded area.
    (setq evil-jumps-post-jump-hook (cons #'evil-open-fold-rec evil-jumps-post-jump-hook))


    (setq-default evil-want-visual-char-semi-exclusive t)
    (setq-default evil-move-curser-back nil)
    (setq-default evil-escape-key-sequence nil)
    ;; (setq-default evil-emacs-state-modes nil)
    (setq-default evil-insert-state-modes '(magit-popup-mode))
    (setq evil-kill-on-visual-paste nil)
    ;; (setq-default evil-motion-state-modes nil)

    ;; Make evil-mode up/down operate in screen lines instead of logical lines
    (define-key evil-motion-state-map "j" 'evil-next-visual-line)
    (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
    ;; Also in visual mode
    (define-key evil-visual-state-map "j" 'evil-next-visual-line)
    (define-key evil-visual-state-map "k" 'evil-previous-visual-line)

    ;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
    (defalias #'forward-evil-word #'forward-evil-symbol))

  (with-eval-after-load 'yasnippet
    ;; (setq yasnippet-snippets-dir (f-join default-directory "private" "snippets"))
    )

  (with-eval-after-load 'company
    (setq company-dabbrev-other-buffers nil)
    (setq company-search-filtering t)
    (setq company-idle-delay nil)
    ;; (setq company-backends-emacs-lisp-mode
    ;;       (cons '(company-elisp :with company-yasnippet)
    ;;             (seq-remove (lambda (x) (eq (car x) 'company-capf))
    ;;                         company-backends-emacs-lisp-mode)))
    (spacemacs|add-company-backends :backends (company-elisp)
                                    :modes emacs-lisp-mode)
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
    (define-key company-active-map (kbd "C-y") 'company-complete-selection)
    (define-key evil-insert-state-map (kbd "C-n") #'company-select-next)
    (define-key evil-insert-state-map (kbd "C-p") #'company-select-previous))

  (with-eval-after-load 'window-purpose
    ;; This was commented out in `purpose--fix-helm' from `window-purpose-fixes.el'.
    (with-eval-after-load 'helm
      (add-to-list 'purpose-action-function-ignore-buffer-names "^\\*Helm"))
    (with-eval-after-load 'helm
      (add-to-list 'purpose-action-function-ignore-buffer-names "^\\*helm"))
    (with-eval-after-load 'helm
      (purpose-set-extension-configuration :helm purpose--helm-conf)))

  (with-eval-after-load 'window-purpose-switch
    (setq purpose-display-at-bottom-height 0.4))

  (with-eval-after-load 'helm
    (setq-default helm-follow-mode-persistent nil)
    (setq helm-always-two-windows nil)
    (setq helm-autoresize-mode nil)
    (setq helm-split-window-inside-p nil)
    (setq helm-split-window-default-side 'below)

    ;; NOTE: `window-purpose-switch' can destroy expected
    ;; pop-up/window placement behavior; look at the default
    ;; values for vars like `purpose-display-at-bottom-height'
    ;; if there are problems.

    (define-key helm-map (kbd "C-w") 'evil-delete-backward-word))

  (with-eval-after-load 'evil-jumps
    (setq evil-jumps-cross-buffers nil))

  (with-eval-after-load 'c++-mode
    (defun btw/clang-format-bindings ()
      (define-key c++-mode-map [tab] 'clang-format-buffer)
      (global-set-key [C-M-tab] 'clang-format-region))

    (add-hook 'c++-mode-hook 'btw/clang-format-bindings))

  (with-eval-after-load 'term
    ;; TODO: See https://github.com/emacs-evil/evil-collection and
    ;; `evil-collection-term-sync-state-and-mode-p'.
    ;; For now, try some of the additions from this PR:
    ;; https://github.com/syl20bnr/spacemacs/pull/10844
    ;; TODO: Remove when merged.
    (setq shell-default-full-span nil)

    (defun btw/term-enable-line-mode ()
      "Enable `term-line-mode' when in `term-mode' buffer."
      (when (eq major-mode 'term-mode)
        (term-line-mode)))

    (defun term-after-prompt-p ()
      "Is point after the prompt?"
      (let ((cur-point (point)))
        (save-mark-and-excursion
          (goto-char (term-process-mark))
          (term-bol nil)
          (<= (point) cur-point))))

    (setq-default term-char-mode-point-at-process-mark nil)

    (defun btw/term-enable-char-mode-maybe-goto-prompt ()
      (when (and (eq major-mode 'term-mode)
                 (get-buffer-process (current-buffer)))
        (let ((term-char-mode-point-at-process-mark nil))
          (unless (term-after-prompt-p)
            (goto-char (term-process-mark)))
          ;; TODO: set process mark to the end of any existing text after the prompt.

          ;; Stop the mode initialization code from sending the current input.
          ;; (flet ((term-send-string (&rest r) nil))
          ;;   (term-char-mode))

          (term-char-mode)

          ;; (save-mark-and-excursion
          ;;   (goto-char (point-max))
          ;;   (when (= (line-beginning-position) (line-end-position))
          ;;     (ignore-errors (backward-char)))
          ;;   ;; (set-marker (process-mark (get-buffer-process (current-buffer))) (term-previous-prompt 1))
          ;;   ;; (set-marker (process-mark (get-buffer-process (current-buffer))) (point))
          ;;   ;; (setq last-prompt (max (term-bol nil) (line-beginning-position)))
          ;;
          ;;   )
          )))

    (add-hook 'evil-insert-state-entry-hook
              #'btw/term-enable-char-mode-maybe-goto-prompt)
    (add-hook 'evil-hybrid-state-entry-hook
              #'btw/term-enable-char-mode-maybe-goto-prompt)
    (add-hook 'evil-normal-state-entry-hook
              #'btw/term-enable-line-mode)

    (declare-function term-send-raw-string "term.el")
    (declare-function term-send-del "term.el")

    (defun btw/send-C-r ()
      (interactive)
      (term-send-raw-string "\C-r"))

    ;; This allow us to use the same global shortcut for `eval-expression'.
    (unbind-key "M-:" term-raw-map)

    (defun btw/setup-term-mode ()
      ;; (setq-local fringe-mode nil)
      ;; (setq-local fringes-outside-margins t)
      ;; (setq-local mouse-yank-at-point t)
      ;; (setq-local transient-mark-mode nil)
      ;; (setq tab-width 8 )
      ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] ")
      (setq term-prompt-regexp "^.* [#$%>»] ")
      (auto-fill-mode -1)
      (evil-local-set-key 'insert (kbd "<delete>") #'term-send-del)
      ;; From https://github.com/syl20bnr/spacemacs/issues/2345
      (evil-local-set-key 'insert (kbd "C-r") #'btw/send-C-r))

    (add-hook 'term-mode-hook #'btw/setup-term-mode)

    (defvar term-width)
    ;; This was apparently renamed.
    ;; (defvar term-terminal-parameter)
    ;; (define-obsolete-variable-alias 'term-terminal-parameter 'term-terminal-undecoded-bytes)
    (defvar term-terminal-undecoded-bytes)
    (declare-function term-move-columns "term.el")
    (declare-function term-current-column "term.el")

    (defun btw/term-handle-more-ansi-escapes (proc params char)
      "Handle additional ansi escapes.
From https://emacs.stackexchange.com/a/10698"
      (cond
       ;; \E[nG - Cursor Horizontal Absolute, e.g. move cursor to column n
       ((eq char ?G)
        (let ((col (min term-width
                        (max 0 (or term-terminal-undecoded-bytes 0)))))
          (term-move-columns (- col
                                (term-current-column)))))
       (t)))

    (advice-add 'term-handle-ansi-escape :before #'btw/term-handle-more-ansi-escapes))

  (with-eval-after-load 'sql

    (setq sqlfmt-options '())

    ;; (defun btw//sql-connect (name)
    ;;   (interactive (list
    ;;                 (sql-read-connection "Connection: " nil '(nil))))
    ;;   (let* ((sql-product
    ;;           (or (cadadr
    ;;                (assoc 'sql-product (cdr (assoc name sql-connection-alist))))
    ;;               sql-product)))
    ;;     (sql-connect name name)))
    ;;
    ;; (advice-add #'sql-connect :override #'btw//sql-connect)

    (setq sql-connection-alist '())
    (add-to-list 'sql-connection-alist
                 `("datalake-rw" .
                   ((sql-product 'postgres)
                    (sql-database ,(format "postgresql://ds_admin:%s@localhost:54320/datalake"
                                           (shell-command-to-string "vault kv get -field=password /secret/dev/datascience/data-lake"))))))

    (setq sql-send-terminator t)

    (defvar sql-last-prompt-pos 1
      "position of last prompt when added recording started")
    (make-variable-buffer-local 'sql-last-prompt-pos)
    (put 'sql-last-prompt-pos 'permanent-local t)

    (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
    (sql-set-product-feature 'postgres :prompt-cont-regexp
                             "^[-[:alnum:]_]*[-(][#>] "))

  (spacemacs|define-custom-layout "@Spacemacs"
    :binding "e"
    :body (progn (spacemacs/find-dotfile)
                 (set-persp-parameter 'projectile-project-root
                                      (ignore-errors (projectile-project-root)))
                 ;; (set-window-dedicated-p (get-buffer-window) t)
                 (display-buffer-in-side-window (messages-buffer) '((side . right)))
                 (balance-windows-area)))

  ;; Initialization steps

  (defun btw/after-user-config-setup ()
    ;; (require 'frame-cmds)
    ;; (enlarge-font -3)
    ;; (debug-on-entry #'enlarge-font)
    ;; (persp-mode +1)
    (add-hook 'persp-mode-hook
              (lambda ()
                (spacemacs/custom-perspective-@Spacemacs)))
    (when (file-exists-p custom-file)
      (load-file custom-file)))

  (spacemacs/defer-until-after-user-config
   #'btw/after-user-config-setup))
