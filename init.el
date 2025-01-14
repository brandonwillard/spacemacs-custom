;; -*- mode: emacs-lisp; lexical-binding: t -*-

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   dotspacemacs-configuration-layers
   '(javascript
     rust
     ;; (rust :packages (not flycheck-rust))
     ;; javascript
     ;; kubernetes
     coq
     sphinx
     ;; terraform
     ;; eww
     gnus
     ;; helpful
     ;; ocaml
     ;; elixir
     ;; javascript
     ;; clojure
     ;; go
     csv
     restructuredtext
     ;; (javascript :packages (not tern))
     (lsp :variables
          lsp-ui-doc-enable nil
          lsp-ui-sideline-enable nil
          ;; lsp-ui-remap-xref-keybindings t
          lsp-headerline-breadcrumb-segments '(file symbols)
          lsp-navigation 'simple
          ;; :packages (not flycheck-lsp)
          )

     (spacemacs-evil :variables
                     spacemacs-evil-collection-allowed-list
                     '((buff-menu "buff-menu") forge magit vterm eww dired quickrun ediff info
                                        ; company debug edebug debbugs
                       ))
     html
     markdown
     (latex :variables
            latex-build-command "Make")
     bibtex
     graphviz
     ;; ess
     ;; (ess :variables
     ;;      ess-disable-underscore-assign t
     ;;      :packages (not ess-R-object-popup))
     (python :variables
             ;; NOTE: These can also be .dir-local/project specific.
             python-test-runner 'pytest
             python-backend 'lsp
             python-lsp-server 'pylsp
             python-formatter 'black
             python-format-on-save nil
             :packages (not live-py-mode))
     (python-extras :variables
                    python-auto-set-local-pyvenv-virtualenv 'on-project-switch)
     (hy :variables hy-shell-spy-delim "\n--spy-output--\n")
     yaml
     sql
     ;; noweb
     (c-c++ :variables
            c-c++-backend 'lsp-clangd
            ;; company-c-headers-path-user '("../include" "./include" "." "../../include"
            ;;                               "../inc" "../../inc")
            c-c++-enable-clang-support t
            c-c++-default-mode-for-headers 'c++-mode)
     ;; (helm :variables helm-enable-auto-resize nil helm-position 'bottom)
     (ivy :variables ivy-enable-advanced-buffer-information t)
     (auto-completion :variables
                      ;; auto-completion-enable-sort-by-usage t
                      spacemacs-default-company-backends '((:separate company-capf company-yasnippet company-files))
                      ;; ((company-semantic company-dabbrev-code company-gtags company-etags company-keywords) company-files company-dabbrev)
                      auto-completion-return-key-behavior nil
                      auto-completion-idle-delay nil
                      auto-completion-tab-key-behavior nil
                      auto-completion-complete-with-key-sequence "C-y"
                      auto-completion-private-snippets-directory nil
                      auto-completion-use-company-box t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip 'manual)
     emacs-lisp
     git
     (version-control :variables version-control-global-margin nil)
     scheme
     racket
     pdf
     (org :variables
          org-enable-org-journal-support t
          org-journal-dir "~/Documents/journal"
          org-journal-file-format "%Y%m%d.org"
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
     ;; common-lisp
     asm)

   ;; FYI: You can use MELPA recipes here (i.e. https://github.com/melpa/melpa#recipe-format).
   dotspacemacs-additional-packages '(;; elisp file manipulation library
                                      f
                                      ;; elisp string manipulation library
                                      s
                                      string-inflection
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


                                      (llvm-mode :location "~/projects/code/emacs/llvm-mode")

                                      cython-mode
                                      jupyter
                                      ;; importmagic
                                      python-docstring

                                      geiser-racket

                                      ;; TODO: Broken; see https://github.com/doomemacs/doomemacs/issues/7191
                                      ;; code-review

                                      ;; evil-textobj-tree-sitter

                                      multi-vterm
                                      ;; (multi-vterm :location (recipe :fetcher github
                                      ;;                                :repo "suonlight/multi-vterm"))

                                      (ob-racket :location (recipe :fetcher github
                                                                   :repo "wallyqs/ob-racket"))

                                      (org-btw :location "~/projects/code/emacs/org-btw")
                                      (hy-mode :location "~/projects/code/emacs/hy-mode")
                                      (org-ref :location "~/projects/code/emacs/org-ref")
                                      (ob-hy :location "~/projects/code/emacs/ob-hy")
                                      (proj-persp-extras :location "~/projects/code/emacs/proj-persp-extras")
                                      (pyvenv-extras :location "~/projects/code/emacs/pyvenv-extras")
                                      (python-btw :location "~/projects/code/emacs/python-btw")

                                      ;; coterm

                                      ;; TODO: This is a temporary fix for a
                                      ;; `forge' dependency; remove me.
                                      sqlite3

                                      ;; (copilot :location (recipe
                                      ;;                     :fetcher github
                                      ;;                     :repo "zerolfx/copilot.el"
                                      ;;                     :files ("*.el" "dist")))

                                      yasnippet-snippets
                                      ;; Use a newer version of python.el.
                                      (python :location elpa :min-version "0.26.1"))
   dotspacemacs-undo-system 'undo-tree
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(hl-todo company-emoji emoji-cheat-sheet-plus emojify)
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory 'emacs-version
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading t
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists nil
   dotspacemacs-startup-buffer-responsive t
   ;; dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-mode-line-theme 'spacemacs
   dotspacemacs-default-font '("JuliaMono"
                               :size 12.0
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
   dotspacemacs-which-key-delay 0.7
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
   dotspacemacs-whitespace-cleanup 'trailing
   dotspacemacs-import-env-vars-shell-file-name shell-file-name
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-read-process-output-max (* 1 1024 1024)
   dotspacemacs-gc-cons `(,(* 100 1024 1024) 0.1)))

(defun dotspacemacs/user-init ()

  ;; A temporary fix for bleeding edge Emacs's dropping `defmethod'
  (require 'eieio-compat)

  ;; Another temporary fix for bleeding edge Emacs's changes
  (setq read-symbol-positions-list nil)
  (setq read-with-symbol-position nil)

  ;; A speed improvement over `hash-table'-usage?
  ;; (setq lsp-use-plists nil)

  ;; Prevent this annoying command from making the Emacs frame disappear
  (put 'suspend-frame 'disabled t)

  (setq delete-by-moving-to-trash nil)

  ;; (jit-lock-debug-mode +1)
  (setq jit-lock-defer-time 0)

  (setq comp-async-jobs-number 4
        comp-deferred-compilation t
        comp-deferred-compilation-black-list '())

  (setq history-delete-duplicates t)

  (setq auto-save-timeout nil)
  (setq init-file-debug nil)
  (setq debug-on-error nil)
  (setq debug-on-quit nil)

  ;; Helps with delays while handling very long lines.
  (setq-default bidi-display-reordering nil)
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq-default bidi-inhibit-bpa t)
  (setq debugger-stack-frame-as-list t)
  (setq edebug-print-circle t)
  (setq edebug-print-level 20)
  (setq print-circle t)

  ;; Let's not get inundated with pop-ups for warnings
  (setq warning-minimum-level :error)

  ;; This will help avoid errors with old `ert'-based code
  (when (> emacs-major-version 26)
    (defalias 'ert--print-backtrace 'backtrace-to-string))

  (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))

  ;; TODO: Set these for Python venv variables?
  ;; (setq safe-local-variable-values ...)
  ;; (setq safe-local-eval-forms ...)

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

  (add-to-list 'exec-path (expand-file-name (concat (expand-file-name "~")
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

  (setq browse-url-browser-function nil)
  (setq browse-url-handlers '((".*slack.*" . browse-url-chrome)
                              (".*youtube.*" . browse-url-chrome)
                              ("." . browse-url-chrome)))
  ;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)

  ;; Be more permissive about the accepted forms of version strings
  (add-to-list 'version-regexp-alist '("^[-._+ ]?[dD]ev[i]?$" . -4)))

(defun dotspacemacs/user-config ()

  ;; TODO: Hack fix; This fixes an issue introduced by the new built-in `restart-emacs' function
  (require 'restart-emacs)

  (blink-cursor-mode)

  (with-eval-after-load 'x86-lookup
    (setq x86-lookup-pdf "~/projects/papers/references/325462-sdm-vol-1-2abcd-3abcd.pdf"))

  ;; powerline (and potentially other packages) use an old(?)
  ;; interface to `create-image' that allows `:scale t', but, now
  ;; `:scale' values must be numeric, so we make `create-image'
  ;; capable of handling both types of `:scale' values.
  (defun btw//create-image (orig-fun file-or-data &optional type data-p &rest props)
    (let* ((scale (plist-get props :format)))
      (when (booleanp scale)
        (cl-remf props :scale))
      (apply orig-fun (append (list file-or-data type data-p) props))))

  (advice-add #'create-image :around #'btw//create-image)

  ;; Make word motions include underscores
  (add-hook 'prog-mode-hook #'(lambda ()
                                ;; (modify-syntax-entry ?_ "w")
                                (superword-mode +1)))

  ;; Just a helper function for whatever.
  (defun hash-table-to-alist (hash-table)
    (json-read-from-string (json-encode hash-table)))

  ;; TODO: Hack fix; consider fixing, and then removing, this.
  (defvar string-edit-mode nil)

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

  ;; (setq default-input-method "TeX")
  (setq default-input-method nil)
  (setq comment-empty-lines t)
  (setq evil-move-beyond-eol t)
  (setq evil-search-wrap nil)
  (setq-default evil-symbol-word-search t)
  (setq-default evil-shift-round nil)

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
  (add-to-list 'debug-ignored-errors "^Invalid register$")
  (add-to-list 'debug-ignored-errors "^Candidates function .*helm-ag")
  (add-to-list 'debug-ignored-errors "^Before first heading$")
  (add-to-list 'debug-ignored-errors "^Nothing to complete$")
  (add-to-list 'debug-ignored-errors "^No such page: ")
  (add-to-list 'debug-ignored-errors "^Beginning of history$")
  (add-to-list 'debug-ignored-errors "^End of history$")
  (add-to-list 'debug-ignored-errors "^No surrounding delimiters found$")
  (add-to-list 'debug-ignored-errors "^Invalid search bound (wrong side of point)$")
  (add-to-list 'debug-ignored-errors "^Selecting deleted buffer$")
  (add-to-list 'debug-ignored-errors "ValueError: ‘line‘ parameter is not in a valid range")
  (add-to-list 'debug-ignored-errors
               "^Company: backend \(:?.*?\) error \"Nothing to complete\"")
  (add-to-list 'debug-ignored-errors 'lsp-timed-out-error)
  (add-to-list 'debug-ignored-errors
               "^Candidates function ‘helm-ag--do-ag-candidate-process’ should run a process")
  (add-to-list 'debug-ignored-errors
               "^Current buffer has no process")
  (add-to-list 'debug-ignored-errors
               "Attempt to delete minibuffer or sole ordinary window")
  (add-to-list 'debug-ignored-errors
               "Hash table data is not a list of even length")

  (setq-default sentence-end-double-space t)

  ;; XXX: This will stop completion with TAB (but also fix annoying noops when
  ;; attempting to indent lines).
  (setq tab-always-indent t)
  (setq fill-indent-according-to-mode t)

  ;; (spacemacs/toggle-aggressive-indent-on)

  (setq compilation-scroll-output #'first-error)

  ;; Change default spacemacs keybinding.
  (define-key minibuffer-local-map (kbd "C-n") #'next-history-element)
  (define-key minibuffer-local-map (kbd "C-p") #'previous-history-element)
  (spacemacs/set-leader-keys "nd" 'narrow-to-defun)
  (spacemacs/set-leader-keys "kx" 'sp-split-sexp)
  (spacemacs/set-leader-keys "hds" 'describe-symbol)
  (unbind-key (kbd "nf") spacemacs-default-map)

  ;; (require 'treesit)
  (add-to-list 'treesit-extra-load-path (f-canonical "~/projects/code/rust/tree-sitter/modules/dist"))
  ;; (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

  ;; (with-eval-after-load 'tree-sitter
  ;;   (global-tree-sitter-mode)
  ;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
  ;;
  ;; (use-package evil-textobj-tree-sitter
  ;;   :disabled t
  ;;   :ensure t
  ;;   :config (progn
  ;;             ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  ;;             (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;;             ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  ;;             (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  ;;
  ;;             ;; You can also bind multiple items and we will match the first one we can find
  ;;             (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))))

  ;; (with-eval-after-load 'copilot
  ;;   (add-hook 'prog-mode-hook 'copilot-mode)
  ;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  ;;   (define-key evil-insert-state-map (kbd "C-y") 'copilot-accept-completion-by-word))

  (use-package code-review
    :disabled t
    :after (magit forge)
    :init (with-eval-after-load 'evil-collection-magit
            ;; From Doom Emacs
            (dolist (binding evil-collection-magit-mode-map-bindings)
              (pcase-let* ((`(,states _ ,evil-binding ,fn) binding))
                (dolist (state states)
                  (evil-collection-define-key state 'code-review-mode-map evil-binding fn))))
            (evil-set-initial-state 'code-review-mode evil-default-state))
    :config (progn
              (evil-make-overriding-map code-review-mode-map evil-default-state)
              (setq code-review-auth-login-marker 'forge)
              (add-hook 'code-review-mode-hook
                        (lambda ()
                          ;; include *Code-Review* buffer into current workspace
                          (persp-add-buffer (current-buffer))))
              ;; From Doom Emacs
              (defun magit/start-code-review (arg)
                (interactive "P")
                (call-interactively
                 (if (or arg (not (featurep 'forge)))
                     #'code-review-start
                   #'code-review-forge-pr-at-point)))

              (transient-append-suffix 'magit-merge "i"
                '("y" "Review pull request" magit/start-code-review))
              (with-eval-after-load 'forge
                (transient-append-suffix 'forge-dispatch "c u"
                  '("c r" "Review pull request" magit/start-code-review)))))

  (use-package debbugs :defer t)

  (use-package kubernetes-tramp
    :defer t
    :config (setq tramp-remote-shell-executable "sh"))

  (use-package helpful
    :disabled t
    :init (progn
            ;; (defun btw//setup-helpful-mode ()
            ;;   ;; Make `helpful-mode' hijack `help-mode''s history features
            ;;   (setq-local revert-buffer-function #'help-mode-revert-buffer)
            ;;   (help-make-xrefs (current-buffer)))
            ;;
            ;; (add-hook 'helpful-mode-hook #'btw//setup-helpful-mode)

            ;; These aren't available yet.  See https://github.com/Wilfred/helpful/issues/250.
            ;; (spacemacs/set-leader-keys-for-major-mode 'helpful-mode "]" #'help-go-forward)
            ;; (spacemacs/set-leader-keys-for-major-mode 'helpful-mode "[" #'help-go-back))

            ;; (with-eval-after-load 'editorconfig
            ;;   (add-to-list 'editorconfig-exclude-modes 'helpful-mode))

            (add-to-list 'purpose-x-popwin-major-modes 'helpful-mode)

            (setq helpful-max-buffers 1)

            (add-hook 'persp-common-buffer-filter-functions
                      ;; there is also `persp-add-buffer-on-after-change-major-mode-filter-functions'
                      #'(lambda (b) (or (string-prefix-p " " (buffer-name b))
                                        (eq (buffer-local-value 'major-mode b) 'helpful-mode))))

            ;; (defun btw//helpful-switch-to-buffer (buffer-or-name)
            ;;   (if (eq major-mode 'helpful-mode)
            ;;       (switch-to-buffer buffer-or-name)
            ;;     (pop-to-buffer buffer-or-name)))

            ;; (setq helpful-switch-buffer-function #'btw//helpful-switch-to-buffer)
            ))

  ;; TODO: This is a temporary fix for a `forge' dependency; remove me.
  (use-package sqlite3 :ensure t)

  (use-package multi-vterm :ensure t)

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

              (setq jupyter-repl-echo-eval-p t)

              (when (fboundp 'purpose-set-extension-configuration)
                ;; NOTE: To delete this configuration...
                ;; (purpose-del-extension-configuration :jupyter)
                (purpose-set-extension-configuration
                 :jupyter (purpose-conf :mode-purposes
                                        '((jupyter-repl-mode . repl)
                                          (jupyter-repl-interaction-mode . repl)))))

              ;; (spacemacs|add-company-backends :backends company-capf :modes jupyter-repl-mode)
              ))

  (use-package ox-json
    :defer t
    :after (org)
    :commands (ox-json-export-to-buffer ox-json-export-to-file)
    :load-path "~/projects/code/emacs/ox-json")

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

  (use-package pyvenv-extras
    :after (pyvenv projectile persp-mode))

  (use-package proj-persp-extras
    :after (projectile persp-mode))

  (use-package org-btw-python
    :after (org)
    :config (progn
              (org-btw-python-mode +1))
    :commands (org-btw//ob-python-generate-plots))

  (use-package org-ref+
    :after (org-ref)
    :config (progn
              (org-ref+-mode +1)))

  (use-package ox-latex+
    :after (ox-latex))

  (use-package python-btw
    :after (python)
    :config (progn
              (python-btw-mode +1)))

  ;; Disabled because it's adding superfluous spacing to Python output
  ;; (use-package coterm
  ;;   :after (python)
  ;;   :config (progn (coterm-mode +1)))

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

  (use-package python-docstring
    :defer t
    :after (python)
    :init (progn
            (defun btw/python-docstring-mode ()
              (python-docstring-mode t))
            (add-hook 'python-base-mode-hook #'btw/python-docstring-mode)))

  (use-package llvm-mode
    :defer t
    :interpreter ("llvm" . llvm-mode)
    ;; :mode (("\\.ll\\'" . llvm-mode))
    )

  (use-package cython-mode
    :defer t
    :interpreter ("cython" . cython-mode)
    :mode (("\\.pyx\\'" . cython-mode)
           ("\\.pxd\\'" . cython-mode)
           ("\\.pxi\\'" . cython-mode))
    :config (progn
              (spacemacs/set-leader-keys-for-major-mode 'cython-mode
                "cc" #'cython-compile)))

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

  (with-eval-after-load 'lsp-rust
    ;; (setq lsp-rust-analyzer-library-directories ...)
    ;; (setq lsp-rust-analyzer-call-info-full nil)
    ;; We need this; otherwise, we'll ignore files and get "file not included in
    ;; crate" warnings when something is an optional feature
    (setq lsp-rust-all-features t))

  (with-eval-after-load 'rustic-cargo
    ;; Allows output from Rust tests in compilation buffers
    (add-hook 'rustic-cargo-test-mode-hook (lambda () (setenv "RUST_TEST_NOCAPTURE" "1")))
    ;; (add-hook 'rustic-mode-hook
    ;;              (lambda ()
    ;;                (setq lsp-inlay-hint-enable t)
    ;;                (lsp-inlay-hints-mode t)))
    )

  (with-eval-after-load 'undo-tree
    ;; Disabling for now, because it takes seconds to save a file.
    (setq undo-tree-auto-save-history nil))

  (with-eval-after-load 'lsp-clangd
    (setq lsp-clients-clangd-args '("--header-insertion-decorators=0" "-j=4" "-background-index" "-log=error")))

  (with-eval-after-load 'smartparens

    (defun btw//sp-indent-region (start end)
      (interactive "r")
      (sp--indent-region start end))

    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "=r" #'btw//sp-indent-region)
    (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode "=d" #'sp-indent-defun))

  (with-eval-after-load 'auto-highlight-symbol
    (define-key evil-motion-state-map (kbd "*") 'evil-search-word-forward)
    (define-key evil-motion-state-map (kbd "#") 'evil-search-word-backward)
    (setq ahs-idle-interval 2.0))

  (with-eval-after-load 'kubernetes
    (setq kubernetes-poll-frequency 30)
    (setq kubernetes-clean-up-interactive-exec-buffers nil))

  (with-eval-after-load 'vterm
    (setq vterm-shell "/bin/zsh")
    ;; (add-hook 'vterm-mode-hook
    ;;           (lambda ()
    ;;             (setq-local evil-insert-state-cursor 'box)
    ;;             (evil-insert-state)))
    ;; (defun evil-collection-vterm-escape-stay ()
    ;;       "Go back to normal state but don't move cursor backwards.
    ;; Moving cursor backwards is the default vim behavior but
    ;; it is not appropriate in some cases like terminals."
    ;;       (setq-local evil-move-cursor-back nil))

    ;; (add-hook 'vterm-mode-hook #'evil-collection-vterm-escape-stay)
    (defun vterm-evil-insert ()
      (interactive)
      (vterm-goto-char (point))
      (call-interactively #'evil-insert))

    (defun vterm-evil-append ()
      (interactive)
      (vterm-goto-char (1+ (point)))
      (call-interactively #'evil-append))

    (defun vterm-evil-append-line ()
      (interactive)
      (vterm-goto-char (vterm--get-end-of-line))
      (call-interactively #'evil-append))

    (defun vterm-evil-insert-line ()
      (interactive)
      (vterm-goto-char (vterm--get-beginning-of-line))
      (call-interactively #'evil-append))

    (defun vterm-evil-delete ()
      "Provide similar behavior as `evil-delete'."
      (interactive)
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
          (call-interactively 'evil-delete))))

    (defun vterm-evil-change ()
      "Provide similar behavior as `evil-change'."
      (interactive)
      (let ((inhibit-read-only t))
        (cl-letf (((symbol-function #'delete-region) #'vterm-delete-region))
          (call-interactively 'evil-change))))

    (defun my-vterm-hook()
      (evil-local-mode 1)
      (evil-define-key 'normal 'local "A" 'vterm-evil-append-line)
      (evil-define-key 'normal 'local "a" 'vterm-evil-append)
      (evil-define-key 'normal 'local "d" 'vterm-evil-delete)
      (evil-define-key 'normal 'local "I" 'vterm-evil-insert-line)
      (evil-define-key 'normal 'local "i" 'vterm-evil-insert)
      (evil-define-key 'normal 'local "c" 'vterm-evil-change))

    (add-hook 'vterm-mode-hook 'my-vterm-hook)

    (define-key vterm-mode-map [return] #'vterm-send-return)

    (keymap-unset vterm-mode-map "M-:")

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
      ;; (kbd "i") #'evil-insert-resume
      ;; (kbd "o") #'evil-insert-resume
      (kbd "p") #'vterm-yank
      (kbd "P") #'vterm-yank
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

    ;; (setq compilation-error-regexp-alist-alist
    ;;       (assq-delete-all 'overseer compilation-error-regexp-alist-alist))

    ;; This fixes an issue that appears when parsing `ert-runner' output
    (add-to-list 'compilation-error-regexp-alist-alist '(overseer "(error \\(.*\\))" nil nil nil))

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

  (with-eval-after-load 'evil-embrace
    ;; Add `yasnippet' integration to `evil-surround' (after `evil-embrace' overrides it)
    (defun evil-yasnippet-surround-region (oldfun beg end type char &optional force-new-line)
      (if (eq char ?y)
          (progn
            (push-mark beg)
            (goto-char end)
            (yas-insert-snippet))
        (funcall oldfun beg end type char force-new-line)))

    (advice-add #'evil-embrace-evil-surround-region :around #'evil-yasnippet-surround-region))

  (with-eval-after-load 'evil-search
    (defun btw-evil-ex-hl-match-hook (oldfun hl)
      (or (funcall oldfun hl)
          (lambda (_ ov)
            (overlay-put ov 'priority 0))))
    (advice-add #'evil-ex-hl-match-hook :around #'btw-evil-ex-hl-match-hook))

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

    (evil-define-key 'normal git-rebase-mode-map
      (kbd "C-j") 'git-rebase-move-line-down
      (kbd "C-k") 'git-rebase-move-line-up)

    ;; TODO: Hack fix for https://github.com/magit/magit/issues/4739; consider
    ;; fixing, and then removing, this.
    ;; (add-hook 'magit-section-mode-hook (defun btw//disable-truncate-lines ()
    ;;                                      (setq-local truncate-lines nil)))

    (put 'git-rebase-move-line-down :advertised-binding (kbd "C-j"))
    (put 'git-rebase-move-line-up :advertised-binding (kbd "C-k"))

    (setq magit-branch-prefer-remote-upstream '("upstream/main" "origin/main"))
    (setq magit-repository-directories '(("~/" . 1)
                                         ("~/projects/code" . 3)
                                         ("~/projects/papers" . 3)
                                         ("~/apps/qtile" . 0))))

  (with-eval-after-load 'magithub-dash
    (setq magithub-dashboard-show-read-notifications nil))

  (with-eval-after-load 'git-link
    (setq git-link-default-remote "origin"))

  ;; (with-eval-after-load 'editorconfig
  ;;   (add-to-list 'editorconfig-exclude-modes 'help-mode)
  ;;   (add-to-list 'editorconfig-exclude-modes 'edebug-mode)
  ;;   (add-to-list 'editorconfig-exclude-modes 'debugger-mode))

  (with-eval-after-load 'flycheck
    ;; TODO: Consider adding logic to `flycheck-python-find-module' that only
    ;; matches checker modules in the virtualenv (if any).
    (add-to-list 'flycheck-disabled-checkers 'python-pylint)
    (add-to-list 'flycheck-disabled-checkers 'python-pyright)
    (add-to-list 'flycheck-disabled-checkers 'python-mypy)
    (add-to-list 'flycheck-disabled-checkers 'python-pycompile)

    (defun btw//disable-python-flycheckers ()
      (add-to-list 'flycheck-disabled-checkers 'python-pylint)
      (add-to-list 'flycheck-disabled-checkers 'python-pyright)
      (add-to-list 'flycheck-disabled-checkers 'python-mypy)
      (add-to-list 'flycheck-disabled-checkers 'python-pycompile))

    (add-hook 'python-base-mode-hook #'btw//disable-python-flycheckers)


    ;; This is a little aggressive...
    ;; (setq flycheck-checker-error-threshold nil)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-indication-mode 'right-fringe)

    ;; Without this, Flycheck will raise (nearly) silent errors when LSP is used
    ;; inside an `org-edit-special' buffer
    (defun btw//flycheck-python-find-project-root (checker)
      (projectile-project-root))
    (advice-add #'flycheck-python-find-project-root :override #'btw//flycheck-python-find-project-root))

  (with-eval-after-load 'python

    ;; TODO: Prevent use of tree-sitter until some things are worked out
    (defalias 'python-ts-mode 'python-mode)

    ;; TODO: Prevent slow shell/session starts
    (advice-add #'spacemacs//python-setup-shell :override (lambda (&rest r) nil))
    ;; TODO: Prevent `flycheck' failures during shell initialization
    (advice-add #'spacemacs/python-setup-everything :override (lambda (&rest r) nil))

    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt")
    (setq python-shell-completion-native-output-timeout 3.0)
    (setq python-pdbtrack-activate nil)

    (defun btw//set-comint-output-filters ()
      (add-hook 'comint-output-filter-functions
                #'btw/comint-preoutput-turn-buffer-read-only
                nil t)
      (add-hook 'comint-output-filter-functions
                #'comint-postoutput-scroll-to-bottom
                nil t))
    (add-hook 'inferior-python-mode-hook #'btw//set-comint-output-filters)

    (defun btw//python-util-comint-last-prompt ()
      (cons
       (save-excursion (forward-line 0) (point))
       (save-excursion (comint-previous-prompt 1))))

    (advice-add #'python-util-comint-last-prompt :override #'btw//python-util-comint-last-prompt)

    ;; Make `breakpoint()' use `ipdb' (if it's installed, of course)
    (add-to-list 'python-shell-process-environment "PYTHONBREAKPOINT=ipdb.set_trace")

    (when (fboundp 'purpose-set-extension-configuration)
      ;; NOTE: To delete this configuration...
      ;; (purpose-del-extension-configuration :python)
      (purpose-set-extension-configuration
       :python (purpose-conf :mode-purposes
                             '((python-base-mode . python)
                               (inferior-python-mode . repl))
                             ;; :regexp-purposes
                             ;; '(("^test-.*\\.py$" . test))
                             )))
    (setq-default python-eldoc-get-doc nil)
    (setq pytest-cmd-flags "-r A --verbose --tb=short -x -s --pdbcls=IPython.terminal.debugger:Pdb"))

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
    (setq pyvenv-tracking-ask-before-change nil)
    (pyvenv-tracking-mode +1))

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
    ;; XXX: A temporary work-around for a (likely) local issue
    (defun lsp-keyword->string (keyword)
      "Convert a KEYWORD to string."
      (substring (symbol-name keyword) 1))

    ;; Temporary fix (until a PR takes care of this)
    ;; (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
    ;;   "bd" #'lsp-describe-session)

    ;; https://github.com/emacs-lsp/lsp-mode/issues/2910#issue-908672841
    ;; E.g. Add the following to a `.dir-locals.el' file:
    ;; ((nil . ((lsp-file-watch-ignored-directories-additional . ("[\////]ignore-this\\'")))))
    (defvar lsp-file-watch-ignored-directories-additional nil
      "Additional ignored directories added to lsp-file-watch-ignored-directories.")
    (put 'lsp-file-watch-ignored-directories-additional 'safe-local-variable #'lsp--string-listp)
    (add-function :around (symbol-function 'lsp-file-watch-ignored-directories)
                  (lambda (orig)
                    (append lsp-file-watch-ignored-directories-additional (funcall orig))))

    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]doc\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]bin\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.ropeproject\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.benchmarks\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\w+\\.egg-info\\'")
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.asv\\'")
    (setq lsp-response-timeout 180)
    (setq lsp-enable-imenu nil)
    (setq lsp-enable-indentation nil)
    (setq lsp-enable-symbol-highlighting nil)
    (setq lsp-enable-links nil)
    (setq lsp-enable-dap-auto-configure nil)
    (setq lsp-auto-guess-root t)
    (setq lsp-enable-snippet t)
    (setq lsp-document-sync-method lsp--sync-incremental)
    (setq lsp-enable-on-type-formatting nil)
    (setq lsp-before-save-edits nil)
    (setq lsp-headerline-breadcrumb-enable t)
    (setq lsp-eldoc-enable-hover nil)
    ;; Was `lsp-hover'
    (setq lsp-signature-auto-activate nil)
    (setq lsp-eldoc-hook                ;'(lsp-document-highlight)
          nil)
    (setq lsp-eldoc-render-all nil)
    (setq lsp-disabled-clients '(pyright))

    (with-eval-after-load 'org
      (cl-defmacro lsp-org-babel-enable (lang)
        "Support LANG in org source code block.

Taken from https://tecosaur.github.io/emacs-config/config.html#lsp-support-src"
        (cl-check-type lang stringp)
        (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
               (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
          `(progn

             (defun ,intern-pre (info)
               ;; This is needed to prevent issues with `lsp-breadcrumb' in `org-src-mode'
               ;; buffers
               (lsp-headerline-breadcrumb-mode -1)
               (setq-local header-line-format nil)

               (let ((file-name (->> info caddr (alist-get :file))))
                 (unless file-name
                   (setq file-name (make-temp-file "babel-lsp-")))
                 (setq buffer-file-name file-name)
                 (lsp-deferred)))

             (put ',intern-pre 'function-documentation
                  (format "Enable lsp-mode in the buffer of org source block (%s)."
                          (upcase ,lang)))
             (if (fboundp ',edit-pre)
                 (advice-add ',edit-pre :after ',intern-pre)
               (progn
                 (defun ,edit-pre (info)
                   (,intern-pre info))
                 (put ',edit-pre 'function-documentation
                      (format "Prepare local buffer environment for org source block (%s)."
                              (upcase ,lang))))))))

      (pcase-dolist (`(,lang . ,enabled) org-babel-load-languages)
        (eval `(lsp-org-babel-enable ,(symbol-name lang))))))

  (with-eval-after-load 'lsp-pyright
    (setq lsp-pyright-diagnostic-mode "openFilesOnly" ;; "workspace"
          lsp-pyright-log-level "warnings"
          lsp-pyright-typechecking-mode "off")

    (defun btw//lsp-pyright-locate-venv (oldfun &rest r)
      (if (fboundp 'pyvenv-extras//run-in-pyvenv)
          (pyvenv-extras//run-in-pyvenv
           (if python-shell-virtualenv-root
               (setq-local lsp-pyright-venv-path python-shell-virtualenv-root)
             (progn
               (warn "lsp-pyright could not find venv!")
               (apply oldfun r))))
        (progn
          (warn "lsp-pyright could not find venv!")
          (apply oldfun r))))

    (advice-add #'lsp-pyright-locate-venv :around #'btw//lsp-pyright-locate-venv))

  (with-eval-after-load 'lsp-pylsp
    ;; For debugging `pylsp', set the following
    ;; (setq lsp-pylsp-server-command '("pylsp" "-vvv"))
    (setq lsp-pylsp-server-command '("pylsp"))
    ;; Look for the output in the `*pylsp::stderr*' buffer
    (setq lsp-pylsp-plugins-pydocstyle-enabled nil
          lsp-pylsp-plugins-pyflakes-enabled nil
          lsp-pylsp-plugins-yapf-enabled nil
          lsp-pylsp-plugins-pycodestyle-enabled nil
          lsp-pylsp-plugins-pylint-enabled nil
          lsp-pylsp-plugins-preload-enabled nil
          lsp-pylsp-plugins-mccabe-enabled nil
          lsp-pylsp-plugins-autopep8-enabled nil
          lsp-pylsp-plugins-jedi-symbols-enabled t
          lsp-pylsp-plugins-jedi-signature-help-enabled nil
          lsp-pylsp-plugins-jedi-hover-enabled t
          lsp-pylsp-plugins-jedi-completion-include-params nil
          ;; lsp-pylsp-plugins-flake8-enabled t
          ;; lsp-pylsp-plugins-flake8-max-line-length nil
          lsp-pylsp-rename-backend 'jedi ; 'rope
          ;; lsp-pylsp-plugins-jedi-use-pyenv-environment nil
          ;; lsp-pylsp-plugins-jedi-completion-enabled nil
          ;; lsp-pylsp-rope-rope-folder ""
          ;; lsp-pylsp-plugins-jedi-environment "..."
          )
    (setq lsp-pylsp-configuration-sources ["flake8"])
    (setq lsp-clients-pylsp-library-directories `(,(or (getenv "ANACONDA_HOME") "/sys")))

    (defun btw/lsp-pylsp-library-folders-fn (_workspace)
      "Find workspaces based on virtualenvs.  Function which returns the
 folders that are considered to be not projects but library files.  "
      (if (fboundp 'pyvenv-extras//run-in-pyvenv)
          (pyvenv-extras//run-in-pyvenv
           (if python-shell-virtualenv-root
               (list python-shell-virtualenv-root)
             ;; TODO: Could just make this variable buffer-local.
             lsp-clients-pylsp-library-directories))
        lsp-clients-pylsp-library-directories))

    ;; Update the existing client object.
    ;; (declare-function lsp--client-library-folders-fn "ext:lsp-mode" nil t)
    ;; (let ((client (gethash 'pylsp lsp-clients)))
    ;;   (setf (lsp--client-library-folders-fn client) #'btw/lsp-pylsp-library-folders-fn))
    )

  (with-eval-after-load 'lsp-ui
    (setq lsp-ui-peek-enable nil)
    (setq lsp-ui-doc-enable nil)
    (setq lsp-ui-sideline-delay nil)
    (setq lsp-ui-sideline-show-hover nil)
    (setq lsp-ui-sideline-enable nil))

  (with-eval-after-load 'org

    ;; Remove this "functionality" that Spacemacs adds.  I really need to
    ;; switch to Doom.
    (advice-remove #'org-export-output-file-name #'spacemacs//org-export-output-project-file-name)

    ;; TODO: Consider this...
    ;; (org-babel-make-language-alias "python" "jupyter-python")

    ;; (setq btw//org-show-entry-active nil)
    ;;
    ;; (defun btw//org-show-entry (&rest r)
    ;;   "Expand collapsed blocks when using goto-char."
    ;;   (when (and (eq major-mode 'org-mode) (not btw//org-show-entry-active))
    ;;     (let ((btw//org-show-entry-active t))
    ;;       (save-excursion
    ;;         (org-reveal)))))
    ;; (advice-add 'goto-line :after #'btw//org-show-entry)
    ;; (advice-add 'forward-line :after #'btw//org-show-entry)

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

    ;; (spacemacs|add-company-backends :backends company-yasnippet
    ;;                                 :append-hook t
    ;;                                 :modes org-mode)

    (defvaralias 'org-plantuml-jar-path 'plantuml-jar-path)
    ;; (setq org-plantuml-jar-path plantuml-jar-path)

    ;; (add-to-list 'org-babel-load-languages '(llvm . t))
    (add-to-list 'org-babel-load-languages '(plantuml . t))
    (add-to-list 'org-babel-load-languages '(dot . t))
    (add-to-list 'org-babel-load-languages '(scheme . t))
    ;; (add-to-list 'org-babel-load-languages '(jupyter . t) t)
    (add-to-list 'org-babel-load-languages '(latex . t) t)
    ;; (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

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

    (setq org-highlight-latex-and-related nil) ;'(native))

    ;; Most often, we'll use inline src statements (e.g. src_python{...}) to
    ;; simply display formatted text.
    (setq org-babel-default-inline-header-args
          '((:exports . "code")
            (:eval . "never")
            (:results . "none")))

    (setq org-edit-src-content-indentation 0
          org-src-ask-before-returning-to-edit-buffer nil
          org-src-tab-acts-natively t
          org-src-window-setup 'current-window
          org-src-fontify-natively t
          org-support-shift-select 'always)

    (setq org-latex-pdf-process #'spacemacs//org-latex-pdf-process)

    (setq org-indirect-buffer-display 'current-window)

    (setq org-startup-folded 'fold
          org-hide-block-startup 'hideblocks)

    ;; What to allow before and after markup
    ;; See https://emacs.stackexchange.com/a/13828
    ;; (setcar (nthcdr 1 org-emphasis-regexp-components)
    ;;         (concat (nth 0 org-emphasis-regexp-components) "s"))
    (setq org-link-file-path-type 'relative)
    (setq org-confirm-babel-evaluate nil)
    (setq org-default-notes-file (f-join (expand-file-name "~") "Documents" "notes.org")))

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

  (with-eval-after-load 'counsel-projectile
    ;; This command allows one to easily set arbitrary `ag' options with `C-u'
    (setq counsel-ag-base-command '("ag" "--vimgrep" "--hidden" "--ignore" ".git/" "--ignore" "*.svg" "--ignore" "*.js" "%s"))
    (spacemacs/set-leader-keys "sp" 'counsel-projectile-ag))

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
                  '("~/projects/code/python" "~/projects/code/emacs" "~/projects/papers")))
    (setq projectile-indexing-method 'hybrid)
    ;; To sort files by recently active buffers and then recently opened files:
    (setq projectile-sort-order
          'default
          ;; XXX: This is really slow
          ;; 'recently-active
          )
    ;; helm-source-projectile-files-list
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
    (setq projectile-per-project-compilation-buffer t)
    (setq projectile-use-git-grep nil))

  (with-eval-after-load 'org-projectile
    (setq org-projectile-capture-template "* TODO %?
  %u
  %a"))

  (with-eval-after-load 'hideshow

    (defun btw//hs-show-block-rec ()
      "Unfold/show all blocks at point."
      (ignore-errors
        (while (hs-already-hidden-p)
          (save-mark-and-excursion
            (hs-show-block)))))

    ;; Add a recursive unfold to `evil''s mappings for `hideshow'
    (when-let ((opt (cl-assoc-if (lambda (x) (memq 'hs-minor-mode x)) evil-fold-list)))
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
    (setq hs-hide-comments-when-hiding-all nil)

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
    (setq persp-autokill-buffer-on-remove 'kill-weak)
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

    (defun btw/persp-projectile-project-root (oldfun &rest r)
      "Use the persp project name and regular `projectile-project-root' as a
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

    (defun btw/persp-projectile-project-name (oldfun &rest r)
      "Query the persp layout for the projectile project name and use projectile
 for the fallback."
      (let* ((persp-name (spacemacs//current-layout-name))
             (persp-projectile-name (if (f-dir? persp-name)
                                        (funcall projectile-project-name-function
                                                 persp-name)
                                      (persp-parameter 'projectile-project-root))))
        (or persp-projectile-name (apply oldfun r))))

    (define-minor-mode persp-projectile-tracking-mode
      "Toggle persp-projectile-tracking-mode mode."
      :require 'persp-mode
      :init-value nil
      :global t
      (if persp-projectile-tracking-mode
          (progn
            (add-hook 'persp-created-functions #'btw/persp-assign-projectile-root)
            (advice-add #'projectile-project-root :around #'btw/persp-projectile-project-root)
            (advice-add #'projectile-project-name :around #'btw/persp-projectile-project-name))
        (progn
          (remove-hook 'persp-created-functions #'btw/persp-assign-projectile-root)
          (advice-remove #'projectile-project-root #'btw/persp-projectile-project-root)
          (advice-remove #'projectile-project-name #'btw/persp-projectile-project-name))))

    (persp-projectile-tracking-mode +1)

    ;; (setq persp-set-ido-hooks nil)

    (defun btw/persp-restrict-ido-buffers (&rest r)
      "Remove `nil' buffer names from the returned results.
This fixes some `helm' issues."
      (setq ido-temp-list (remove nil ido-temp-list)))

    (advice-add #'persp-restrict-ido-buffers :after #'btw/persp-restrict-ido-buffers)

    ;; Restore eshell buffers.
    ;; See https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-inferior-python-save-load-el
    ;; for more examples (e.g. Python inferior shells).
    (persp-def-buffer-save/load
     :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
     :save-vars '(major-mode default-directory))

    (defun btw//get-persp-idx-for-layout (&optional persp-name)
      (seq-find #'identity
                (seq-map-indexed (lambda (name i)
                                   (if (eq name (or persp-name (spacemacs//current-layout-name)))
                                       i
                                     nil))
                                 (persp-names-current-frame-fast-ordered))))

    (defun btw/projectile-shell-pop ()
      "Open a term buffer at projectile project root for the current perspective."
      (let* ((persp-idx (btw//get-persp-idx-for-layout))
             (shell (if (eq 'multi-term shell-default-shell)
                        'multiterm
                      shell-default-shell))
             (shell-pop-func (intern (format "spacemacs/shell-pop-%S" shell))))
        (funcall shell-pop-func persp-idx)))

    (advice-add #'spacemacs/projectile-shell-pop :override #'btw/projectile-shell-pop))

  (with-eval-after-load 'comint

    (defun btw//comint-set-C-r ()
      (evil-local-set-key 'insert (kbd "C-r") #'evil-paste-from-register))

    (add-hook 'inferior-python-mode-hook #'btw//comint-set-C-r)

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

    (defun btw//comint-output-filter (&rest r)
      "From https://github.com/michalrus/dotfiles/blob/c4421e361400c4184ea90a021254766372a1f301/.emacs.d/init.d/040-terminal.el.symlink#L33

    This will cause C-c C-c to fail; that seems to be due to `comint-skip-input'.
    "
      (with-silent-modifications
        (-when-let* ((start-marker comint-last-output-start)
                     (proc (get-buffer-process (current-buffer)))
                     (end-marker (if proc (process-mark proc) (point-max-marker))))
          (when (< start-marker end-marker) ;; Account for some of the IELM’s wilderness.
            (let ((inhibit-read-only t))
              ;; Disallow interleaving
              (remove-text-properties start-marker (1- end-marker) '(rear-nonsticky))
              ;; Make sure that at `max-point' you can always append.
              ;; Important for bad REPLs that keep writing after giving us prompt .
              (add-text-properties (1- end-marker) end-marker '(rear-nonsticky t))
              ;; Protect fence (newline of input, just before output).
              (when (eq (char-before start-marker) ?\n)
                (remove-text-properties (1- start-marker) start-marker '(rear-nonsticky))
                (add-text-properties    (1- start-marker) start-marker '(read-only t))))))))

    ;; (add-hook 'comint-output-filter-functions
    ;;           #'btw/comint-preoutput-turn-buffer-read-only
    ;;           'append)
    ;; (advice-add #'comint-output-filter :after #'btw//comint-output-filter)

    ;; Workaround for read-only issues
    ;; TODO: Remove when this is fixed upstream.
    ;; (defun btw//python-shell-font-lock-post-command-hook (fn &rest args)
    ;;   (with-silent-modifications
    ;;     (let ((inhibit-read-only t))
    ;;       (apply fn args)))
    ;;   )

    ;; (advice-add #'python-shell-font-lock-post-command-hook :around #'btw//python-shell-font-lock-post-command-hook)
    ;; (advice-remove #'python-shell-font-lock-post-command-hook #'btw//python-shell-font-lock-post-command-hook)
    )

  (with-eval-after-load 'evil
    ;; Fix for https://github.com/Somelauw/evil-org-mode/issues/93
    (unless (fboundp 'evil-redirect-digit-argument)
      (defun evil-redirect-digit-argument (&rest r)
        nil))

    (defun btw//scroll-to-top (&rest args)
      (evil-scroll-line-to-top (line-number-at-pos)))

    (define-minor-mode jumps-scroll-to-top-mode
      "Toggle jumps-scroll-to-top-mode mode."
      :require 'evil
      :init-value nil
      :global t
      (if jumps-scroll-to-top-mode
          (progn
            (advice-add #'evil-goto-line :after #'btw//scroll-to-top)
            (advice-add #'spacemacs/jump-to-definition :after #'btw//scroll-to-top)
            ;; (advice-add #'primitive-undo :after #'btw//scroll-to-top)
            (advice-add #'helm-ag--find-file-action :after #'btw//scroll-to-top)
            (add-hook 'xref-after-jump-hook #'btw//scroll-to-top)
            (add-hook 'evil-jumps-post-jump-hook #'btw//scroll-to-top))
        (progn
          (advice-remove #'evil-goto-line #'btw//scroll-to-top)
          (advice-remove #'spacemacs/jump-to-definition #'btw//scroll-to-top)
          ;; (advice-remove #'primitive-undo #'btw//scroll-to-top)
          (advice-remove #'helm-ag--find-file-action #'btw//scroll-to-top)
          (remove-hook 'xref-after-jump-hook #'btw//scroll-to-top)
          (remove-hook 'evil-jumps-post-jump-hook #'btw//scroll-to-top))))

    (jumps-scroll-to-top-mode +1)

    (when nil
      ;; All this should be obviated by `reveal-mode'

      (defun btw//evil-open-on-movement (fn &rest r)
        "Expand collapsed blocks after FN moves the point."
        (let ((start-point (point)))
          (prog1
              (apply fn r)
            (unless (eq start-point (point))
              (save-mark-and-excursion
                (ignore-errors
                  (evil-open-fold-rec)))))))

      (define-minor-mode jumps-open-folds-mode
        "Toggle jumps-open-folds-mode mode."
        :require 'evil
        :init-value nil
        :global t
        (if jumps-open-folds-mode
            (progn
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
              (add-hook 'evil-jumps-post-jump-hook #'evil-open-fold-rec))
          (progn
            (advice-remove #'evil-goto-line #'btw//evil-open-on-movement)
            (advice-remove #'spacemacs/jump-to-definition #'btw//evil-open-on-movement)
            (advice-remove #'primitive-undo #'btw//evil-open-on-movement)
            (advice-remove #'helm-ag--find-file-action #'btw//evil-open-on-movement)
            (remove-hook 'xref-after-jump-hook #'evil-open-fold-rec)
            (remove-hook 'evil-jumps-post-jump-hook #'evil-open-fold-rec))))

      (jumps-open-folds-mode +1))

    (defun btw//enable-reveal-mode ()
      (reveal-mode +1))

    (add-hook 'prog-mode-hook #'btw//enable-reveal-mode)

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

    (evil-define-text-object btw//evil-inner-defun (count &optional beg end type)
      (save-excursion
        (mark-defun)
        (evil-range (region-beginning) (region-end) type :expanded t)))

    (define-key evil-inner-text-objects-map "d" 'btw//evil-inner-defun)
    (define-key evil-outer-text-objects-map "d" 'btw//evil-inner-defun)

    ;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
    (defalias #'forward-evil-word #'forward-evil-symbol))

  (with-eval-after-load 'yasnippet
    ;; (setq yasnippet-snippets-dir (f-join default-directory "private" "snippets"))
    )

  (with-eval-after-load 'company
    ;; Disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends)

    (setq company-dabbrev-other-buffers nil)
    (setq-local company-search-filtering t)
    (setq company-idle-delay nil)
    ;; (setq company-backends-emacs-lisp-mode
    ;;       (cons '(company-elisp :with company-yasnippet)
    ;;             (seq-remove (lambda (x) (eq (car x) 'company-capf))
    ;;                         company-backends-emacs-lisp-mode)))
    (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)
    (define-key company-active-map (kbd "C-y") 'company-complete-selection)
    ;; This doesn't work.  See https://github.com/syl20bnr/spacemacs/issues/4242
    ;; (define-key company-active-map (kbd "ESC") 'company-abort)
    (define-key evil-insert-state-map (kbd "C-n") #'company-select-next)
    (define-key evil-insert-state-map (kbd "C-p") #'company-select-previous))

  (with-eval-after-load 'window-purpose
    ;; This was commented out in `purpose--fix-helm' from `window-purpose-fixes.el'.
    (with-eval-after-load 'helm
      (purpose-set-extension-configuration :helm purpose--helm-conf)
      (add-to-list 'purpose-action-function-ignore-buffer-names "^\\*helm")
      (add-to-list 'purpose-action-function-ignore-buffer-names "^\\*Helm")))

  (with-eval-after-load 'window-purpose-switch
    (setq purpose-display-at-bottom-height 0.4))

  (with-eval-after-load 'helm

    ;; Attempt to sort entries based on usage/access.
    (helm-adaptive-mode +1)

    (setq-default helm-follow-mode-persistent nil)
    (setq helm-always-two-windows nil)
    (setq helm-split-window-inside-p nil)
    (setq helm-split-window-default-side 'below)

    ;; NOTE: `window-purpose-switch' can destroy expected
    ;; pop-up/window placement behavior; look at the default
    ;; values for vars like `purpose-display-at-bottom-height'
    ;; if there are problems.

    (define-key helm-map (kbd "C-w") 'evil-delete-backward-word))

  (with-eval-after-load 'evil-jumps
    (setq evil-jumps-cross-buffers nil))

  (with-eval-after-load 'clang-format
    (setq clang-format-style "Google"))

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
    (setq sql-send-terminator t)
    (setq sql-connection-alist '())

    (defvar sql-last-prompt-pos 1
      "position of last prompt when added recording started")
    (make-variable-buffer-local 'sql-last-prompt-pos)
    (put 'sql-last-prompt-pos 'permanent-local t)

    (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
    (sql-set-product-feature 'postgres :prompt-cont-regexp
                             "^[-[:alnum:]_]*[-(][#>] "))

  (with-eval-after-load 'reftex
    ;; Prevent whatever is setting this.
    (defun reftex-toggle-auto-view-crossref ()
      nil))

  (with-eval-after-load 'gnus
    (setq gnus-select-method
          '(nntp "news.gmane.io"))

    (setq gnus-secondary-select-methods
          '(;; (nnimap "gmail"
            ;;         (nnimap-address
            ;;          "imap.gmail.com")
            ;;         (nnimap-server-port 993)
            ;;         (nnimap-stream ssl))
            ))

    ;; Archive outgoing email in Sent folder on imap.gmail.com:
    ;; (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
    ;;       gnus-message-archive-group "[Gmail]/Sent Mail")

    ;; Store email in ~/gmail directory
    ;; (setq nnml-directory "~/Documents/gmail")
    ;; (setq message-directory "~/Documents/gmail")
    )

  (with-eval-after-load 'helm-ag
    (setq helm-ag-command-option "--hidden"
          helm-ag-use-grep-ignore-list t))

  (with-eval-after-load 'pytest

    (defun pytest-failed-first (&optional flags)
      "Run the failures first and then the rest of the tests.
Optional argument FLAGS py.test command line flags."
      (interactive)
      (pytest-all (concat "--failed-first " flags)))

    (defun pytest-pdb-last-failed ()
      "Run the failures first and then the rest of the tests, enter debugger on error."
      (interactive)
      (pytest-failed-first (concat "--pdb " pytest-cmd-flags)))

    ;; Fix for recent `python.el' changes that cause `pytest-start-command' to
    ;; block indefinitely.
    (defun btw//python-shell-comint-end-of-output-p (old-fn output)
      (when python-shell--prompt-calculated-input-regexp
        (funcall old-fn output)))

    (advice-add #'python-shell-comint-end-of-output-p :around #'btw//python-shell-comint-end-of-output-p)

    (defun btw//pytest-start-command (orig-fn command)
      (flet ((python-shell-prompt-set-calculated-regexps () nil))
        (funcall orig-fn command)))

    (advice-add #'pytest-start-command :around #'btw//pytest-start-command))

  (with-eval-after-load 'ob-shell
    ;; TODO FIXME: Prompts are not handled well and it has to do with the prompt
    ;; regex used by `shell-mode'.
    ;; (defun btw//org-babel-sh-initiate-session (old-fn session _params)
    ;;   (let ((shell-prompt-pattern "$ "))
    ;;     (funcall old-fn session _params)))
    ;;
    ;; (advice-add #'org-babel-sh-initiate-session :around #'btw//org-babel-sh-initiate-session)
    )

  (with-eval-after-load 'forge-topics
    (setq-default forge-topic-list-limit '(40 . 0))
    (setq forge-status-buffer-default-topic-filters
          (forge--topics-spec :type 'topic :active nil :state 'open :order 'newest)))

  (with-eval-after-load 'forge-db
    ;; This is a workaround for some code that keeps overwriting
    ;; with a value containing tildes that aren't supported by
    ;; `call-process'
    (when (boundp 'forge-database-file)
      (setq forge-database-file (f-expand forge-database-file))))

  (with-eval-after-load 'isearch
    (setq search-invisible t))

  (with-eval-after-load 'org-journal
    (setq org-journal-enable-encryption nil
          org-journal-encrypt-journal nil)
    (add-to-list 'org-agenda-files org-journal-dir))

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
