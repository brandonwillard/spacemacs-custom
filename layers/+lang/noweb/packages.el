;;; packages.el --- noweb layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Brandon Willard <brandonwillard@gmail.com>
;; URL: https://github.com/brandonwillard/noweb-layer
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `noweb-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `noweb/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `noweb/pre-init-PACKAGE' and/or
;;   `noweb/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst noweb-packages
  '(
    (polymode :location local
              ;; TODO: Set the following as default (when not using local dev version).
              ;; :fetcher github :repo "brandonwillard/polymode"
              )
    (poly-python :location local)
    )
)

(defun noweb/init-polymode ()
  (use-package polymode
    :defer t
    ;; XXX: Hack for local dev package.
    :load-path ("~/.emacs.d/private/local/polymode/")
    :init (setq polymode-prefix-key "\C-P")
    :config
    (progn
      ;; TODO: Do some/all of these only need to be defined once?
      (evil-define-motion noweb-chunk-forward (count)
        "Move forward a chunk"
        :type inclusive
        (polymode-next-chunk count))
      (evil-define-motion noweb-chunk-forward-same-type (count)
        "Move forward a chunk (same type of chunk as current location)"
        :type inclusive
        (polymode-next-chunk-same-type count))
      (evil-define-motion noweb-chunk-backward (count)
        "Move backward a chunk."
        :type inclusive
        (polymode-previous-chunk count))
      (evil-define-motion noweb-chunk-backward-same-type (count)
        "Move backward a chunk (same type of chunk as current location)."
        :type inclusive
        (polymode-previous-chunk-same-type count))

      (evil-add-command-properties #'polymode-next-chunk :jump t)
      (evil-add-command-properties #'polymode-next-chunk-same-type :jump t)
      (evil-add-command-properties #'polymode-previous-chunk :jump t)
      (evil-add-command-properties #'polymode-previous-chunk-same-type :jump t)

      ;; TODO: Setup text-objects
      ;; (evil-define-text-object noweb-chunk-object (count) ...)

      ;; TODO: Make jumps work across indirect buffers.
      ;; E.g.
      ;; (defadvice anaconda-mode-goto (before python/anaconda-mode-goto activate)
      ;;   (evil--jumps-push))
      ;; Perhaps use `polymode-switch-buffer-hook'.  It executes in the new/switched-to(?) buffer, though,
      ;; and `evil--jumps-push' needs to be executed in the old/switched-from buffer.

      ;; (evilified-state-evilify polymode-minor-mode polymode-mode-map)
      ;; (evil-define-key 'normal polymode-mode-map "]c" 'polymode-next-chunk-same-type)
      ;; (evil-define-key 'normal polymode-mode-map "]C" 'polymode-next-chunk)
      ;; (evil-define-key 'normal polymode-mode-map "[c" 'polymode-previous-chunk-same-type)
      ;; (evil-define-key 'normal polymode-mode-map "[C" 'polymode-previous-chunk)

      ;; FIXME: Still doesn't work well.
      ;; See https://github.com/noctuid/evil-guide#why-dont-keys-defined-with-evil-define-key-work-immediately
      ;; (add-hook 'polymode-init-host-hook #'evil-normalize-keymaps)
      ;; (add-hook 'polymode-init-inner-hook #'evil-normalize-keymaps)

      )
    ))

(defun noweb/init-poly-python ()
  (use-package poly-python
    :defer t
    ;; XXX: Hack for local dev package.
    :load-path
    ("~/.emacs.d/private/local/polymode/modes/")
    :config
    (progn
      ;; (evilified-state-evilify poly-noweb+python-mode poly-noweb+python-mode-map)
      (noweb//setup-polymode-evil-maps 'poly-noweb+python-mode)

      ;; FYI: mode information is kept in `pm/polymode'.

      ;; TODO: Set a generic polymode function variable to `python-shell-send-region'.

      ;; TODO: This is more-or-less what R's polymode does (via `ess-mode'); consider
      ;; using this simpler approach.
      ;; (when (fboundp 'advice-add)
      ;;   (advice-add 'python-shell-send-buffer :around 'pm-execute-narrowed-to-span))
      ;; (evil-leader/set-key-for-mode 'python-mode
      ;;   "sc" 'python-shell-send-buffer)

      (defun python-shell-send-chunk ()
        "Send chunk under cursor via Python `comint' REPL.

See `pm-eval-from-here' and `python-shell-send-region'"
        (interactive)
        (let ((span (pm-get-innermost-span nil t)))
          (when (eq (nth 0 span) 'body)
            (python-shell-send-region
             (1+ (nth 1 span)) (1- (nth 2 span))))
          ))

      (defun python-shell-send-chunks-from-here ()
        "Send all preceding code chunks up to the current POINT via the Python `comint' REPL.

See `pm-eval-from-here' and `python-shell-send-region'"
        (interactive)
        ;; TODO: Check the chunk header for enabled flags (e.g. 'enabled=True').
        (pm-eval-from-here
         #'(lambda () (python-shell-send-region
                       (1+ (point-min))
                       (1- (point-max))))))

      (evil-leader/set-key-for-mode 'python-mode
        "sc" 'python-shell-send-chunk)

      (evil-leader/set-key-for-mode 'python-mode
        "sC" 'python-shell-send-chunks-from-here)
      )
    :mode
    ("\\.texw" . poly-noweb+python-mode)
    )
  )

(defmacro noweb//setup-polymode-evil-maps (mode)
  "Set up evil maps for implementations of polymode.

Would love for one setup to apply to all derived modes, but nothing seems to
do that well."
  ;; (spacemacs/declare-prefix "m n" "polymode")

  `(evil-define-minor-mode-key 'normal ,mode
     (kbd (concat dotspacemacs-major-mode-leader-key " n n")) 'polymode-next-chunk-same-type
     (kbd (concat dotspacemacs-major-mode-leader-key " n N")) 'polymode-next-chunk
     (kbd (concat dotspacemacs-major-mode-leader-key " n p")) 'polymode-previous-chunk-same-type
     (kbd (concat dotspacemacs-major-mode-leader-key " n P")) 'polymode-previous-chunk
     (kbd (concat dotspacemacs-major-mode-leader-key " n K")) 'polymode-kill-chunk
     (kbd (concat dotspacemacs-major-mode-leader-key " n t")) 'polymode-toggle-chunk-narrowing
     (kbd (concat dotspacemacs-major-mode-leader-key " n w")) 'polymode-weave
     (kbd (concat dotspacemacs-major-mode-leader-key " n $")) 'polymode-show-process-buffer
     (kbd (concat dotspacemacs-major-mode-leader-key " n m")) 'polymode-mark-or-extend-chunk
     )
  ;; `(spacemacs/declare-prefix-for-mode ,mode "n" "polymode")
  ;; `(spacemacs/set-leader-keys-for-major-mode ,mode
  ;;    "nn" 'polymode-next-chunk-same-type
  ;;    "nN" 'polymode-next-chunk
  ;;    "np" 'polymode-previous-chunk-same-type
  ;;    "nP" 'polymode-previous-chunk
  ;;    "nK" 'polymode-kill-chunk
  ;;    "nt" 'polymode-toggle-chunk-narrowing
  ;;    "nw" 'polymode-weave
  ;;    "n$" 'polymode-show-process-buffer
  ;;    "nm" 'polymode-mark-or-extend-chunk
  ;;    )

  )

;;; packages.el ends here
