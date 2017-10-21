;;
;; Noweb + Python
;; Modeled after https://github.com/vspinu/polymode/blob/master/modes/poly-R.el
;;
;; Author: Brandon T. Willard
;; Maintainer: Brandon T. Willard
;; Copyright (C) 2017-2017, Brandon T. Willard, all rights reserved.
;; Version: 1.0
;; URL: https://github.com/brandonwillard/spacemacs-custom
;; Keywords: emacs
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'polymode)

(defcustom pm-poly/python
  (pm-polymode-one "python"
                   :hostmode 'pm-host/python
                   :innermode 'pm-inner/fundamental)
  "Python root polymode. Not intended to be used directly."
  :group 'polymodes
  :type 'object)

;; NOWEB
(require 'poly-noweb)
(defcustom pm-poly/noweb+python
  (clone pm-poly/noweb :innermode 'pm-inner/noweb+python)
  "Noweb for Python configuration"
  :group 'polymodes
  :type 'object)

(defcustom pm-inner/noweb+python
  (clone pm-inner/noweb
         :mode 'python-mode)
  "Noweb for Python"
  :group 'innermodes
  :type 'object)

;;;###autoload (autoload 'poly-noweb+python-mode "poly-python")
(define-polymode poly-noweb+python-mode pm-poly/noweb+python :lighter " PM-texw")
;;(add-hook 'noweb+python-mode-hook 'poly-noweb+python-mode)

;; or as in https://emacs.stackexchange.com/a/20446...
;; (defcustom pm-inner/python
;;   (pm-hbtchunkmode "python"
;;                    :mode 'python-mode
;;                    :head-reg  "\\\\begin{pycode}"
;;                    :tail-reg  "\\\\end{pycode}")
;;   "python typical chunk."
;;   :group 'innermodes
;;   :type 'object)

;; (defcustom pm-poly/latex-python
;;   (pm-polymode-one "latex-python"
;;                    :hostmode 'pm-host/latex
;;                    :innermode 'pm-inner/python)
;;   "latex-python typical polymode."
;;   :group 'polymodes
;;   :type 'object)

;; (define-polymode poly-latex+python-mode pm-poly/latex-python)
;; (add-to-list 'auto-mode-alist '("\\.tex$" . poly-latex+python-mode))

;; Pweave 
;; Also, see examples here: https://github.com/vspinu/polymode/blob/master/modes/poly-R.el
;; TODO: Check for existence of `pweave' command.  E.g.
;; (defun spacemacs/pweave-executable-find (command)
;;   "Find executable taking pyenv shims into account."
;;   (if (executable-find "pyenv")
;;       (progn
;;         (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command))))
;;           (unless (string-match "not found" pyenv-string)
;;             pyenv-string)))
;;     (executable-find command)))
;; (let ((trace (cond ((spacemacs/pyenv-executable-find "pweave") "import wdb; wdb.set_trace()")
;;                    ((spacemacs/pyenv-executable-find "ipdb") "import ipdb; ipdb.set_trace()")
;;                    ((spacemacs/pyenv-executable-find "pudb") "import pudb; pudb.set_trace()")
;;                    ((spacemacs/pyenv-executable-find "ipdb3") "import ipdb; ipdb.set_trace()")
;;                    ((spacemacs/pyenv-executable-find "pudb3") "import pudb; pudb.set_trace()")
;;                    (t "import pdb; pdb.set_trace()")))
(defcustom pm-weaver/pweave
  (pm-shell-weaver "pweave"
                   :from-to
                   '(("latex" "\\.[tT]exw\\'" "tex" "LaTeX" "pweave -i noweb -o %o %i")
                     ("markdown" "\\.[pP]?md]\\'" "md" "Markdown" "pweave -i markdown -o %o %i")
                     ))
  "Shell Pweave weaver."
  :group 'polymode-weave
  :type 'object)

(polymode-register-weaver pm-weaver/pweave nil
                          pm-poly/noweb+python
                          ;; pm-poly/markdown
                          )

(provide 'poly-python)
