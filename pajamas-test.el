;;; pajamas-test.el --- Unit tests for pajamas.      -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:  <trevor.m.murphy@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'pajamas)

(require 'assess)
(require 'buttercup)
(require 'with-simulated-input)

;;; from python.el, so we can silence the warning
(defvar python-indent-guess-indent-offset-verbose)

(describe "`pajamas-current'"

  (it "finds an Eldev project"
    (assess-with-filesystem '("Eldev"
                              "code.el"
                              "code-test.el")
      (assess-with-find-file "code-test.el"
        (expect (car-safe (pajamas-current)) :to-be 'Eldev))))

  (it "prefers the `pajamas-current' variable"
    (assess-with-filesystem '("Eldev"
                              "code.el"
                              "code-test.el"
                              (".dir-locals.el"
                               "((nil (pajamas-current . foo)))"))
      (assess-with-find-file "code-test.el"
        (expect (car-safe (pajamas-current)) :not :to-be 'Eldev))))

  (it "finds a Make project"
    (assess-with-filesystem '("Makefile"
                              "lisp/ob-awk.el"
                              "testing/lisp/test-ob-awk.el"
                              "testing/examples/ob-awk-test.in"
                              "testing/examples/ob-awk-test.org")
      (assess-with-find-file "testing/lisp/test-ob-awk.el"
        (expect (car-safe (pajamas-current)) :to-be 'Make))))

  (it "prefers Make over Eldev"
    (assess-with-filesystem '("Makefile"
                              "Eldev"
                              "code.el")
      (assess-with-find-file "code.el"
        (expect (car-safe (pajamas-current)) :to-be 'Make)))))


;;; hey is it kinda weird that `pajamas-build' always triggers an
;;; interactive call?  like in practice it's fine, I only ever use it
;;; interactively.  but maybe it should be better than that.
(describe "`pajamas-build'"

  (before-each
    (spy-on 'compile))

  (it "runs `eldev package` in an Eldev project"
    (assess-with-filesystem '("Eldev"
                              "code.el"
                              "code-test.el")
      (let (compile-history)
        (pajamas-build))
      (expect 'compile :to-have-been-called-with "eldev package")))

  (it "runs `make` in a Make project"
    (assess-with-filesystem '("Makefile")
      (let (compile-history)
        (with-simulated-input "RET"
          (pajamas-build))))
    (expect (car-safe (spy-calls-args-for 'compile 0)) :to-match "\\`make "))

  (it "runs `make` from the toplevel of a recursive Make project"
    (cl-letf* ((make-in nil)
               (project-root nil)
               (project-find-functions
                (list (lambda (_dir) (cons 'transient project-root))))
               ((symbol-function 'compile)
                (lambda ()
                  (interactive)
                  (setq make-in default-directory))))
      (assess-with-filesystem '("Makefile"
                                "subdir/Makefile")
        (setq project-root default-directory)
        (assess-with-find-file "subdir/Makefile"
          (let (compile-history)
            (with-simulated-input "RET"
              (pajamas-build)))))
      (expect make-in :to-equal project-root))))


(describe "`pajamas-test'"

  (before-each
    (spy-on 'compile))

  (it "runs `eldev test` in an Eldev project"
    (assess-with-filesystem '("Eldev"
                              "code.el"
                              "code-test.el")
      (let (compile-history)
        (pajamas-test))
      (expect 'compile :to-have-been-called-with "eldev test")))

  (it "runs `make test` in a Make project"
    (assess-with-filesystem '("Makefile")
      (let (compile-history)
        (with-simulated-input "RET"
          (pajamas-test)))
      (expect (car-safe (spy-calls-args-for 'compile 0))
              :to-match "\\`make test "))))


(describe "`pajamas-find-test-file'"
  :var (project-root)

  (before-each
    (spy-on 'project-current :and-call-fake
            (lambda (&rest args) (cons 'transient project-root))))

  (it "jumps between Python source and test files"
    (let ((python-indent-guess-indent-offset-verbose nil))
      (assess-with-filesystem '("Makefile"
                                "pyproject.toml"
                                "src/mymodule/__init__.py"
                                "src/mymodule/one.py"
                                "tests/test_one.py")
        (setq project-root default-directory)
        (assess-with-find-file "src/mymodule/one.py"
          (pajamas-find-test-file)
          (expect (buffer-file-name) :to-match "tests/test_one.py\\'"))
        (assess-with-find-file "tests/test_one.py"
          (pajamas-find-test-file)
          (expect (buffer-file-name) :to-match "src/mymodule/one.py\\'")))))

  (it "jumps between Elisp source and test files"
    (assess-with-filesystem '("Eldev"
                              "code.el"
                              "code-test.el")
      (setq project-root default-directory)
      (assess-with-find-file "code.el"
        (pajamas-find-test-file)
        (expect (buffer-file-name) :to-match "code-test.el\\'"))
      (assess-with-find-file "code-test.el"
        (pajamas-find-test-file)
        (expect (buffer-file-name) :to-match "code.el\\'")))))


(describe "`pajamas-mode'"

  (after-each
    (pajamas-mode -1))

  (it "shadows bindings in `project-prefix-map'"
    (pajamas-mode 1)
    (expect (keymap-lookup project-prefix-map "t") :to-be 'pajamas-test))

  (it "restores `project-prefix-map' bindings when disabled"
    (let ((prev-binding (keymap-lookup project-prefix-map "t")))
      (expect prev-binding :not :to-be 'pajamas-test)
      (pajamas-mode 1)
      (pajamas-mode -1)
      (expect (keymap-lookup project-prefix-map "t") :to-be prev-binding))))


(provide 'pajamas-test)
;;; pajamas-test.el ends here
