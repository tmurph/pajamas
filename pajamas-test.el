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
(require 'dash)
(require 'with-simulated-input)

(describe "`pajamas-current'"
  (it "finds an Eldev project"
    (assess-with-filesystem '("Eldev"
                              "code.el"
                              "code-test.el")
      (assess-with-find-file "code-test.el"
        (expect (car-safe (pajamas-current))
                :to-be 'Eldev))))
  (it "prefers the `pajamas-current' variable"
    (assess-with-filesystem '("Eldev"
                              "code.el"
                              "code-test.el"
                              (".dir-locals.el"
                               "((nil (pajamas-current . foo)))"))
      (assess-with-find-file "code-test.el"
        (expect (car-safe (pajamas-current))
                :not :to-be 'Eldev))))
  (it "finds a Make project"
    (assess-with-filesystem '("Makefile"
                              "lisp/ob-awk.el"
                              "testing/lisp/test-ob-awk.el"
                              "testing/examples/ob-awk-test.in"
                              "testing/examples/ob-awk-test.org")
      (assess-with-find-file "testing/lisp/test-ob-awk.el"
        (expect (car-safe (pajamas-current))
                :to-be 'Make)))))

(describe "`pajamas-build'"
  :var (make-directory)

  (defun set-make-directory (_command &optional _comint)
    (interactive
     (list
      (let ((command (eval compile-command)))
        (if (or compilation-read-command current-prefix-arg)
	    (compilation-read-command command)
	  command))
      (consp current-prefix-arg)))
    (setq make-directory default-directory))

  (before-each
    (spy-on 'compile :and-call-fake 'set-make-directory))

  (it "runs `make` in a Make project"
    (assess-with-filesystem '("Makefile")
      (with-simulated-input "RET"
        (pajamas-build))
      (expect (car-safe (spy-calls-args-for 'compile 0))
              :to-match "\\`make ")))

  (it "runs `make` from the toplevel of a recursive Make project"
    (assess-with-filesystem '("Makefile"
                              "subdir/Makefile")
      (spy-on 'project-current :and-return-value
              (cons 'transient default-directory))
      (assess-with-find-file "subdir/Makefile"
        (with-simulated-input "RET"
          (pajamas-build))
        (expect make-directory :not :to-be default-directory)))))

(describe "`pajamas-test'"
  (it "runs `eldev test` in an Eldev project"
    (spy-on 'compile)
    (assess-with-filesystem '("Eldev"
                              "code.el"
                              "code-test.el")
      (pajamas-test)
      (expect 'compile :to-have-been-called-with "eldev test")))
  (it "runs `make test` in a Make project"
    (spy-on 'compile)
    (assess-with-filesystem '("Makefile")
      (with-simulated-input "RET"
        (pajamas-test))
      (expect (car-safe (spy-calls-args-for 'compile 0))
              :to-match "\\`make test "))))

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
