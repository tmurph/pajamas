;;; pajamas.el --- Wrap your dev commands in a warm blanket. -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Version: 0.0.1
;; Package-Requires: (project dash)

;; Author:  <trevor.m.murphy@gmail.com>
;; Keywords: convenience, tools, lisp

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

;;; Commentary:

;; This file contains generic infrastructure for dealing with build and
;; test commands on a project-by-project basis.
;;
;; The goal is to have one key that just "does the right thing" based on
;; `project-current'.
;;
;; Commands:
;;
;; `pajamas-build' will default to Emacs' `compile' but can be
;; customized to any other command
;;
;; `pajamas-test' defaults to "make test" but can be customized to any
;; other command

;;; TODO:

;; * Remove the "dependency" on dash library.  It's not actually
;;   necessary, I just really hate how ugly one function looks without
;;   it.

;;; Code:

(require 'cl-generic)
(require 'dash)

(defgroup pajamas nil
  "Build commands on the current project."
  :version "30.1"
  :group 'tools)

;;; Customization:

(defvar pajamas-find-functions (list #'pajamas--try-fallback)
  "Special hook to find the build system in the current project.

Each function on this hook is called in turn with one argument, the directory
in which to look, and should return either nil to mean that it is no
applicable, or a pajamas instance.  The exact form of the pajamas instance is
up to each respective function; the only practical limitation is to use values
that `cl-defmethod' can dispatch on, like a cons cell, or a list, or a CL
struct.")

(defvar pajamas-common-backend-markers-alist
  '(("Makefile" . Make)
    ("GNUMakefile" . Make)
    ("Eldev" . Eldev))
  "Assoc list assigning marker files to common backend symbols.")

;;; Internal Variables:



;;; Commands:

(cl-defgeneric pajamas-build (project)
  "Call an appropriate build command for PROJECT."
  (compile))

(cl-defgeneric pajamas-test (project)
  "Call an appropriate test command for PROJECT.")

;;; Internal Functions:

(defun pajamas--try-fallback (dir)
  "Try searching above DIR for files indicating a common build system."
  (let* ((backend-markers (->> pajamas-common-backend-markers-alist
                               (mapcar 'car-safe)
                               (delete nil)))
         (marker-re (rx-to-string
                     `(seq string-start (or ,@backend-markers)
                           string-end)))
         match
         (root
          (locate-dominating-file
           dir
           (lambda (d)
             (setq match
                   (condition-case nil
                       (car (directory-files d nil marker-re t 1))
                     (file-missing nil))))))
         (backend (cdr (assoc match pajamas-common-backend-markers-alist))))
    (when backend (cons backend root))))

(defun pajamas--find-in-directory (dir)
  "Run irregular hook `pajamas-find-functions' starting at DIR."
  (condition-case nil
      (run-hook-with-args-until-success 'pajamas-find-functions dir)
    (permission-denied nil)))

;;; Public Functions:

;;;###autoload
(defun pajamas-current (&optional directory)
  "Return the pajamas instance in DIRECTORY, defaulting to `default-directory'.

See the doc string of `pajamas-find-functions' for the general form of the
pajamas instance object."
  (pajamas--find-in-directory (or directory default-directory)))

(provide 'pajamas)
;;; pajamas.el ends here

