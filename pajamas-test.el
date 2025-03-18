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

(require 'buttercup)
(require 'assess)
(require 'dash)

(describe "`pajamas-current'"
  (it "finds an Eldev project"
    (assess-with-filesystem '("Eldev"
                              "code.el"
                              "code-test.el")
      (expect (->> "code-test.el"
                   (file-name-concat default-directory)
                   (pajamas-current)
                   car)
              :to-be 'Eldev))))

(describe "`pajamas-test'"
  (it "runs `eldev test` in an Eldev project"
    (spy-on 'compile)
    (assess-with-filesystem '("Eldev"
                              "code.el"
                              "code-test.el")
      (pajamas-test)
      (expect 'compile :to-have-been-called-with "eldev test"))))

(provide 'pajamas-test)
;;; pajamas-test.el ends here
