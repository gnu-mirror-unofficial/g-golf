;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2020
;;;; Free Software Foundation, Inc.

;;;; This file is part of GNU G-Golf

;;;; GNU G-Golf is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as
;;;; published by the Free Software Foundation; either version 3 of the
;;;; License, or (at your option) any later version.

;;;; GNU G-Golf is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.

;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with GNU G-Golf.  If not, see
;;;; <https://www.gnu.org/licenses/lgpl.html>.
;;;;

;;; Commentary:

;;; Code:


(define-module (tests override)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (g-golf)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last))


(for-each (lambda (item)
            (gi-import-by-name "Gtk" item #:version "3.0"))
    '("HPaned"
      "VPaned"
      "ListStore"
      "TreeView"
      "init"))

(gtk-init 0 '())


(define-class <g-golf-test-override-gtk> (<test-case>))


(define-method (test-gtk-container-child-get-property
                (self <g-golf-test-override-gtk>))
  (let ((hpane  (make <gtk-hpaned>))
        (vpane  (make <gtk-vpaned>)))
    (gtk-container-add hpane vpane)
    (assert-false
     (gtk-container-child-get-property hpane
                                       vpane
                                       "resize"))
    (assert
     (gtk-container-child-set-property hpane
                                       vpane
                                       "resize"
                                       #t))
    (assert-true
     (gtk-container-child-get-property hpane
                                       vpane
                                       "resize"))))


(define-method (test-gtk-list-store-set-value (self <g-golf-test-override-gtk>))
  (let* ((store (assert (gtk-list-store-new 2 '(int string))))
         (iter (gtk-list-store-insert store 0)))
    (assert (gtk-list-store-set-value store iter 0 10))
    (assert (gtk-list-store-set-value store iter 1 "hello-world"))
    (assert-true (= (gtk-tree-model-get-value store iter 0)
                    10))
    (assert-true (string=? (gtk-tree-model-get-value store iter 1)
                           "hello-world"))))


(exit-with-summary (run-all-defined-test-cases))
