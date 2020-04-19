;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2020
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


(define-module (g-golf gobject generic-values)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support libg-golf)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (%g-value-struct

            g-value-size
            g-value-new

            g-value-init
            g-value-unset))


;;;
;;; GObject Low level API
;;;

(define %g-value-struct
  (list unsigned-long double double))

;; from libg-golf
(define (g-value-size)
  (g_value_size))

(define (g-value-new)
  (make-c-struct %g-value-struct
                 (list 0 0 0)))

(define (g-value-init g-type)
  (let ((g-value (g-value-new)))
    (g_value_init g-value g-type)
    g-value))

(define (g-value-unset g-value)
  (g_value_unset g-value)
  (values))


;;;
;;; GObject Bindings
;;;

(define g_value_init
  (pointer->procedure '*
                      (dynamic-func "g_value_init"
				    %libgobject)
                      (list '*
                            unsigned-long)))

(define g_value_unset
  (pointer->procedure void
                      (dynamic-func "g_value_unset"
				    %libgobject)
                      (list '*)))
