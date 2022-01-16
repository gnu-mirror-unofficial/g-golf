;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2022
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


(define-module (g-golf gobject type-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support flags)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-type->symbol
            symbol->g-type

            g-type-name
            g-type-from-name
            g-type-parent
            g-type-is-a
            g-type-class-ref
            g-type-class-peek
            g-type-class-unref
            g-type-fundamental
            g-type-ensure

	    %g-type-fundamental-flags
            %g-type-fundamental-types))


;;;
;;; G-Golf Low level API
;;;

(define %g-type-fundamental-shift 2)

(define (g-type->symbol g-type)
  ;; note that sbank special treat the 'boxed g-type maybe I'll have to
  ;; do it to, let's see how thngs goes, but keep this in mind ...
  (enum->symbol %g-type-fundamental-types
                (bitwise-arithmetic-shift-right (g-type-fundamental g-type)
                                                %g-type-fundamental-shift)))

(define (symbol->g-type symbol)
  (let ((value (enum->value %g-type-fundamental-types
                            (case symbol
                              ((utf8) 'string)
                              ((int32) 'int)
                              ((uint32) 'uint)
                              (else symbol)))))
    (and value
         (bitwise-arithmetic-shift value
                                   %g-type-fundamental-shift))))


;;;
;;; GObject Low level API
;;;

(define (g-type-from-name name)
  (let ((result (g_type_from_name (string->pointer name))))
    (if (= result 0)
        #f
        result)))

(define (g-type-name g-type)
  (gi->scm (g_type_name g-type) 'string))

(define (g-type-parent g-type)
  (g_type_parent g-type))

(define (g-type-is-a g-type is-a-g-type)
  (gi->scm (g_type_is_a g-type is-a-g-type)
           'boolean))

(define (g-type-class-ref g-type)
  (gi->scm (g_type_class_ref g-type) 'pointer))

(define (g-type-class-peek g-type)
  (gi->scm (g_type_class_peek g-type) 'pointer))

(define (g-type-class-unref g-class)
  (g_type_class_unref g-class))

(define (g-type-fundamental g-type)
  (g_type_fundamental g-type))

(define (g-type-ensure g-type)
  (g_type_ensure g-type))


;;;
;;; GObject Bindings
;;;

(define g_type_name
  (pointer->procedure '*
                      (dynamic-func "g_type_name"
				    %libgobject)
                      (list unsigned-long)))

(define g_type_from_name
  (pointer->procedure unsigned-long
                      (dynamic-func "g_type_from_name"
				    %libgobject)
                      (list '*)))

(define g_type_parent
  (pointer->procedure unsigned-long
                      (dynamic-func "g_type_parent"
				    %libgobject)
                      (list unsigned-long)))

(define g_type_is_a
  (pointer->procedure int
                      (dynamic-func "g_type_is_a"
				    %libgobject)
                      (list unsigned-long
                            unsigned-long)))

(define g_type_class_ref
  (pointer->procedure '*
                      (dynamic-func "g_type_class_ref"
				    %libgobject)
                      (list unsigned-long)))

(define g_type_class_peek
  (pointer->procedure '*
                      (dynamic-func "g_type_class_peek"
				    %libgobject)
                      (list unsigned-long)))

(define g_type_class_unref
  (pointer->procedure void
                      (dynamic-func "g_type_class_unref"
				    %libgobject)
                      (list '*)))

(define g_type_fundamental
  (pointer->procedure size_t
                      (dynamic-func "g_type_fundamental"
				    %libgobject)
                      (list unsigned-long)))

(define g_type_ensure
  (pointer->procedure void
                      (dynamic-func "g_type_ensure"
				    %libgobject)
                      (list unsigned-long)))


;;;
;;; Types and Values
;;;

(define %g-type-fundamental-flags
  (make <gi-flags>
    #:g-name "GTypeFundamentalFlags"
    #:enum-set '((classed . 1)
                 (instantiable . 2)
                 (derivable . 4)
                 (deep-derivable . 8))))

(define %g-type-fundamental-types
  ;; manually built, from the list of fundamental types starting with
  ;; G_TYPE_INVALID -> G_TYPE_OBJECT
  (make <gi-enum>
    #:g-name "GTypeFundamentalTypes"
    #:enum-set '(invalid
                 none
                 interface
                 char
                 uchar
                 boolean
                 int
                 uint
                 long
                 ulong
                 int64
                 uint64
                 enum
                 flags
                 float
                 double
                 string
                 pointer
                 boxed
                 param
                 object
                 variant)))
