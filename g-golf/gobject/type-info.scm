;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2019
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
  #:use-module (g-golf support flag)  

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-type->symbol
            symbol->g-type
            g-type-name
            g-type-fundamental
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

(define (g-type-name g-type)
  (gi->scm (g_type_name g-type) 'string))

(define (g-type-fundamental g-type)
  (g_type_fundamental g-type))


;;;
;;; GObject Bindings
;;;

(define g_type_name
  (pointer->procedure '*
                      (dynamic-func "g_type_name"
				    %libgobject)
                      (list int64)))

(define g_type_fundamental
  (pointer->procedure size_t
                      (dynamic-func "g_type_fundamental"
				    %libgobject)
                      (list unsigned-long)))


;;;
;;; Types and Values
;;;

(define %g-type-fundamental-flags
  (make <gi-flag>
    #:gi-name "GTypeFundamentalFlags"
    #:enum-set '((classed . 1)
                 (instantiable . 2)
                 (derivable . 4)
                 (deep-derivable . 8))))

(define %g-type-fundamental-types
  ;; manually built, from the list of fundamental types starting with
  ;; G_TYPE_INVALID -> G_TYPE_OBJECT
  (make <gi-enum>
    #:gi-name "GTypeFundamentalTypes"
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
                 object)))
