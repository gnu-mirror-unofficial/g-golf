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


(define-module (g-golf gobject gobject)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi common-types)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi registered-type-info)
  #:use-module (g-golf gi property-info)
  #:use-module (g-golf gi type-info)
  #:use-module (g-golf support libg-golf)
  #:use-module (g-golf support enum)
  #:use-module (g-golf gobject type-info)
  #:use-module (g-golf gobject generic-values)
  #:use-module (g-golf gobject params-vals)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (gi-object-property-g-type
            gi-interface-g-type
            g-object-get-property
	    g-object-set-property
            g-object-new
            #;g-object-type-name
            g-object-type))


;;;
;;; GObject Low level API
;;;

(define (gi-object-property-g-type property)
  (let* ((type-info (g-property-info-get-type property))
	 (type-tag (g-type-info-get-tag type-info)))
    (case type-tag
      ((interface)
       (gi-interface-g-type type-info))
      (else
       (symbol->g-type type-tag)))))

(define (gi-interface-g-type info)
  (let* ((interface (g-type-info-get-interface info))
         (g-type (g-registered-type-info-get-g-type interface)))
    (g-base-info-unref interface)
    g-type))

(define* (g-object-get-property object property #:optional (g-type #f))
  (let* ((name (g-base-info-get-name property))
         (g-type (or g-type
                     (gi-object-property-g-type property)))
	 (g-value (g-value-init g-type)))
    (g_object_get_property object
			   (string->pointer name)
			   g-value)
    (let ((result (g-value-ref g-value)))
      (g-value-unset g-value)
      result)))

(define* (g-object-set-property object property value #:optional (g-type #f))
  (let* ((name (g-base-info-get-name property))
         (g-type (or g-type
                     (gi-object-property-g-type property)))
	 (g-value (g-value-init g-type)))
    (g-value-set! g-value value)
    (g_object_set_property object
			   (string->pointer name)
			   g-value)
    (g-value-unset g-value)
    value))

(define (g-object-new gtype)
  (gi->scm (g_object_new gtype %null-pointer) 'pointer))


;;;
;;; From libg-golf
;;;

(define (g-object-type object)
  (g-object-type-c object))

#!

Not working yet, see libg-golf.scm for a problem description

(define (g-object-type-name object)
  (gi->scm (g-object-type-name-c object) 'string))

!#


;;;
;;; GObject Bindings
;;;

(define g_object_new
  (pointer->procedure '*
                      (dynamic-func "g_object_new"
				    %libgobject)
                      (list unsigned-long '*)))

(define g_object_get_property
  (pointer->procedure void
                      (dynamic-func "g_object_get_property"
				    %libgobject)
                      (list '* '* '*)))

(define g_object_set_property
  (pointer->procedure void
                      (dynamic-func "g_object_set_property"
				    %libgobject)
                      (list '* '* '*)))
