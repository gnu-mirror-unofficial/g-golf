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


(define-module (g-golf gi base-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support enum)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-base-info-ref
	    g-base-info-unref
	    g-base-info-equal
	    g-base-info-get-type
	    g-base-info-get-typelib
	    g-base-info-get-namespace
	    g-base-info-get-name
	    g-base-info-get-attribute
	    g-base-info-iterate-attributes
	    g-base-info-get-container
	    g-base-info-is-deprecated
            %gi-info-type))


;;;
;;; Low level API
;;;

(define (g-base-info-ref info)
  (g_base_info_ref info))

(define (g-base-info-unref info)
  (g_base_info_unref info))

(define (g-base-info-equal info1 info2)
  (gi->scm (g_base_info_equal info1 info2) 'boolean))
  
(define (g-base-info-get-type info)
  (enum->symbol %gi-info-type
                (g_base_info_get_type info)))

(define (g-base-info-get-typelib info)
  (g_base_info_get_typelib info))

(define (g-base-info-get-namespace info)
  (gi->scm (g_base_info_get_namespace info) 'string))

(define (g-base-info-get-name info)
  (gi->scm (g_base_info_get_name info) 'string))

(define (g-base-info-get-attribute info name)
  (gi->scm (g_base_info_get_attribute info
                                      (string->pointer name))
           'string))

(define (g-base-info-iterate-attributes info proc)
  (let ((iter (gi-attribute-iter-new))
	(name (gi-pointer-new))
	(value (gi-pointer-new)))
    (while (> (g_base_info_iterate_attributes info iter name value)
	      0)
      (proc name value))
    (values)))

(define (g-base-info-get-container info)
  (let ((pointer (g_base_info_get_container info)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-base-info-is-deprecated info)
  (gi->scm (g_base_info_is_deprecated info) 'boolean))


;;;
;;; GI Bindings
;;;

(define g_base_info_ref
  (pointer->procedure '*
                      (dynamic-func "g_base_info_ref"
				    %libgirepository)
                      (list '*)))

(define g_base_info_unref
  (pointer->procedure void
                      (dynamic-func "g_base_info_unref"
				    %libgirepository)
                      (list '*)))

(define g_base_info_equal
  (pointer->procedure int
                      (dynamic-func "g_base_info_equal"
				    %libgirepository)
                      (list '* '*)))

(define g_base_info_get_type
  (pointer->procedure int
                      (dynamic-func "g_base_info_get_type"
				    %libgirepository)
                      (list '*)))

(define g_base_info_get_typelib
  (pointer->procedure '*
                      (dynamic-func "g_base_info_get_typelib"
				    %libgirepository)
                      (list '*)))

(define g_base_info_get_namespace
  (pointer->procedure '*
                      (dynamic-func "g_base_info_get_namespace"
				    %libgirepository)
                      (list '*)))

(define g_base_info_get_name
  (pointer->procedure '*
                      (dynamic-func "g_base_info_get_name"
				    %libgirepository)
                      (list '*)))

(define g_base_info_get_attribute
  (pointer->procedure '*
                      (dynamic-func "g_base_info_get_attribute"
				    %libgirepository)
                      (list '* '*)))

(define g_base_info_iterate_attributes
  (pointer->procedure int
                      (dynamic-func "g_base_info_iterate_attributes"
				    %libgirepository)
                      (list '* '* '* '*)))

(define g_base_info_get_container
  (pointer->procedure '*
                      (dynamic-func "g_base_info_get_container"
				    %libgirepository)
                      (list '*)))

(define g_base_info_is_deprecated
  (pointer->procedure int
                      (dynamic-func "g_base_info_is_deprecated"
				    %libgirepository)
                      (list '*)))


;;;
;;; Types and Values
;;;

(define %gi-info-type
  (make <gi-enum>
    #:g-name "GIInfoType"
    #:enum-set '(invalid
                 function
                 callback
                 struct
                 boxed
                 enum
                 flags
                 object
                 interface
                 constant
                 error-domain ;; invalid_0 - deleted
                 union
                 value
                 signal
                 vfunc
                 property
                 field
                 arg
                 type
                 unresolved)))
