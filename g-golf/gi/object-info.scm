;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016, 2020
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

;; GIObjectInfo represents a GObject. This doesn't represent a specific
;; instance of a GObject, instead this represent the object type (eg
;; class).

;;; Code:


(define-module (g-golf gi object-info)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support utils)
  #:use-module (g-golf gobject enum-flags)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi registered-type-info)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (gi-object-import
            gi-object-show

	    g-object-info-get-abstract
	    g-object-info-get-parent
	    g-object-info-get-type-name
            g-object-info-get-type-init
	    g-object-info-get-n-constants
	    g-object-info-get-constant
	    g-object-info-get-n-fields
	    g-object-info-get-field
	    g-object-info-get-n-interfaces
	    g-object-info-get-interface
	    g-object-info-get-n-methods
	    g-object-info-get-method
	    g-object-info-find-method
	    g-object-info-get-n-properties
	    g-object-info-get-property
	    g-object-info-get-n-signals
	    g-object-info-get-signal
	    g-object-info-find-signal
	    g-object-info-get-n-vfuncs
	    g-object-info-get-vfunc
	    g-object-info-find-vfunc
	    g-object-info-get-class-struct))


;;;
;;; Import Interface
;;;


(define (gi-object-import info)
  ;; fixme
  #f)

(define %object-fmt
  "
~S is a (pointer to a) GIObjectInfo:

  Parent:
          namespace: ~S
               name: ~S
             g-type: ~A
        g-type-name: ~S

  Object:
          namespace: ~S
               name: ~S
             g-type: ~A
        g-type-name: ~S
           abstract: ~A
        n-constants: ~A
           n-fields: ~A
       n-interfaces: ~A
          n-methods: ~A
       n-properties: ~A
          n-signals: ~A
          n-vfuncts: ~A

")

(define* (gi-object-show info
                         #:optional (port (current-output-port)))
  (let* ((parent (g-object-info-get-parent info))
         (parent-namespace (or (and parent
                                    (g-base-info-get-namespace parent))
                               #f))
         (parent-name (or (and parent
                               (g-base-info-get-name parent))
                          #f))
         (parent-g-type (or (and parent
                                 (g-registered-type-info-get-g-type parent))
                            #f))
         (parent-g-type-name (or (and parent
                                      (g-registered-type-info-get-type-name parent))
                                 #f)))
    (format port "~?" %object-fmt
            (list
             info
             parent-namespace
             parent-name
             parent-g-type
             parent-g-type-name
             (g-base-info-get-namespace info)
             (g-base-info-get-name info)
             (g-registered-type-info-get-g-type info)
             (g-registered-type-info-get-type-name info)
             (g-object-info-get-abstract info)
             (g-object-info-get-n-constants info)
             (g-object-info-get-n-fields info)
             (g-object-info-get-n-interfaces info)
             (g-object-info-get-n-methods info)
             (g-object-info-get-n-properties info)
             (g-object-info-get-n-signals info)
             (g-object-info-get-n-vfuncs info)))
    (values)))


;;;
;;; Low level API
;;;

(define (g-object-info-get-abstract info)
  (gi->scm (g_object_info_get_abstract info) 'boolean))

(define (g-object-info-get-parent info)
  (gi->scm (g_object_info_get_parent info) 'pointer))

(define (g-object-info-get-type-name info)
  (gi->scm (g_object_info_get_type_name info) 'string))

(define (g-object-info-get-type-init info)
  (gi->scm (g_object_info_get_type_init info) 'string))

(define (g-object-info-get-n-constants info)
  (g_object_info_get_n_constants info))

(define (g-object-info-get-constant info index)
  (let ((pointer (g_object_info_get_constant info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-object-info-get-n-fields info)
  (g_object_info_get_n_fields info))

(define (g-object-info-get-field info index)
  (let ((pointer (g_object_info_get_field info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-object-info-get-n-interfaces info)
  (g_object_info_get_n_interfaces info))

(define (g-object-info-get-interface info index)
  (let ((pointer (g_object_info_get_interface info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-object-info-get-n-methods info)
  (g_object_info_get_n_methods info))

(define (g-object-info-get-method info index)
  (let ((pointer (g_object_info_get_method info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-object-info-find-method info name)
  (let ((pointer (g_object_info_find_method info
					    (string->pointer name))))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-object-info-get-n-properties info)
  (g_object_info_get_n_properties info))

(define (g-object-info-get-property info index)
  (let ((pointer (g_object_info_get_property info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-object-info-get-n-signals info)
  (g_object_info_get_n_signals info))

(define (g-object-info-get-signal info index)
  (let ((pointer (g_object_info_get_signal info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-object-info-find-signal info name)
  (let ((pointer (g_object_info_find_signal info
					    (string->pointer name))))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-object-info-get-n-vfuncs info)
  (g_object_info_get_n_vfuncs info))

(define (g-object-info-get-vfunc info index)
  (let ((pointer (g_object_info_get_vfunc info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-object-info-find-vfunc info name)
  (let ((pointer (g_object_info_find_vfunc info
					    (string->pointer name))))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-object-info-get-class-struct info)
  (let ((pointer (g_object_info_get_class_struct info)))
    (if (null-pointer? pointer)
	#f
	pointer)))


;;;
;;; GI Bindings
;;;

(define g_object_info_get_abstract
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_abstract"
				    %libgirepository)
                      (list '*)))

(define g_object_info_get_parent
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_parent"
				    %libgirepository)
                      (list '*)))

(define g_object_info_get_type_name
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_type_name"
				    %libgirepository)
                      (list '*)))

(define g_object_info_get_type_init
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_type_init"
				    %libgirepository)
                      (list '*)))

(define g_object_info_get_n_constants
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_constants"
				    %libgirepository)
                      (list '*)))

(define g_object_info_get_constant
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_constant"
				    %libgirepository)
                      (list '* int)))

(define g_object_info_get_n_fields
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_fields"
				    %libgirepository)
                      (list '*)))

(define g_object_info_get_field
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_field"
				    %libgirepository)
                      (list '* int)))

(define g_object_info_get_n_interfaces
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_interfaces"
				    %libgirepository)
                      (list '*)))

(define g_object_info_get_interface
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_interface"
				    %libgirepository)
                      (list '* int)))

(define g_object_info_get_n_methods
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_methods"
				    %libgirepository)
                      (list '*)))

(define g_object_info_get_method
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_method"
				    %libgirepository)
                      (list '* int)))

(define g_object_info_find_method
  (pointer->procedure '*
                      (dynamic-func "g_object_info_find_method"
				    %libgirepository)
                      (list '* '*)))

(define g_object_info_get_n_properties
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_properties"
				    %libgirepository)
                      (list '*)))

(define g_object_info_get_property
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_property"
				    %libgirepository)
                      (list '* int)))

(define g_object_info_get_n_signals
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_signals"
				    %libgirepository)
                      (list '*)))

(define g_object_info_get_signal
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_signal"
				    %libgirepository)
                      (list '* int)))

(define g_object_info_find_signal
  (pointer->procedure '*
                      (dynamic-func "g_object_info_find_signal"
				    %libgirepository)
                      (list '* '*)))

(define g_object_info_get_n_vfuncs
  (pointer->procedure int
                      (dynamic-func "g_object_info_get_n_vfuncs"
				    %libgirepository)
                      (list '*)))

(define g_object_info_get_vfunc
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_vfunc"
				    %libgirepository)
                      (list '* int)))

(define g_object_info_find_vfunc
  (pointer->procedure '*
                      (dynamic-func "g_object_info_find_vfunc"
				    %libgirepository)
                      (list '* '*)))

(define g_object_info_get_class_struct
  (pointer->procedure '*
                      (dynamic-func "g_object_info_get_class_struct"
				    %libgirepository)
                      (list '*)))
