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


(define-module (g-golf gi interface-info)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support utils)
  #:use-module (g-golf gobject type-info)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi registered-type-info)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (gi-interface-import
            gi-interface-show
            
	    g-interface-info-get-n-prerequisites
	    g-interface-info-get-prerequisite
	    g-interface-info-get-n-properties
	    g-interface-info-get-property
	    g-interface-info-get-n-methods
	    g-interface-info-get-method
	    g-interface-info-find-method
	    g-interface-info-get-n-signals
	    g-interface-info-get-signal
	    g-interface-info-find-signal
	    g-interface-info-get-n-vfuncs
	    g-interface-info-get-vfunc
	    g-interface-info-find-vfunc

	    g-interface-info-get-n-constants
	    g-interface-info-get-constant

            g-interface-info-get-iface-struct))


;;;
;;; Import Interface
;;;

(define (gi-interface-import info)
  (let* ((id (g-registered-type-info-get-g-type info))
         (g-name (g-type-name id))
         (name (g-name->name g-name)))
    (list 'interface name g-name id #t)))

(define %iface-fmt
  "
~S is a (pointer to a) GIInterfaceInfo:

          namespace: ~S
               name: ~S
             g-type: ~A
        g-type-name: ~s
    n-prerequisites: ~A
       n-properties: ~A
          n-methods: ~A
          n-signals: ~A
          n-vfuncts: ~A
        n-constants: ~A
       iface-struct: ~S
  iface-struct-name: ~S

")

(define* (gi-interface-show info
                            #:optional (port (current-output-port)))
  (let* ((iface-struct (g-interface-info-get-iface-struct info))
         (iface-struct-name (g-base-info-get-name iface-struct)))
    (format port "~?" %iface-fmt
            (list
             info
             (g-base-info-get-namespace info)
             (g-base-info-get-name info)
             (g-registered-type-info-get-g-type info)
             (g-registered-type-info-get-type-name info)
             (g-interface-info-get-n-prerequisites info)
             (g-interface-info-get-n-properties info)
             (g-interface-info-get-n-methods info)
             (g-interface-info-get-n-signals info)
             (g-interface-info-get-n-vfuncs info)
             (g-interface-info-get-n-constants info)
             iface-struct
             iface-struct-name))
    (values)))


;;;
;;; Low level API
;;;


(define (g-interface-info-get-n-prerequisites info)
  (g_interface_info_get_n_prerequisites info))

(define (g-interface-info-get-prerequisite info index)
  (gi->scm (g_interface_info_get_prerequisite info index)
           'pointer))

(define (g-interface-info-get-n-properties info)
  (g_interface_info_get_n_properties info))

(define (g-interface-info-get-property info index)
  (gi->scm (g_interface_info_get_property info index)
           'pointer))

(define (g-interface-info-get-n-methods info)
  (g_interface_info_get_n_methods info))

(define (g-interface-info-get-method info index)
  (gi->scm (g_interface_info_get_method info index)
           'pointer))

(define (g-interface-info-find-method info name)
  (gi->scm (g_interface_info_find_method info
					 (string->pointer name))
           'pointer))

(define (g-interface-info-get-n-signals info)
  (g_interface_info_get_n_signals info))

(define (g-interface-info-get-signal info index)
  (gi->scm (g_interface_info_get_signal info index)
           'pointer))

(define (g-interface-info-find-signal info name)
  (gi->scm (g_interface_info_find_signal info
					 (string->pointer name))
           'pointer))

(define (g-interface-info-get-n-vfuncs info)
  (g_interface_info_get_n_vfuncs info))

(define (g-interface-info-get-vfunc info index)
  (gi->scm (g_interface_info_get_vfunc info index)
           'pointer))

(define (g-interface-info-find-vfunc info name)
  (gi->scm (g_interface_info_find_vfunc info
					(string->pointer name))
           'pointer))

(define (g-interface-info-get-n-constants info)
  (g_interface_info_get_n_constants info))

(define (g-interface-info-get-constant info index)
  (gi->scm (g_interface_info_get_constant info index)
           'pointer))

(define (g-interface-info-get-iface-struct info)
  (gi->scm (g_interface_info_get_iface_struct info)
           'pointer))


;;;
;;; GI Bindings
;;;

(define g_interface_info_get_n_prerequisites
  (pointer->procedure int
                      (dynamic-func "g_interface_info_get_n_prerequisites"
				    %libgirepository)
                      (list '*)))

(define g_interface_info_get_prerequisite
  (pointer->procedure '*
                      (dynamic-func "g_interface_info_get_prerequisite"
				    %libgirepository)
                      (list '* int)))

(define g_interface_info_get_n_properties
  (pointer->procedure int
                      (dynamic-func "g_interface_info_get_n_properties"
				    %libgirepository)
                      (list '*)))

(define g_interface_info_get_property
  (pointer->procedure '*
                      (dynamic-func "g_interface_info_get_property"
				    %libgirepository)
                      (list '* int)))

(define g_interface_info_get_n_methods
  (pointer->procedure int
                      (dynamic-func "g_interface_info_get_n_methods"
				    %libgirepository)
                      (list '*)))

(define g_interface_info_get_method
  (pointer->procedure '*
                      (dynamic-func "g_interface_info_get_method"
				    %libgirepository)
                      (list '* int)))

(define g_interface_info_find_method
  (pointer->procedure '*
                      (dynamic-func "g_interface_info_find_method"
				    %libgirepository)
                      (list '* '*)))

(define g_interface_info_get_n_signals
  (pointer->procedure int
                      (dynamic-func "g_interface_info_get_n_signals"
				    %libgirepository)
                      (list '*)))

(define g_interface_info_get_signal
  (pointer->procedure '*
                      (dynamic-func "g_interface_info_get_signal"
				    %libgirepository)
                      (list '* int)))

(define g_interface_info_find_signal
  (pointer->procedure '*
                      (dynamic-func "g_interface_info_find_signal"
				    %libgirepository)
                      (list '* '*)))

(define g_interface_info_get_n_vfuncs
  (pointer->procedure int
                      (dynamic-func "g_interface_info_get_n_vfuncs"
				    %libgirepository)
                      (list '*)))

(define g_interface_info_get_vfunc
  (pointer->procedure '*
                      (dynamic-func "g_interface_info_get_vfunc"
				    %libgirepository)
                      (list '* int)))

(define g_interface_info_find_vfunc
  (pointer->procedure '*
                      (dynamic-func "g_interface_info_find_vfunc"
				    %libgirepository)
                      (list '* '*)))

(define g_interface_info_get_n_constants
  (pointer->procedure int
                      (dynamic-func "g_interface_info_get_n_constants"
				    %libgirepository)
                      (list '*)))

(define g_interface_info_get_constant
  (pointer->procedure '*
                      (dynamic-func "g_interface_info_get_constant"
				    %libgirepository)
                      (list '* int)))

(define g_interface_info_get_iface_struct
  (pointer->procedure '*
                      (dynamic-func "g_interface_info_get_iface_struct"
				    %libgirepository)
                      (list '*)))
