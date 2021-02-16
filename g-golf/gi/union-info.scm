;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2019, 2021
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


(define-module (g-golf gi union-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support flags)
  #:use-module (g-golf init)
  #:use-module (g-golf gobject type-info)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi registered-type-info)
  #:use-module (g-golf gi field-info)
  #:use-module (g-golf gi type-info)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (gi-union-import
	    gi-union-field-desc

	    g-union-info-get-n-fields
	    g-union-info-get-field
	    g-union-info-get-n-methods
	    g-union-info-get-method
            g-union-info-is-discriminated?
            g-union-info-get-discriminator-offset
            g-union-info-get-discriminator-type
            g-union-info-get-discriminator
            g-union-info-find-method
	    g-union-info-get-size
            g-union-info-get-alignment))


;;;
;;; Build Interface
;;;

(define* (gi-union-import info #:key (flags #f))
  (let (#;(id (g-registered-type-info-get-g-type info))
        #;(g-name (g-type-name id))
        (u-fields (gi-union-field-desc info)))
    ;; The union fields need to be further processed, but the
    ;; functionality to do so is part of the (g-golf hl-api function)
    ;; module. For this reason, unlike for gi-struct-import, we do not
    ;; create the <gi-union> instance and let this to be done by the
    ;; caller.
    u-fields))

(define (gi-union-field-desc info)
  (letrec ((get-union-field-desc
	    (lambda (info n i u-desc)
	      (if (= i n)
		  (reverse! u-desc)
		  (let* ((f-info (g-union-info-get-field info i))
			 (f-name (g-base-info-get-name f-info))
			 (f-type-info (g-field-info-get-type f-info))
                         (f-type (g-type-info-get-tag f-type-info)))
		    (g-base-info-unref f-info)
		    (get-union-field-desc info
				          n
				          (+ i 1)
				          (cons (list (string->symbol
                                                       (g-studly-caps-expand f-name))
                                                      f-type-info)
					        u-desc)))))))
    (get-union-field-desc info
		          (g-union-info-get-n-fields info)
		          0
		          '())))


;;;
;;; Low level API
;;;

(define (g-union-info-get-n-fields info)
  (g_union_info_get_n_fields info))

(define (g-union-info-get-field info n)
  (gi->scm (g_union_info_get_field info n)
           'pointer))

(define (g-union-info-get-n-methods info)
  (g_union_info_get_n_methods info))

(define (g-union-info-get-method info n)
  (gi->scm (g_union_info_get_method info n)
           'pointer))

(define (g-union-info-is-discriminated? info)
  (gi->scm (g_union_info_is_discriminated info)
           'boolean))

(define (g-union-info-get-discriminator-offset info)
  (g_union_info_get_discriminator_offset info))

(define (g-union-info-get-discriminator-type info)
  (gi->scm (g_union_info_get_discriminator_type info)
           'pointer))

(define (g-union-info-get-discriminator info n)
  (gi->scm (g_union_info_get_discriminator info n)
           'pointer))

(define (g-union-info-find-method info name)
  (gi->scm (g_union_info_find_method info
                                     (string->pointer name))
           'pointer))

(define (g-union-info-get-size info)
  (g_union_info_get_size info))

(define (g-union-info-get-alignment info)
  (g_union_info_get_alignment info))


;;;
;;; GI Bindings
;;;

(define g_union_info_get_n_fields
  (pointer->procedure int
                      (dynamic-func "g_union_info_get_n_fields"
				    %libgirepository)
                      (list '*)))

(define g_union_info_get_field
  (pointer->procedure '*
                      (dynamic-func "g_union_info_get_field"
				    %libgirepository)
                      (list '*		;; info
                            int)))	;; n

(define g_union_info_get_n_methods
  (pointer->procedure int
                      (dynamic-func "g_union_info_get_n_methods"
				    %libgirepository)
                      (list '*)))

(define g_union_info_get_method
  (pointer->procedure '*
                      (dynamic-func "g_union_info_get_method"
				    %libgirepository)
                      (list '*		;; info
                            int)))	;; n

(define g_union_info_is_discriminated
  (pointer->procedure int
                      (dynamic-func "g_union_info_is_discriminated"
				    %libgirepository)
                      (list '*)))

(define g_union_info_get_discriminator_offset
  (pointer->procedure int
                      (dynamic-func "g_union_info_get_discriminator_offset"
				    %libgirepository)
                      (list '*)))

(define g_union_info_get_discriminator_type
  (pointer->procedure '*
                      (dynamic-func "g_union_info_get_discriminator_type"
				    %libgirepository)
                      (list '*)))

(define g_union_info_get_discriminator
  (pointer->procedure '*
                      (dynamic-func "g_union_info_get_discriminator"
				    %libgirepository)
                      (list '*		;; info
                            int)))	;; n

(define g_union_info_find_method
  (pointer->procedure '*
                      (dynamic-func "g_union_info_find_method"
				    %libgirepository)
                      (list '*		;; info
                            '*)))	;; name

(define g_union_info_get_size
  (pointer->procedure unsigned-long
                      (dynamic-func "g_union_info_get_size"
				    %libgirepository)
                      (list '*)))

(define g_union_info_get_alignment
  (pointer->procedure unsigned-long
                      (dynamic-func "g_union_info_get_alignment"
				    %libgirepository)
                      (list '*)))
