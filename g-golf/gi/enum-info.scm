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


(define-module (g-golf gi enum-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support flag)
  #:use-module (g-golf init)
  #:use-module (g-golf gobject type-info)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi registered-type-info)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (gi-enum-import
	    gi-enum-value-values

	    g-enum-info-get-n-values
	    g-enum-info-get-value
	    g-enum-info-get-n-methods
	    g-enum-info-get-method

	    g-value-info-get-value))


;;;
;;; Build Interface
;;;

(define* (gi-enum-import info #:key (flag #f))
  (let* ((id (g-registered-type-info-get-g-type info))
         (name (g-type-name id))
	 (e-vals (gi-enum-value-values info)))
    (make (if flag <gi-flag> <gi-enum>)
      #:g-type id
      #:gi-name name
      #:enum-set e-vals)))

(define (gi-enum-value-values info)
  (letrec ((get-enum-values
	    (lambda (info n i v-set)
	      (if (= i n)
		  (reverse! v-set)
		  (let* ((value-info (g-enum-info-get-value info i))
			 (name (g-base-info-get-name value-info))
			 (value (g-value-info-get-value value-info)))
		    (g-base-info-unref value-info)
		    (get-enum-values info
				     n
				     (+ i 1)
				     (cons (cons (string->symbol
                                                  (g-studly-caps-expand name))
                                                 value)
					   v-set)))))))
    (get-enum-values info
		     (g-enum-info-get-n-values info)
		     0
		     '())))


;;;
;;; Low level API
;;;

(define (g-enum-info-get-n-values info)
  (g_enum_info_get_n_values info))

(define (g-enum-info-get-value info index)
  (let ((pointer (g_enum_info_get_value info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-enum-info-get-n-methods info)
  (g_enum_info_get_n_methods info))

(define (g-enum-info-get-method info index)
  (let ((pointer (g_enum_info_get_method info index)))
    (if (null-pointer? pointer)
	#f
	pointer)))

(define (g-value-info-get-value info)
  (g_value_info_get_value info))


;;;
;;; GI Bindings
;;;

(define g_enum_info_get_n_values
  (pointer->procedure int
                      (dynamic-func "g_enum_info_get_n_values"
				    %libgirepository)
                      (list '*)))

(define g_enum_info_get_value
  (pointer->procedure '*
                      (dynamic-func "g_enum_info_get_value"
				    %libgirepository)
                      (list '* int)))

(define g_enum_info_get_n_methods
  (pointer->procedure int
                      (dynamic-func "g_enum_info_get_n_methods"
				    %libgirepository)
                      (list '*)))

(define g_enum_info_get_method
  (pointer->procedure '*
                      (dynamic-func "g_enum_info_get_method"
				    %libgirepository)
                      (list '* int)))

(define g_value_info_get_value
  (pointer->procedure int64
                      (dynamic-func "g_value_info_get_value"
				    %libgirepository)
                      (list '*)))
