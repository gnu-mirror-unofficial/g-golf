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


(define-module (g-golf gi function-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support flag)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (gi-function-info-is-method?

            g-function-info-get-flags
            g-function-info-get-property
            g-function-info-get-symbol
            g-function-info-get-vfunc
            g-function-info-invoke
            %g-function-info-flags))


;;;
;;; High level API
;;;

(define* (gi-function-info-is-method? info #:optional (flags #f))
  (let ((flags (or flags
                   (g-function-info-get-flags info))))
    ;; although the result of the memq result would be sufficient of
    ;; course, <function> instances store the result return by this
    ;; procedure, but it also store the flags, not returning a 'strict'
    ;; boolean would be redundant.
    (and (memq 'is-method flags)
         #t)))


;;;
;;; Low level API
;;;

(define (g-function-info-get-flags info)
  (gi-integer->gflags %g-function-info-flags
                      (g_function_info_get_flags info)))

(define (g-function-info-get-property info)
  ;; The GI manual says only those functions with the flag 'is-getter or
  ;; 'is-setter have an associated property, and other wise it returns
  ;; NULL1.  But it doesn't say that if called upon any function that is
  ;; not a getter or a setter, it displays a critical warning, like
  ;; this: ** (process:17919): CRITICAL **: 00:43:57.404:
  ;; g_interface_info_get_property: assertion 'GI_IS_INTERFACE_INFO
  ;; (info)' failed. Let's avoid this ...
  (let ((flags (g-function-info-get-flags info)))
    (if (or (memq 'is-getter flags)
            (memq 'is-setter flags))
        (gi->scm (g_function_info_get_property info) 'pointer)
        #f)))

(define (g-function-info-get-symbol info)
  (pointer->string (g_function_info_get_symbol info)))

(define (g-function-info-get-vfunc info)
  (let ((flags (g-function-info-get-flags info)))
    (if (memq 'wraps-vfunc flags)
        (gi->scm (g_function_info_get_vfunc info) 'pointer)
        #f)))

(define (g-function-info-invoke info
                                in-args n-int
                                out-args n-out
                                r-val
                                g-error)
  (g_function_info_invoke info in-args n-int out-args n-out r-val g-error))


;;;
;;; GI Bindings
;;;

(define g_function_info_get_flags
  (pointer->procedure uint32
                      (dynamic-func "g_function_info_get_flags"
				    %libgirepository)
                      (list '*)))

(define g_function_info_get_property
  (pointer->procedure '*
                      (dynamic-func "g_function_info_get_property"
				    %libgirepository)
                      (list '*)))

(define g_function_info_get_symbol
  (pointer->procedure '*
                      (dynamic-func "g_function_info_get_symbol"
				    %libgirepository)
                      (list '*)))

(define g_function_info_get_vfunc
  (pointer->procedure '*
                      (dynamic-func "g_function_info_get_vfunc"
				    %libgirepository)
                      (list '*)))

(define g_function_info_invoke
  (pointer->procedure int
                      (dynamic-func "g_function_info_invoke"
				    %libgirepository)
                      (list '*		;; info
                            '*		;; in-args
                            int		;; n-in
                            '*		;; out-args
                            int		;; n-out
                            '*  	;; r-val
                            '*)))	;; g-error


;;;
;;; Type and Values
;;;

(define %g-function-info-flags
  (make <gi-flag>
    #:gi-name "GIFunctionInfoFlags"
    #:enum-set '((is-method . 1)
                 (is-constructor . 2)
                 (is-getter . 4)
                 (is-setter . 8)
                 (wraps-vfunc . 16)
                 (throws . 32))))
