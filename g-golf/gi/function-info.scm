;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2018
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
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi types)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-golf-fi-get-flags
            g-golf-fi-get-symbol
             %g-golf-fi-flags))


;;;
;;; Low level API
;;;

(define (g-golf-fi-get-flags info)
  (g-golf-integer->gflags %g-golf-fi-flags
                          (g-function-info-get-flags info)))

(define (g-golf-fi-get-symbol info)
  (pointer->string (g-function-info-get-symbol info)))


;;;
;;; GI Bindings
;;;

(define g-function-info-get-flags
  (pointer->procedure uint32
                      (dynamic-func "g_function_info_get_flags"
				    %libgirepository)
                      (list '*)))

(define g-function-info-get-symbol
  (pointer->procedure '*
                      (dynamic-func "g_function_info_get_symbol"
				    %libgirepository)
                      (list '*)))


;;;
;;; Type and Values
;;;

(define %g-golf-fi-flags
  (make <gi-enum>
    #:gi-name "GIFunctionInfoFlags"
    #:scm-name "gi-function-info-flags"
    #:enum-set '(is-method
                 is-constructor
                 is-getter
                 is-setter
                 wraps-vfunc
                 throws)))
