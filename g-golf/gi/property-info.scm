;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2019, 2021
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


(define-module (g-golf gi property-info)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support flags)
  #:use-module (g-golf init)
  #:use-module (g-golf gobject type-info)
  #:use-module (g-golf gobject param-spec)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi registered-type-info)
  #:use-module (g-golf gi arg-info)
  #:use-module (g-golf gi type-info)
  
  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (gi-property-g-type
	    g-property-info-get-flags
	    g-property-info-get-ownership-transfer
	    g-property-info-get-type))


;;;
;;;
;;;

(define (gi-property-g-type info)
  (let* ((type-info (g-property-info-get-type info))
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


;;;
;;; Low level API
;;;

(define (g-property-info-get-flags info)
  (integer->flags %g-param-flags
                  (g_property_info_get_flags info)))

(define (g-property-info-get-ownership-transfer info)
  (enum->symbol %gi-transfer
                (g_property_info_get_ownership_transfer info)))

(define (g-property-info-get-type info)
  (g_property_info_get_type info))


;;;
;;; GI Bindings
;;;

(define g_property_info_get_flags
  (pointer->procedure unsigned-int
                      (dynamic-func "g_property_info_get_flags"
				    %libgirepository)
                      (list '*)))

(define g_property_info_get_ownership_transfer
  (pointer->procedure int
                      (dynamic-func "g_property_info_get_ownership_transfer"
				    %libgirepository)
                      (list '*)))

(define g_property_info_get_type
  (pointer->procedure '*
                      (dynamic-func "g_property_info_get_type"
				    %libgirepository)
                      (list '*)))
