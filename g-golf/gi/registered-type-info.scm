;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016
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


(define-module (golf gi registered-type-info)
  #:use-module (system foreign)
  #:use-module (golf gi init)
  #:use-module (golf gi utils)
  #:use-module (golf gi types)

  #:export (golf-rt-get-type-name
	    golf-rt-get-type-init
	    golf-rt-get-g-type))


;;;
;;; Low level API
;;;

(define (golf-rt-get-type-name info)
  (golf-gtype->scm (g-registered-type-info-get-type-name info)
		    'gchar*))

;; this should not be called by language bindings
(define (golf-rt-get-type-init info)
  (golf-gtype->scm (g-registered-type-info-get-type-init info)
		    'gchar*))

(define (golf-rt-get-g-type info)
  (g-registered-type-info-get-g-type info))


;;;
;;; GI Bindings
;;;

(define g-registered-type-info-get-type-name
  (pointer->procedure '*
                      (dynamic-func "g_registered_type_info_get_type_name"
				    %libgirepository)
                      (list '*)))

(define g-registered-type-info-get-type-init
  (pointer->procedure '*
                      (dynamic-func "g_registered_type_info_get_type_init"
				    %libgirepository)
                      (list '*)))

(define g-registered-type-info-get-g-type
  (pointer->procedure int64
                      (dynamic-func "g_registered_type_info_get_g_type"
				    %libgirepository)
                      (list '*)))