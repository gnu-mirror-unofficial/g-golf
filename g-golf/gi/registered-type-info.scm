;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016, 2021
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


(define-module (g-golf gi registered-type-info)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)

  #:export (gi-registered-type-info-name

            g-registered-type-info-get-type-name
	    g-registered-type-info-get-type-init
	    g-registered-type-info-get-g-type))


(define (gi-registered-type-info-name info)
  ;; Some registered type are not 'registered', and getting their
  ;; registered-info name returns #f. Another symptom for those is if
  ;; you call (g-type-name g-type), it returns "void".  Since even
  ;; though they are 'unnamed', some are present in their typelib, like
  ;; "GLib" "SpawnFlags", or "GObject" "ParamFlags", they may be
  ;; imported, sometimes manually, sometimes automatically. G-Golf
  ;; needs to make sure they have a (unique) name.
  (or (g-registered-type-info-get-type-name info)
      (string-append (g-base-info-get-namespace info)
                     (g-base-info-get-name info))))


;;;
;;; Low level API
;;;

(define (g-registered-type-info-get-type-name info)
  (gi->scm (g_registered_type_info_get_type_name info) 'string))

;; this should not be called by language bindings
(define (g-registered-type-info-get-type-init info)
  (gi->scm (g_registered_type_info_get_type_init info) 'string))

(define (g-registered-type-info-get-g-type info)
  (g_registered_type_info_get_g_type info))


;;;
;;; GI Bindings
;;;

(define g_registered_type_info_get_type_name
  (pointer->procedure '*
                      (dynamic-func "g_registered_type_info_get_type_name"
				    %libgirepository)
                      (list '*)))

(define g_registered_type_info_get_type_init
  (pointer->procedure '*
                      (dynamic-func "g_registered_type_info_get_type_init"
				    %libgirepository)
                      (list '*)))

(define g_registered_type_info_get_g_type
  (pointer->procedure int64
                      (dynamic-func "g_registered_type_info_get_g_type"
				    %libgirepository)
                      (list '*)))
