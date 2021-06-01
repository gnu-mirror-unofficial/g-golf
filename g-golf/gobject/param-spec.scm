;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2021
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


(define-module (g-golf gobject param-spec)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support flags)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support libg-golf)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gobject params-vals)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (gi-g-param-spec-show
            g-param-spec-type
            g-param-spec-type-name
            g-param-spec-get-default-value
            g-param-spec-get-name
            g-param-spec-get-nick
            g-param-spec-get-blurb
            ;; from libg-golf
            g-param-spec-get-flags

            %g-param-flags))


;;;
;;; G-Gold additional functionality
;;;

(define %g-param-spec-fmt
  "
~S is a (pointer to a) GParamSpec:

               name: ~S
               nick: ~S
              blurb: ~S
             g-type: ~A
        g-type-name: ~S
          type-name: ~S
              flags: ~S

")

(define* (gi-g-param-spec-show info
                               #:optional (port (current-output-port)))
  (let ((g-type-name (g-param-spec-type-name info)))
    (format port "~?" %g-param-spec-fmt
            (list info
                  (g-param-spec-get-name info)
                  (g-param-spec-get-nick info)
                  (g-param-spec-get-blurb info)
                  (g-param-spec-type info)
                  g-type-name
                  (g-name->name g-type-name)
                  (g-param-spec-get-flags info)))
    (values)))


;;;
;;; GObject Low Level API
;;;

(define (g-param-spec-type p-spec)
  (g-value-type
   (g_param_spec_get_default_value p-spec)))

(define (g-param-spec-type-name p-spec)
  (g-value-type-name
   (g_param_spec_get_default_value p-spec)))

(define (g-param-spec-get-default-value p-spec)
  (g_param_spec_get_default_value p-spec))

(define (g-param-spec-get-name p-spec)
  (gi->scm (g_param_spec_get_name p-spec)
           'string))

(define (g-param-spec-get-nick p-spec)
  (gi->scm (g_param_spec_get_nick p-spec)
           'string))

(define (g-param-spec-get-blurb p-spec)
  (gi->scm (g_param_spec_get_blurb p-spec)
           'string))

;; from libg-golf
(define (g-param-spec-get-flags p-spec)
  (integer->flags %g-param-flags
                  (g_param_spec_get_flags p-spec)))


;;;
;;; GObject Bindings
;;;

(define g_param_spec_get_default_value
  (pointer->procedure '*
                      (dynamic-func "g_param_spec_get_default_value"
				    %libgobject)
                      (list '*)))	;; g-param-spec

(define g_param_spec_get_name
  (pointer->procedure '*
                      (dynamic-func "g_param_spec_get_name"
				    %libgobject)
                      (list '*)))	;; g-param-spec

(define g_param_spec_get_nick
  (pointer->procedure '*
                      (dynamic-func "g_param_spec_get_nick"
				    %libgobject)
                      (list '*)))	;; g-param-spec

(define g_param_spec_get_blurb
  (pointer->procedure '*
                      (dynamic-func "g_param_spec_get_blurb"
				    %libgobject)
                      (list '*)))	;; g-param-spec


;;;
;;; Types and Values
;;;

(define %g-param-flags
  (make <gi-flags>
    #:g-name "GParamFlags"
    #:enum-set '((readable . 1)
                 (writable . 2)
                 (readwrite . 3)
                 (construct . 4)
                 (construct-only . 8)
                 (lax-validation . 16)
                 (static-name . 32)
                 ;#(private . 32)
                 (static-nick . 64)
                 (static-blurb . 128)
                 (explicit-notify . 1073741824)
                 (deprecated . 2147483648))))
