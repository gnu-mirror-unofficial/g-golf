;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2019
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


(define-module (g-golf gobject signals)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (srfi srfi-4)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support flag)
  #:use-module (g-golf glib mem-alloc)
  #:use-module (g-golf glib quarks)
  #:use-module (g-golf gobject type-info)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf init)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-signal-query
            g-signal-lookup
            g-signal-list-ids
            g-signal-connect-closure-by-id

            %g-signal-flags))


;;;
;;; Low level API
;;;

(define %g-signal-query-struct
  (list unsigned-int	;; id
        '*		;; name
        unsigned-long	;; g-type
        unsigned-int	;; flags
        unsigned-long	;; return-type
        unsigned-int	;; n-param
        '*))		;; param-types

(define (g-signal-query-parse g-signal-query)
  (parse-c-struct g-signal-query
                  %g-signal-query-struct))

(define (g-signal-query-make)
  (make-c-struct %g-signal-query-struct
                 (list 0
                       %null-pointer
                       0
                       0
                       0
                       0
                       %null-pointer)))

(define (g-signal-query id)
  (let ((gsq (g-signal-query-make)))
    (g_signal_query id gsq)
    (match (parse-c-struct gsq
                           %g-signal-query-struct)
      ((id name g-type flags return-type n-param param-types)
       (list id
             (gi->scm name 'string)
             g-type
             (gi-integer->gflags %g-signal-flags flags)
             (g-type->symbol return-type)
             n-param
             (decode-param-types n-param param-types))))))

(define (g-signal-list-ids g-type)
  (let* ((s-uint (sizeof unsigned-int))
         (n-id-bv (make-bytevector s-uint 0))
         (ids (g_signal_list_ids g-type
                                 (bytevector->pointer n-id-bv)))
         (n-id (u32vector-ref n-id-bv 0))
         (results (u32vector->list
                   (pointer->bytevector ids
                                        (* n-id s-uint) 0))))
    (g-free ids)
    results))

(define (g-signal-lookup name g-type)
  (let ((gsl (g_signal_lookup (scm->gi name 'string)
                              g-type)))
    (case gsl
      ((0)
       #f)
      (else
       gsl))))

(define (g-signal-connect-closure-by-id g-inst
                                        signal-id detail closure after?)
  (g_signal_connect_closure_by_id g-inst
                                  signal-id
                                  (if detail
                                      (g-quark-from-string detail)
                                      0)
                                  closure
                                  (scm->gi after? 'boolean)))


;;;
;;; Utils
;;;

(define (decode-param-types n-param param-types)
  (if (= n-param 0)
      '()
      (map g-type->symbol
        (u64vector->list
         (pointer->bytevector param-types
                              (* n-param
                                 (sizeof unsigned-long)))))))


;;;
;;; Signals Bindings
;;;

(define g_signal_query
  (pointer->procedure void
                      (dynamic-func "g_signal_query"
				    %libgobject)
                      (list unsigned-int	;; id
                            '*)))		;; query

(define g_signal_list_ids
  (pointer->procedure '*
                      (dynamic-func "g_signal_list_ids"
				    %libgobject)
                      (list unsigned-long	;; g-type
                            '*)))		;; n-id (pointer to guint)

(define g_signal_lookup
  (pointer->procedure unsigned-int
                      (dynamic-func "g_signal_lookup"
				    %libgobject)
                      (list '*			;; name
                            unsigned-long)))	;; g-type

(define g_signal_connect_closure_by_id
  (pointer->procedure unsigned-long
                      (dynamic-func "g_signal_connect_closure_by_id"
				    %libgobject)
                      (list '*			;; g-inst
                            unsigned-int	;; signal id
                            uint32		;; detail (g-quark)
                            '*			;; closure
                            int)))		;; after (boolean)


;;;
;;; Types and Values
;;;

(define %g-signal-flags
  (make <gi-flag>
    #:gi-name "GsignalFlags"
    #:enum-set '((run-first . 1)
                 (run-last . 2)
                 (run-cleanup . 4)
                 (no-recurse . 8)
                 (detailed . 16)
                 (action . 32)
                 (no-hooks . 64)
                 (must-collect . 128)
                 (deprecated . 256))))
