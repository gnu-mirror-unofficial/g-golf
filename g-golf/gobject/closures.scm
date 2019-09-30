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


(define-module (g-golf gobject closures)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support libg-golf)
  #:use-module (g-golf gi utils)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-closure-size
            g-closure-ref
            g-closure-sink
            g-closure-unref
            g-closure-set-marshal
            g-source-set-closure))


;;;
;;; GObject Low level API
;;;

;; from libg-golf
(define (g-closure-size)
  (g_closure_size))

(define (g-closure-ref closure)
  (gi->scm (g_closure_ref closure) 'pointer))

(define (g-closure-sink closure)
  (g_closure_sink closure))

(define (g-closure-unref closure)
  (g_closure_unref closure))

(define (g-closure-set-marshal closure marshal)
  (g_closure_set_marshal closure marshal))

(define (g-source-set-closure source closure)
  (g_source_set_closure source closure))


;;;
;;; GObject Bindings
;;;

(define g_closure_ref
  (pointer->procedure '*
                      (dynamic-func "g_closure_ref"
				    %libgobject)
                      (list '*)))	;; closure

(define g_closure_sink
  (pointer->procedure void
                      (dynamic-func "g_closure_sink"
				    %libgobject)
                      (list '*)))	;; closure

(define g_closure_unref
  (pointer->procedure void
                      (dynamic-func "g_closure_unref"
				    %libgobject)
                      (list '*)))	;; closure

(define g_closure_set_marshal
  (pointer->procedure void
                      (dynamic-func "g_closure_set_marshal"
				    %libgobject)
                      (list '*		;; closure
                            '*)))	;; marshal

(define g_source_set_closure
  (pointer->procedure void
                      (dynamic-func "g_source_set_closure"
				    %libgobject)
                      (list '*		;; source
                            '*)))	;; closure
