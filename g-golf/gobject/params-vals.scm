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


(define-module (g-golf gobject params-vals)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (g-golf init)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support flags)
  #:use-module (g-golf support struct)
  #:use-module (g-golf support union)
  #:use-module (g-golf support utils)
  #:use-module (g-golf gi cache)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gobject type-info)
  #:use-module (g-golf gobject enum-flags)
  #:use-module (g-golf gobject generic-values)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-value-type
            g-value-type-tag
            g-value-type-name
            g-value-ref
            g-value-set!
            g-value-get-int
            g-value-set-int
            g-value-get-uint
            g-value-set-uint
            g-value-get-boolean
            g-value-set-boolean
            g-value-get-float
            g-value-set-float
            g-value-get-double
            g-value-set-double
            g-value-get-enum
            g-value-get-flags
            g-value-get-string
            g-value-set-string
            g-value-get-boxed
            g-value-set-boxed
            g-value-get-pointer
            g-value-set-pointer
            g-value-get-object
            g-value-set-object))


(g-export g-value-set-enum
          g-value-set-flags)


;;;
;;; G-Golf Low Level API
;;;

(define (g-value-parse g-value)
  (parse-c-struct g-value %g-value-struct))

(define (g-value-type g-value)
  (match (g-value-parse g-value)
    ((g-type _ _) g-type)))

(define (g-value-type-tag g-value)
  (match (g-value-parse g-value)
    ((g-type _ _)
     (g-type->symbol g-type))))

(define (g-value-type-name g-value)
  (match (g-value-parse g-value)
    ((g-type _ _)
     (g-type-name g-type))))

(define (g-value-ref g-value)
  (let ((type-tag (g-value-type-tag g-value)))
    (case type-tag
      ((boolean)
       (g-value-get-boolean g-value))
      ((uint)
       (g-value-get-uint g-value))
      ((int)
       (g-value-get-int g-value))
      ((float)
       (g-value-get-float g-value))
      ((double)
       (g-value-get-double g-value))
      ((enum)
       (g-value-get-enum g-value))
      ((flags)
       (g-value-get-flags g-value))
      ((string)
       (g-value-get-string g-value))
      ((boxed)
       (g-value-get-boxed g-value))
      ((pointer)
       (g-value-get-pointer g-value))
      ((object
        interface)
       (g-value-get-object g-value))
      (else
       (error "Not implemented:" type-tag)))))

(define (g-value-set! g-value value)
  (let ((type-tag (g-value-type-tag g-value)))
    (case type-tag
      ((boolean)
       (g-value-set-boolean g-value value))
      ((uint)
       (g-value-set-uint g-value value))
      ((int)
       (g-value-set-int g-value value))
      ((float)
       (g-value-set-float g-value value))
      ((double)
       (g-value-set-double g-value value))
      ((enum)
       (g-value-set-enum g-value value))
      ((flags)
       (g-value-set-flags g-value value))
      ((string)
       (g-value-set-string g-value value))
      ((boxed)
       (g-value-set-boxed g-value value))
      ((pointer)
       (g-value-set-pointer g-value value))
      ((object
        interface)
       (g-value-set-object g-value value))
      (else
       (error "Not implemented:" type-tag)))))


;;;
;;; GObject Low level API
;;;

(define (g-value-get-boolean g-value)
  (if (= (g_value_get_boolean g-value) 0) #f #t))

(define (g-value-set-boolean g-value bool)
  (g_value_set_boolean g-value
                       (if bool 1 0)))

(define (g-value-get-int g-value)
  (g_value_get_int g-value))

(define (g-value-set-int g-value int)
  (g_value_set_int g-value int))

(define (g-value-get-uint g-value)
  (g_value_get_uint g-value))

(define (g-value-set-uint g-value uint)
  (g_value_set_uint g-value uint))

(define (g-value-get-float g-value)
  (g_value_get_float g-value))

(define (g-value-set-float g-value float)
  (g_value_set_float g-value float))

(define (g-value-get-double g-value)
  (g_value_get_double g-value))

(define (g-value-set-double g-value double)
  (g_value_set_double g-value double))

(define (g-value-get-gi-enum g-value)
  (let* ((g-name (g-value-type-name g-value))
         (name (g-name->name g-name)))
    (or (gi-cache-ref 'enum name)
        (error "No such enum type: " name))))

(define (g-value-get-enum g-value)
  (let ((gi-enum (g-value-get-gi-enum g-value))
        (val (g_value_get_enum g-value)))
    (or (enum->symbol gi-enum val)
        (error "No such " (!name gi-enum) " value: " val))))

(define-method (g-value-set-enum g-value (val <integer>))
  (let ((gi-enum (g-value-get-gi-enum g-value)))
    (if (enum->symbol gi-enum val)
        (g_value_set_enum g-value val)
        (error "No such " (!name gi-enum) " value: " val))))

(define-method (g-value-set-enum g-value (sym <symbol>))
  (let* ((gi-enum (g-value-get-gi-enum g-value))
         (val (enum->value gi-enum sym)))
    (if val
        (g_value_set_enum g-value val)
        (error "No such " (!name gi-enum) " key: " sym))))

(define (g-value-get-gi-flags g-value)
  (let* ((g-name (g-value-type-name g-value))
         (name (g-name->name g-name)))
    (or (gi-cache-ref 'flags name)
        (error "No such flags type: " name))))

(define (g-value-get-flags g-value)
  (let ((gi-flags (g-value-get-gi-flags g-value))
        (val (g_value_get_flags g-value)))
    (or (integer->flags gi-flags val)
        (error "No such " (!name gi-flags) " value: " val))))

(define (g-value-set-flags g-value flags)
  (let* ((gi-flags (g-value-get-gi-flags g-value))
         (val (flags->integer gi-flags flags)))
    (if val
        (g_value_set_flags g-value val)
        (error "No such " (!name gi-flags) " key: " flags))))

(define (g-value-get-string g-value)
  (let ((pointer (g_value_get_string g-value)))
    (if (null-pointer? pointer)
        ""
        (pointer->string pointer))))

(define (g-value-set-string g-value str)
  (g_value_set_string g-value
                      (string->pointer str)))

(define %gdk-event-class
  (@ (g-golf gdk events) gdk-event-class))

(define (g-value-get-boxed g-value)
  (let* ((g-name (g-value-type-name g-value))
         (name (g-name->name g-name))
         (gi-boxed (gi-cache-ref 'boxed name))
         (value (g_value_get_boxed g-value)))
    (if gi-boxed
        (cond ((is-a? gi-boxed <gi-union>)
               (if (eq? (!name gi-boxed) 'gdk-event)
                   ;; This means that we are in gdk3/gtk3 environment, where
                   ;; the <gdk-event> class and accessors are (must be)
                   ;; defined dynamically - hence (gdk-event-class)
                   (make (%gdk-event-class) #:event value)
                   value))
              ((or (!is-opaque? gi-boxed)
                   (!is-semi-opaque? gi-boxed))
               value)
              (else
               (parse-c-struct value
                               (!scm-types gi-boxed))))
        (case name
          ((g-strv)
           (gi-strings->scm value))
          (else
           (error "Unimplemented boxed type: " name))))))

(define (g-value-set-boxed g-value boxed)
  (let* ((g-name (g-value-type-name g-value))
         (name (g-name->name g-name))
         (gi-boxed (gi-cache-ref 'boxed name))
         (value (if gi-boxed
                    (cond  ((!is-opaque? gi-boxed)
                            %null-pointer)
                           ((!is-semi-opaque? gi-boxed)
                            boxed)
                           (else
                            (make-c-struct (!scm-types gi-boxed) boxed)))
                    (case name
                      ((g-strv)
                       (scm->gi-strings boxed))
                      (else
                       (error "Unimplemented boxed type: " name))))))
    (g_value_set_boxed g-value value)))

(define (g-value-get-pointer g-value)
  (let ((pointer (g_value_get_pointer g-value)))
    (if (null-pointer? pointer)
        #f
        pointer)))

(define (g-value-set-pointer g-value pointer)
  (g_value_set_pointer g-value
                       (if pointer pointer %null-pointer)))

(define (g-value-get-object g-value)
  (let ((object (g_value_get_object g-value)))
    (if (null-pointer? object)
        #f
        object)))

(define (g-value-set-object g-value object)
  (g_value_set_object g-value
                      (if object object %null-pointer)))


;;;
;;; GObject Bindings
;;;

(define g_value_get_boolean
  (pointer->procedure int
                      (dynamic-func "g_value_get_boolean"
				    %libgobject)
                      (list '*)))

(define g_value_set_boolean
  (pointer->procedure void
                      (dynamic-func "g_value_set_boolean"
				    %libgobject)
                      (list '*
                            int)))

(define g_value_get_uint
  (pointer->procedure unsigned-int
                      (dynamic-func "g_value_get_uint"
				    %libgobject)
                      (list '*)))

(define g_value_set_uint
  (pointer->procedure void
                      (dynamic-func "g_value_set_uint"
				    %libgobject)
                      (list '*
                            unsigned-int)))

(define g_value_get_int
  (pointer->procedure int
                      (dynamic-func "g_value_get_int"
				    %libgobject)
                      (list '*)))

(define g_value_set_int
  (pointer->procedure void
                      (dynamic-func "g_value_set_int"
				    %libgobject)
                      (list '*
                            int)))

(define g_value_get_float
  (pointer->procedure float
                      (dynamic-func "g_value_get_float"
				    %libgobject)
                      (list '*)))

(define g_value_set_float
  (pointer->procedure void
                      (dynamic-func "g_value_set_float"
				    %libgobject)
                      (list '*
                            float)))

(define g_value_get_double
  (pointer->procedure double
                      (dynamic-func "g_value_get_double"
				    %libgobject)
                      (list '*)))

(define g_value_set_double
  (pointer->procedure void
                      (dynamic-func "g_value_set_double"
				    %libgobject)
                      (list '*
                            double)))

(define g_value_get_enum
  (pointer->procedure int
                      (dynamic-func "g_value_get_enum"
				    %libgobject)
                      (list '*)))

(define g_value_set_enum
  (pointer->procedure void
                      (dynamic-func "g_value_set_enum"
				    %libgobject)
                      (list '*
                            int)))

(define g_value_get_flags
  (pointer->procedure unsigned-int
                      (dynamic-func "g_value_get_flags"
				    %libgobject)
                      (list '*)))

(define g_value_set_flags
  (pointer->procedure void
                      (dynamic-func "g_value_set_flags"
				    %libgobject)
                      (list '*
                            unsigned-int)))

(define g_value_get_string
  (pointer->procedure '*
                      (dynamic-func "g_value_get_string"
				    %libgobject)
                      (list '*)))

(define g_value_set_string
  (pointer->procedure void
                      (dynamic-func "g_value_set_string"
				    %libgobject)
                      (list '*
                            '*)))

(define g_value_get_boxed
  (pointer->procedure '*
                      (dynamic-func "g_value_get_boxed"
				    %libgobject)
                      (list '*)))

(define g_value_set_boxed
  (pointer->procedure void
                      (dynamic-func "g_value_set_boxed"
				    %libgobject)
                      (list '*
                            '*)))

(define g_value_get_pointer
  (pointer->procedure '*
                      (dynamic-func "g_value_get_pointer"
				    %libgobject)
                      (list '*)))

(define g_value_set_pointer
  (pointer->procedure void
                      (dynamic-func "g_value_set_pointer"
				    %libgobject)
                      (list '*
                            '*)))

(define g_value_get_object
  (pointer->procedure '*
                      (dynamic-func "g_value_get_object"
				    %libgobject)
                      (list '*)))

(define g_value_set_object
  (pointer->procedure void
                      (dynamic-func "g_value_set_object"
				    %libgobject)
                      (list '*
                            '*)))
