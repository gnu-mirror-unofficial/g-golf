;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2019
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


(define-module (g-golf gi utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-60)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support bytevector)
  #:use-module (g-golf glib mem-alloc)
  #:use-module (g-golf glib glist)
  #:use-module (g-golf glib gslist)

  #:export (%gi-pointer-size
	    gi-pointer-new
	    gi-pointer-inc
	    gi-attribute-iter-new
	    with-gerror
	    gi->scm
            gi-boolean->scm
            gi-string->scm
            gi-strings->scm
            gi-csv-string->scm
	    gi-pointer->scm
            gi-glist->scm
            gi-gslist->scm
            scm->gi
            scm->gi-boolean
            scm->gi-string
            scm->gi-strings
            scm->gi-pointer))


(define %gi-pointer-size (sizeof '*))

(define (gi-pointer-new)
  ;; (bytevector->pointer (make-bytevector %gi-pointer-size 0))
  ;; The above would work iif none of Glib, Gobject and GI would ever call
  ;; any of there respective *_free functions upon pointers returned by
  ;; this procedure [it segfaults - C can't free Guile's mem]. This
  ;; statement is _not_ guaranteed, hence we have to allocate using the
  ;; glib API.
  (g-malloc0 %gi-pointer-size))

(define* (gi-pointer-inc pointer
                         #:optional
                         (offset %gi-pointer-size))
  (make-pointer (+ (pointer-address pointer)
		   offset)))

(define (gi-attribute-iter-new)
  (make-c-struct (list '* '* '* '*)
		 (list %null-pointer
		       %null-pointer
		       %null-pointer
		       %null-pointer)))

(define-syntax with-gerror
  (syntax-rules ()
    ((with-gerror ?var ?body)
     (let* ((?var (gi-pointer-new))
	    (result ?body)
	    (d-pointer (dereference-pointer ?var)))
       (if (null-pointer? d-pointer)
	   (begin
	     (g-free ?var)
	     result)
	   (match (parse-c-struct d-pointer
				  (list uint32 int8 '*))
	     ((domain code message)
	      (g-free ?var)
	      (error (pointer->string message)))))))))


;;;
;;; gi->scm procedures
;;;

(define* (gi->scm value type #:optional (type-desc #f))
  (case type
    ((boolean) (gi-boolean->scm value))
    ((string) (gi-string->scm value))
    ((strings) (gi-strings->scm value))
    ((csv-string) (gi-csv-string->scm value))
    ((pointer) (gi-pointer->scm value))
    ((glist) (gi-glist->scm value type-desc))
    ((gslist) (gi-gslist->scm value type-desc))
    (else
     (error "No such type: " type))))

(define (gi-boolean->scm value)
  (if (= value 0) #f #t))

(define (gi-string->scm pointer)
  (if (null-pointer? pointer)
      #f
      (pointer->string pointer)))

(define (gi-strings->scm pointer)
  (and pointer
       (if (null-pointer? pointer)
           #f
           (letrec ((gi-strings->scm-1
                     (lambda (pointer result)
                       (receive (d-pointer)
	                   (dereference-pointer pointer)
                         (if (null-pointer? d-pointer)
                             (reverse! result)
                             (gi-strings->scm-1 (gi-pointer-inc pointer)
                                                (cons (pointer->string d-pointer)
                                                      result)))))))
             (gi-strings->scm-1 pointer '())))))

(define (gi-csv-string->scm pointer)
  (if (null-pointer? pointer)
       #f
       (string-split (pointer->string pointer)
                     #\,)))

(define (gi-pointer->scm pointer)
  (if (null-pointer? pointer)
      #f
      pointer))

(define (gi-glist->scm g-list type-desc)
  ;; The reason g-list, which is supposed to be a pointer, can be #f is
  ;; that the caller may have already processed its value, which is what
  ;; gi-argument-ref does for 'v-pointer fields for example. In this
  ;; case, gi-pointer->scm has been called, which returns #f its
  ;; argument is a %null-pointer.
  (if (or (not g-list)
          (null-pointer? g-list))
      '()
      (let ((result (gi-glist-1->scm g-list type-desc)))
        (g-list-free g-list)
        result)))

(define (gi-glist-1->scm g-list type-desc)
  (match type-desc
    ((_ interface? i-desc is-pointer?)
     (if interface?
         (gi-glist-interface->scm g-list i-desc)
         (warning "Unimplemented glist type" type-desc)))))

(define (gi-glist-interface->scm g-list i-desc)
  (match i-desc
    ((type name gi-type g-type confirmed?)
     (case type
       ((object)
        (let loop ((g-list g-list)
                   (result '()))
          (if (null-pointer? g-list)
              (reverse! result)
              (loop (g-list-next g-list)
                    (cons (make gi-type
                            #:g-inst (g-list-data g-list))
                          result)))))
       (else
        (warning "Unimplemented glist type" i-desc))))))

(define (gi-gslist->scm g-slist type-desc)
  ;; The reason g-slist, which is supposed to be a pointer, can be #f is
  ;; that the caller may have already processed its value, which is what
  ;; gi-argument-ref does for 'v-pointer fields for example. In this
  ;; case, gi-pointer->scm has been called, which returns #f its
  ;; argument is a %null-pointer.
  (if (or (not g-slist)
          (null-pointer? g-slist))
      '()
      (let ((result (gi-gslist-1->scm g-slist type-desc)))
        (g-slist-free g-slist)
        result)))

(define (gi-gslist-1->scm g-slist type-desc)
  (match type-desc
    ((_ interface? i-desc is-pointer?)
     (if interface?
         (gi-gslist-interface->scm g-slist i-desc)
         (warning "Unimplemented gslist type" type-desc)))))

(define (gi-gslist-interface->scm g-slist i-desc)
  (match i-desc
    ((type name gi-type g-type confirmed?)
     (case type
       ((object)
        (let loop ((g-slist g-slist)
                   (result '()))
          (if (null-pointer? g-slist)
              (reverse! result)
              (loop (g-slist-next g-slist)
                    (cons (make gi-type
                            #:g-inst (g-slist-data g-slist))
                          result)))))
       (else
        (warning "Unimplemented gslist type" i-desc))))))


;;;
;;; scm->gi procedures
;;;

(define (scm->gi value type)
  (case type
    ((boolean) (scm->gi-boolean value))
    ((string) (scm->gi-string value))
    ((strings) (scm->gi-strings value))
    #;((csv-string) (scm->gi-csv-string value))
    ((pointer) (scm->gi-pointer value))
    (else
     value)))

(define (scm->gi-boolean value)
  (if value 1 0))

(define (scm->gi-string value)
  (string->pointer value))

(define (scm->gi-strings lst)
  (if (null? lst)
      %null-pointer
      (let* ((p-size %gi-pointer-size)
             (n-string (length lst))
             (bv (make-bytevector (* (+ n-string 1) p-size) 0))
             (ptr (bytevector->pointer bv)))
        (let loop ((ptr ptr)
                   (lst lst))
          (if (null? lst)
              (bv-ptr-set! ptr %null-pointer)
              (match lst
                ((str . rest)
                 (bv-ptr-set! ptr (string->pointer str))
                 (loop (gi-pointer-inc ptr)
                       rest)))))
        ptr)))

(define (scm->gi-pointer value)
  (if value
      value
      %null-pointer))
