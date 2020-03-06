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
            gi-n-string->scm
            gi-strings->scm
            gi-csv-string->scm
	    gi-pointer->scm
            gi-n-pointer->scm
            gi-pointers->scm
            gi-glist->scm
            gi-gslist->scm
            scm->gi
            scm->gi-boolean
            scm->gi-string
            scm->gi-n-string
            scm->gi-strings
            scm->gi-pointer
            scm->gi-n-pointer
            scm->gi-pointers))


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

(define* (gi->scm value type #:optional (cmpl #f))
  (case type
    ((boolean) (gi-boolean->scm value))
    ((string) (gi-string->scm value))
    ((strings) (gi-strings->scm value))
    ((n-string) (gi-n-string->scm value cmpl))
    ((csv-string) (gi-csv-string->scm value))
    ((pointer) (gi-pointer->scm value))
    ((n-pointer) (gi-n-pointer->scm value cmpl))
    ((pointers) (gi-pointers->scm value))
    ((glist) (gi-glist->scm value cmpl))
    ((gslist) (gi-gslist->scm value cmpl))
    (else
     (error "No such type: " type))))

(define (gi-boolean->scm value)
  (if (= value 0) #f #t))

(define (gi-string->scm pointer)
  (if (null-pointer? pointer)
      #f
      (pointer->string pointer)))

(define (gi-n-string->scm pointer n-string)
  (if (or (not pointer)
          (null-pointer? pointer)
          (= n-string 0))
      #f
      (let loop ((i 0)
                 (pointer pointer)
                 (results '()))
        (if (= i n-string)
            (reverse! results)
            (loop (+ i 1)
                  (gi-pointer-inc pointer)
                  (cons (pointer->string (dereference-pointer pointer))
                        results))))))

(define (gi-strings->scm pointer)
  (if (or (not pointer)
          (null-pointer? pointer))
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
             (gi-strings->scm-1 pointer '()))))

(define (gi-csv-string->scm pointer)
  (if (null-pointer? pointer)
       #f
       (string-split (pointer->string pointer)
                     #\,)))

(define (gi-pointer->scm pointer)
  (if (null-pointer? pointer)
      #f
      pointer))

(define (gi-n-pointer->scm pointer n-pointer)
  (if (or (not pointer)
          (null-pointer? pointer)
          (= n-pointer 0))
      #f
      (let loop ((i 0)
                 (pointer pointer)
                 (results '()))
        (if (= i n-pointer)
            (reverse! results)
            (loop (+ i 1)
                  (gi-pointer-inc pointer)
                  (cons (dereference-pointer pointer)
                        results))))))

(define (gi-pointers->scm pointer)
  (if (or (not pointer)
          (null-pointer? pointer))
      #f
      (letrec ((gi-pointers->scm-1
                (lambda (pointer result)
                  (receive (d-pointer)
	              (dereference-pointer pointer)
                    (if (null-pointer? d-pointer)
                        (reverse! result)
                        (gi-pointers->scm-1 (gi-pointer-inc pointer)
                                            (cons d-pointer
                                                  result)))))))
        (gi-pointers->scm-1 pointer '()))))

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
    ((n-string) (scm->gi-n-string value))
    ((strings) (scm->gi-strings value))
    #;((csv-string) (scm->gi-csv-string value))
    ((pointer) (scm->gi-pointer value))
    ((n-pointer) (scm->gi-n-pointer value))
    ((pointers) (scm->gi-pointers value))
    (else
     value)))

(define (scm->gi-boolean value)
  (if value 1 0))

(define (scm->gi-string value)
  (string->pointer value))

;; The following two procedures need a bit more work, be cause a
;; reference to the 'inner' pointers mst be kept (and returned to the
;; caller). otherwise, they might be GC'ed ...

(define (scm->gi-n-string lst)
  (if (null? lst)
      (values %null-pointer
              '())
      (let* ((p-size %gi-pointer-size)
             (n-string (length lst))
             (bv (make-bytevector (* n-string p-size) 0))
             (o-ptr (bytevector->pointer bv)))
        (let loop ((w-ptr o-ptr)
                   (i-ptrs '())
                   (lst lst))
          (if (null? lst)
              (values o-ptr
                      (reverse! i-ptrs))
              (match lst
                ((str . rest)
                 (let ((i-ptr (string->pointer str)))
                   (bv-ptr-set! w-ptr i-ptr)
                   (loop (gi-pointer-inc w-ptr)
                         (cons i-ptr i-ptrs)
                         rest)))))))))

(define (scm->gi-strings lst)
  (if (null? lst)
      (values %null-pointer
              '())
      (let* ((p-size %gi-pointer-size)
             (n-string (length lst))
             (bv (make-bytevector (* (+ n-string 1) p-size) 0))
             (o-ptr (bytevector->pointer bv)))
        (let loop ((w-ptr o-ptr)
                   (i-ptrs '())
                   (lst lst))
          (if (null? lst)
              (begin
                (bv-ptr-set! w-ptr %null-pointer)
                (values o-ptr
                        (reverse! i-ptrs)))
              (match lst
                ((str . rest)
                 (let ((i-ptr (string->pointer str)))
                   (bv-ptr-set! w-ptr i-ptr)
                   (loop (gi-pointer-inc w-ptr)
                         (cons i-ptr i-ptrs)
                         rest)))))))))

(define (scm->gi-pointer value)
  (if value
      value
      %null-pointer))

(define (scm->gi-n-pointer lst)
  (if (null? lst)
      %null-pointer
      (let* ((p-size %gi-pointer-size)
             (n-pointer (length lst))
             (bv (make-bytevector (* n-pointer p-size) 0))
             (o-ptr (bytevector->pointer bv)))
        (let loop ((w-ptr o-ptr)
                   (lst lst))
          (if (null? lst)
              o-ptr
              (match lst
                ((i-ptr . rest)
                 (bv-ptr-set! w-ptr i-ptr)
                 (loop (gi-pointer-inc w-ptr)
                       rest))))))))

(define (scm->gi-pointers lst)
  (if (null? lst)
      %null-pointer
      (let* ((p-size %gi-pointer-size)
             (n-pointer (length lst))
             (bv (make-bytevector (* (+ n-pointer 1) p-size) 0))
             (o-ptr (bytevector->pointer bv)))
        (let loop ((w-ptr o-ptr)
                   (lst lst))
          (if (null? lst)
              (begin
                (bv-ptr-set! w-ptr %null-pointer)
                o-ptr)
              (match lst
                ((l-ptr . rest)
                 (bv-ptr-set! w-ptr l-ptr)
                 (loop (gi-pointer-inc w-ptr)
                       rest))))))))
