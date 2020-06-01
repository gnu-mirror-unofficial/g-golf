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


(define-module (g-golf gi common-types)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support union)
  #:use-module (g-golf gi utils)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (%gi-type-tag
	    %gi-array-type
            %gi-argument-desc
            %gi-argument-fields
            %gi-argument-types
            %gi-argument-size
            make-gi-argument
            gi-argument-ref
            gi-argument-set!
            gi-type-tag->field))


;;;
;;; Common Types
;;;

(define %gi-type-tag
  (make <gi-enum>
    #:g-name "GITypeTag"
    #:enum-set '(void
                 boolean
                 int8
                 uint8
                 int16
                 uint16
                 int32
                 uint32
                 int64
                 uint64
                 float
                 double
                 gtype
                 utf8
                 filename
                 array
                 interface
                 glist
                 gslist
                 ghash
                 error
                 unichar)))

(define %gi-array-type
  (make <gi-enum>
    #:g-name "GIArrayType"
    #:enum-set '(c
                 array
                 ptr-array
                 byte-array)))

(define %gi-argument-desc
  `((v-boolean . ,int)
    (v-int8 . ,int8)
    (v-uint8 . ,uint8)
    (v-int16 . ,int16)
    (v-uint16 . ,uint16)
    (v-int32 . ,int32)
    (v-uint32 . ,uint32)
    (v-int64 . ,int64)
    (v-uint64 . ,uint64)
    (v-float . ,float)
    (v-double . ,double)
    (v-short . ,short)
    (v-ushort . ,unsigned-short)
    (v-int . ,int)
    (v-uint . ,unsigned-int)
    (v-long . ,long)
    (v-ulong . ,unsigned-long)
    (v-ssize . ,long)
    (v-size . ,unsigned-long)
    (v-string . ,'*)
    (v-pointer . ,'*)))

(define %gi-argument-fields
  (map car %gi-argument-desc))

(define %gi-argument-types
  (map cdr %gi-argument-desc))

(define %gi-argument-size
  (apply max (map sizeof %gi-argument-types)))

(define (make-gi-argument)
  (make-c-union %gi-argument-types))

(define (gi-argument-ref gi-argument field)
  (let* ((type (assq-ref %gi-argument-desc field))
         (val (if type
                  (c-union-ref gi-argument %gi-argument-size type)
                  (error "No such field: " field))))
    (case field
      ((v-boolean)
       (gi->scm val 'boolean))
      ((v-string)
       (gi->scm val 'string))
      ((v-pointer)
       (gi->scm val 'pointer))
      (else
       val))))

(define (gi-argument-set! gi-argument field val)
  (let ((type (assq-ref %gi-argument-desc field)))
    (if type
        (let ((u-val (case field
                       ((v-boolean)
                        (scm->gi val 'boolean))
                       ((v-string)
                        (scm->gi val 'string))
                       ((v-pointer)
                        (scm->gi val 'pointer))
                       (else
                        val))))
          (c-union-set! gi-argument %gi-argument-size type u-val))
        (error "No such field: " field))))

(define (gi-type-tag->field type-tag)
  "Returns the GI argument field for type-tag. You'll note that a few
tags are not members of GITypeTag, but I see the CL implementation lists
those, so since it does not cost anything, I decided to add these
to (their gtype exists, in GIArgument, except time-t, which I decided to
add as a comment)."
  (case type-tag
    ((boolean) 'v-boolean)
    ((int8) 'v-int8)
    ((uint8) 'v-uint8)
    ((int16) 'v-int16)
    ((uint16) 'v-uint16)
    ((int32) 'v-int32)
    ((uint32) 'v-uint32)
    ((int64) 'v-int64)
    ((uint64) 'v-uint64)
    ((float) 'v-float)
    ((double) 'v-double)
    ((gtype) 'v-ulong)
    ((short) 'v-short)		;; <- from CL implementtion
    ((ushort) 'v-ushort)
    ((int) 'v-int)
    ((uint) 'v-uint)
    ((long) 'v-long)
    ((ulong) 'v-ulong)
    ((ssize) 'v-long)
    ((size) 'v-ulong)
    ;; ((time-t) 'v-long)	;; <- till here
    ((utf8
      filename)
     'v-string)
    ((pointer			;; <- forced type
      array
      interface
      glist
      gslist
      ghash
      error)
     'v-pointer)
    ((unichar) 'v-uint32)
    (else
     (error "No such GI type tag: " type-tag))))
