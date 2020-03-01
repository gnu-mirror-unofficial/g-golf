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

;; A C union type low level interface: both c-union-ref and c-union-set!
;; expect a size argument, no (scheme) booleans, no strings (just
;; pointers, users must call string->pointer and pointer->string), and
;; also, not field-names, just types.

;; For an example of use, look at the (g-golf gi common-types) module -
;; %gi-argument-desc, ..., make-gi-argument, gi-argument-ref and
;; gi-argument-set!, all related to GIArgument.

;;; Code:


(define-module (g-golf support union)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (oop goops)
  #:use-module (g-golf support goops)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support struct)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (make-c-union
            c-union-ref
            c-union-set!

            <gi-union>))


(g-export !gtype-id
          !gi-name
          !scm-name
          !size
          !alignment
          !fields
          !is-discriminated?
          !discriminator-offset
          !discriminator)


(define %readers
  (@@ (system foreign) *readers*))

(define %writers
  (@@ (system foreign) *writers*))

(define %align
  (@@ (system foreign) align))

(define* (make-c-union types #:optional (type #f) (val #f))
  (let* ((size (apply max (map sizeof types)))
         (bv (make-bytevector size 0))
         (foreign (bytevector->pointer bv)))
    (if type
        (c-union-set! foreign size type val))
    (values foreign size)))

(define (c-union-ref foreign size type)
  (let ((bv (pointer->bytevector foreign size))
        (offset (%align 0 (alignof type))))
    ((assv-ref %readers type) bv offset)))

(define (c-union-set! foreign size type val)
  (let ((bv (pointer->bytevector foreign size))
        (offset (%align 0 (alignof type))))
    ((assv-ref %writers type) bv offset val)))


(define-class <gi-union> ()
  (gtype-id #:accessor !gtype-id #:init-keyword #:gtype-id)
  (gi-name #:accessor !gi-name #:init-keyword #:gi-name)
  (scm-name #:accessor !scm-name)
  (size #:accessor !size #:init-keyword #:size)
  (alignment #:accessor !alignment #:init-keyword #:alignment)
  (fields #:accessor !fields #:init-keyword #:fields)
  (is-discriminated? #:accessor !is-discriminated?
                     #:init-keyword #:is-discriminated?)
  (discriminator-offset #:accessor !discriminator-offset
                        #:init-keyword #:discriminator-offset
                        #:init-value #f)
  (discriminator #:accessor !discriminator
                 #:init-keyword #:discriminator
                 #:init-value #f))

(define-method (initialize (self <gi-union>) initargs)
  (next-method)
  (let ((gi-name (get-keyword #:gi-name initargs #f)))
    (and gi-name
         (slot-set! self
                    'scm-name (g-name->scm-name gi-name)))))
