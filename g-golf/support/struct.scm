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


(define-module (g-golf support struct)
  #:use-module (oop goops)
  #:use-module (g-golf support goops)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support utils)

  #:export (<gi-struct>))


(g-export !gi-name
          !scm-name
          !alignment
          !size
          !is-gtype-struct?
          !is-foreign?
          !field-types
          !scm-types
          !init-vals
          is-opaque?
          is-semi-opaque?)


(define-class <gi-struct> ()
  (gi-name #:accessor !gi-name
           #:init-keyword #:gi-name)
  (scm-name #:accessor !scm-name)
  (alignment #:accessor !alignment
             #:init-keyword #:alignment)
  (size #:accessor !size
        #:init-keyword #:size)
  (is-gtype-struct? #:accessor !is-gtype-struct?
                    #:init-keyword #:is-gtype-struct?)
  (is-foreign? #:accessor !is-foreign?
               #:init-keyword #:is-foreign?)
  (field-types #:accessor !field-types
               #:init-keyword #:field-types)
  (scm-types #:accessor !scm-types)
  (init-vals #:accessor !init-vals)
  (is-opaque? #:accessor !is-opaque?)
  (is-semi-opaque? #:accessor !is-semi-opaque?))

(define-method (initialize (self <gi-struct>) initargs)
  (next-method)
  (let ((gi-name (get-keyword #:gi-name initargs))
        (field-types (get-keyword #:field-types initargs #f)))
    (and gi-name
         (mslot-set! self
                     'gi-name gi-name
                     'scm-name (g-name->scm-name gi-name)))
    (and field-types
         (mslot-set! self
                     'scm-types (map gi-type-tag->ffi field-types)
                     'init-vals (map gi-type-tag->init-val field-types)
                     'is-opaque? (null? (!field-types self))
                     'is-semi-opaque? (if (memq 'void field-types) #t #f)))))
