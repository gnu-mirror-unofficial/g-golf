;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2019 - 2021
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


(define-module (g-golf support flags)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support goops)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support keyword)
  #:use-module (g-golf support enum)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<flags>
            <gi-flags>))


(g-export flags->integer
          integer->flags
          !g-type
          !g-name
          !name)


(define-class <flags> (<enum>))


(define-method (flags->integer (self <flags>) flags)
  (let ((enum-set (!enum-set self)))
    (apply logior
           (map
               (lambda (flag)
                 (or (assq-ref enum-set flag)
                     (error "Unknown flag: " flag)))
             flags))))

(define-method (integer->flags (self <flags>) n)
  (let ((enum-set (!enum-set self)))
    (filter-map
        (match-lambda
          ((key . val)
           (and (if (zero? val)
                    (zero? n)
                    (= (logand n val) val))
                key)))
        enum-set)))


;;;
;;; GI Flag
;;;

(define-class <gi-flags> (<flags>)
  (g-type #:accessor !g-type
          #:init-keyword #:g-type
          #:init-value #f)
  (g-name #:accessor !g-name
          #:init-keyword #:g-name)
  (name #:accessor !name))

(define-method (initialize (self <gi-flags>) initargs)
  (next-method)
  (let ((g-name (get-keyword #:g-name initargs #f)))
    (and g-name
         (set! (!g-name self) g-name)
         (set! (!name self)
               (g-name->name g-name)))))
