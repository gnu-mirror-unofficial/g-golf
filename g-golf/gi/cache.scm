;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2018
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

;; We need a cache mecanism to avoid reconstructing things on-the-fly
;; unnecessarily, such as already imported <gi-enum> instances.  Till we
;; need something else, let's keep it simple.  We'll use an alist of
;; alists to start with.  For example:

;;   (... (enum . ((clutter-actor-align . #<<gi-enum> 5629de89fcc0>))))

;;; Code:


(define-module (g-golf gi cache)

  #:export (%gi-cache

            gi-cache-ref
            gi-cache-set!))


(define %gi-cache
  '())

(define (gi-cache-ref m-key s-key)
  ;; m-key, s-key stand for main key, secondary key
  (let ((subcache (assq-ref %gi-cache m-key)))
    (and subcache
         (assq-ref subcache s-key))))

(define (gi-cache-set! m-key s-key val)
  (let ((subcache (assq-ref %gi-cache m-key)))
    (set! %gi-cache
          (assq-set! %gi-cache m-key
                     (assq-set! (or subcache '()) s-key
                                val)))))