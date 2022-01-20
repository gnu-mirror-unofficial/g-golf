;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2018 - 2022
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

;; Hack to mark this module as non-declarative in Guile 3+ (which would
;; otherwise print a warning) but not break when compiling on earlier
;; versions of Guile. Thanks to David Thompson who wrote this macro and
;; shared his solution withus on irc ...
(define-syntax-rule (define-module* name args ...)
  (cond-expand
   (guile-3
    (define-module name
      #:declarative? #f
      args ...))
   (guile
    (define-module name args ...))))


(define-module* (g-golf gi cache-gi)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (g-golf gobject boxed-types)

  #:export (%gi-cache
            gi-cache-ref
            gi-cache-set!
            gi-cache-remove!
            gi-cache-show
            gi-cache-find))


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

(define (gi-cache-remove! m-key s-key)
  ;; m-key, s-key stand for main key, secondary key
  (let ((subcache (assq-ref %gi-cache m-key)))
    (and subcache
         (assq-remove! subcache s-key))))

(define* (gi-cache-show #:optional (m-key #f))
  (format #t "%gi-cache~%")
  (if m-key
      (begin
        (format #t "  ~A~%" m-key)
        (match (assq-ref %gi-cache m-key)
          (#f
           (format #t "    -- is empty --~%"))
          (else
           (for-each (lambda (s-entry)
                       (match s-entry
                         ((s-key . s-val)
                          (format #t "    ~A~%      ~S~%" s-key s-val))))
               (assq-ref %gi-cache m-key)))))
      (for-each (lambda (m-entry)
                  (match m-entry
                    ((m-key . m-vals)
                     (format #t "  ~A~%" m-key))))
          %gi-cache))
  (values))

(define (gi-cache-find m-key pred)
  "Obtains the %gi-cache subcache for M-KEY, an (S-KEY . S-VAL) alist,
and returns a list of the S-KEY for which (PRED S-VAL) was satisfied."
  (filter-map
      (lambda (s-entry)
        (match s-entry
          ((s-key . s-val)
           (and (pred s-val)
                s-key))))
      (assq-ref %gi-cache m-key)))
