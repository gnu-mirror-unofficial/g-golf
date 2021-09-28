;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2018 - 2021
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
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (system foreign)
  #:use-module (g-golf gobject boxed-types)

  #:export (%gi-cache
            gi-cache-ref
            gi-cache-set!
            gi-cache-remove!
            gi-cache-show
            gi-cache-find

            ;; instance cache
            %g-inst-cache
            g-inst-cache-ref
            g-inst-cache-set!
            g-inst-cache-for-each

            ;; boxed sa - scheme allocated - cache
            %g-boxed-sa-cache
            g-boxed-sa-cache-ref
            g-boxed-sa-cache-set!
            g-boxed-sa-cache-remove!
            g-boxed-sa-cache-show
            g-boxed-sa-guard

            ;; boxed ga - gobject allocated - cache
            %g-boxed-ga-cache
            g-boxed-ga-cache-ref
            g-boxed-ga-cache-set!
            g-boxed-ga-cache-remove!
            g-boxed-ga-cache-show
            g-boxed-ga-guard))


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


;;;
;;; The g-inst(ance) cache
;;;

(define %g-inst-cache-default-size 1013)

(define %g-inst-cache
  (make-weak-value-hash-table %g-inst-cache-default-size))

(define (g-inst-cache-ref g-inst)
  (hashq-ref %g-inst-cache
             (pointer-address g-inst)))

(define (g-inst-cache-set! g-inst inst)
  (hashq-set! %g-inst-cache
              (pointer-address g-inst)
              inst))

(define (g-inst-cache-for-each proc)
  (hash-for-each proc
                 %g-inst-cache))


;;;
;;; The g-boxed(instance) scheme allocated cache
;;;

(define %dimfi
  (@ (g-golf support utils) dimfi))

(define %g-boxed-sa-cache-default-size 1013)

(define %g-boxed-sa-cache
  (make-weak-key-hash-table %g-boxed-sa-cache-default-size))

(define (g-boxed-sa-cache-ref ptr)
  (hashq-ref %g-boxed-sa-cache ptr))

(define (g-boxed-sa-cache-set! ptr bv)
  (hashq-set! %g-boxed-sa-cache ptr bv))

(define (g-boxed-sa-cache-remove! ptr)
  (hashq-remove! %g-boxed-sa-cache ptr))

(define (g-boxed-sa-cache-show)
  (hash-for-each (lambda (key value)
                   (%dimfi key value))
                 %g-boxed-sa-cache))


;;;
;;; g-boxed-sa-guard
;;;

(define-syntax make-g-boxed-sa-guard
  (syntax-rules ()
    ((make-g-boxed-sa-guard)
     (let ((guardian (make-guardian)))
       (add-hook! after-gc-hook
                  (lambda ()
                    #;(%dimfi guardian)
                    (let loop ()
                      (let ((ptr (guardian)))
                        (when ptr
                          #;(%dimfi "  cleaning" ptr)
                          (g-boxed-sa-cache-remove! ptr)
                          (loop))))))
       (lambda (ptr bv)
         (g-boxed-sa-cache-set! ptr bv)
         (guardian ptr)
         ptr)))))

(define g-boxed-sa-guard (make-g-boxed-sa-guard))


;;;
;;; The g-boxed(instance) gobject allocated cache
;;;

;; in this case, we need to use a normal hash table, because when a
;; guardian returns a pointer, as part of an after-gc-hook procedure, a
;; weak hash table would already have cleared the entry, and unlike for
;; the other caches, we need to retreive the g-type of the (opaque)
;; boxedtype pointer.

;; a consequence of the above, is that we can't hold a reference to the
;; ffi pointer, otherwise it would never become unreachable ... and
;; hence, we specifically use the pointer address as the key.

(define %g-boxed-ga-cache-default-size 1013)

(define %g-boxed-ga-cache
  (make-hash-table %g-boxed-ga-cache-default-size))

(define (g-boxed-ga-cache-ref ptr)
  (hashq-ref %g-boxed-ga-cache
             (pointer-address ptr)))

(define (g-boxed-ga-cache-set! ptr g-type)
  (hashq-set! %g-boxed-ga-cache
              (pointer-address ptr)
              g-type))

(define (g-boxed-ga-cache-remove! ptr)
  (hashq-remove! %g-boxed-ga-cache
                 (pointer-address ptr)))

(define (g-boxed-ga-cache-show)
  (hash-for-each (lambda (key value)
                   (%dimfi key value))
                 %g-boxed-ga-cache))


;;;
;;; g-boxed-ga-guard
;;;

(define-syntax make-g-boxed-ga-guard
  (syntax-rules ()
    ((make-g-boxed-ga-guard)
     (let ((guardian (make-guardian)))
       (add-hook! after-gc-hook
                  (lambda ()
                    #;(%dimfi guardian)
                    (let loop ()
                      (let ((ptr (guardian)))
                        (when ptr
                          #;(%dimfi "  cleaning" ptr)
                          (let ((g-type (g-boxed-ga-cache-ref ptr)))
                            (g-boxed-ga-cache-remove! ptr)
                            (g-boxed-free g-type ptr)
                            (loop)))))))
       (lambda (ptr g-type)
         (g-boxed-ga-cache-set! ptr g-type)
         (guardian ptr)
         ptr)))))

(define g-boxed-ga-guard (make-g-boxed-ga-guard))
