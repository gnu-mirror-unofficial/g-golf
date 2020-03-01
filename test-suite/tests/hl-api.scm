;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2019 - 2020
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


(define-module (tests hl-api)
  #:use-module (ice-9 threads)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (g-golf))


(g-irepository-require "Clutter")


(define %grid-layout-info
  (g-irepository-find-by-name "Clutter" "GridLayout"))

(gi-import-object %grid-layout-info)

(define %actor-info
  (g-irepository-find-by-name "Clutter" "Actor"))

(gi-import-object %actor-info)


(define %actor-align-info
  (g-irepository-find-by-name "Clutter" "ActorAlign"))

(gi-import-enum %actor-align-info)

(define %actor-flags-info
  (g-irepository-find-by-name "Clutter" "ActorFlags"))

(gi-import-flag %actor-flags-info)


(define-class <g-golf-test-hl-api> (<test-case>))


(define-method (test-g-property-accessor (self <g-golf-test-hl-api>))
  (let ((a-grid (make <clutter-grid-layout>)))
    (assert-true (eq? (!orientation a-grid) 'horizontal))
    (assert (set! (!orientation a-grid) 'vertical))
    (assert-true (eq? (!orientation a-grid) 'vertical))))


(define-method (test-accessor-inheritance (self <g-golf-test-hl-api>))
  (let* ((module (resolve-module '(tests hl-api)))
         (c-name '<foo>)
         (c-inst (make-class (list <clutter-grid-layout>)
                             '()
                             #:name c-name))
         (dummy (module-define! module c-name c-inst))
         (a-foo (make <foo>)))
    (assert-true (eq? (!orientation a-foo) 'horizontal))
    (assert (set! (!orientation a-foo) 'vertical))
    (assert-true (eq? (!orientation a-foo) 'vertical))))


(define-method (test-closure-enum (self <g-golf-test-hl-api>))
  (let* ((enum %gi-type-tag)
         (closure (make <closure>
                    #:function (lambda (a) a)
                    #:return-type enum
                    #:param-types (list enum))))
    (assert-true (eq? (invoke closure 'interface)
                      'interface))
    (assert (free closure))))


(define-method (test-closure-gi-enum (self <g-golf-test-hl-api>))
  (let* ((enum (gi-cache-ref 'enum 'clutter-actor-align))
         (closure (make <closure>
                    #:function (lambda (a) a)
                    #:return-type enum
                    #:param-types (list enum))))
    (assert-true (eq? (invoke closure 'start)
                      'start))
    (assert (free closure))))


(define-method (test-closure-flags (self <g-golf-test-hl-api>))
  (let* ((flags %g-type-fundamental-flags)
         (closure (make <closure>
                    #:function (lambda (a) a)
                    #:return-type flags
                    #:param-types (list flags))))
    (assert-true (let ((result (invoke closure '(classed))))
                   (eq? (car result) 'classed)))
    (assert (free closure))))


(define-method (test-closure-gi-flags (self <g-golf-test-hl-api>))
  (let* ((flags (gi-cache-ref 'flag 'clutter-actor-flags))
         (closure (make <closure>
                    #:function (lambda (a) a)
                    #:return-type flags
                    #:param-types (list flags))))
    (assert-true (let ((result (invoke closure '(realized))))
                   (eq? (car result) 'realized)))
    (assert (free closure))))


(define-method (test-closure-gobject (self <g-golf-test-hl-api>))
  (let* ((actor (make <clutter-actor>))
         (closure (make <closure>
                    #:function (lambda (a) a)
                    #:return-type <clutter-actor>
                    #:param-types (list <clutter-actor>))))
    (assert-true (eq? (invoke closure actor)
                      actor))
    (assert (free closure))))


(define-method (test-closure-sum (self <g-golf-test-hl-api>))
  (let ((closure (make <closure>
                   #:function (lambda (a b) (+ a b))
                   #:return-type 'int
                   #:param-types '(int int))))
    (assert-true (= (invoke closure 2 3) 5))
    (assert (free closure))))


(define-method (test-g-idle-add (self <g-golf-test-hl-api>))
  (let* ((loop (g-main-loop-new #f #f))
         (idle (assert (g-idle-add (lambda ()
                                     'ok
                                     #f))))
         (thread (make-thread g-main-loop-run loop)))
    (cancel-thread thread)))


(define-method (test-g-timeout-add (self <g-golf-test-hl-api>))
  (let* ((loop (g-main-loop-new #f #f))
         (idle (assert (g-timeout-add 1000
                                      (lambda ()
                                        'ok
                                        #f))))
         (thread (make-thread g-main-loop-run loop)))
    (cancel-thread thread)))


(define-method (test-g-timeout-add-seconds (self <g-golf-test-hl-api>))
  (let* ((loop (g-main-loop-new #f #f))
         (idle (assert (g-timeout-add-seconds 1
                                              (lambda ()
                                                'ok
                                                #f))))
         (thread (make-thread g-main-loop-run loop)))
    (cancel-thread thread)))


(exit-with-summary (run-all-defined-test-cases))
