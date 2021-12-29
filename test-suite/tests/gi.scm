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


(define-module (tests gi)
  #:use-module (ice-9 receive)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (g-golf)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last))


(define-class <g-golf-test-gi> (<test-case>))


;;;
;;; Utils
;;;

(define-method (test-utils (self <g-golf-test-gi>))
  (assert (gi-pointer-new))
  (assert (gi-pointer-inc (gi-pointer-new) 0))
  (assert (gi-attribute-iter-new)))

(define-method (test-utils-n-string (self <g-golf-test-gi>))
  (let ((a '("the" "bluefox" "and" "the" "red" "bear")))
    (assert-true
     (receive (ptr i-ptrs)
         (scm->gi a 'n-string)
       (let ((b (gi->scm ptr 'n-string 6)))
         (and (= (length a) (length b))
              (every string=? a b)))))))

(define-method (test-utils-strings (self <g-golf-test-gi>))
  (let ((a '("the" "bluefox" "and" "the" "red" "bear")))
    (receive (ptr i-ptrs)
        (scm->gi a 'strings)
    (assert-true
     (let ((b (gi->scm ptr 'strings)))
       (and (= (length a) (length b))
            (every string=? a b)))))))

(define-method (test-utils-n-pointer (self <g-golf-test-gi>))
  (let ((a '("the" "bluefox" "and" "the" "red" "bear")))
    (assert-true
     (receive (ptr i-ptrs)
         (scm->gi a 'n-string)
       (let* ((b (scm->gi i-ptrs 'n-pointer))
              (c (gi->scm b 'n-pointer 6)))
         (and (= (length i-ptrs) (length c))
              (every (lambda (p1 p2)
                       (= (pointer-address p1)
                          (pointer-address p2)))
                     i-ptrs
                     c)))))))

(define-method (test-utils-pointers (self <g-golf-test-gi>))
  (let ((a '("the" "bluefox" "and" "the" "red" "bear")))
    (assert-true
     (receive (ptr i-ptrs)
         (scm->gi a 'strings)
       (let* ((b (scm->gi i-ptrs 'pointers))
              (c (gi->scm b 'pointers)))
         (and (= (length i-ptrs) (length c))
              (every (lambda (p1 p2)
                       (= (pointer-address p1)
                          (pointer-address p2)))
                     i-ptrs
                     c)))))))

(define-method (test-utils-n-gtype (self <g-golf-test-gi>))
  (let ((a '(int double object)))
    (assert-true
     (receive (ptr)
         (scm->gi a 'n-gtype 3)
       (let ((b (gi->scm ptr 'n-gtype 3)))
         (and (= (length a) (length b))
              (every eq? a (map g-type->symbol b))))))))

(define-method (test-utils-gtypes (self <g-golf-test-gi>))
  (let ((a '(int double object)))
    (assert-true
     (receive (ptr)
         (scm->gi a 'gtypes)
       (let ((b (gi->scm ptr 'gtypes)))
         (and (= (length a) (length b))
              (every eq? a (map g-type->symbol b))))))))


;;;
;;; Repository
;;;

(define-method (test-repository (self <g-golf-test-gi>))
  (assert-true (g-irepository-require "Clutter"))
  (assert-true (g-irepository-require "Gtk"))
  (assert-true (g-irepository-get-typelib-path "Clutter"))
  (assert-true (g-irepository-find-by-name "Clutter" "Actor"))
  (assert-true (g-irepository-find-by-name "Clutter" "ActorAlign"))
  (assert-exception (g-irepository-require "ClutterBlue"))
  (assert-false (g-irepository-find-by-name "Gtk" "button"))
  (assert-true (g-irepository-find-by-name "Gtk" "Button"))
  (assert-true (g-irepository-get-version "Gtk")))


;;;
;;; Base Info
;;;

(define-method (test-base-info (self <g-golf-test-gi>))
  (assert-true (g-base-info-get-name
                (g-irepository-find-by-name "Clutter" "Actor"))))


;;;
;;; Callable Info
;;;


;;;
;;; Function Info
;;;

(define-method (test-function-info (self <g-golf-test-gi>))
  (let* ((actor (g-irepository-find-by-name "Clutter" "Actor"))
         (actor-m1 (g-object-info-get-method actor 0)))
    (assert (g-function-info-get-flags actor-m1))
    (assert (g-function-info-get-property actor-m1))
    (assert (g-function-info-get-symbol actor-m1))
    (assert (g-function-info-get-vfunc actor-m1))))


;;;
;;; Registered Type Info
;;;

(define-method (test-registered-type-info (self <g-golf-test-gi>))
  (let ((g-param-flags-info (g-irepository-find-by-name "GObject" "ParamFlags")))
    (assert (g-registered-type-info-get-g-type g-param-flags-info))
    (assert-false (g-registered-type-info-get-type-name g-param-flags-info))
    (assert-equal (gi-registered-type-info-name g-param-flags-info)
                  "GObjectParamFlags")))


;;;
;;; Enum Info
;;;

(define-method (test-enum-info (self <g-golf-test-gi>))
  (let ((align-info (g-irepository-find-by-name "Clutter" "ActorAlign")))
    (assert (g-enum-info-get-n-values align-info))
    (assert (g-enum-info-get-value align-info 0))
    (assert (gi-enum-value-values align-info))
    (assert (gi-enum-import align-info))))


;;;
;;; Struct Info
;;;

(define-method (test-struct-info (self <g-golf-test-gi>))
  (let ((info (g-irepository-find-by-name "Clutter" "Color")))
    (assert-true (= (g-struct-info-get-n-fields info)
                    4)) ;; RGBA fields
    (assert (g-struct-info-get-field info 0))
    (assert (gi-struct-field-types info))
    (assert (gi-struct-import info))
    ;; the following should also work, but a real pointer is returned
    ;; instad. I have asked on #introspection, but lost the connection
    ;; just after I asked, lets see what they tell me once I get a
    ;; connection back.
    #;(assert-false (g-struct-info-get-field info 4))))


;;;
;;; Union Info
;;;

(define-method (test-union-info (self <g-golf-test-gi>))
  (assert (g-irepository-require "Gdk"))
  (let ((info (g-irepository-find-by-name "Gdk" "Event")))
    (assert (g-union-info-get-n-fields info))
    (assert (g-union-info-get-field info 0))
    (assert (g-union-info-get-n-methods info))
    (assert (g-union-info-get-method info 0))
    (assert (g-union-info-is-discriminated? info))
    (assert (g-union-info-get-size info))
    (assert (g-union-info-get-alignment info))))


;;;
;;; Object Info
;;;

(define-method (test-object-info (self <g-golf-test-gi>))
  (let ((actor (g-irepository-find-by-name "Clutter" "Actor")))
    (assert-true (g-object-info-get-n-methods actor))
    (assert-true (g-object-info-get-method actor 0))
    (assert-true (g-object-info-get-property actor 5))))


;;;
;;; Arg Info
;;;


;;;
;;; Constant Info
;;;

(define-method (test-constant-info (self <g-golf-test-gi>))
  (let ((info (g-irepository-find-by-name "GLib" "PRIORITY_DEFAULT"))
        (value (make-gi-argument)))
    (assert (g-constant-info-get-type info))    
    (assert (g-constant-info-get-value info value))
    (assert (g-constant-info-free-value info value))))


;;;
;;; Field Info
;;;

(define-method (test-field-info (self <g-golf-test-gi>))
  (let* ((info (g-irepository-find-by-name "Clutter" "Color"))
         (field (g-struct-info-get-field info 0)))
    (assert (g-field-info-get-type field))
    (assert (g-type-info-get-tag (g-field-info-get-type field)))))


;;;
;;; Property Info
;;;

(define-method (test-property-info (self <g-golf-test-gi>))
  (let* ((actor (g-irepository-find-by-name "Clutter" "Actor"))
         (property (g-object-info-get-property actor 5)))
    (assert-true (g-property-info-get-type property))
    (assert-true (g-property-info-get-flags property))))


;;;
;;; Type Info
;;;

(define-method (test-type-info (self <g-golf-test-gi>))
  (let* ((actor (g-irepository-find-by-name "Clutter" "Actor"))
         (property (g-object-info-get-property actor 5))
         (type-info (g-property-info-get-type property))
         (type-tag (g-type-info-get-tag type-info))
         (interface (g-type-info-get-interface type-info))
         (i-type (g-base-info-get-type interface)))
    (assert (g-type-tag-to-string type-tag))
    (assert (g-type-tag-to-string 'interface))
    (assert-false (g-type-tag-to-string 1000))
    (assert-false (g-type-tag-to-string 'blue))
    (assert (g-info-type-to-string i-type))
    (assert (g-info-type-to-string 'struct))
    (assert-false (g-info-type-to-string 1000))
    (assert-false (g-type-tag-to-string 'fox))
    (assert-false (g-type-info-is-pointer type-info))
    (assert-false (g-type-info-get-param-type type-info 0))
    (assert-true (= (g-type-info-get-array-length type-info) -1))
    (assert-true (= (g-type-info-get-array-fixed-size type-info) -1))
    (assert-false (g-type-info-is-zero-terminated type-info))
    (assert-false (g-type-info-get-array-type type-info))))


;;;
;;; Typelib
;;;

(define-method (test-type-lib (self <g-golf-test-gi>))
  (let ((filename (g-irepository-get-typelib-path "Clutter")))
    (assert-equal "Clutter"
		  (call-with-input-typelib filename
					   (lambda (typelib)
					     (g-typelib-get-name-space typelib))))))


(exit-with-summary (run-all-defined-test-cases))
