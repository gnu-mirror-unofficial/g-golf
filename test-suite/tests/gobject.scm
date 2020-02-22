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


(define-module (tests gobject)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (g-golf)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last))


(g-irepository-require "Clutter")

(define %align-info
  (g-irepository-find-by-name "Clutter" "ActorAlign"))

(define %align-info-g-type
  (g-registered-type-info-get-g-type %align-info))

(gi-import-enum %align-info)

(define %flags-info
  (g-irepository-find-by-name "Clutter" "ActorFlags"))

(define %flags-info-g-type
  (g-registered-type-info-get-g-type %flags-info))

(gi-import-flag %flags-info)


(define-class <g-golf-test-gobject> (<test-case>))


(define-method (test-g-value-size (self <g-golf-test-gobject>))
  (assert (g-value-size)))

(define-method (test-g-value-init (self <g-golf-test-gobject>))
  (assert (g-value-init (symbol->g-type 'float))))

(define-method (test-g-value-unset (self <g-golf-test-gobject>))
  (assert (g-value-unset
           (g-value-init (symbol->g-type 'float)))))

(define-method (test-g-value->g-type* (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init %align-info-g-type)))
    (assert-true (= (g-value->g-type-id g-value) %align-info-g-type))
    (assert-true (eq? (g-value->g-type g-value) 'enum))))

(define-method (test-g-value-get-boolean (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'boolean))))
    (assert (g-value-ref g-value))))

(define-method (test-g-value-set-boolean (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'boolean))))
    (assert (g-value-set! g-value #f))
    (assert-false (g-value-ref g-value))
    (assert (g-value-set! g-value 'true))
    (assert-true (g-value-ref g-value))))

(define-method (test-g-value-get-int (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'int))))
    (assert (g-value-ref g-value))))

(define-method (test-g-value-set-int (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'int))))
    (assert (g-value-set! g-value 5))
    (assert (g-value-set! g-value -5))))

(define-method (test-g-value-get-uint (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'uint))))
    (assert (g-value-ref g-value))))

(define-method (test-g-value-set-uint (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'uint))))
    (assert (g-value-set! g-value 5))))

(define-method (test-g-value-get-float (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'float))))
    (assert (g-value-ref g-value))))

(define-method (test-g-value-set-float (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'float))))
    (assert (g-value-set! g-value 5.0))))

(define-method (test-g-value-get-double (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'double))))
    (assert (g-value-ref g-value))))

(define-method (test-g-value-set-double (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'double))))
    (assert (g-value-set! g-value 5.0))))

(define-method (test-g-value-get-enum (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init %align-info-g-type)))
    (assert (g-value-ref g-value))))

(define-method (test-g-value-set-enum (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init %align-info-g-type)))
    (assert (g-value-set! g-value 1))
    (assert (g-value-set! g-value 'start))))

(define-method (test-g-value-get-flags (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init %flags-info-g-type)))
    (assert (g-value-ref g-value))))

(define-method (test-g-value-set-flags (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init %flags-info-g-type)))
    (assert (g-value-set! g-value '(mapped)))))

(define-method (test-g-value-get-string (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'string))))
    (assert (g-value-ref g-value))))

(define-method (test-g-value-set-string (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'string))))
    (assert (g-value-set! g-value ""))
    (assert (g-value-set! g-value "Hello!"))
    (assert (g-value-set! g-value "Apresentação"))))

(define %g-io-channel
  (gi-cache-ref 'boxed 'gio-channel))

(define-method (test-g-value-boxed-semi-opaque (self <g-golf-test-gobject>))
  (let* ((port (open "/dev/tty" O_RDONLY))
         (fd (fileno port))
         (channel (g-io-channel-unix-new fd))
         (g-value (g-value-init (slot-ref %g-io-channel 'gtype-id))))
    (assert (g-value-set! g-value channel))
    (assert-true (eq? (pointer-address (g-value-ref g-value))
                      (pointer-address channel)))
    (close port)))

(define-method (test-g-value-get-pointer (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'pointer))))
    (assert (g-value-ref g-value))))

(define-method (test-g-value-set-pointer (self <g-golf-test-gobject>))
  (let ((g-value (g-value-init (symbol->g-type 'pointer))))
    (assert (g-value-set! g-value g-value))))

;; I can't test g-value-get-object and g-value-set-object using G-Golf,
;; till it is able to build an interface. I did manually test these two
;; procedures though, by making a manual binding to clutter-init and
;; clutter-actor-new, which requires "libclutter-1.0", something G-Golf
;; does not need to depend upon.  As soon as G-Golf can make instances,
;; we will add a proper test here.


(define-method (test-g-type-name (self <g-golf-test-gobject>))
  (assert-equal "ClutterActorAlign" (g-type-name %align-info-g-type))
  (assert-equal "gfloat" (g-type-name 56)))


(exit-with-summary (run-all-defined-test-cases))
