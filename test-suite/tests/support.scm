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


(define-module (tests support)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (g-golf)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last))


(define-class <g-golf-test-support> (<test-case>))


;;;
;;; Bytevector
;;;

(define-method (test-bytevector (self <g-golf-test-support>))
  (let* ((bv (make-bytevector (sizeof '*) 0))
         (ptr (bytevector->pointer bv)))
    (assert (bv-ptr-set! ptr %null-pointer))
    (assert-true (eq? (bv-ptr-ref ptr) %null-pointer))))


;;;
;;; Goops
;;;

(define-class <a> () (name) (val))

(define-method (test-mslot-set! (self <g-golf-test-support>))
  (let ((inst (make <a>)))
    (assert (mslot-set! inst
                        'name 'alice
                        'val 7))
    (assert-true (eq? (slot-ref inst 'name) 'alice))
    (assert-true (= (slot-ref inst 'val) 7))
    (assert-exception (mslot-set! inst
                                  'name 'alice
                                  'val))))


;;;
;;; Enum
;;;

(define-method (test-enum (self <g-golf-test-support>))
  (let* ((a-set '((foo . 0) (bar  . 1)))
         (enum (make <enum> #:enum-set a-set)))
    (assert-exception (make <enum>))
    (assert-true (enum->value enum 'foo))
    (assert-true (enum->value enum 'bar))
    (assert-false (enum->value enum 'baz))
    (assert-true (enum->symbol enum 0))
    (assert-true (enum->symbol enum 1))
    (assert-false (enum->symbol enum 2))
    (assert-true (enum->name enum 'foo))
    (assert-true (enum->name enum 'bar))
    (assert-false (enum->name enum 'baz))
    (assert-true (enum->name enum 0))
    (assert-true (enum->name enum 1))
    (assert-false (enum->name enum 2))
    (assert (enum->names enum))))

;;;
;;; Utils
;;;

(define-method (test-utils (self <g-golf-test-support>))
  (assert-equal "g-studly-caps-expand"
                (g-studly-caps-expand "GStudlyCapsExpand"))
  (assert-equal "webkit-web-content"
                (g-studly-caps-expand "WebKitWebContent"))
  (assert-equal '<webkit-web-content>
		(g-name->class-name "WebKitWebContent"))
  (assert-equal "webkit-network-proxy-settings-new"
                (g-name->scm-name "webkit_network_proxy_settings_new"))
  (assert-equal "bluefox"
                (g-name->scm-name "BLuefox"))
  (assert-equal "clutter-actor"
		(g-name->scm-name "ClutterActor"))
  (assert-equal '<clutter-actor>
		(g-name->class-name "ClutterActor"))
  (assert-equal "g-variant-type-checked-"
                (g-name->scm-name "g_variant_type_checked_")))


(exit-with-summary (run-all-defined-test-cases))
