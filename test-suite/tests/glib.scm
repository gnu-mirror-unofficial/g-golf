;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2021
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


(define-module (tests glib)
  #:use-module (ice-9 threads)
  #:use-module (oop goops)
  #:use-module (unit-test)
  #:use-module (g-golf)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last))


#;(g-irepository-require "Clutter")


(define-class <g-golf-test-glib> (<test-case>))


(define-method (test-g-malloc (self <g-golf-test-glib>))
  (assert-exception (g-malloc -1.5))
  (assert-exception (g-malloc -1))
  (assert-exception (g-malloc 1.5))
  (assert-false (g-malloc -0))
  (assert-false (g-malloc 0))
  (assert (g-malloc 1)))


(define-method (test-g-malloc0 (self <g-golf-test-glib>))
  (assert-exception (g-malloc0 -1.5))
  (assert-exception (g-malloc0 -1))
  (assert-exception (g-malloc0 1.5))
  (assert-false (g-malloc0 -0))
  (assert-false (g-malloc0 0))
  (assert (g-malloc0 1)))


(define-method (test-g-free (self <g-golf-test-glib>))
  (assert-exception (g-free 10))
  (assert (g-free %null-pointer))
  (assert (g-free (g-malloc 10))))


(define-method (test-g-memdup (self <g-golf-test-glib>))
  (let ((mem (g-malloc 10)))
    (assert-exception (g-memdup mem -1.5))
    (assert-exception (g-memdup mem -1))
    (assert-exception (g-memdup mem 1.5))
    (assert-false (g-memdup %null-pointer 10))
    (assert-false (g-memdup mem -0))
    (assert-false (g-memdup mem 0))
    (assert (g-memdup mem 1))
    (assert (g-memdup mem 10))
    (assert (g-memdup mem 20))))


(define-method (test-main-loop (self <g-golf-test-glib>))
  (let* ((loop (assert (g-main-loop-new #f #f)))
         (thread (make-thread g-main-loop-run loop)))
    (assert (g-main-loop-ref loop))
    (assert (g-main-loop-unref loop))
    (assert (g-main-loop-quit loop))
    (cancel-thread thread)))


(define-method (test-main-context (self <g-golf-test-glib>))
  (assert (g-main-context-new))
  (assert (g-main-context-default)))


(define-method (test-idle-source (self <g-golf-test-glib>))
  (let* ((source (assert (g-idle-source-new)))
         (context (g-main-context-new))
         (id (assert (g-source-attach source context))))
    (assert (g-source-get-priority source))
    (assert (g-source-set-priority source 300))
    (assert-true (= (g-source-get-priority source)
                    300))
    (assert (g-source-ref-count source))
    (assert (g-source-ref source))
    ;; the g-source-remove test was wrong, but I'll keep it to remember
    ;; my mistake, which is that (g-source-remove id) only works for
    ;; sources attached to the GMainContext. Here, 'source' was attached
    ;; to 'context'.
    ;;   (assert (g-source-remove id))
    (assert (g-source-unref source))
    (assert-false (g-source-is-destroyed? source))
    (assert (g-source-destroy source))
    (assert-true (g-source-is-destroyed? source))
    (assert (g-source-free source))))


(define-method (test-timeout-source (self <g-golf-test-glib>))
  (assert (g-timeout-source-new 1000)))


(define-method (test-timeout-source-seconds (self <g-golf-test-glib>))
  (assert (g-timeout-source-new-seconds 1)))


(define-method (test-g-quark (self <g-golf-test-glib>))
  (assert-true (string=? (g-quark-to-string
                          (g-quark-from-string "destroy"))
                         "destroy")))


;;;
;;; Miscellaneous Utility Functions
;;;

(define-method (test-g-get/set-prgname (self <g-golf-test-glib>))
  (assert-false (g-get-prgname))
  (assert (g-set-prgname "org.gtk.example"))
  (assert-true (string=? (g-get-prgname)
                         "org.gtk.example")))

(define-method (test-g-get-system-*-dirs (self <g-golf-test-glib>))
  (assert (g-get-system-data-dirs))
  (assert (g-get-system-config-dirs)))


;;;
;;; GSList
;;;

(define-method (test-g-slist-append (self <g-golf-test-glib>))
  (let* ((a (string->pointer "hello"))
         (b (string->pointer "world!"))
         (g-first (assert (g-slist-append #f a)))
         (g-first (assert (g-slist-append g-first b))))
    (assert-true (equal? (list "hello" "world!")
                         (map pointer->string
                           (gi->scm g-first 'gslist))))))

(define-method (test-g-slist-prepend (self <g-golf-test-glib>))
  (let* ((a (string->pointer "hello"))
         (b (string->pointer "world!"))
         (g-first (assert (g-slist-prepend #f b)))
         (g-first (assert (g-slist-prepend g-first a))))
    (assert-true (equal? (list "hello" "world!")
                         (map pointer->string
                           (gi->scm g-first 'gslist))))))


(exit-with-summary (run-all-defined-test-cases))
