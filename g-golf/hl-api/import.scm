;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2018 - 2019
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


(define-module (g-golf hl-api import)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf gi)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)
  #:use-module (g-golf hl-api gtype)
  #:use-module (g-golf hl-api gobject)
  #:use-module (g-golf hl-api function)
  #:use-module (g-golf hl-api object)
  #:use-module (g-golf hl-api callback)  

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export (%gi-base-info-types
            %gi-imported-base-info-types
            gi-is-info-a?
            gi-import
            gi-import-info
            gi-import-by-name
            gi-import-enum
            gi-import-flag
            gi-import-struct))


#;(g-export )


;;;
;;;
;;;

(define %gi-base-info-types '())
(define %gi-imported-base-info-types '())

(define (gi-is-info-a? info type)
  (eq? (g-base-info-get-type info) type))

(define* (gi-import namespace #:key (debug #f))
  (g-irepository-require namespace)
  (let ((n-info (g-irepository-get-n-infos namespace)))
    (do ((i 0
            (+ i 1)))
        ((= i n-info))
      (gi-import-info (g-irepository-get-info namespace i)
                      #:debug debug))
    (values)))

(define* (gi-import-info info #:key (debug #f))
  (let ((i-type (g-base-info-get-type info)))
    (unless (memq i-type
                  %gi-base-info-types)
      (push! i-type %gi-base-info-types))
    (case i-type
      ((enum)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-enum info))
      ((flags)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-flag info))
      ((struct)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-struct info))
      ((function)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-function info))
      ((object)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-object info))
      #;((callback)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-callback info))
      (else
       (if debug
           (if (procedure? debug)
               (and (debug info)
                    (dimfi i-type (g-base-info-get-name info) "not imported"))
               (dimfi i-type (g-base-info-get-name info) "not imported"))
           'nothing)))
    (values)))

(define* (gi-import-by-name namespace name #:key (debug #f))
  (g-irepository-require namespace)
  (let ((info (g-irepository-find-by-name namespace name)))
    (if info
        (gi-import-info info #:debug debug)
        (error "No such namespace name: " namespace name))))

(define (gi-import-enum info)
  (let* ((id (g-registered-type-info-get-g-type info))
         (name (g-studly-caps-expand (g-type-name id)))
         (key (string->symbol name)))
    (or (gi-cache-ref 'enum key)
        (let ((gi-enum (gi-enum-import info)))
          (gi-cache-set! 'enum key gi-enum)
          (gi-enum-import-methods info)
          gi-enum))))

(define (gi-import-flag info)
  (let* ((id (g-registered-type-info-get-g-type info))
         (name (g-studly-caps-expand (g-type-name id)))
         (key (string->symbol name)))
    (or (gi-cache-ref 'flag key)
        (let ((gi-flag (gi-enum-import info #:flag #t)))
          (gi-cache-set! 'flag key gi-flag)
          (gi-enum-import-methods info)
          gi-flag))))

(define (gi-import-struct info)
  (let* ((id (g-registered-type-info-get-g-type info))
         (name (g-studly-caps-expand (g-type-name id)))
         (key (string->symbol name)))
    (or (gi-cache-ref 'boxed key)
        (let ((gi-struct (gi-struct-import info)))
          (gi-cache-set! 'boxed key gi-struct)
          (gi-struct-import-methods info)
          gi-struct))))
