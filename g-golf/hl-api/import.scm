;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2018 - 2020
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
  #:use-module (ice-9 match)
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
            gi-import-by-name
            gi-import-info
            gi-import-enum
            gi-import-flag
            gi-import-struct
            gi-import-union
            gi-import-constant
            gi-import-interface))


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

(define* (gi-import-by-name namespace name
                            #:key (debug #f) (recur #t))
  (g-irepository-require namespace)
  (let ((info (g-irepository-find-by-name namespace name)))
    (if info
        (gi-import-info info #:debug debug #:recur recur)
        (error "No such namespace name: " namespace name))))

(define* (gi-import-info info #:key (debug #f) (recur #t))
  (let ((i-type (g-base-info-get-type info)))
    (unless (memq i-type
                  %gi-base-info-types)
      (push! i-type %gi-base-info-types))
    (case i-type
      ((enum)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-enum info #:recur recur))
      ((flags)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-flag info #:recur recur))
      ((struct)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-struct info #:recur recur))
      ((union)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-union info #:recur recur))
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
      ((interface)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-interface info #:recur recur))
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
           'nothing)))))

(define* (gi-import-enum info #:key (recur #t))
  (let* ((id (g-registered-type-info-get-g-type info))
         (name (g-studly-caps-expand (g-type-name id)))
         (key (string->symbol name)))
    (or (gi-cache-ref 'enum key)
        (let ((gi-enum (gi-enum-import info)))
          (gi-cache-set! 'enum key gi-enum)
          (when recur
            (gi-enum-import-methods info))
          gi-enum))))

(define* (gi-import-flag info #:key (recur #t))
  (let* ((id (g-registered-type-info-get-g-type info))
         (name (g-studly-caps-expand (g-type-name id)))
         (key (string->symbol name)))
    (or (gi-cache-ref 'flag key)
        (let ((gi-flag (gi-enum-import info #:flag #t)))
          (gi-cache-set! 'flag key gi-flag)
          (when recur
            (gi-enum-import-methods info))
          gi-flag))))

(define* (gi-import-struct info #:key (recur #t))
  (let* ((id (g-registered-type-info-get-g-type info))
         (name (g-studly-caps-expand (g-type-name id)))
         (key (string->symbol name)))
    (or (gi-cache-ref 'boxed key)
        (let ((gi-struct (gi-struct-import info)))
          (gi-cache-set! 'boxed key gi-struct)
          (when recur
            (gi-struct-import-methods info))
          gi-struct))))

(define %type-description
  (@@ (g-golf hl-api function) type-description))

(define* (gi-import-union info #:key (recur #t))
  (let* ((id (g-registered-type-info-get-g-type info))
         (name (g-type-name id))
         (scm-name (string->symbol (g-studly-caps-expand name))))
    (or (gi-cache-ref 'boxed scm-name)
        (let* ((fields
                (map (lambda (field)
                       (match field
                         ((f-name f-type-info)
                          (let ((f-desc (%type-description f-type-info)))
                            (g-base-info-unref f-type-info)
                            (list f-name f-desc)))))
                  (gi-union-import info)))
               (gi-union
                (make <gi-union>
                  #:gtype-id id
                  #:gi-name name
                  #:scm-name scm-name
                  #:size (g-union-info-get-size info)
                  #:alignment (g-union-info-get-alignment info)
                  #:fields fields
                  #:is-discriminated? (g-union-info-is-discriminated? info)
                  #:discriminator-offset (g-union-info-get-discriminator-offset info))))
          (gi-cache-set! 'boxed scm-name gi-union)
          #;(when recur
            (gi-union-import-methods info))
          (g-base-info-unref info)
          gi-union))))

(define* (gi-import-constant info)
  (let* ((gi-name (g-base-info-get-name info))
         ;; (scm-name (g-name->scm-name gi-name))
         ;; (name (string->symbol scm-name))
         (type-info (g-constant-info-get-type info))
         (type-tag (g-type-info-get-tag type-info))
         (field (gi-type-tag->field type-tag))
         (value (make-gi-argument))
         (dummy (g-constant-info-get-value info value))
         (constant (gi-argument-ref value field)))
    (g-base-info-unref type-info)
    (values constant
            gi-name)))

(define* (gi-import-interface info #:key (recur #t))
  (let* ((id (g-registered-type-info-get-g-type info))
         (name (g-type-name id))
         (key (string->symbol (g-studly-caps-expand name))))
    (or (gi-cache-ref 'iface key)
        (let ((gi-iface (list 'interface key name id #t)))
          (gi-cache-set! 'iface key gi-iface)
          (when recur
            (gi-interface-import-methods info))
          gi-iface))))
