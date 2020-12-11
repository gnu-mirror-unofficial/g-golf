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
            gi-import-constant))


#;(g-export )


;;;
;;;
;;;

(define %gi-base-info-types '())
(define %gi-imported-base-info-types '())

(define (gi-is-info-a? info type)
  (eq? (g-base-info-get-type info) type))

(define* (gi-import namespace
                    #:key (version #f)
                    (not-imported-warnings? #f))
  (g-irepository-require namespace #:version version)
  (when not-imported-warnings?
    (dimfi "Namespace constants are not imported"))
  (let ((n-info (g-irepository-get-n-infos namespace)))
    (do ((i 0
            (+ i 1)))
        ((= i n-info))
      (gi-import-info (g-irepository-get-info namespace i)
                      #:not-imported-warnings? not-imported-warnings?))
    (values)))

(define* (gi-import-by-name namespace name
                            #:key (version #f)
                            (not-imported-warnings? #f)
                            (with-methods? #t)
                            (force? #f))
  (when (or force?
            (not (is-namespace-import-exception? namespace)))
    (g-irepository-require namespace #:version version)
    (let ((info (g-irepository-find-by-name namespace name)))
      (if info
          (gi-import-info info
                          #:not-imported-warnings? not-imported-warnings?
                          #:with-methods? with-methods?
                          #:force? force?)
          (error "No such namespace name: " namespace name)))))

(define* (gi-import-info info
                         #:key (not-imported-warnings? #f)
                         (with-methods? #t)
                         (force? #f))
  (let ((i-type (g-base-info-get-type info)))
    (unless (memq i-type
                  %gi-base-info-types)
      (push! i-type %gi-base-info-types))
    (case i-type
      ((enum)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-enum info #:with-methods? with-methods? #:force? force?))
      ((flags)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-flag info #:with-methods? with-methods? #:force? force?))
      ((struct)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-struct info #:with-methods? with-methods? #:force? force?))
      ((union)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-union info #:with-methods? with-methods? #:force? force?))
      ((function)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-function info #:force? force?))
      ((object)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-object info #:with-methods? with-methods? #:force? force?))
      ((interface)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-interface info #:with-methods? with-methods? #:force? force?))
      #;((callback)
       (unless (memq i-type
                     %gi-imported-base-info-types)
         (push! i-type %gi-imported-base-info-types))
       (gi-import-callback info))
      (else
       (if not-imported-warnings?
           (if (procedure? not-imported-warnings?)
               (not-imported-warnings? info)
               (case i-type
                 ((constant) #f) ;; too many constant
                 (else
                  (dimfi (g-base-info-get-namespace info)
                         (g-base-info-get-name info)
                         i-type
                         "not imported"))))
           #f)))))

(define* (gi-import-constant info)
  (let* ((g-name (g-base-info-get-name info))
         ;; (name (g-name->name g-name))
         (type-info (g-constant-info-get-type info))
         (type-tag (g-type-info-get-tag type-info))
         (field (gi-type-tag->field type-tag))
         (value (make-gi-argument))
         (dummy (g-constant-info-get-value info value))
         (constant (gi-argument-ref value field)))
    (g-base-info-unref type-info)
    (values constant
            g-name)))
