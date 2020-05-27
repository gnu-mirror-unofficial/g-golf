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


(define-module (g-golf hl-api object)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf gi)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)
  #:use-module (g-golf hl-api gtype)
  #:use-module (g-golf hl-api gobject)
  #:use-module (g-golf hl-api function)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export (gi-import-object))


#;(g-export )

(define* (gi-import-object info #:key (with-methods? #t) (force? #f))
  (let* ((namespace (g-base-info-get-namespace info))
         (with-methods? (if (is-namespace-import-exception? namespace)
                            #f
                            with-methods?))
         (module (resolve-module '(g-golf hl-api gobject)))
         (r-info-cpl (reverse! (g-object-class-precedence-list info))))
    (unless (is-g-object-subclass? r-info-cpl)
      ;; A 'fundamental type' class, not a GObject subclass.
      (match r-info-cpl
        ((parent . rest)
         (g-object-import-with-supers parent '() module
                                      #:with-methods? with-methods?
                                      #:force? force?))))
    (let loop ((r-info-cpl r-info-cpl))
      (match r-info-cpl
        ((item) item) ;; the g-object subclass for info
        ((parent child . rest)
         (match parent
           ((p-info p-namespace p-name)
            (let* ((p-r-type (g-registered-type-info-get-g-type p-info))
                   (p-gi-name (g-type-name p-r-type))
                   (p-c-name (g-name->class-name p-gi-name))
                   (child-class
                    (g-object-import-with-supers child
                                                 (list (module-ref module p-c-name))
                                                 module
                                                 #:with-methods? with-methods?
                                                 #:force? force?)))
                  (loop (cons (if (null? rest)
                                  child-class
                                  child)
                              rest))))))))))

(define (is-g-object-subclass? info-cpl)
  (letrec ((is-g-object-info-cpl-item?
            (lambda (what info-cpl-item)
              (match info-cpl-item
                ((info namespace name)
                 (string=? name what))))))
    (member "GObject" info-cpl is-g-object-info-cpl-item?)))

(define* (g-object-import-with-supers child supers module
                                      #:key (with-methods? #t) (force? #f))
  (match child
    ((info namespace name)
     (unless (member namespace
                     (g-irepository-get-loaded-namespaces)
                     string=?)
       (g-irepository-require namespace))
     (let* ((r-type (g-registered-type-info-get-g-type info))
            (gi-name (g-type-name r-type))
            (c-name (g-name->class-name gi-name)))
       (if (module-bound? module c-name)
           (module-ref module c-name)
           (let ((c-inst (make-class supers
                                     '()
                                     #:name c-name
                                     #:info info)))
             (module-define! module c-name c-inst)
             (module-g-export! module `(,c-name))
             (when with-methods?
               (gi-import-object-methods info #:force? force?)
               (gi-import-object-class-methods info #:force? force?))
             ;; We do not import signals, they are imported on
             ;; demand. Visit (g-golf hl-api signal) signal-connect and
             ;; the %gi-signal-cache related code to see how this is
             ;; achieved.
             #;(gi-import-object-signals info)
             c-inst))))))

(define (g-object-class-precedence-list info)
  (let  loop ((parent (g-object-info-get-parent info))
              (results (list (list info
                                   (g-base-info-get-namespace info)
                                   (g-object-info-get-type-name info)))))
    (if (not parent)
        (reverse! results)
        (loop (g-object-info-get-parent parent)
              (cons (list parent
                          (g-base-info-get-namespace parent)
                          (g-object-info-get-type-name parent))
                    results)))))

(define* (gi-import-object-methods info
                                   #:key (force? #f))
  (let ((namespace (g-base-info-get-namespace info))
        (n-method (g-object-info-get-n-methods info)))
    (do ((i 0
            (+ i 1)))
        ((= i n-method))
      (let* ((m-info (g-object-info-get-method info i))
             (namespace (g-base-info-get-namespace m-info))
             (name (g-function-info-get-symbol m-info)))
        ;; Some methods listed here are functions: (a) their flags is an
        ;; empty list; (b) they do not expect an additional instance
        ;; argument (their GIargInfo list is complete); (c) they have a
        ;; GIFuncInfo entry in the namespace (methods do not). We do not
        ;; (re)import those here.
        (unless (g-irepository-find-by-name namespace name)
          (gi-import-function m-info #:force? force?))))))

(define* (gi-import-object-class-methods info
                                         #:key (force? #f))
  (let* ((namespace (g-base-info-get-namespace info))
         (class-struct (g-object-info-get-class-struct info))
         (n-method (g-struct-info-get-n-methods class-struct)))
    (do ((i 0
            (+ i 1)))
        ((= i n-method))
      (let* ((m-info (g-struct-info-get-method class-struct i))
             (namespace (g-base-info-get-namespace m-info))
             (name (g-function-info-get-symbol m-info)))
        ;; Some methods listed here are functions: (a) their flags is an
        ;; empty list; (b) they do not expect an additional instance
        ;; argument (their GIargInfo list is complete); (c) they have a
        ;; GIFuncInfo entry in the namespace (methods do not). We do not
        ;; (re)import those here.
        (unless (g-irepository-find-by-name namespace name)
          (gi-import-function m-info #:force? force?))))))

#!

;; As said above, we do not import signals, they are imported on
;; demand. Visit (g-golf hl-api signal) signal-connect and the
;; %gi-signal-cache related code to see how this is achieved.

(define (gi-import-object-signals info)
  (let ((n-signal (g-object-info-get-n-signals info)))
    #;(dimfi (g-object-info-get-type-name info)
           " " n-signal "signals")
    (do ((i 0
            (+ i 1)))
        ((= i n-signal))
      (let ((s-info (g-object-info-get-signal info i)))
        #;(dimfi "  " (g-base-info-get-namespace s-info)
               (g-base-info-get-name s-info)
               " (signal)")
        'wip))))

!#
