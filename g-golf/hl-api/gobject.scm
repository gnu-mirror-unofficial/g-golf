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

;; This code is largely inspired by the Guile-Gnome module (gnome
;; gobject gobject), see:

;;   https://www.gnu.org/software/guile-gnome

;;   http://git.savannah.gnu.org/cgit/guile-gnome.git
;;     tree/glib/gnome/gobject/gobject.scm

;;; Code:


(define-module (g-golf hl-api gobject)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)
  #:use-module (g-golf gi)
  #:use-module (g-golf hl-api gtype)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<gobject>
            <gobject-class>
            gobject-class?))


#;(g-export )


;;;
;;; <gobject-class>
;;;


(define-class <gobject-class> (<gtype-class>))

(define (has-slot? name slots)
  (and (pair? slots)
       (or (eq? name (slot-definition-name (car slots)))
           (has-slot? name (cdr slots)))))

(define (has-valid-property-flag? g-flags)
  (let ((valid-flags '(readable writable readwrite))
        (invalid-flags '(deprecated)))
    (let loop ((g-flags g-flags))
      (if (null? g-flags)
          #f)
      (match g-flags
        ((flag . rest)
         (if (and (memq flag valid-flags)
                  (not (memq flag invalid-flags)))
             #t
             (loop rest)))))))

(define (compute-extra-slots class g-properties slots)
  (let* ((c-name (class-name class))
         (extra-slots (filter-map
                          (lambda (g-property)
                            (let* ((module (resolve-module '(g-golf hl-api gobject)))
                                   (g-name (g-base-info-get-name g-property))
                                   (g-flags (g-property-info-get-flags g-property))
	                           (g-type (gi-property-g-type g-property))
                                   (scm-name (g-name->name g-name 'as-string))
                                   (s-name (string->symbol scm-name))
                                   (k-name (with-input-from-string
                                               (string-append "#:" scm-name)
                                             read)))
                              (and #;(has-valid-property-flag? g-flags)
                               (not (has-slot? scm-name slots))
                               g-type
                               (let* ((a-name (string->symbol
                                               (string-append "!" scm-name)))
                                      (a-inst (if (module-variable module a-name)
                                                  (module-ref module a-name)
                                                  (let ((a-inst (make-accessor a-name)))
                                                    (module-g-export! module `(,a-name))
                                                    (module-set! module a-name a-inst)
                                                    a-inst)))
                                      (slot (make <slot>
                                              #:name s-name
                                              #:g-property g-property
                                              #:g-type g-type
                                              #:g-flags g-flags
                                              #:allocation #:g-property
                                              #:accessor a-inst
                                              #:init-keyword k-name)))
                                 slot))))
                          g-properties)))
    extra-slots))

(define (gobject-class-properties class)
  (if (or (boolean? (!info class))
          (!derived class))
      '()
      (let* ((info (!info class))
             (n-prop (g-object-info-get-n-properties info)))
        (let loop ((i 0)
                   (result '()))
          (if (= i n-prop)
              (reverse! result)
              (loop (+ i 1)
                    (cons (g-object-info-get-property info i)
                          result)))))))

(define-method (compute-slots (class <gobject-class>))
  (let* ((slots (next-method))
         (extra (compute-extra-slots class
                                     (gobject-class-properties class)
                                     slots)))
    (slot-set! class 'direct-slots
               (append (slot-ref class 'direct-slots)
                       extra))
    (append slots extra)))

(define* (is-readable? slot #:optional (slot-opts #f))
  (let* ((slot-opts (or slot-opts
                        (slot-definition-options slot)))
         (g-flags (get-keyword #:g-flags slot-opts #f)))
    (and g-flags
         (memq 'readable g-flags))))

(define* (is-writable? slot #:optional (slot-opts #f))
  (let* ((slot-opts (or slot-opts
                        (slot-definition-options slot)))
         (g-flags (get-keyword #:g-flags slot-opts #f)))
    (and g-flags
         (memq 'writable g-flags)
         (not (memq 'construct-only g-flags)))))

(define-method (compute-get-n-set (class <gobject-class>) slot-def)
  (let ((name (slot-definition-name slot-def)))
    (case (slot-definition-allocation slot-def)
      ((#:g-property)
       (list (lambda (obj)
               (let* ((slot-opts (slot-definition-options slot-def))
                      (g-property (get-keyword #:g-property slot-opts #f))
                      (g-type (get-keyword #:g-type slot-opts #f)))
                 (if (is-readable? slot-def slot-opts)
                     (g-inst-get-property (!g-inst obj) g-property g-type)
                     (error "Unreadable slot:" name))))
             (lambda (obj val)
               (let* ((slot-opts (slot-definition-options slot-def))
                      (g-property (get-keyword #:g-property slot-opts #f))
                      (g-type (get-keyword #:g-type slot-opts #f)))
                 (if (is-writable? slot-def slot-opts)
                     (g-inst-set-property (!g-inst obj) g-property val g-type)
                     (error "Unwritable slot:" name))))))
      (else
       (next-method)))))

(define-method (initialize (class <gobject-class>) initargs)
  (let ((info (get-keyword #:info initargs #f)))
    (next-method
     class
     (if info
         (cons* #:info info initargs)
         (cons* #:derived #t
                #:info (!info (find gobject-class?
                                    (apply append
                                           (map class-precedence-list
                                             (get-keyword #:dsupers initargs '())))))
                initargs))))
  #;(install-properties!)
  #;(install-signals! class))

(define* (g-inst-get-property object property #:optional (g-type #f))
  (let* ((p-name (g-base-info-get-name property))
         (g-type (or g-type
                     (gi-property-g-type property)))
	 (g-value (g-value-init g-type)))
    (g-object-get-property object p-name g-value)
    (%g-inst-get-property-value g-value)))

(define (%g-inst-get-property-value g-value)
  (let ((value (g-value-ref g-value)))
    (case (g-value->g-type g-value)
      ((object)
       (if (or (not value)
               (null-pointer? value))
           #f
           (or (g-inst-cache-ref value)
               (let* ((module (resolve-module '(g-golf hl-api object)))
                      (r-type (g-value->g-type-id g-value))
                      (info (g-irepository-find-by-gtype r-type))
                      (g-name (g-registered-type-info-get-type-name info))
                      (c-name (g-name->class-name g-name))
                      (type (module-ref module c-name)))
                 (make type #:g-inst value)))))
      ((interface)
       (if (or (not value)
               (null-pointer? value))
           #f
           (or (g-inst-cache-ref value)
               (let* ((module (resolve-module '(g-golf hl-api object)))
                      (r-type (g-value->g-type-id g-value))
                      (info (g-irepository-find-by-gtype r-type))
                      (g-name (g-registered-type-info-get-type-name info))
                      (c-name (g-name->class-name g-name))
                      (type (module-ref module c-name)))
                 (make type #:g-inst value)))))
      (else
       value))))

(define* (g-inst-set-property object property value #:optional (g-type #f))
  (let* ((p-name (g-base-info-get-name property))
         (g-type (or g-type
                     (gi-property-g-type property)))
	 (g-value (g-value-init g-type)))
    (g-value-set! g-value
                  (%g-inst-set-property-value g-type value))
    (g-object-set-property object p-name g-value)
    (g-value-unset g-value)
    (values)))

(define (%g-inst-set-property-value g-type value)
  (let ((g-type (if (symbol? g-type)
                    g-type
                    (g-type->symbol g-type))))
    (case g-type
      ((object)
       (and value
            (!g-inst value)))
      ((interface)
       (and value
            (if (pointer? value)
                value
                ;; It is (should be) an instance
                (!g-inst value))))
      (else
       value))))


;;;
;;; Signals
;;;

(define (install-signals! class)
  (let ((signals (gobject-class-signals class)))
    (dimfi class)
    (for-each (lambda (info)
                (dimfi "  " (g-base-info-get-name info)))
        signals)))

(define (gobject-class-signals class)
  (if (boolean? (!info class))
      '()
      (let* ((info (!info class))
             (n-signal (g-object-info-get-n-signals info)))
        (let loop ((i 0)
                   (result '()))
          (if (= i n-signal)
              (reverse! result)
              (loop (+ i 1)
                    (cons (g-object-info-get-signal info i)
                          result)))))))


;;;
;;; <gobject>
;;;

(define-class <gobject> (<gtype-instance>)
  #:info #t
  #:metaclass <gobject-class>)

(define safe-class-name
  (@@ (oop goops describe) safe-class-name))

(define-method (describe (x <gobject>))
  (format #t "~S is an instance of class ~A~%"
	  x
          (safe-class-name (class-of x)))
  (format #t "Slots are: ~%")
  (for-each (lambda (slot)
	      (let ((name (slot-definition-name slot))
                    (slot-opts (slot-definition-options slot)))
                (case (slot-definition-allocation slot)
                  ((#:g-property)
                   (if (is-readable? slot slot-opts)
                       (format #t "     ~S = ~A~%"
			       name
			       (if (slot-bound? x name)
			           (format #f "~S" (slot-ref x name))
			           "#<unbound>"))
                       (format #t "     ~S = n/a [the slot is ~S only]~%"
			       name
                               (get-keyword #:g-flags slot-opts #f))))
                  (else
		   (format #t "     ~S = ~A~%"
			   name
			   (if (slot-bound? x name)
			       (format #f "~S" (slot-ref x name))
			       "#<unbound>"))))))
	    (class-slots (class-of x)))
  *unspecified*)

(define (gobject-class? val)
  (memq val
        (class-subclasses <gobject>)))
