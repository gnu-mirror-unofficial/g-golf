;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2018 - 2021
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
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)
  #:use-module (g-golf gi)
  #:use-module (g-golf hl-api gtype)

  #:replace (connect)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<gobject>
            gobject-class?
            <ginterface>))


#;(g-export )


;;;
;;; Connect
;;;

;; This code was in (g-golf hl-api signal), but (a) all other guile core
;; or user procedure 'promotion' to gf are done using (g-golf hl-api
;; gobject), and (b) a Gio based example, iif using the 'open signal,
;; revealed that unless connect is promoted as a gf in this module
;; (which is defined and imported before signal, and of course before
;; any user import(s)), then an 'overrides core binding' warning is
;; displayed, followed by a "module-lookup"connect exception.

(define %connect
  (module-ref the-root-module 'connect))

(define-method (connect . args)
  "The core Guile implementation of the connect(2) POSIX call"
  (apply %connect args))


;;;
;;; <gobject-class>
;;;


(define-class <gobject-class> (<gtype-class>))

(define (has-slot? slots name)
  (let loop ((slots slots))
    (match slots
      (#f #f)
      (() #f)
      ((slot . rest)
       (or (eq? (slot-definition-name slot) name)
           (loop rest))))))

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
  (if (null? g-properties)
      '()
      (let* ((module (resolve-module '(g-golf hl-api gobject)))
             (info (!info class))
             (c-name (class-name class))
             ;; the g-class (class) slot is set by the initialize method
             ;; of <gtype-class>, the meta-class - but when the
             ;; compute-slots method is called, which itself calls this
             ;; procedure, that step hasn't been realized yet, hence the
             ;; following necessary let variable binding.
             (g-class (and info ;; info is #f for derived class(es)
                           (g-type-class info)))
             (extra-slots (filter-map
                              (lambda (g-property)
                                (let* ((g-name (g-base-info-get-name g-property))
                                       (name (g-name->name g-name)))
                                  (if (has-slot? slots name)
                                      #f
                                      (let* ((k-name (symbol->keyword name))
                                             (a-name (symbol-append '! name))
                                             (a-inst (if (module-variable module a-name)
                                                         (module-ref module a-name)
                                                         (let ((a-inst (make-accessor a-name)))
                                                           (module-g-export! module `(,a-name))
                                                           (module-set! module a-name a-inst)
                                                           a-inst)))
                                             (g-param-spec
                                              (and g-class
                                                   (g-object-class-find-property g-class g-name)))
                                             (g-type (if g-param-spec
                                                         (g-param-spec-type g-param-spec)
                                                         (gi-property-g-type g-property)))
                                             (g-flags (if g-param-spec
                                                          (g-param-spec-get-flags g-param-spec)
                                                          (g-property-info-get-flags g-property)))
                                             (slot (make <slot>
                                                     #:name name
                                                     #:g-property g-property
                                                     #:g-name g-name
                                                     #:g-param-spec g-param-spec
                                                     #:g-type g-type
                                                     #:g-flags g-flags
                                                     #:allocation #:g-property
                                                     #:accessor a-inst
                                                     #:init-keyword k-name)))
                                        slot))))
                              g-properties)))
        extra-slots)))

(define (n-prop-prop-accessors class)
  ;; Note that at this point, the g-type slot of the class is still
  ;; unbound.
  (let* ((info (!info class))
         (g-type (g-registered-type-info-get-g-type info)))
    (case (g-type->symbol g-type)
      ((object)
       (values g-object-info-get-n-properties
               g-object-info-get-property))
      ((interface)
       (values g-interface-info-get-n-properties
               g-interface-info-get-property))
      (else
       (error "Not a GObject nor an Ginterface class:" class)))))

(define (gobject-ginterface-direct-properties class)
  (if (or (boolean? (!info class))
          (!derived class))
      '()
      (receive (get-n-properties get-property)
          (n-prop-prop-accessors class)
        (let* ((info (!info class))
               (n-prop (get-n-properties info)))
          (let loop ((i 0)
                     (result '()))
            (if (= i n-prop)
                (reverse! result)
                (loop (+ i 1)
                      (cons (get-property info i)
                            result))))))))

(define-method (compute-slots (class <gobject-class>))
  (let* ((slots (next-method))
         (extra (compute-extra-slots class
                                     (gobject-ginterface-direct-properties class)
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
  (case (slot-definition-allocation slot-def)
    ((#:g-property)
     (let* ((name (slot-definition-name slot-def))
            (slot-opts (slot-definition-options slot-def))
            (g-name (get-keyword #:g-name slot-opts #f))
            (g-type (get-keyword #:g-type slot-opts #f)))
       (list (lambda (obj)
               #;(if (is-readable? slot-def slot-opts)
                   (g-inst-get-property (!g-inst obj) g-name g-type)
                   (error "Unreadable slot:" name))
               (g-inst-get-property (!g-inst obj) g-name g-type))
             (lambda (obj val)
               #;(if (is-writable? slot-def slot-opts)
                   (g-inst-set-property (!g-inst obj) g-name g-type val)
                   (error "Unwritable slot:" name))
               (g-inst-set-property (!g-inst obj) g-name g-type val)))))
    (else
     (next-method))))

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

(define (g-inst-get-property inst g-name g-type)
  (let* ((g-value (g-value-init g-type))
         (dummy (g-object-get-property inst g-name g-value))
         (result (%g-inst-get-property-value g-value)))
    (g-value-unset g-value)
    result))

(define (%g-inst-get-property-value g-value)
  (let ((value (g-value-ref g-value)))
    (case (g-value-type-tag g-value)
      ((object)
       (if (or (not value)
               (null-pointer? value))
           #f
           (or (g-inst-cache-ref value)
               (let* ((module (resolve-module '(g-golf hl-api gobject)))
                      (r-type (g-value-type g-value))
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
               (let* ((module (resolve-module '(g-golf hl-api gobject)))
                      (r-type (g-value-type g-value))
                      (info (g-irepository-find-by-gtype r-type))
                      (g-name (g-registered-type-info-get-type-name info))
                      (c-name (g-name->class-name g-name))
                      (type (module-ref module c-name)))
                 (make type #:g-inst value)))))
      (else
       value))))

(define* (g-inst-set-property inst g-name g-type value)
  (let ((g-value (g-value-init g-type)))
    (g-value-set! g-value
                  (%g-inst-set-property-value g-type value))
    (g-object-set-property inst g-name g-value)
    (g-value-unset g-value)
    (values)))

(define %g-inst-set-property-value
  (@@ (g-golf hl-api gtype) %g-inst-set-property-value))


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
  ;; For the record, Guile-Gnome also defines the same procedure, but in
  ;; Guile-Gnome, it is guaranteed to always receive a class as its
  ;; argument (called c).  Here in G-Golf, gobject-class? is/must be
  ;; called in context that val is a type tag, not a class.
  (and (is-a? val <class>)
       (memq <gobject>
             (class-precedence-list val))
       #t))


;;;
;;; <ginterface>
;;;

#;(define-class <ginterface-class> (<gtype-class>))

(define-class <ginterface> (<gtype-instance>)
  #:info #t
  #:metaclass <gobject-class>)
