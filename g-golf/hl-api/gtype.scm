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
;; gobject gtype), see:

;;   https://www.gnu.org/software/guile-gnome

;;   http://git.savannah.gnu.org/cgit/guile-gnome.git
;;     tree/glib/gnome/gobject/gtype.scm

;;; Code:


(define-module (g-golf hl-api gtype)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)
  #:use-module (g-golf gi)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export (<gtype-class>
            <gtype-instance>

            g-type-class
            g-inst-cache-show))


(g-export !info
          !derived
          !namespace
          !g-type
          !g-name
          !g-class

          !g-inst
          unref
          g-inst-cache-remove!)


;;;
;;; 
;;;

;; The metaclass of all GType classes.

(define-class <gtype-class> (<class>)
  (info #:accessor !info
        #:init-keyword #:info)
  (derived #:accessor !derived
           #:init-keyword #:derived #:init-value #f)
  (namespace #:accessor !namespace)
  (g-type #:accessor !g-type)
  (g-name #:accessor !g-name)
  (g-class #:accessor !g-class))

(define-method (initialize (self <gtype-class>) initargs)
  (let ((info (or (get-keyword #:info initargs #f)
                  (error "Missing #:info initarg: " initargs))))
    (next-method)
    (unless (boolean? info) ;; it is #t for gtype instances
      (let* ((namespace (g-base-info-get-namespace info))
             (g-type (g-registered-type-info-get-g-type info))
             (g-f-type (g-type-fundamental g-type))
             (g-name (g-registered-type-info-get-type-name info))
             (g-class (g-type-class info #:g-type g-type)))
        (mslot-set! self
                    'namespace namespace
                    'g-type g-type
                    'g-name g-name
                    'g-class g-class)))))

(define* (g-type-class info #:key (g-type #f))
  (let* ((g-type (or g-type
                     (g-registered-type-info-get-g-type info)))
         (g-f-type (g-type-fundamental g-type))
         (g-class (case (g-type->symbol g-f-type)
                    ((object)
                     (g-type-class-ref g-type))
                    (else
                     #f))))
    (and g-class
         (g-type-class-unref g-class))
    g-class))

;; The root class of all instantiable GType classes.

(define-class <gtype-instance> ()
  (g-inst #:accessor !g-inst)
  #:info #t
  #:metaclass <gtype-class>)

(define-method (make (class <gtype-class>) . initargs)
  ;; If #:g-inst is passed, we first check if the g-inst is cached, in
  ;; which case we just return the goops instance associated with it.
  (let ((g-inst (get-keyword #:g-inst initargs #f)))
    (or (and g-inst
             (g-inst-cache-ref g-inst))
        (next-method))))

(define-method (initialize (self <gtype-instance>) initargs)
  (receive (split-kw split-rest)
      (split-keyword-args (map slot-definition-init-keyword
                            (class-g-property-slots (class-of self)))
                          initargs)
    (let ((g-inst (or (get-keyword #:g-inst initargs #f)
                      (g-inst-construct self split-kw))))
        (next-method self split-rest)
        (when (g-object-is-floating g-inst)
          (g-object-ref-sink g-inst))
        (set! (!g-inst self) g-inst)
        (g-inst-cache-set! g-inst self))))

(define %g_value_init
  (@@ (g-golf gobject generic-values) g_value_init))

(define-method (g-inst-construct (self <gtype-instance>)
                                 g-property-initargs)
  (if (null? g-property-initargs)
      (g-object-new (!g-type (class-of self)))
      (let* ((class (class-of self))
             (g-type (!g-type class))
             (slot-def-init-val-pairs
              (slot-definition-init-value-pairs self g-property-initargs))
             (n-prop (length slot-def-init-val-pairs))
             (%g-value-size (g-value-size))
             (g-values (bytevector->pointer
                        (make-bytevector (* n-prop %g-value-size) 0)))
             (names
              (let loop ((i 0)
                         (names '())
                         (g-value g-values)
                         (slot-def-init-val-pairs slot-def-init-val-pairs))
                (match slot-def-init-val-pairs
                  (()
                   (reverse! names))
                  ((slot-def-init-val-pair . rest)
                   (match slot-def-init-val-pair
                     ((slot-def . init-val)
                      (let* ((slot-opts (slot-definition-options slot-def))
                             (g-name (get-keyword #:g-name slot-opts #f))
                             (g-type (get-keyword #:g-type slot-opts #f)))
                        (%g_value_init g-value g-type)
                        (g-value-set! g-value
                                      (%g-inst-set-property-value g-type init-val))
                        (loop (+ i 1)
                              (cons g-name names)
                              (gi-pointer-inc g-value %g-value-size)
                              rest)))))))))
        (g-object-new-with-properties g-type
                                      n-prop
                                      (scm->gi names 'strings)
                                      g-values))))

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

(define-method (slot-definition-init-value-pairs (self <gtype-instance>)
                                                 initargs)
  (let ((class (class-of self)))
    (let loop ((initargs initargs)
               (results '()))
      (match initargs
        (()
         (reverse! results))
        ((kw val . rest)
         (loop rest
               (cons (cons (class-slot-definition class
                                                  (keyword->symbol kw))
                           val)
                     results)))))))

(define-method (unref (self <gtype-instance>))
  (let* ((g-inst (!g-inst self))
         (prev-ref-count (g-object-ref-count g-inst)))
    (g-object-unref g-inst)
    (when (= prev-ref-count 1)
      (g-inst-cache-remove! self)
      (set! (!g-inst self) #f))
    (values)))


;;;
;;; Instance cache methods 'completion'
;;;

(define-method (g-inst-cache-remove! (self <foreign>))
  (hashq-remove! %g-inst-cache
                 (pointer-address self)))

(define-method (g-inst-cache-remove! (self <gtype-instance>))
  (hashq-remove! %g-inst-cache
                 (pointer-address (!g-inst self))))

(define* (g-inst-cache-show #:optional
                            (port (current-output-port)))
  (let ((n-entry (hash-count (const #t) %g-inst-cache)))
    (case n-entry
      ((0)
       (display "The g-inst cache is empty\n" port))
      (else
       (letrec ((show (lambda (key value)
                        (format port "  ~S  -  ~S~%"
                                (!g-inst value)
                                value))))
         (format port "The g-inst cache has ~A entry(ies):~%"
                 n-entry)
         (g-inst-cache-for-each show))))))
