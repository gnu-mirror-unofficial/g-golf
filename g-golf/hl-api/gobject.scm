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

;; This code is largely inspired by the Guile-Gnome module (gnome
;; gobject gobject), see:

;;   https://www.gnu.org/software/guile-gnome

;;   http://git.savannah.gnu.org/cgit/guile-gnome.git
;;     tree/glib/gnome/gobject/gobject.scm

;;; Code:


(define-module (g-golf hl-api gobject)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support goops)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi object-info)
  #:use-module (g-golf gobject gobject)
  #:use-module (g-golf hl-api gtype)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export (<gobject-class>
            <gobject>
            gobject-class?
            ))


#;(g-export )


;;;
;;; 
;;;


(define-class <gobject-class> (<gtype-class>))

(define (has-slot? name slots)
  (and (pair? slots)
       (or (eq? name (slot-definition-name (car slots)))
           (has-slot? name (cdr slots)))))

(define (compute-extra-slots props slots)
  (filter-map (lambda (prop)
                (and (not (has-slot? prop slots))
                     (make <slot>
                       #:name (string->symbol (g-base-info-get-name prop))
                       #:property prop
                       #:allocation #:gproperty)))
      props))

(define (gobject-class-get-properties class)
  (if (boolean? (!info class))
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
         (extra (compute-extra-slots
                 (gobject-class-get-properties class) slots)))
    (slot-set! class 'direct-slots
               (append (slot-ref class 'direct-slots)
                       extra))
    (append slots extra)))

(define-method (compute-get-n-set (class <gobject-class>) slot-def)
  (let ((name (slot-definition-name slot-def)))
    (case (slot-definition-allocation slot-def)
      ((#:gproperty)
       (list (lambda (obj)
               (let ((property (get-keyword #:property
                                            (slot-definition-options slot-def)
                                            #f)))
                 (g-object-get-property (!ginst obj) property)))
             (lambda (obj val)
               (dimfi obj name val)
               (g-object-set-property obj name val))))
      (else
       (next-method)))))

(define (gobject-class? class)
  (memq <gobject> (class-precedence-list class)))

(define-method (initialize (class <gobject-class>) initargs)
  (next-method)
  #;(install-properties!)
  #;(install-signals!))


(define-class <gobject> (<gtype-instance>)
  #:info #t ;; Need to find the pointer to GI
  #:metaclass <gobject-class>)

