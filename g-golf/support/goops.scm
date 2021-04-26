;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016, 2021
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

;; this file is a copy of (grip goops)
;; http://www.nongnu.org/grip/

;; define-method* has been written by Mark H Weaver, I just changed
;; let-values -> receive, which I find (a lot) more readable. I can't
;; point to 'an original definition location' though, because Mark
;; pasted his definition while chating on #guile, a few years ago.  I
;; don't think Mark ever published this code anywhere, but I could be
;; wrong, if that is the case, please let me know, I'd be happy to add
;; the link here.

;;; Code:


(define-module (g-golf support goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support module)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (define-method*
            mslot-set!
            generic?
            texinfo-class-definition))


(g-export class-direct-virtual-slots
	  class-virtual-slots
          class-direct-g-property-slots
          class-g-property-slots
	  #;describe)


(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
                              (oop goops describe)))


(define-syntax define-method*
  (lambda (x)
    (syntax-case x ()
      ((_ (generic arg-spec ... . tail) body ...)
       (receive (required-arg-specs other-arg-specs)
           (break (compose keyword? syntax->datum)
                  #'(arg-spec ...))
         #`(define-method (generic #,@required-arg-specs . rest)
             (apply (lambda* (#,@other-arg-specs . tail)
                      body ...)
                    rest)))))))

(define-method (class-direct-virtual-slots (c <class>))
  (filter-map (lambda (slot-definition)
		(and (eq? (slot-definition-allocation slot-definition)
			  #:virtual)
		     slot-definition))
      (class-direct-slots c)))

(define-method (class-virtual-slots (c <class>))
  (filter-map (lambda (slot-definition)
		(and (eq? (slot-definition-allocation slot-definition)
			  #:virtual)
		     slot-definition))
      (class-slots c)))

(define-method (class-direct-g-property-slots (self <class>))
  (filter-map (lambda (slot-definition)
                (and (eq? (slot-definition-allocation slot-definition)
                          #:g-property)
                     slot-definition))
      (class-direct-slots self)))

(define-method (class-g-property-slots (self <class>))
  (filter-map (lambda (slot-definition)
                (and (eq? (slot-definition-allocation slot-definition)
                          #:g-property)
                     slot-definition))
      (class-slots self)))

#;(define-method* (describe (self <object>) #:key (port #t))
  (format port "~S - instance of ~A~%"
	  self
	  (class-name (class-of self)))
  (format port "  slots and values are:~%")
  (for-each (lambda (slot)
	      (let ((name (slot-definition-name slot)))
		(format port "    ~S = ~A~%"
			name
			(if (slot-bound? self name) 
			    (format #f "~S" (slot-ref self name))
			    "#<unbound>"))))
	    (class-slots (class-of self)))
  *unspecified*)

(define (mslot-set! self . args)
  (if (even? (length args))
      (let loop ((args args))
        (match args
          (()
           (values))
          ((name val . rest)
           (slot-set! self name val)
           (loop rest))))
      (error "Wrong number of arguments: " args)))

(define (generic? value)
  (is-a? value <generic>))


;;;
;;; Doc support - Draft, wip
;;;

(define* (texinfo-class-definition class #:key (port #t) (anchor? #t))
  (let ((name (class-name class)))
    (when anchor?
      (format port "@anchor{~A}~%" name))
    (format port "@deftp Class ~A~%" name)
    (texinfo-class-precedence-list class #:port port)
    (texinfo-class-slots class #:port port)
    (format port "@end deftp~%")
    (values)))

(define %texinfo-cpl-start
   "\nClass Precedence List:

@indentedblock
@table @code\n")

(define %texinfo-cpl-end
   "@end table
@end indentedblock\n")

(define* (texinfo-class-precedence-list class #:key (port #t))
  (format port "~A" %texinfo-cpl-start)
  (for-each (lambda (cpl-item)
              (format port "@item ~A~%" (class-name cpl-item)))
      (class-precedence-list class))
  (format port "~A" %texinfo-cpl-end))

(define %texinfo-class-direct-slots-start
   "\nDirect slots are:

@indentedblock
@table @code\n")

(define %texinfo-class-direct-virtual-slots-start
   "\nDirect virtual slots are:

@indentedblock
@table @code\n")

(define %texinfo-class-slots-end
   "@end table
@end indentedblock\n")

(define* (texinfo-class-slots class #:key (port #t))
  (let ((name (class-name class))
        (slots (class-direct-slots class))
        (virtual-slots (class-direct-virtual-slots class)))
    (format port "~A" %texinfo-class-direct-slots-start)
    (for-each texinfo-slot-definition
        #;(lset-difference eq? slots virtual-slots)
        slots)
    (format port "~A" %texinfo-class-slots-end)
    #;(format port "~A" %texinfo-class-direct-virtual-slots-start)
    #;(for-each texinfo-slot-definition
        virtual-slots)
    #;(format port "~A" %texinfo-class-slots-end)))

(define* (texinfo-slot-definition slot #:key (port #t))
  (let ((slot-options (slot-definition-options slot)))
    (let loop ((options slot-options))
      (match options
        (() 'nothing)
        ((kw val . rests)
         (case kw
           ((#:name)
            (format port "~%@item ~A @*~%" val))
           ((#:accessor)
            (format port "~A ~A @*~%" kw
                    (generic-function-name val)))
           #;((#:allocation)
            'nothing)
           ((#:slot-ref #:slot-set!)
            'nothing)
           (else
            (format port "~A ~A @*~%" kw val)))
         (loop rests))))))
