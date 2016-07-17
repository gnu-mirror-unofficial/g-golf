;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016
;;;; Free Software Foundation, Inc.

;;;; This file is part of GNU Gbank

;;;; GNU Gbank is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published
;;;; by the Free Software Foundation, either version 3 of the License,
;;;; or (at your option) any later version.

;;;; GNU Gbank is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with GNU Gbank.  If not, see <http://www.gnu.org/licenses/>.
;;;;

;;; Commentary:

;; I don't like the rnrs enums interface very much and besides it misses
;; an essential procedure for FFI users, which to get the symbol from an
;; integer, since all C functions return enum as int.

;; Then gbank will, in the end, extensively use goops, so I decided to
;; start here.

;;; Code:


(define-module (gbank support enum)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 optargs)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (gbank support goops)
  #:use-module (gbank support g-export)
  #:use-module (gbank support utils)
  #:use-module (gbank support keyword)
  
  #:export (<enum>))


(g-export !value-set
	  e-value
	  e-sym)


(define-class <enum> ()
  (value-set #:accessor !value-set #:init-keyword #:value-set))

(define-method (initialize (self <enum>) initargs)
  (receive (kw enum-kw)
      (split-keyword-args initargs '(#:value-set))
    ;; (dimfi kw enum-kw)
    (let-keywords initargs #t
		  ((value-set #f))
      (if value-set
	  (if (pair? (car value-set))
	      (next-method self initargs)
	      (let ((real-value-set (map (lambda (i)
					   (cons (list-ref value-set i) i))
				      (iota (length value-set)))))
		(next-method self (append kw
					  (list #:value-set real-value-set)))))
	  (begin
	    (warning "initialize <enum>"
		     "<enum> instance value-set should not be empty."
		     (current-output-port))
	    (next-method self initargs))))))

(define-method (e-value (self <enum>) (item <symbol>))
  (assq-ref (!value-set self) item))

(define-method (e-sym (self <enum>) (item <integer>))
  (let ((entry (find (lambda (x)
		       (= (cdr x) item))
		     (!value-set self))))
    (and entry
	 (car entry))))
