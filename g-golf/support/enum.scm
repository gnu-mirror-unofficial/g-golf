;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2018
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

;; I don't like the rnrs enums interface very much and besides it misses
;; an essential procedure for FFI users, which to get the symbol from an
;; integer, since all C functions return enum as int.

;; Then golf will, in the end, extensively use goops, so I decided to
;; start here.

;;; Code:


(define-module (g-golf support enum)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support goops)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support keyword)
  
  #:export (<enum>
            <gi-enum>))


(g-export enum-set
	  enum->value
	  enum->values
	  enum->symbol
	  enum->symbols
          gi-name
          scm-name)


(define-class <enum> ()
  (enum-set #:getter enum-set #:init-keyword #:enum-set))

(define-method (initialize (self <enum>) initargs)
  (receive (kw enum-kw)
      (split-keyword-args initargs '(#:enum-set))
    ;; (dimfi kw enum-kw)
    (let-keywords initargs #t
		  ((enum-set #f))
      (if enum-set
	  (if (pair? (car enum-set))
	      (next-method self initargs)
	      (let ((real-enum-set (map (lambda (i)
					   (cons (list-ref enum-set i) i))
				      (iota (length enum-set)))))
		(next-method self (append kw
					  (list #:enum-set real-enum-set)))))
	  (error "Initialize <enum>:"
		 " enum-set can not be empty."
		 (current-output-port))))))

(define-method (enum->value (self <enum>) (item <symbol>))
  (assq-ref (enum-set self) item))

(define-method (enum->values (self <enum>))
  (map (lambda (x)
	 (match x ((symbol . id) id)))
    (enum-set self)))

(define-method (enum->symbol (self <enum>) (item <integer>))
  (let ((entry (find (lambda (x)
		       (= (match x ((symbol . id) id))
			  item))
		     (enum-set self))))
    (and entry
	 (match entry ((symbol . id) symbol)))))

(define-method (enum->symbols (self <enum>))
  (map (lambda (x)
	 (match x ((symbol . id) symbol)))
    (enum-set self)))


;;;
;;; GI Enum
;;;

(define-class <gi-enum> (<enum>)
  (gi-name #:getter gi-name #:init-keyword #:gi-name)
  (scm-name #:getter scm-name #:init-keyword #:scm-name))
