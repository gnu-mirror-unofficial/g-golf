;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2020
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


(define-module (g-golf gdk key-values)
  ;; #:use-module (ice-9 match)
  ;; #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  ;; #:use-module (g-golf support libg-golf)
  ;; #:use-module (g-golf support utils)
  ;; #:use-module (g-golf support enum)
  ;; #:use-module (g-golf support struct)
  ;; #:use-module (g-golf support union)
  #:use-module (g-golf gi utils)
  ;; #:use-module (g-golf gi cache)
  
  ;; #:duplicates (merge-generics
  ;;	replace
  ;;	warn-override-core
  ;;	warn
  ;;	last)

  #:export (gdk-keyval-name))


;;;
;;; Low level API
;;;

(define (gdk-keyval-name keyval)
  (let ((name (gi->scm (gdk_keyval_name keyval)
                       'string)))
    (and name
         (string->symbol name))))


;;;
;;; Gdk Bindings
;;;

(define gdk_keyval_name
  (pointer->procedure '*
                      (dynamic-func "gdk_keyval_name"
				    %libgdk)
                      (list unsigned-int)))	;; keyval


;;;
;;; Types and Values
;;;
