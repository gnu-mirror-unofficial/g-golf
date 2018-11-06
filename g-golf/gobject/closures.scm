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

;;; Code:


(define-module (g-golf gobject closures)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-golf-go-source-set-closure))


;;;
;;; GObject Low level API
;;;

(define (g-golf-go-source-set-closure source closure)
  (g-source-set-closure source closure))


;;;
;;; GObject Bindings
;;;

(define g-source-set-closure
  (pointer->procedure float
                      (dynamic-func "g_source_set_closure"
				    %libgobject)
                      (list '*		;; source
                            '*)))	;; closure