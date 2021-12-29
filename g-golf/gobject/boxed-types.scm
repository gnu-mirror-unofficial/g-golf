;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2021
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


(define-module (g-golf gobject boxed-types)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support libg-golf)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-boxed-free))


;;;
;;; GObject Low level API
;;;

(define (g-boxed-free g-type foreign)
  (g_boxed_free g-type foreign))


;;;
;;; GObject Bindings
;;;

(define g_boxed_free
  (pointer->procedure void
                      (dynamic-func "g_boxed_free"
				    %libgobject)
                      (list unsigned-long	;; g-type
                            '*)))		;; g-pointer
