;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2020
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


(define-module (g-golf init)
  #:use-module (system foreign)

  #:export (%libgirepository
	    %libglib
            ;; %libgdk
	    %libgobject
            %libg-golf
            %use-par-map))


(define %libgirepository (dynamic-link "libgirepository-1.0"))
(define %libglib (dynamic-link "libglib-2.0"))
;; (define %libgdk (dynamic-link "libgdk-3"))
(define %libgobject (dynamic-link "libgobject-2.0"))

(define %libg-golf (dynamic-link "libg-golf"))

(define %use-par-map (make-parameter #t))
