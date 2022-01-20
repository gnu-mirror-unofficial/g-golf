;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2020 - 2022
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


(define-module (g-golf gi)
  #:use-module (oop goops)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi cache-gi)
  #:use-module (g-golf gi cache-others)
  #:use-module (g-golf gi repository)
  #:use-module (g-golf gi common-types)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi callable-info)
  #:use-module (g-golf gi signal-info)
  #:use-module (g-golf gi function-info)
  #:use-module (g-golf gi registered-type-info)
  #:use-module (g-golf gi enum-info)
  #:use-module (g-golf gi struct-info)
  #:use-module (g-golf gi union-info)
  #:use-module (g-golf gi object-info)
  #:use-module (g-golf gi interface-info)
  #:use-module (g-golf gi arg-info)
  #:use-module (g-golf gi constant-info)
  #:use-module (g-golf gi field-info)
  #:use-module (g-golf gi property-info)
  #:use-module (g-golf gi type-info)
  #:use-module (g-golf gi typelib)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last))


(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
			      (ice-9 binary-ports)
			      (rnrs bytevectors)
			      (system foreign)
                              (g-golf init)
                              (g-golf support)
                              (g-golf gi utils)
                              (g-golf gi cache-gi)
                              (g-golf gi cache-others)
                              (g-golf gi repository)
                              (g-golf gi common-types)
                              (g-golf gi base-info)
                              (g-golf gi callable-info)
                              (g-golf gi signal-info)
                              (g-golf gi function-info)
                              (g-golf gi registered-type-info)
                              (g-golf gi enum-info)
                              (g-golf gi struct-info)
                              (g-golf gi union-info)
                              (g-golf gi object-info)
                              (g-golf gi interface-info)
                              (g-golf gi arg-info)
                              (g-golf gi constant-info)
                              (g-golf gi field-info)
                              (g-golf gi property-info)
                              (g-golf gi type-info)
                              (g-golf gi typelib)))
