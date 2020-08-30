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


;; useless in guile-2.2, we have to declare #:duplicates in each module
;; we keep it in case things change in the future, who knows...
#;(eval-when (expand load eval)
  (use-modules (oop goops))
  (default-duplicate-binding-handler
    '(merge-generics replace warn-override-core warn last)))


(define-module (g-golf gobject)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (g-golf support module)
  #:use-module (g-golf support goops)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf init)
  #:use-module (g-golf gobject type-info)
  #:use-module (g-golf gobject gobject)
  #:use-module (g-golf gobject enum-flags)
  #:use-module (g-golf gobject generic-values)
  #:use-module (g-golf gobject params-vals)
  #:use-module (g-golf gobject param-spec)
  #:use-module (g-golf gobject closures)
  #:use-module (g-golf gobject signals)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last))

(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
			      (system foreign)
			      (rnrs bytevectors)
			      (rnrs arithmetic bitwise)
			      (g-golf support goops)
			      (g-golf support g-export)
			      (g-golf support utils)
			      (g-golf support enum)
			      (g-golf init)
			      (g-golf gobject type-info)
			      (g-golf gobject gobject)
			      (g-golf gobject enum-flags)
			      (g-golf gobject generic-values)
                              (g-golf gobject params-vals)
			      (g-golf gobject param-spec)
                              (g-golf gobject closures)
                              (g-golf gobject signals)))
