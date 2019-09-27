;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2019
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


(define-module (g-golf)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs arithmetic bitwise)
  #:use-module (system foreign)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-60)
  #:use-module (g-golf support libg-golf)
  #:use-module (g-golf support float)
  #:use-module (g-golf support modules)
  #:use-module (g-golf support goops)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support push)
  #:use-module (g-golf support keyword)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support struct)
  #:use-module (g-golf support union)
  #:use-module (g-golf support bytevector)
  #:use-module (g-golf init)
  #:use-module (g-golf glib mem-alloc)
  #:use-module (g-golf glib main-event-loop)
  #:use-module (g-golf glib glist)
  #:use-module (g-golf glib gslist)
  #:use-module (g-golf gobject type-info)
  #:use-module (g-golf gobject gobject)
  #:use-module (g-golf gobject enum-flags)
  #:use-module (g-golf gobject generic-values)
  #:use-module (g-golf gobject params-vals)
  #:use-module (g-golf gobject param-spec)
  #:use-module (g-golf gobject closures)
  #:use-module (g-golf gobject signals)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi cache)
  #:use-module (g-golf gi repository)
  #:use-module (g-golf gi common-types)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi callable-info)
  #:use-module (g-golf gi signal-info)
  #:use-module (g-golf gi function-info)
  #:use-module (g-golf gi registered-type-info)
  #:use-module (g-golf gi enum-info)
  #:use-module (g-golf gi struct-info)
  #:use-module (g-golf gi object-info)
  #:use-module (g-golf gi arg-info)
  #:use-module (g-golf gi field-info)
  #:use-module (g-golf gi property-info)
  #:use-module (g-golf gi type-info)
  #:use-module (g-golf hl-api gtype)
  #:use-module (g-golf hl-api gobject)
  #:use-module (g-golf hl-api function)
  #:use-module (g-golf hl-api object)
  #:use-module (g-golf hl-api callback)
  #:use-module (g-golf hl-api import)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last))

(eval-when (expand load eval)
  (re-export-public-interface (oop goops)
                              (oop goops describe)
			      (ice-9 match)
			      (ice-9 binary-ports)
			      (rnrs bytevectors)
			      (rnrs arithmetic bitwise)
			      (system foreign)
			      (srfi srfi-1)
			      (srfi srfi-60)
                              (g-golf support libg-golf)
                              (g-golf support float)
                              (g-golf support modules)
			      (g-golf support goops)
			      (g-golf support g-export)
			      (g-golf support utils)
                              (g-golf support push)
			      (g-golf support keyword)
			      (g-golf support enum)
                              (g-golf support struct)
                              (g-golf support union)
                              (g-golf support bytevector)
			      (g-golf init)
			      (g-golf glib mem-alloc)
                              (g-golf glib main-event-loop)
                              (g-golf glib glist)
                              (g-golf glib gslist)
			      (g-golf gobject type-info)
                              (g-golf gobject gobject)
			      (g-golf gobject enum-flags)
			      (g-golf gobject generic-values)
                              (g-golf gobject params-vals)
			      (g-golf gobject param-spec)
			      (g-golf gobject closures)
                              (g-golf gobject signals)
			      (g-golf gi utils)
                              (g-golf gi cache)
			      (g-golf gi repository)
                              (g-golf gi common-types)
  			      (g-golf gi base-info)
			      (g-golf gi callable-info)
                              (g-golf gi signal-info)
                              (g-golf gi function-info)
			      (g-golf gi registered-type-info)
			      (g-golf gi enum-info)
                              (g-golf gi struct-info)
			      (g-golf gi object-info)
			      (g-golf gi arg-info)
                              (g-golf gi field-info)
			      (g-golf gi property-info)
			      (g-golf gi type-info)
			      (g-golf gi typelib)
                              (g-golf hl-api gtype)
                              (g-golf hl-api gobject)
                              (g-golf hl-api function)
                              (g-golf hl-api object)
                              (g-golf hl-api callback)
                              (g-golf hl-api import)))
