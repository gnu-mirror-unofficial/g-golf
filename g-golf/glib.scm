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


(define-module (g-golf glib)
  #:use-module (oop goops)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (system foreign)
  #:use-module (g-golf support module)
  #:use-module (g-golf support goops)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support enum)
  #:use-module (g-golf init)
  #:use-module (g-golf glib mem-alloc)
  #:use-module (g-golf glib main-event-loop)
  #:use-module (g-golf glib io-channels)
  #:use-module (g-golf glib misc-util-funcs)
  #:use-module (g-golf glib unix-utils)
  #:use-module (g-golf glib glist)
  #:use-module (g-golf glib gslist)
  #:use-module (g-golf glib quarks)


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
			      (g-golf support goops)
			      (g-golf support g-export)
			      (g-golf support utils)
			      (g-golf support enum)
			      (g-golf init)
			      (g-golf glib mem-alloc)
                              (g-golf glib main-event-loop)
                              (g-golf glib io-channels)
                              (g-golf glib misc-util-funcs)
                              (g-golf glib unix-utils)
                              (g-golf glib glist)
                              (g-golf glib gslist)
                              (g-golf glib quarks)))
