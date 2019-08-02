;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2018 - 2019
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


(define-module (g-golf hl-api)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support modules)
  #:use-module (g-golf support goops)
  #:use-module (g-golf hl-api gtype)
  #:use-module (g-golf hl-api gobject)
  #:use-module (g-golf hl-api function)
  #:use-module (g-golf hl-api object)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last))


(re-export-public-interface (oop goops)
                            (oop goops describe)
                            (g-golf support utils)
                            (g-golf support goops)
                            (g-golf hl-api gtype)
                            (g-golf hl-api gobject)
                            (g-golf hl-api function)
                            (g-golf hl-api object))
