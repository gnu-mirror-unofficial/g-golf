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


(define-module (g-golf hl-api utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf gi)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export (gi-find-by-property-name))


#;(g-export )


;;;
;;;
;;;

(define (gi-find-by-property-name namespace name)
  (g-irepository-require namespace)
  (let loop ((n-info (g-irepository-get-n-infos namespace))
             (i 0)
             (results '()))
    (if (= i n-info)
        (reverse! results)
        (let ((info (g-irepository-get-info namespace i)))
          (case (g-base-info-get-type info)
            ((object)
             (if (member name
                         (gi-object-property-names info)
                         string=?)
                 (loop n-info
                       (+ i 1)
                       (cons info results))
                 (loop n-info
                       (+ i 1)
                       results)))
            (else
             (loop n-info
                   (+ i 1)
                   results)))))))
