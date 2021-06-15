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


(define-module (g-golf override gdk)
  #:export (gdk-clipboard-set-value-ov))


(define (gdk-clipboard-set-value-ov proc)
  (values
   #f
   `(lambda (clipboard value)
      (let* ((i-func ,proc)
             (g-value-set-value
              ,(@@ (g-golf hl-api gobject) %g-inst-set-property-value))
             (g-type (scm->g-type value))
             (g-value (g-value-init g-type)))
        (g-value-set! g-value
                      (g-value-set-value g-type value))
        (i-func clipboard g-value)
        (g-value-unset g-value)
        (values)))
   '(0 1)))
