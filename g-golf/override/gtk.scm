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


(define-module (g-golf override gtk)
  #:export (gtk-list-store-set-value-ov
            gtk-tree-model-get-value-ov))


(define (gtk-list-store-set-value-ov proc)
  (values
   '(("Gtk" "TreeModel"))
   `(lambda (store iter column value)
      (let* ((i-func ,proc)
             (g-value-set-value
              ,(@@ (g-golf hl-api gobject) %g-inst-set-property-value))
             (g-type (gtk-tree-model-get-column-type store column))
             (g-value (g-value-init (symbol->g-type g-type))))
        (g-value-set! g-value
                      (g-value-set-value g-type value))
        (i-func store iter column g-value)
        (g-value-unset g-value)
        (values)))))

(define (gtk-tree-model-get-value-ov proc)
  (values
   #f
   `(lambda (model iter column)
      (let* ((i-func ,proc)
             (g-value-get-value
              ,(@@ (g-golf hl-api gobject) %g-inst-get-property-value))
             (g-value (g-value-new))
             (dum (i-func model iter column g-value))
             (value (g-value-get-value g-value)))
        (g-value-unset g-value)
        value))))
