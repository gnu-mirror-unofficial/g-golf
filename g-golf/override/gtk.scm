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
  #:export (gtk-container-child-get-property-ov
            gtk-container-child-set-property-ov
            gtk-list-store-set-value-ov
            gtk-tree-store-set-value-ov
            gtk-tree-model-get-value-ov))


(define (gtk-container-child-get-property-ov proc)
  (values
   #f
   `(lambda (container child name)
      (let* ((i-func ,proc)
             (g-value-get-value
              ,(@@ (g-golf hl-api gobject) %g-inst-get-property-value))
             (g-class (!g-class (class-of container)))
             (p-spec
              (gtk-container-class-find-child-property g-class name)))
        (if p-spec
            (let* ((default-value (g-param-spec-get-default-value p-spec))
                   (g-type (g-value-type default-value))
                   (g-value (g-value-init g-type))
                   (dum (begin
                          (i-func container child name g-value)
                          #t))
                   (value (g-value-get-value g-value)))
              (g-value-unset g-value)
              value)
            (error "No child property" container name))))))

(define (gtk-container-child-set-property-ov proc)
  (values
   #f
   `(lambda (container child name value)
      (let* ((i-func ,proc)
             (g-value-set-value
              ,(@@ (g-golf hl-api gobject) %g-inst-set-property-value))
             (g-class (!g-class (class-of container)))
             (p-spec
              (gtk-container-class-find-child-property g-class name)))
        (if p-spec
            (let* ((default-value (g-param-spec-get-default-value p-spec))
                   (g-type (g-value-type default-value))
                   (g-value (g-value-init g-type)))
              (g-value-set! g-value
                            (g-value-set-value g-type value))
              (i-func container child name g-value)
              (g-value-unset g-value)
              (values))
            (error "No child property" container name))))))

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

(define (gtk-tree-store-set-value-ov proc)
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
