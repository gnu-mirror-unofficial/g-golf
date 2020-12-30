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


(define-module (g-golf gdk events)
  #:use-module (oop goops)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (gdk-event-class))


(define (gdk-event-class)
  (let ((module (resolve-module '(g-golf hl-api gobject)))
        (c-name '<gdk-event>))
    (if (module-bound? module c-name)
        (module-ref module c-name)
        (let* ((public-i (module-public-interface module))
               (c-inst (make-class `(,<object>)
                                   `(,(gdk-event-slot module public-i))
                                   #:name c-name)))
          (module-define! module c-name c-inst)
          (module-add! public-i c-name
                       (module-variable module c-name))
          c-inst))))

(define (gdk-event-slot module public-i)
  (let* ((name 'event)
         (k-name #:event)
         (a-name '!event)
         (a-inst (if (module-variable module a-name)
                     (module-ref module a-name)
                     (let ((a-inst (make-accessor a-name)))
                       (module-define! module a-name a-inst)
                       (module-add! public-i a-name
                                    (module-variable module a-name))
                       a-inst))))
    (make <slot>
      #:name name
      #:accessor a-inst
      #:init-keyword k-name)))
