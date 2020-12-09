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


(define-module (g-golf glib misc-util-funcs)
  #:use-module (system foreign)
  #:use-module (g-golf init)

  #:export (g-get-prgname
	    g-set-prgname))


;;;
;;; Glib Low level API
;;;

(define (g-get-prgname)
  (let ((foreign (g_get_prgname)))
    (if (null-pointer? foreign)
        #f
        (pointer->string foreign))))

(define (g-set-prgname name)
  (g_set_prgname (string->pointer name)))


;;;
;;; Glib Bindings
;;;

(define g_get_prgname
  (pointer->procedure '*
                      (dynamic-func "g_get_prgname"
				    %libglib)
                      (list )))		;; void

(define g_set_prgname
  (pointer->procedure void
                      (dynamic-func "g_set_prgname"
				    %libglib)
                      (list '*)))	;; name
