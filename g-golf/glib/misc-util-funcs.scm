;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2020 - 2021
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
  #:use-module (g-golf gi utils)

  #:export (g-get-prgname
	    g-set-prgname
            g-get-system-data-dirs
            g-get-system-config-dirs))


;;;
;;; Glib Low level API
;;;

(define (g-get-prgname)
  (gi->scm (g_get_prgname) 'string))

(define (g-set-prgname name)
  (g_set_prgname (scm->gi name 'string)))

(define (g-get-system-data-dirs)
  (gi->scm (g_get_system_data_dirs) 'strings))

(define (g-get-system-config-dirs)
  (gi->scm (g_get_system_config_dirs) 'strings))


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

(define g_get_system_data_dirs
  (pointer->procedure '*
                      (dynamic-func "g_get_system_data_dirs"
				    %libglib)
                      (list )))		;; void

(define g_get_system_config_dirs
  (pointer->procedure '*
                      (dynamic-func "g_get_system_config_dirs"
				    %libglib)
                      (list )))		;; void
