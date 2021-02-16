;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2019, 2021
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


(define-module (g-golf glib unix-utils)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support flags)
  #:use-module (g-golf support utils)
  #:use-module (g-golf glib io-channels)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-unix-fd-source-new))


;;;
;;; Glib Low level API
;;;

(define (g-unix-fd-source-new fd condition)
  (g_unix_fd_source_new fd
                        (flags->integer %g-io-condition
                                        condition)))


;;;
;;; Glib Bindings
;;;

(define g_unix_fd_source_new
  (pointer->procedure '*
                      (dynamic-func "g_unix_fd_source_new"
				    %libglib)
                      (list int		;; fd
                            uint32)))	;; condition
