;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2019 - 2020
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


(define-module (g-golf glib gslist)
  #:use-module (ice-9 match)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)

  #:export (g-slist-data
            g-slist-next

            g-slist-append
            g-slist-prepend
            g-slist-free
            g-slist-length
            g-slist-nth-data))


;;;
;;; Glib Low level API
;;;

(define %g-slist-struct
  (list '* '*))

(define (g-slist-parse g-slist)
  (parse-c-struct g-slist %g-slist-struct))

(define (g-slist-data g-slist)
  (match (g-slist-parse g-slist)
    ((data _) data)))

(define (g-slist-next g-slist)
  (match (g-slist-parse g-slist)
    ((_ next) next)))

(define (g-slist-append g-slist data)
  (g_slist_append (scm->gi g-slist 'pointer)
                  (scm->gi data 'pointer)))

(define (g-slist-prepend g-slist data)
  (g_slist_prepend (scm->gi g-slist 'pointer)
                   (scm->gi data 'pointer)))

(define (g-slist-free g-slist)
  (g_slist_free g-slist))

(define (g-slist-length g-slist)
  (g_slist_length g-slist))

(define (g-slist-nth-data g-slist n)
  (let ((foreign (g_slist_nth_data g-slist n)))
    (if (null-pointer? foreign)
        #f
        foreign)))


;;;
;;; Glib Bindings
;;;

(define g_slist_append
  (pointer->procedure '*
                      (dynamic-func "g_slist_append"
				    %libglib)
                      (list '*		;; g-slist
                            '*)))	;; data

(define g_slist_prepend
  (pointer->procedure '*
                      (dynamic-func "g_slist_prepend"
				    %libglib)
                      (list '*		;; g-slist
                            '*)))	;; data

(define g_slist_free
  (pointer->procedure void
                      (dynamic-func "g_slist_free"
				    %libglib)
                      (list '*)))

(define g_slist_length
  (pointer->procedure unsigned-int
                      (dynamic-func "g_slist_length"
				    %libglib)
                      (list '*)))

(define g_slist_nth_data
  (pointer->procedure '*
                      (dynamic-func "g_slist_nth_data"
				    %libglib)
                      (list '*
                            unsigned-int)))
