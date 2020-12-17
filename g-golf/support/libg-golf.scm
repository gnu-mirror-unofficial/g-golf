;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2020
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


(define-module (g-golf support libg-golf)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  
  #:export (;; Misc.
            pointer_address_size

            ;; Floats
            float_to_int

            ;; Glib
            g_source_ref_count

            ;; GObject
            g_value_size
            g_object_type
            #;g_object_type_name
            g_object_ref_count
            g_closure_size
            g_closure_ref_count

            ;; Gdk
            ;; gdk_event_get_changed_mask
            ;; gdk_event_get_new_window_state

            ;; Test suite
            test_suite_n_string_ptr
            test_suite_strings_ptr))


;;;
;;; Misc.
;;;

(define pointer_address_size
  (pointer->procedure size_t
                      (dynamic-func "pointer_address_size"
                                    %libg-golf)
                      (list)))


;;;
;;; Floats
;;;

(define float_to_int
  (pointer->procedure int
                      (dynamic-func "float_to_int"
                                    %libg-golf)
                      (list float)))


;;;
;;; Glib
;;;


(define g_source_ref_count
  (pointer->procedure unsigned-int
                      (dynamic-func "g_source_ref_count"
                                    %libg-golf)
                      (list '*)))


;;;
;;; GObject
;;;

(define g_value_size
  (pointer->procedure size_t
                      (dynamic-func "g_value_size"
                                    %libg-golf)
                      (list)))

(define g_object_type
  (pointer->procedure unsigned-long
                      (dynamic-func "g_object_type"
                                    %libg-golf)
                      (list '*)))

#!

The following is not working yet, it segfault, complaining that:

  /opt2/bin/guile: symbol lookup error: /opt2/lib/libg-golf.so:
  undefined symbol: g_type_name

  [ which is weird since g_type_name is defined in GObject Type info,
  [ and so it should be part of glib-object.h, afaict at least.

It is not a real problem though, because we already bind g_type_name
using the ffi, in (g-golf gobject type-info).  I'll try to fix this
later.

(define g_object_type_name
  (pointer->procedure '*
                      (dynamic-func "g_object_type_name"
                                    %libg-golf)
                      (list '*)))

!#

(define g_object_ref_count
  (pointer->procedure unsigned-int
                      (dynamic-func "g_object_ref_count"
                                    %libg-golf)
                      (list '*)))

(define g_closure_size
  (pointer->procedure size_t
                      (dynamic-func "g_closure_size"
                                    %libg-golf)
                      (list)))

(define g_closure_ref_count
  (pointer->procedure unsigned-int
                      (dynamic-func "g_closure_ref_count"
                                    %libg-golf)
                      (list '*)))


;;;
;;; Gdk
;;;

#;(define gdk_event_get_changed_mask
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_changed_mask"
                                    %libg-golf)
                      (list '*)))

#;(define gdk_event_get_new_window_state
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_new_window_state"
                                    %libg-golf)
                      (list '*)))


;;;
;;; Test suite
;;;

(define test_suite_n_string_ptr
  (pointer->procedure '*
                      (dynamic-func "test_suite_n_string_ptr"
                                    %libg-golf)
                      (list)))

(define test_suite_strings_ptr
  (pointer->procedure '*
                      (dynamic-func "test_suite_strings_ptr"
                                    %libg-golf)
                      (list)))
