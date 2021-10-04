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


(define-module (g-golf override override)
  #:use-module (srfi srfi-1)

  #:export (%gi-override
            gi-override?))


(define %gi-override
  '("gdk_clipboard_set_value"
    "gdk_content_provider_get_value"

    "gtk_container_child_get_property"
    "gtk_container_child_set_property"
    "gtk_list_store_newv"
    "gtk_list_store_set_value"
    "gtk_tree_store_set_value"
    "gtk_tree_model_get_value"
    "gtk_text_buffer_insert"))

(define (gi-override? name)
  (and (member name %gi-override string=?)
       #t)) ;; not to store the member call result
