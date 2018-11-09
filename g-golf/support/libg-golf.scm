;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2018
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
  
  #:export (;; misc.
            pointer-address-size-c

            ;; floats
            float-to-int-c))


;;;
;;; misc.
;;;

(define pointer-address-size-c
  (pointer->procedure size_t
                      (dynamic-func "pointer_address_size_c"
                                    %libg-golf)
                      (list)))


;;;
;;; floats
;;;

(define float-to-int-c
  (pointer->procedure int
                      (dynamic-func "float_to_int_c"
                                    %libg-golf)
                      (list float)))
