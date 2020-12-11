;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016, 2020
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


(define-module (g-golf gi repository)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf gi utils)

  #:export (g-irepository-get-default
	    g-irepository-get-dependencies
	    g-irepository-get-loaded-namespaces
	    g-irepository-get-n-infos
	    g-irepository-get-info
            g-irepository-enumerate-versions
	    g-irepository-get-typelib-path
	    g-irepository-require
	    g-irepository-get-c-prefix
	    g-irepository-get-shared-library
	    g-irepository-get-version
	    g-irepository-find-by-gtype
	    g-irepository-find-by-name))


;;;
;;; Low level API
;;;

(define (g-irepository-get-default)
  (g_irepository_get_default))
  
(define* (g-irepository-get-dependencies namespace
                                         #:key (repository %null-pointer))
  (gi->scm (g_irepository_get_dependencies repository
                                           (string->pointer namespace))
           'strings))

(define* (g-irepository-get-loaded-namespaces #:key (repository %null-pointer))
  (gi->scm (g_irepository_get_loaded_namespaces repository)
           'strings))

(define* (g-irepository-get-n-infos namespace
                                    #:key (repository %null-pointer))
  (g_irepository_get_n_infos repository
			     (string->pointer namespace)))

(define* (g-irepository-get-info namespace index
                                 #:key (repository %null-pointer))
  (g_irepository_get_info repository
			  (string->pointer namespace)
			  index))

(define* (g-irepository-enumerate-versions namespace
                                           #:key (repository #f))
  (map pointer->string
    (gi->scm (g_irepository_enumerate_versions (scm->gi repository 'pointer)
                                               (string->pointer namespace))
             'glist)))

(define* (g-irepository-get-typelib-path namespace
                                         #:key (repository %null-pointer))
  (gi->scm (g_irepository_get_typelib_path repository
                                           (string->pointer namespace))
           'string))

(define* (g-irepository-require namespace
                                #:key (version #f) (repository %null-pointer))
  (with-gerror g-error
	       (g_irepository_require repository
				      (string->pointer namespace)
				      (if version
					  (string->pointer version)
					  %null-pointer)
				      0
				      g-error)))

(define* (g-irepository-get-c-prefix namespace
                                     #:key (repository %null-pointer))
  (gi->scm (g_irepository_get_c_prefix repository
                                       (string->pointer namespace))
           'string))

(define* (g-irepository-get-shared-library namespace
                                           #:key (repository %null-pointer))
  (gi->scm (g_irepository_get_shared_library repository
                                             (string->pointer namespace))
           'csv-string))

(define* (g-irepository-get-version namespace
                                    #:key (repository %null-pointer))
  (gi->scm (g_irepository_get_version repository
                                      (string->pointer namespace))
           'string))

(define* (g-irepository-find-by-gtype gtype
                                      #:key (repository %null-pointer))
  (gi->scm (g_irepository_find_by_gtype repository gtype)
           'pointer))

(define* (g-irepository-find-by-name namespace name
                                     #:key (repository %null-pointer))
  (gi->scm (g_irepository_find_by_name repository
                                       (string->pointer namespace)
                                       (string->pointer name))
           'pointer))


;;;
;;; GI Bindings
;;;

(define g_irepository_get_default
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_default"
				    %libgirepository)
                      '()))

(define g_irepository_get_dependencies
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_dependencies"
				    %libgirepository)
                      (list '* '*)))

(define g_irepository_get_loaded_namespaces
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_loaded_namespaces"
				    %libgirepository)
                      (list '*)))

(define g_irepository_get_n_infos
  (pointer->procedure int
                      (dynamic-func "g_irepository_get_n_infos"
				    %libgirepository)
                      (list '* '*)))

(define g_irepository_get_info
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_info"
				    %libgirepository)
                      (list '* '* int)))

(define g_irepository_enumerate_versions
  (pointer->procedure '*
                      (dynamic-func "g_irepository_enumerate_versions"
				    %libgirepository)
                      (list '* '*)))

(define g_irepository_get_typelib_path
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_typelib_path"
				    %libgirepository)
                      (list '* '*)))

(define g_irepository_require
  (pointer->procedure '*
                      (dynamic-func "g_irepository_require"
				    %libgirepository)
                      (list '* '* '* int '*)))

(define g_irepository_get_c_prefix
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_c_prefix"
				    %libgirepository)
                      (list '* '*)))

(define g_irepository_get_shared_library
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_shared_library"
				    %libgirepository)
                      (list '* '*)))

(define g_irepository_get_version
  (pointer->procedure '*
                      (dynamic-func "g_irepository_get_version"
				    %libgirepository)
                      (list '* '*)))

(define g_irepository_find_by_gtype
  (pointer->procedure '*
                      (dynamic-func "g_irepository_find_by_gtype"
				    %libgirepository)
                      (list '* unsigned-long)))

(define g_irepository_find_by_name
  (pointer->procedure '*
                      (dynamic-func "g_irepository_find_by_name"
				    %libgirepository)
                      (list '* '* '*)))
