;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2018 - 2019
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

;; This code is largely inspired by the Guile-Gnome module (gnome
;; gobject gobject), see:

;;   https://www.gnu.org/software/guile-gnome

;;   http://git.savannah.gnu.org/cgit/guile-gnome.git
;;     tree/glib/gnome/gobject/gobject.scm

;;; Code:


(define-module (g-golf hl-api import)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support goops)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi repository)
  #:use-module (g-golf gi base-info)
  #:use-module (g-golf gi registered-type-info)
  #:use-module (g-golf gi object-info)
  #:use-module (g-golf gobject type-info)
  #:use-module (g-golf hl-api gtype)
  #:use-module (g-golf hl-api gobject)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export (gi-import))


#;(g-export )


;;;
;;; 
;;;

(define (gi-import namespace)
  (g-irepository-require namespace)
  (let ((n-info (g-irepository-get-n-infos namespace)))
    (do ((i 0
            (+ i 1)))
        ((= i n-info))
      (let* ((info (g-irepository-get-info namespace i)))
        (case (g-base-info-get-type info)
          ((object)
           (gi-import-object info))
          (else
           ;; I won't do nothing, not even displaying a message, till
           ;; G-Golf is complete, because it would fill the repl ...
           ;; Ultimately, it will raise an exception
           'nothing))))
    (values)))

(define (gi-import-object info)
  (let* ((r-type (g-registered-type-info-get-g-type info))
         (gi-name (g-type-name r-type))
         (class-name (gi-name->class-name gi-name))
         (class (make-class (list <gobject>)
                            '()
                            #:name class-name
                            #:info info)))
    (module-define! (current-module) class-name class)
    (export class-name)))