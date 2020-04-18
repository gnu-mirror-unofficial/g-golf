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

;; This code is largely inspired by the Guile-Gnome modules
;; (gnome gobject generics) and (gnome gobject gsignal), see:

;;   https://www.gnu.org/software/guile-gnome

;;   http://git.savannah.gnu.org/cgit/guile-gnome.git
;;     tree/glib/gnome/gobject/generic.scm
;;     tree/glib/gnome/gobject/gsignal.scm

;;; Code:


(define-module (g-golf hl-api signal)
  #:use-module (ice-9 threads)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf gi)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)
  #:use-module (g-golf hl-api gtype)
  #:use-module (g-golf hl-api gobject)
  #:use-module (g-golf hl-api function)
  #:use-module (g-golf hl-api closure)

  #:replace (connect)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)
  
  #:export (<signal>

            %gi-signal-cache

            gi-signal-cache-ref
            gi-signal-cache-set!

            gi-signal-cache-show
            gi-signal-cache-find))


(g-export describe
          !id
          !name
          !flags
          !iface-type
          !iface-name
          !iface-class
          !return-type
          !param-types
          !param-args

          connect-after)


;;;
;;; Signal connection
;;;

(define %connect
  (module-ref the-root-module 'connect))

(define-method (connect . args)
  "The core Guile implementation of the connect(2) POSIX call"
  (apply %connect args))

(define-method* (connect (inst <gtype-instance>) name function
                         #:optional (after? #f) (detail #f))
  (signal-connect inst name function after? detail))

(define-method* (connect-after (inst <gtype-instance>) name function
                               #:optional (detail #f))
  (signal-connect inst name function #t detail))

(define (signal-connect inst s-name function after? detail)
  ;; Below, i- stands for instance-, o- for object-, s- for signal-
  (let* ((i-class (class-of inst))
         (i-class-name (class-name i-class))
         (i-type (!gtype-id i-class))
         (s-id (or (g-signal-lookup (symbol->string s-name) i-type)
                   (error "No such signal: " i-class-name s-name)))
         (g-signal (g-signal-query s-id)))
    (match g-signal
      ((id name iface-type flags return-type n-param param-types)
       (let* ((iface-name (g-type-name iface-type))
              (iface-key (string->symbol
                          (g-studly-caps-expand iface-name)))
              (signal (or (gi-signal-cache-ref iface-key s-name)
                          (let* ((iface-info (g-irepository-find-by-gtype iface-type))
                                 (iface-s-info (g-object-info-find-signal iface-info name))
                                 (param-args (signal-arguments iface-s-info))
                                 (s-inst (make <signal>
                                           #:id id
                                           #:name name
                                           #:iface-type iface-type
                                           #:iface-name iface-name
                                           #:flags flags
                                           #:return-type return-type
                                           #:n-param n-param
                                           #:param-types param-types
                                           #:param-args param-args)))
                            (gi-signal-cache-set! iface-key s-name s-inst)
                            s-inst)))
              (closure (make <closure>
                         #:function function
                         #:return-type (!return-type signal)
                         #:param-types (cons 'object
                                             (!param-types signal))
                         #:param-args (cons #f
                                            (!param-args signal)))))
         (g-signal-connect-closure-by-id (!g-inst inst)
                                         (!id signal)
                                         detail
                                         (!g-closure closure)
                                         after?)
         (values))))))

(define (signal-arguments info)
  (let loop ((n-arg (g-callable-info-get-n-args info))
             (args '()))
    (if (= n-arg 0)
        args
        (loop (- n-arg 1)
              (cons (make <argument>
                      #:info (g-callable-info-get-arg info
                                                      (- n-arg 1))
                      #:arg-pos n-arg)
                    args)))))


;;;
;;; The <signal> class, accesors and methods
;;;

(define-class <signal> ()
  (id #:accessor !id #:init-keyword #:id #:init-value #f)
  (name #:accessor !name #:init-keyword #:name)
  (iface-type #:accessor !iface-type #:init-keyword #:iface-type)
  (iface-name #:accessor !iface-name #:init-keyword #:iface-name)
  (iface-class #:accessor !iface-class #:init-keyword #:iface-class)
  (flags #:accessor !flags #:init-keyword #:flags)
  (return-type #:accessor !return-type #:init-keyword #:return-type)
  (n-param #:accessor !n-param #:init-keyword #:n-param)
  (param-types #:accessor !param-types #:init-keyword #:param-types)
  (param-args #:accessor !param-args #:init-keyword #:param-args #:init-value #f))

(define-method (initialize (self <signal>) initargs)
  (next-method)
  (let* ((module (resolve-module '(g-golf hl-api gobject)))
         (iface-type (!iface-type self))
         (iface-name (g-type-name iface-type))
         (iface-c-name (g-name->class-name iface-name))
         (iface-class (module-ref module iface-c-name)))
    (mslot-set! self
                'iface-name iface-name
                'iface-class iface-class)))

(define-method (describe (self <signal>))
  (next-method)
  (newline)
  (for-each (lambda (argument)
              (describe argument)
              (newline))
      (!param-args self)))


;;;
;;; The gi-signal-cache
;;;

(define %gi-signal-cache
  '())

(define (gi-signal-cache-ref m-key s-key)
  ;; m-key, s-key stand for main and secondary keys
  (let ((subcache (assq-ref %gi-signal-cache m-key)))
    (and subcache
         (assq-ref subcache s-key))))

(define (gi-signal-cache-set! m-key s-key val)
  (let ((subcache (assq-ref %gi-signal-cache m-key)))
    (set! %gi-signal-cache
          (assq-set! %gi-signal-cache m-key
                     (assq-set! (or subcache '()) s-key
                                val)))))

(define* (gi-signal-cache-show #:optional (m-key #f))
  (format #t "%gi-signal-cahe~%")
  (if m-key
      (begin
        (format #t "  ~A~%" m-key)
        (for-each (lambda (s-entry)
                    (match s-entry
                      ((s-key . s-vals)
                       (format #t "    ~A~%" s-key))))
            (assq-ref %gi-signal-cache m-key)))
      (for-each (lambda (m-entry)
                  (match m-entry
                    ((m-key . m-vals)
                     (format #t "  ~A~%" m-key))))
          %gi-signal-cache)))

(define (gi-signal-cache-find name)
  'wip)
