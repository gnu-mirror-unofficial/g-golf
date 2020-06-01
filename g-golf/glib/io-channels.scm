;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2019
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


(define-module (g-golf glib io-channels)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support flag)
  #:use-module (g-golf support utils)
  #:use-module (g-golf gi cache)
  #:use-module (g-golf gi utils)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-io-channel-unix-new
            g-io-channel-ref
            g-io-channel-unref
            g-io-create-watch
            %g-io-condition))


;;;
;;; Glib Low level API
;;;

(define (g-io-channel-unix-new fd)
  (g_io_channel_unix_new fd))

(define (g-io-channel-ref channel)
  (g_io_channel_ref channel))

(define (g-io-channel-unref channel)
  (g_io_channel_unref channel))

(define (g-io-create-watch channel condition)
  (g_io_create_watch channel
                     (gi-gflags->integer %g-io-condition
                                         condition)))


;;;
;;; Glib Bindings
;;;

(define g_io_channel_unix_new
  (pointer->procedure '*
                      (dynamic-func "g_io_channel_unix_new"
				    %libglib)
                      (list int)))	;; fd

(define g_io_channel_ref
  (pointer->procedure '*
                      (dynamic-func "g_io_channel_ref"
				    %libglib)
                      (list '*)))	;; channel

(define g_io_channel_unref
  (pointer->procedure void
                      (dynamic-func "g_io_channel_unref"
				    %libglib)
                      (list '*)))	;; channel

(define g_io_create_watch
  (pointer->procedure '*
                      (dynamic-func "g_io_create_watch"
				    %libglib)
                      (list '*		;; channel
                            uint32)))	;; condition


;;;
;;; Type and Values
;;;

;; Although I would have used this definitin, I sort of accidentally
;; import selectively the GIOCondition dfinition from the GLib typelib,
;; and to my surprise, they use different enum-set integer values, see
;; below. A few tests show it doesn't make any difference (my bitwise
;; combination (mental) skills are missing at this moment :-()), but I
;; decided to keep the 'imported' enum-set definition anyway.

#;(define %g-io-condition
  (make <gi-flag>
    #:g-name "gio-condition"
    #:enum-set '(in
                 out
                 pri
                 err
                 hup 
                 nval)))

(define %g-io-condition #f)

(eval-when (expand load eval)
  (let ((g-io-condition (make <gi-flag>
                          #:g-name  "GIOCondition"
                          #:enum-set '((in . 1)
                                       (out . 4)
                                       (pri . 2)
                                       (err . 8)
                                       (hup . 16)
                                       (nval . 32)))))
    (set! %g-io-condition g-io-condition)
    (gi-cache-set! 'flag 'gio-condition g-io-condition)))
