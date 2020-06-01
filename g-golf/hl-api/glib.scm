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

;; This code is largely inspired by the Guile-Gnome module (gnome glib)

;;   https://www.gnu.org/software/guile-gnome

;;   http://git.savannah.gnu.org/cgit/guile-gnome.git
;;     tree/glib/gnome/glib.scm

;;; Code:


(define-module (g-golf hl-api glib)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)
  #:use-module (g-golf gi)
  #:use-module (g-golf hl-api closure)
  #:use-module (g-golf hl-api import)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (g-idle-add
            g-timeout-add
            g-timeout-add-seconds

            g-unix-fd-add

            %g-priorities))


#;(g-export )


;;;
;;; Main Event Loop
;;;

(define* (g-idle-add proc #:optional (priority 'default-idle))
  (let* ((closure (make <closure>
                    #:function proc
                    #:return-type 'boolean
                    #:param-types '()))
         (g-closure (!g-closure closure))
         (source (g-idle-source-new))
         (dummy (g-source-set-priority source
                                       (ensure-priority priority)))
         (dummy (g-source-set-closure source g-closure))
         (id (g-source-attach source #f)))
    (g-source-unref source)
    (g-closure-unref g-closure)
    id))

(define* (g-timeout-add interval proc #:optional (priority 'default))
  (let* ((closure (make <closure>
                    #:function proc
                    #:return-type 'boolean
                    #:param-types '()))
         (g-closure (!g-closure closure))
         (source (g-timeout-source-new interval))
         (dummy (g-source-set-priority source
                                       (ensure-priority priority)))
         (dummy (g-source-set-closure source g-closure))
         (id (g-source-attach source #f)))
    (g-source-unref source)
    (g-closure-unref g-closure)
    id))

(define* (g-timeout-add-seconds interval proc
                                #:optional (priority 'default))
  (let* ((closure (make <closure>
                    #:function proc
                    #:return-type 'boolean
                    #:param-types '()))
         (g-closure (!g-closure closure))
         (source (g-timeout-source-new-seconds interval))
         (dummy (g-source-set-priority source
                                       (ensure-priority priority)))
         (dummy (g-source-set-closure source g-closure))
         (id (g-source-attach source #f)))
    (g-source-unref source)
    (g-closure-unref g-closure)
    id))


;;;
;;; IO Channels
;;;

#;(define (g-io-add-watch-fd fd condition proc)
  (let* ((closure (make <closure>
                    #:function proc
                    #:return-type 'boolean
                    #:param-types '(pointer	;; source
                                    uint32)))	;; condition
         (g-closure (!g-closure closure))
         (channel (g-io-channel-unix-new fd))
         (source (g-io-create-watch channel condition))
         (dummy (g-source-set-closure source g-closure))
         (id (g-source-attach source #f)))
    (g-source-unref source)
    (g-closure-unref g-closure)
    (g-io-channel-unref channel)
    id))


;;;
;;; UNIX-specific utilities and integration
;;;

(define* (g-unix-fd-add fd condition proc
                        #:optional (priority 'default))
  (let* ((closure (make <closure>
                    #:function proc
                    #:return-type 'boolean
                    #:param-types '(int		 ;; fd
                                    uint32)))	 ;; condition
         (g-closure (!g-closure closure))
         (source (g-unix-fd-source-new fd condition))
         (dummy (g-source-set-closure source g-closure))
         (dummy (g-source-set-priority source
                                       (ensure-priority priority)))
         (id (g-source-attach source #f)))
    (g-source-unref source)
    (g-closure-unref g-closure)
    id))


;;;
;;; Priorities
;;;

(define %g-priorities #f)

(eval-when (expand load eval)
  (g-irepository-require "GLib")
  (let* ((names '("PRIORITY_DEFAULT"
                  "PRIORITY_DEFAULT_IDLE"
                  "PRIORITY_HIGH"
                  "PRIORITY_HIGH_IDLE"
                  "PRIORITY_LOW"))
         (scm-names '(default
                      default-idle
                      high
                      high-idle
                      low))
         (infos (map (lambda (name)
                       (g-irepository-find-by-name "GLib" name))
                  names))
         (values (map gi-import-constant infos))
         (priorities (map (lambda (priority)
                            (match priority
                              ((name value)
                               (cons name value))))
                       (zip scm-names values))))
    (map g-base-info-unref infos)
    (set! %g-priorities
          (make <gi-enum>
            #:g-name "G_PRIORITIES"
            #:enum-set priorities))))

(define (ensure-priority priority)
  (if (number? priority)
      priority
      (let ((g-priority (enum->value %g-priorities priority)))
        (or g-priority
            (error "No such priority: " priority)))))
