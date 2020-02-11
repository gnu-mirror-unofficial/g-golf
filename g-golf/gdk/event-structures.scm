;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2020
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


(define-module (g-golf gdk event-structures)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support libg-golf)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support enum)
  ;;#:use-module (g-golf support struct)
  ;;#:use-module (g-golf support union)
  #:use-module (g-golf support flag)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi cache)
  #:use-module (g-golf gdk events)
  #:use-module (g-golf gdk key-values)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (make-gdk-event

            <gdk-event-key>
            gdk-event-key:window
            ;; gdk-event-key:send-event
            gdk-event-key:time
            gdk-event-key:state
            gdk-event-key:keyval
            gdk-event-key:keyname
            ;; gdk-event-key:length
            ;; gdk-event-key:string
            gdk-event-key:hardware-keycode
            ;; gdk-event-key:group
            ;; gdk-event-key:is-modifier

            <gdk-event-button>
            gdk-event-button:time
            gdk-event-button:state
            gdk-event-button:button
            gdk-event-button:click-count
            gdk-event-button:coords
            gdk-event-button:x
            gdk-event-button:y
            gdk-event-button:root-coords
            gdk-event-button:x-root
            gdk-event-button:y-root

            <gdk-event-motion>
            gdk-event-motion:time
            gdk-event-motion:state
            gdk-event-motion:coords
            gdk-event-motion:x
            gdk-event-motion:y
            gdk-event-motion:root-coords
            gdk-event-motion:x-root
            gdk-event-motion:y-root

            <gdk-event-crossing>
            gdk-event-crossing:window
            ;;gdk-event-crossing:send-event
            ;;gdk-event-crossing:subwindow
            gdk-event-crossing:time
            gdk-event-crossing:coords
            gdk-event-crossing:x
            gdk-event-crossing:y
            gdk-event-crossing:root-coords
            gdk-event-crossing:x-root
            gdk-event-crossing:y-root
            ;;gdk-event-crossing:mode
            ;;gdk-event-crossing:notify-type
            ;;gdk-event-crossing:focus
            gdk-event-crossing:state

            #;gdk-event-key-parse

            #;%gdk-event-key))


#;(g-export !items)


;;;
;;;
;;;

(define (make-gdk-event event)
  (case (gdk-event-get-event-type event)
    ((key-press
      key-release)
     (make <gdk-event-key> #:event event))
    ((button-press
      button-release
      2button-press
      2button-release
      3button-press
      3button-release)
     (make <gdk-event-button> #:event event))
    ((enter-notify
      leave-notify)
     (make <gdk-event-crossing> #:event event))
    ((motion-notify)
     (make <gdk-event-motion> #:event event))
    (else
     event)))


;;;
;;; Key
;;;

(define-class <gdk-event-key> (<gdk-event>))

(define-method (initialize (self <gdk-event-key>) initargs)
  (let ((event (or (get-keyword #:event initargs #f)
                   (error "Missing #:event initarg: " initargs))))
    (next-method)
    #;(slot-set! self 'event-items
               (gdk-event-key-parse event))))

(define-method (gdk-event-key:window (self <gdk-event-key>))
  (gdk-event-get-window (!event self)))

#;(define-method (gdk-event-key:send-event (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ send-event _ _ _ _ _ _ _ _)
     (gi->scm send-event 'boolean))))

(define-method (gdk-event-key:time (self <gdk-event-key>))
  (gdk-event-get-time (!event self)))

(define-method (gdk-event-key:state (self <gdk-event-key>))
  (gdk-event-get-state (!event self)))

(define-method (gdk-event-key:keyval (self <gdk-event-key>))
  (gdk-event-get-keyval (!event self)))

(define-method (gdk-event-key:keyname (self <gdk-event-key>))
  (gdk-keyval-name (gdk-event-key:keyval self)))

#;(define-method (gdk-event-key:length (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ _ _ _ _ length _ _ _ _)
     length)))

#;(define-method (gdk-event-key:string (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ _ _ _ _ _ str _ _ _)
     (gi->scm str 'string))))

(define-method (gdk-event-key:hardware-keycode (self <gdk-event-key>))
  (gdk-event-get-keycode (!event self)))

#;(define-method (gdk-event-key:group (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ _ _ _ _ _ _ _ group _)
     group)))

#;(define-method (gdk-event-key:is-modifier (self <gdk-event-key>))
  (match (!event-items self)
    ((_ _ _ _ _ _ _ _ _ _ is-modifier)
     (gi->scm is-modifier 'boolean))))


;;;
;;; Button
;;;

(define-class <gdk-event-button> (<gdk-event>))

(define-method (initialize (self <gdk-event-button>) initargs)
  (let ((event (or (get-keyword #:event initargs #f)
                   (error "Missing #:event initarg: " initargs))))
    (next-method)))

(define-method (gdk-event-button:time (self <gdk-event-button>))
  (gdk-event-get-time (!event self)))

(define-method (gdk-event-button:state (self <gdk-event-button>))
  (gdk-event-get-state (!event self)))

(define-method (gdk-event-button:button (self <gdk-event-button>))
  (gdk-event-get-button (!event self)))

(define-method (gdk-event-button:click-count (self <gdk-event-button>))
  (gdk-event-get-click-count (!event self)))

(define-method (gdk-event-button:coords (self <gdk-event-button>))
  (gdk-event-get-coords (!event self)))

(define-method (gdk-event-button:x (self <gdk-event-button>))
  (match (gdk-event-get-coords (!event self))
    ((x y) x)))

(define-method (gdk-event-button:y (self <gdk-event-button>))
  (match (gdk-event-get-coords (!event self))
    ((x y) y)))

(define-method (gdk-event-button:root-coords (self <gdk-event-button>))
  (gdk-event-get-root-coords (!event self)))

(define-method (gdk-event-button:x-root (self <gdk-event-button>))
  (match (gdk-event-get-root-coords (!event self))
    ((x-root y-root) x-root)))

(define-method (gdk-event-button:y-root (self <gdk-event-button>))
  (match (gdk-event-get-root-coords (!event self))
    ((x-root y-root) y-root)))


;;;
;;; Motion
;;;

(define-class <gdk-event-motion> (<gdk-event>))

(define-method (initialize (self <gdk-event-motion>) initargs)
  (let ((event (or (get-keyword #:event initargs #f)
                   (error "Missing #:event initarg: " initargs))))
    (next-method)))

(define-method (gdk-event-motion:time (self <gdk-event-motion>))
  (gdk-event-get-time (!event self)))

(define-method (gdk-event-motion:state (self <gdk-event-motion>))
  (gdk-event-get-state (!event self)))

(define-method (gdk-event-motion:coords (self <gdk-event-motion>))
  (gdk-event-get-coords (!event self)))

(define-method (gdk-event-motion:x (self <gdk-event-motion>))
  (match (gdk-event-get-coords (!event self))
    ((x y) x)))

(define-method (gdk-event-motion:y (self <gdk-event-motion>))
  (match (gdk-event-get-coords (!event self))
    ((x y) y)))

(define-method (gdk-event-motion:root-coords (self <gdk-event-motion>))
  (gdk-event-get-root-coords (!event self)))

(define-method (gdk-event-motion:x-root (self <gdk-event-motion>))
  (match (gdk-event-get-root-coords (!event self))
    ((x-root y-root) x-root)))

(define-method (gdk-event-motion:y-root (self <gdk-event-motion>))
  (match (gdk-event-get-root-coords (!event self))
    ((x-root y-root) y-root)))


;;;
;;; Crossing
;;;

(define-class <gdk-event-crossing> (<gdk-event>))

(define-method (initialize (self <gdk-event-crossing>) initargs)
  (let ((event (or (get-keyword #:event initargs #f)
                   (error "Missing #:event initarg: " initargs))))
    (next-method)))

(define-method (gdk-event-crossing:window (self <gdk-event-crossing>))
  (gdk-event-get-window (!event self)))

;;gdk-event-crossing:send-event
;;gdk-event-crossing:subwindow

(define-method (gdk-event-crossing:time (self <gdk-event-crossing>))
  (gdk-event-get-time (!event self)))

(define-method (gdk-event-crossing:coords (self <gdk-event-crossing>))
  (gdk-event-get-coords (!event self)))

(define-method (gdk-event-crossing:x (self <gdk-event-crossing>))
  (match (gdk-event-get-coords (!event self))
    ((x y) x)))

(define-method (gdk-event-crossing:y (self <gdk-event-crossing>))
  (match (gdk-event-get-coords (!event self))
    ((x y) y)))

(define-method (gdk-event-crossing:root-coords (self <gdk-event-crossing>))
  (gdk-event-get-root-coords (!event self)))

(define-method (gdk-event-crossing:x-root (self <gdk-event-crossing>))
  (match (gdk-event-get-root-coords (!event self))
    ((x-root y-root) x-root)))

(define-method (gdk-event-crossing:y-root (self <gdk-event-crossing>))
  (match (gdk-event-get-root-coords (!event self))
    ((x-root y-root) y-root)))

;;gdk-event-crossing:mode
;;gdk-event-crossing:notify-type
;;gdk-event-crossing:focus

(define-method (gdk-event-crossing:state (self <gdk-event-crossing>))
  (gdk-event-get-state (!event self)))


;;;
;;; Low level API
;;;

#;(define (gdk-event-key-parse event)
  (parse-c-struct event
                  %gdk-event-key-struct))




;;;
;;; Types and Values
;;;

#;(define %gdk-event-key-struct
  (list int		;; type
        '*		;; window
        int8		;; send event
        uint32		;; time
        unsigned-int	;; state
        unsigned-int	;; keyval
        int		;; length
        '*		;; string
        uint16		;; hardware keycode;
        uint8		;; group
        unsigned-int))	;; is-modifier
