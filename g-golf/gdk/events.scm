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


(define-module (g-golf gdk events)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-4)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (g-golf init)
  #:use-module (g-golf support libg-golf)
  #:use-module (g-golf support utils)
  #:use-module (g-golf support g-export)
  #:use-module (g-golf support enum)
  #:use-module (g-golf support struct)
  #:use-module (g-golf support union)
  #:use-module (g-golf support flag)
  #:use-module (g-golf gi utils)
  #:use-module (g-golf gi cache)
  #:use-module (g-golf gdk key-values)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (<gdk-event>

            gdk-event-get-button
            gdk-event-get-click-count
            gdk-event-get-coords
            gdk-event-get-keycode
            gdk-event-get-keyval
            gdk-event-get-root-coords
            gdk-event-get-state
            gdk-event-get-time
            gdk-event-get-window
            gdk-event-get-event-type
            ;; from libg-golf
            ;; gdk-event-get-changed-mask
            ;; gdk-event-get-new-window-state

            %gdk-event-type
            %gdk-window-state))


(g-export !event
          !button
          !click-count
          !coords
          !x
          !y
          !keycode
          !keyval
          !keyname
          !root-coords
          !root-x
          !root-y
          !state
          !time
          !window
          !type
          !changed-mask
          !new-window-state)


(define-class <gdk-event> ()
  (event #:accessor !event #:init-keyword #:event))

(define-method (!button (self <gdk-event>))
  (gdk-event-get-button (!event self)))

(define-method (!click-count (self <gdk-event>))
  (gdk-event-get-click-count (!event self)))

(define-method (!coords (self <gdk-event>))
  (gdk-event-get-coords (!event self)))

(define-method (!x (self <gdk-event>))
  (match (gdk-event-get-coords (!event self))
    ((x y) x)))

(define-method (!y (self <gdk-event>))
  (match (gdk-event-get-coords (!event self))
    ((x y) y)))

(define-method (!keycode (self <gdk-event>))
  (gdk-event-get-keycode (!event self)))

(define-method (!keyval (self <gdk-event>))
  (gdk-event-get-keyval (!event self)))

(define-method (!keyname (self <gdk-event>))
  (gdk-keyval-name (gdk-event-get-keyval (!event self))))

(define-method (!root-coords (self <gdk-event>))
  (gdk-event-get-root-coords (!event self)))

(define-method (!root-x (self <gdk-event>))
  (match (gdk-event-get-root-coords (!event self))
    ((root-x root-y) root-x)))

(define-method (!root-y (self <gdk-event>))
  (match (gdk-event-get-root-coords (!event self))
    ((root-x root-y) root-y)))

(define-method (!state (self <gdk-event>))
  (gdk-event-get-state (!event self)))

(define-method (!time (self <gdk-event>))
  (gdk-event-get-time (!event self)))

(define-method (!window (self <gdk-event>))
  (gdk-event-get-window (!event self)))

(define-method (!type (self <gdk-event>))
  (gdk-event-get-event-type (!event self)))

#;(define-method (!changed-mask (self <gdk-event>))
  (gdk-event-get-changed-mask (!event self)))

#;(define-method (!new-window-state (self <gdk-event>))
  (gdk-event-get-new-window-state (!event self)))


;;;
;;; Gdk Low level API
;;;

(define (gdk-event-get-button event)
  (let ((bv (make-bytevector (sizeof unsigned-int) 0)))
    (and (gi->scm (gdk_event_get_button event
                                        (bytevector->pointer bv))
                  'boolean)
         (u32vector-ref bv 0))))

(define (gdk-event-get-click-count event)
  (let ((bv (make-bytevector (sizeof unsigned-int) 0)))
    (and (gi->scm (gdk_event_get_click_count event
                                             (bytevector->pointer bv))
                  'boolean)
         (u32vector-ref bv 0))))

(define (gdk-event-get-coords event)
  (let ((bv1 (make-bytevector (sizeof double) 0))
        (bv2 (make-bytevector (sizeof double) 0)))
    (and (gi->scm (gdk_event_get_coords event
                                        (bytevector->pointer bv1)
                                        (bytevector->pointer bv2))
                  'boolean)
         (list (f64vector-ref bv1 0)
               (f64vector-ref bv2 0)))))

(define (gdk-event-get-keycode event)
  (let ((bv (make-bytevector (sizeof uint16) 0)))
    (and (gi->scm (gdk_event_get_keycode event
                                         (bytevector->pointer bv))
                  'boolean)
         (u16vector-ref bv 0))))

(define (gdk-event-get-keyval event)
  (let ((bv (make-bytevector (sizeof unsigned-int) 0)))
    (and (gi->scm (gdk_event_get_keyval event
                                        (bytevector->pointer bv))
                  'boolean)
         (u32vector-ref bv 0))))

(define (gdk-event-get-root-coords event)
  (let ((bv1 (make-bytevector (sizeof double) 0))
        (bv2 (make-bytevector (sizeof double) 0)))
    (and (gi->scm (gdk_event_get_root_coords event
                                             (bytevector->pointer bv1)
                                             (bytevector->pointer bv2))
                  'boolean)
         (list (f64vector-ref bv1 0)
               (f64vector-ref bv2 0)))))

(define (gdk-event-get-state event)
  (let ((modifier-flags (gi-cache-ref 'flag 'gdk-modifier-type))
        (bv (make-bytevector (sizeof int) 0)))
    (and (gi->scm (gdk_event_get_state event
                                       (bytevector->pointer bv))
                  'boolean)
         (gi-integer->gflags modifier-flags
                             (s32vector-ref bv 0)))))

(define (gdk-event-get-time event)
  (gdk_event_get_time event))

(define (gdk-event-get-window event)
  (gi->scm (gdk_event_get_window event) 'pointer))

(define (gdk-event-get-event-type event)
  (enum->symbol %gdk-event-type
                (gdk_event_get_event_type event)))

;; From libg-golf

#;(define (gdk-event-get-changed-mask event)
  (let ((changed-mask (gdk_event_get_changed_mask event)))
    (case changed-mask
      ((0)
       #f) ;; the event does not implement the field
      (else
       (gi-integer->gflags %gdk-window-state changed-mask)))))

#;(define (gdk-event-get-new-window-state event)
  (let ((new-window-state (gdk_event_get_new_window_state event)))
    (case new-window-state
      ((0)
       #f) ;; the event does not implement the field
      (else
       (gi-integer->gflags %gdk-window-state new-window-state)))))


;;;
;;; Gdk Bindings
;;;

(define gdk_event_get_button
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_button"
				    %libgdk)
                      (list '*		;; event
                            '*)))	;; *button

(define gdk_event_get_click_count
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_click_count"
				    %libgdk)
                      (list '*		;; event
                            '*)))	;; *click_count

(define gdk_event_get_coords
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_coords"
				    %libgdk)
                      (list '*		;; event
                            '*		;; *x_win
                            '*)))	;; *y_win

(define gdk_event_get_keycode
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_keycode"
				    %libgdk)
                      (list '*		;; event
                            '*)))	;; *keycode

(define gdk_event_get_keyval
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_keyval"
				    %libgdk)
                      (list '*		;; event
                            '*)))	;; *keyval

(define gdk_event_get_root_coords
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_root_coords"
				    %libgdk)
                      (list '*		;; event
                            '*		;; *x_root
                            '*)))	;; *y_root

(define gdk_event_get_state
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_state"
				    %libgdk)
                      (list '*		;; event
                            '*)))	;; *state

(define gdk_event_get_time
  (pointer->procedure unsigned-int
                      (dynamic-func "gdk_event_get_time"
				    %libgdk)
                      (list '*)))	;; event

(define gdk_event_get_window
  (pointer->procedure '*
                      (dynamic-func "gdk_event_get_window"
				    %libgdk)
                      (list '*)))	;; event

(define gdk_event_get_event_type
  (pointer->procedure int
                      (dynamic-func "gdk_event_get_event_type"
				    %libgdk)
                      (list '*)))	;; event


;;;
;;; Types and Values
;;;

(define %gdk-event-type #f)
(define %gdk-window-state #f)

(eval-when (expand load eval)
  (let ((gdk-event-type
         (make <gi-enum>
           #:g-name  "GdkEventType"
           #:enum-set '((nothing . -1)
                        (delete . 0)
                        (destroy . 1)
                        (expose . 2)
                        (motion-notify . 3)
                        (button-press . 4)
                        (2button-press . 5)
                        (double-button-press . 5)
                        (3button-press . 6)
                        (triple-button-press . 6)
                        (button-release . 7)
                        (key-press . 8)
                        (key-release . 9)
                        (enter-notify . 10)
                        (leave-notify . 11)
                        (focus-change . 12)
                        (configure . 13)
                        (map . 14)
                        (unmap . 15)
                        (property-notify . 16)
                        (selection-clear . 17)
                        (selection-request . 18)
                        (selection-notify . 19)
                        (proximity-in . 20)
                        (proximity-out . 21)
                        (drag-enter . 22)
                        (drag-leave . 23)
                        (drag-motion . 24)
                        (drag-status . 25)
                        (drop-start . 26)
                        (drop-finished . 27)
                        (client-event . 28)
                        (visibility-notify . 29)
                        (scroll . 31)
                        (window-state . 32)
                        (setting . 33)
                        (owner-change . 34)
                        (grab-broken . 35)
                        (damage . 36)
                        (touch-begin . 37)
                        (touch-update . 38)
                        (touch-end . 39)
                        (touch-cancel . 40)
                        (touchpad-swipe . 41)
                        (touchpad-pinch . 42)
                        (pad-button-press . 43)
                        (pad-button-release . 44)
                        (pad-ring . 45)
                        (pad-strip . 46)
                        (pad-group-mode . 47)
                        (event-last . 48))))
        (gdk-window-state
         (make <gi-flag>
           #:g-name  "GdkWindowState"
           #:enum-set '((withdrawn . 1)
                        (iconified . 2)
                        (maximized . 4)
                        (sticky . 8)
                        (fullscreen . 16)
                        (above . 32)
                        (below . 64)
                        (focused . 128)
                        (tiled . 256)
                        (top-tiled . 512)
                        (top-resizable . 1024)
                        (right-tiled . 2048)
                        (right-resizable . 4096)
                        (bottom-tiled . 8192)
                        (bottom-resizable . 16384)
                        (left-tiled . 32768)
                        (left-resizable . 65536)))))

    (set! %gdk-event-type gdk-event-type)
    #;(gi-cache-set! 'enum 'gdk-event-type gdk-event-type)

    (set! %gdk-window-state gdk-window-state)
    #;(gi-cache-set! 'flag 'gdk-window-state gdk-window-state)))
