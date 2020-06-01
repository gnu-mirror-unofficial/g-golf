;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2019
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

;; this file is partially a copy of (grip utils)
;; http://www.nongnu.org/grip/

;;; Code:


(define-module (g-golf support utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (system foreign)

  #:export (storage-get
	    storage-set
	    displayln
	    dimfi
	    warning
	    abort
	    and-l
	    identities
            flatten
            g-studly-caps-expand
	    %g-name-transform-exceptions
            %g-studly-caps-expand-token-exceptions
	    g-name->name
	    g-name->class-name
	    #;gi-class-name->method-name
            gi-type-tag->ffi
            gi-type-tag->init-val))


(define storage-get #f)
(define storage-set #f)

(let ((storage (list)))
  (set! storage-get
	(lambda (key)
	  (case key
	    ((all) storage)
	    (else
	     (assq-ref storage key)))))
  (set! storage-set
	(lambda (key value)
	  (set! storage
		(assq-set! storage key value)))))


(define* (displayln msg #:optional (port #f))
  (if port
      (begin
	(display msg port)
	(newline port))
      (begin
	(display msg)
	(newline))))

(define (dimfi . items)
  ;; if the first item is a port, we use it.
  (if (port? (car items))
      (let ((p (car items)))
	(display ";; " p)
	(for-each (lambda (item)
		    (display item p) (display " " p))
	    (cdr items))
	(display "\n" p))
      (begin
	(display ";; ")
	(for-each (lambda (item)
		    (display item) (display " " ))
	    items)
	(display "\n")))
  (car (last-pair items)))

(define* (warning what msg
                  #:key (msg-2 #f)
                  (port (current-output-port)))
  (display (string-append "Warning: " what ": " msg) port)
  (newline port)
  (when msg-2
    (display msg-2 port)
    (newline port)))

(define* (abort what msg
                #:key (msg-2 #f)
                (code -1)
                (port (current-output-port)))
  (display (string-append "ERROR: " what ": " msg) port)
  (newline port)
  (when msg-2
    (display msg-2 port)
    (newline port))
  (exit code))

(define (and-l ll)
  (if (null? ll)
      #t
      (if (car ll)
	  (and-l (cdr ll))
	  #f)))

(define (identities . args)
  args)

(define (flatten lst)
  (reverse! (let loop ((lst lst)
                       (result '()))
              (match lst
                (() result)
                ((x . rests)
                 (if (pair? x)
                     (loop rests (append (loop x '()) result))
                     (loop rests (cons x result))))))))


;;;
;;; Name Transformation
;;;

;; Initially based on Guile-Gnome (gobject gw utils), from which we keep
;; one algorithm - see caps-expand-token-2 - the original idea and
;; procedures have been enhanced to allow special treatment of any token
;; that compose the name to be transformed. A typical example of such a
;; need is from the WebKit2 namespace, where most users prefer that
;; class names use webkit- as their prefix, not web-kit. Up to the point
;; that we made this a default in G-Golf. Those who would prefer not to
;; do this may of course remove the assoc pair from
;; %g-studly-caps-expand-token-exceptions.

(define %g-name-transform-exceptions
  ;; Default name transformations can be overridden, but g-golf won't
  ;; define exceptions for now, let's see.
  '(("BLuefox" . "bluefox") ;; to test
    ;; ("GEnum" . "genum")  ;; no sure yet
    ("GObject" . "gobject")))

(define* (g-name->name g-name #:optional (as-string? #f))
  (let ((scm-name (or (assoc-ref %g-name-transform-exceptions g-name)
                      (g-studly-caps-expand g-name))))
    (if as-string?
        scm-name
        (string->symbol scm-name))))

(define (g-name->class-name g-name)
  (let ((scm-name (g-name->name g-name 'as-string)))
    (string->symbol (string-append "<"
                                   scm-name
                                   ">"))))

;; Not sure this is used but let's keep it as well
#;(define (gi-class-name->method-name class-name name)
  (let ((class-string (symbol->string class-name)))
    (string->symbol
     (string-append (substring class-string 1 (1- (string-length class-string)))
                    ":" (symbol->string name)))))

(define %g-studly-caps-expand-token-exceptions
  '(("WebKit" . "webkit")))

(define (g-studly-caps-expand name)
  (let loop ((tokens (string-split name #\_))
             (result '()))
    (match tokens
      (()
       (string-join (reverse! (flatten result)) "-"))
      ((token . rest)
       (loop rest
             (cons (caps-expand-token token)
                   result))))))

(define (caps-expand-token token)
  ;; that token might be an empty string may happen, if the name received
  ;; and split by the caller has a trailing character that is precisely
  ;; the character used to split.  In these cases, we wish a trailing
  ;; #\- to.
  (if (string-null? token)
      token
      (or (assoc-ref %g-studly-caps-expand-token-exceptions token)
          (caps-expand-token-1 token '()))))

(define (caps-expand-token-1 token subtokens)
  (if (string-null? token)
      subtokens
      (receive (idx exception)
          (any-caps-expand-token-exception token)
        (if exception
            (caps-expand-token-1 (substring token idx)
                                 (cons exception subtokens))
            (cons (caps-expand-token-2 token)
                  subtokens)))))

(define (caps-expand-token-2 token)
  (do ((idx (- (string-length token) 1)
	    (- idx 1)))
      ((> 1 idx)
       (string-downcase token))
    (cond ((and (> idx 2)
                (char-lower-case? (string-ref token (- idx 3)))
                (char-upper-case? (string-ref token (- idx 2)))
                (char-upper-case? (string-ref token (- idx 1)))
                (char-lower-case? (string-ref token idx)))
           (set! idx (- idx 1))
           (set! token
                 (string-append (substring token 0 (- idx 1))
                                "-"
                                (substring token (- idx 1)
                                           (string-length token)))))
          ((and (> idx 1)
                (char-upper-case? (string-ref token (- idx 1)))
                (char-lower-case? (string-ref token idx)))
           (set! token
                 (string-append (substring token 0 (- idx 1))
                                "-"
                                (substring token (- idx 1)
                                           (string-length token)))))
          ((and (char-lower-case? (string-ref token (- idx 1)))
                (char-upper-case? (string-ref token idx)))
           (set! token
                 (string-append (substring token 0 idx)
                                "-"
                                (substring token idx
                                           (string-length token))))))))

(define (any-caps-expand-token-exception token)
  (let loop ((exceptions %g-studly-caps-expand-token-exceptions))
    (match exceptions
      (()
       (values 0 #f))
      ((exception . rest)
       (match exception
         ((key . val)
          (if (string-prefix? key token)
              (values (string-length key) val)
              (loop rest))))))))


;;;
;;;
;;;

(define (gi-type-tag->ffi type-tag)
  (case type-tag
    ((void) void)
    ((boolean) int)
    ((int8
      uint8
      int16
      uint16
      int32
      uint32
      int64
      uint64
      float
      double)
     (eval type-tag (current-module)))
    ((gtype) unsigned-long)
    ((utf8
      filename
      array
      interface
      glist
      gslist
      ghash
      error)
     '*) ;; pointer
    ((unichar) uint32)
    (else
     (error "No such GI type tag: " type-tag))))


(define (gi-type-tag->init-val type-tag)
  "Returns the default init value (to initialize C struct) for
TYPE-TAG."
  (case type-tag
    ((utf8
      filename
      pointer			;; <- forced type
      array
      interface
      glist
      gslist
      ghash
      error)
     %null-pointer)
    (else
     0)))
