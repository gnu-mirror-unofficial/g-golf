;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2019 - 2021
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


(define-module (g-golf hl-api function)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-4)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf gi)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)
  #:use-module (g-golf override)
  #:use-module (g-golf hl-api gtype)
  #:use-module (g-golf hl-api utils)

  #:duplicates (merge-generics
		replace
		warn-override-core
		warn
		last)

  #:export (%gi-import-namespace-exceptions
            is-namespace-import-exception?
            %gi-strip-boolean-result
            gi-import-function
            %gi-method-short-names-skip
            <function>
            <argument>
            gi-import-enum
            gi-import-flags
            gi-import-struct
            gi-import-union))


(g-export describe	;; function and argument
          !g-name
          !name
          !type-desc
          !array-type-desc
          !bv-cache
          !bv-cache-ptr

          !info		;; function
          !namespace
          !m-name
          !c-name
          !override?
          !i-func
          !o-func
          !o-spec-pos
          !flags
          !is-method?
          !n-arg
          !caller-owns
          !return-type
          !may-return-null?
          !arguments
          !n-gi-arg-in
          !args-in
          !gi-args-in
          !gi-args-in-bv
          !n-gi-arg-out
          !args-out
          !gi-args-out
          !gi-args-out-bv
          !gi-arg-result

          !closure	;; argument
          !destroy
          !direction
          !transfert
          !scope
          !type-tag
          !forced-type
          !string-pointer
          !is-pointer?
          !may-be-null?
          !is-caller-allocate?
          !is-optional?
          !is-return-value?
          !is-skip?
          !arg-pos
          !gi-argument-in
          !gi-argument-in-bv-pos
          !gi-argument-out
          !gi-argument-out-bv-pos
          !gi-argument-field)


;;;
;;; 
;;;

(define %gi-import-namespace-exceptions
  '("Glib"
    "GObject"))

(define (is-namespace-import-exception? namespace)
  (member namespace
          %gi-import-namespace-exceptions
          string=?))

(define %gi-strip-boolean-result
  '())

(define (%i-func f-inst)
  (lambda args
    (let ((f-inst f-inst)
          (info (!info f-inst))
          (name (!name f-inst))
          (return-type (!return-type f-inst))
          (n-gi-arg-in (!n-gi-arg-in f-inst))
          (gi-args-in (!gi-args-in f-inst))
          (n-gi-arg-out (!n-gi-arg-out f-inst))
          (gi-args-out (!gi-args-out f-inst))
          (gi-arg-result (!gi-arg-result f-inst)))
      (prepare-gi-arguments f-inst args)
      (with-gerror g-error
                   (g-function-info-invoke info
                                           gi-args-in
                                           n-gi-arg-in
			                   gi-args-out
                                           n-gi-arg-out
			                   gi-arg-result
                                           g-error))
      (if (> n-gi-arg-out 0)
          (case return-type
            ((boolean)
             (if (memq name
                       %gi-strip-boolean-result)
                 (if (return-value->scm f-inst)
                     (apply values
                            (map arg-out->scm (!args-out f-inst)))
                     (error " " name " failed."))
                 (apply values
                        (cons (return-value->scm f-inst)
                              (map arg-out->scm (!args-out f-inst))))))
            ((void)
             (apply values
                    (map arg-out->scm (!args-out f-inst))))
            (else
             (let ((args-out (map arg-out->scm (!args-out f-inst))))
               (apply values
                      (cons (return-value->scm f-inst #:args-out args-out)
                            args-out)))))
          (case return-type
            ((void) (values))
            (else
             (return-value->scm f-inst)))))))

(define (%o-func f-inst i-func)
  (let* ((%gi-import-by-name (@ (g-golf hl-api import) gi-import-by-name))
         (namespace (!namespace f-inst))
         (n-name (string->symbol (string-downcase namespace)))
         (m-name `(g-golf override ,n-name))
         (o-module (resolve-module m-name #:ensure #f))
         (f-name (!name f-inst))
         (o-name (string-append (symbol->string f-name) "-ov"))
         (o-func-ref (module-ref o-module
                                 (string->symbol o-name))))
    (receive (prereqs o-proc o-spec-pos)
        (o-func-ref i-func)
      (when prereqs
        (for-each (lambda (prereq)
                    (match prereq
                      ((namespace name)
                       (%gi-import-by-name namespace name))))
            prereqs))
      (set! (!o-spec-pos f-inst) o-spec-pos)
      (primitive-eval o-proc))))

(define* (gi-import-function info #:key (force? #f))
  (let ((namespace (g-base-info-get-namespace info)))
    (when (or force?
              (not (is-namespace-import-exception? namespace)))
      (receive (name short-name c-name namespace shadows?)
          (gi-function-info-names info namespace)
        (or (gi-cache-ref 'function name)
            (let ((f-inst (make <function> #:info info)))
              ;; Do not (g-base-info-unref info) - unref the function
              ;; info - it is needed by g-function-info-invoke.
              (gi-cache-set! 'function name f-inst)
              (if (!is-method? f-inst)
                  (gi-add-method f-inst gi-add-procedure)
                  (gi-add-procedure f-inst))
              f-inst))))))

(define (gi-add-procedure f-inst)
  (let ((module (resolve-module '(g-golf hl-api function)))
        (name (!name f-inst)))
    (module-g-export! module `(,name))
    (if (!override? f-inst)
        (module-set! module name (!o-func f-inst))
        (module-set! module name (!i-func f-inst)))))

(define (gi-add-method f-inst fallback)
  (let* ((info (!info f-inst))
         (parent (g-base-info-get-container info))
         (type-tag (g-base-info-get-type parent)))
    (case type-tag
      ((interface
        object)
       (let* ((m-long-name (!name f-inst))
              (m-long-generic (gi-add-method-gf m-long-name))
              (m-short-name (!m-name f-inst))
              (m-short-generic (gi-add-method-gf-sn m-short-name))
              (specializers (gi-add-method-specializers f-inst))
              (procedure (if (!override? f-inst)
                             (!o-func f-inst)
                             (!i-func f-inst))))
         (gi-add-methods m-long-generic
                         m-short-generic
                         specializers
                         procedure)))
      (else
       (fallback f-inst)))))

(define (gi-add-methods m-long-generic
                        m-short-generic
                        specializers
                        procedure)
  (for-each (lambda (xp-spec)
              (add-method! m-long-generic
                           (make <method>
                             #:specializers xp-spec
                             #:procedure procedure))
              (when m-short-generic
                (add-method! m-short-generic
                             (make <method>
                               #:specializers xp-spec
                               #:procedure procedure))))
      (explode specializers)))

#!

The gi-add-method-gf code now uses ensure-generic, when its name
argument is find to be bound to a procedure. It should have used it in
the first place, but I've looked at the code and ensure-generic actually
does a better job then what I wrote, because it checks if the procedure
is a procedure-with-setter?, and returns a <generic-with-setter>
instance, otherwise, it returns a <generic> instance.

Nonetheless, I'll keep the code I wrote as an example of 'manually'
promoting a procedure (with no setter) to a generic function, adding a
method with its 'old' definition.

  ...
  (else
   (module-replace! module `(,name))
   (let ((gf (make <generic> #:name name)))
     (module-set! module name gf)
     (add-method! gf
                  (make <method>
                    #:specializers <top>
                    #:procedure (lambda ( . args)
                                  (apply value args))))
     gf))

!#

(define* (gi-add-method-gf name #:optional (module #f))
  (let* ((g-golf (resolve-module '(g-golf)))
         (module (or module
                     (resolve-module '(g-golf hl-api gobject))))
         (variable (module-variable module name))
         (value (and variable
                     (variable-bound? variable)
                     (variable-ref variable)))
         (names `(,name)))
    (if value
        (cond ((generic? value)
               value)
              ((macro? value)
               (gi-add-method-gf (syntax-name->method-name name)
                                 module))
              (else
               (module-replace! module names)
               (re-export-and-replace-names! g-golf names)
               (let ((gf (ensure-generic value name)))
                 (module-set! module name gf)
                 gf)))
        (begin
          (module-export! module names)
          (let ((gf (make <generic> #:name name)))
            (module-set! module name gf)
            gf)))))


(define %gi-method-short-names-skip
  '())

(define* (gi-add-method-gf-sn name #:optional (module #f))
  (let* ((%skip? (cond ((list? %gi-method-short-names-skip)
                        (memq name %gi-method-short-names-skip))
                       ((eq? %gi-method-short-names-skip 'all)
                        #t)
                       (else
                        #f))))
    (if %skip?
        #f
        (gi-add-method-gf name module))))

(define (gi-add-method-specializers f-inst)
  (let ((arguments (!arguments f-inst))
        (o-spec-pos (!o-spec-pos f-inst)))
    (map (lambda (argument)
           (case (!type-tag argument)
             ((interface
               object)
              (match (!type-desc argument)
                ((type name gi-type g-type confirmed?)
                 (case type
                   ((object
                     interface)
                    (if (!may-be-null? argument)
                        (list gi-type <boolean>)
                        gi-type))
                   (else
                    <top>)))))
             (else
              <top>)))
      (if o-spec-pos
          (map (lambda (pos)
                 (list-ref arguments pos))
            o-spec-pos)
          (filter-map (lambda (argument)
                        (case (!direction argument)
                          ((in
                            inout)
                           argument)
                          (else
                           #f)))
              arguments)))))

(define-class <function> ()
  (info #:accessor !info)
  (namespace #:accessor !namespace)
  (name #:accessor !name)
  (m-name #:accessor !m-name)
  (c-name #:accessor !c-name)
  (override? #:accessor !override? #:init-value #f)
  (i-func #:accessor !i-func #:init-value #f)
  (o-func #:accessor !o-func #:init-value #f)
  (o-spec-pos #:accessor !o-spec-pos #:init-value #f)
  (flags #:accessor !flags)
  (is-method? #:accessor !is-method?)
  (n-arg #:accessor !n-arg)
  (caller-owns #:accessor !caller-owns)
  (return-type #:accessor !return-type)
  (type-desc #:accessor !type-desc)
  (array-type-desc #:accessor !array-type-desc)
  (bv-cache #:accessor !bv-cache #:init-value #f)
  (bv-cache-ptr #:accessor !bv-cache-ptr #:init-value #f)
  (may-return-null? #:accessor !may-return-null?)
  (arguments #:accessor !arguments)
  (n-gi-arg-in #:accessor !n-gi-arg-in)
  (args-in #:accessor !args-in)
  (gi-args-in #:accessor !gi-args-in)
  (gi-args-in-bv #:accessor !gi-args-in-bv)
  (n-gi-arg-out #:accessor !n-gi-arg-out)
  (args-out #:accessor !args-out)
  (gi-args-out #:accessor !gi-args-out)
  (gi-args-out-bv #:accessor !gi-args-out-bv)
  (gi-arg-result #:accessor !gi-arg-result))

(define-method (initialize (self <function>) initargs)
  (let ((info (or (get-keyword #:info initargs #f)
                  (error "Missing #:info initarg: " initargs))))
    (next-method self '())
    (receive (name m-name c-name namespace shadows?)
        (gi-function-info-names info)
      (let* ((override? (gi-override? c-name))
             (flags (g-function-info-get-flags info))
             (is-method? (gi-function-info-is-method? info flags))
             (return-type-info (g-callable-info-get-return-type info))
             (return-type (g-type-info-get-tag return-type-info)))
        (receive (type-desc array-type-desc)
            (type-description return-type-info #:type-tag return-type)
          (g-base-info-unref return-type-info)
          (receive (n-arg args
                          n-gi-arg-in args-in gi-args-in gi-args-in-bv
                          n-gi-arg-out args-out gi-args-out gi-args-out-bv)
              (function-arguments-and-gi-arguments info is-method? override?)
            (mslot-set! self
                        'info info
                        'namespace namespace
                        'name name
                        'm-name m-name
                        'c-name c-name
                        'override? override?
                        'flags flags
                        'is-method? is-method?
                        'n-arg n-arg
                        'caller-owns (g-callable-info-get-caller-owns info)
                        'return-type return-type
                        'type-desc type-desc
                        'array-type-desc array-type-desc
                        'may-return-null? (g-callable-info-may-return-null info)
                        'arguments args
                        'n-gi-arg-in n-gi-arg-in
                        'args-in args-in
                        'gi-args-in gi-args-in
                        'gi-args-in-bv gi-args-in-bv
                        'n-gi-arg-out n-gi-arg-out
                        'args-out args-out
                        'gi-args-out gi-args-out
                        'gi-args-out-bv gi-args-out-bv
                        'gi-arg-result (make-gi-argument))
            (function-finalizer self)))))))

(define (function-finalizer f-inst)
  (let ((i-func (%i-func f-inst)))
    (mslot-set! f-inst
                'i-func i-func
                'o-func (and (!override? f-inst)
                             (%o-func f-inst i-func)))))

#;(define-method* (describe (self <function>) #:key (port #t))
  (next-method self #:port port)
  (if (boolean? port)
      (newline)
      (newline port))
  (for-each (lambda (argument)
              (describe argument #:port port)
              (if (boolean? port)
                  (newline)
                  (newline port)))
      (!arguments self)))

(define-method (describe (self <function>))
  (next-method)
  (newline)
  (for-each (lambda (argument)
              (describe argument)
              (newline))
      (!arguments self)))

(define-class <argument> ()
  (g-name #:accessor !g-name #:init-keyword #:g-name)
  (name #:accessor !name #:init-keyword #:name)
  (closure #:accessor !closure)
  (destroy #:accessor !destroy)
  (direction #:accessor !direction #:init-keyword #:direction)
  (transfert #:accessor !transfert)
  (scope #:accessor !scope)
  (type-tag #:accessor !type-tag #:init-keyword #:type-tag)
  (type-desc #:accessor !type-desc #:init-keyword #:type-desc)
  (array-type-desc #:accessor !array-type-desc)
  (forced-type #:accessor !forced-type #:init-keyword #:forced-type)
  (string-pointer #:accessor !string-pointer)
  (bv-cache #:accessor !bv-cache #:init-value #f)
  (bv-cache-ptr #:accessor !bv-cache-ptr #:init-value #f)
  (is-pointer? #:accessor !is-pointer? #:init-keyword #:is-pointer?)
  (may-be-null? #:accessor !may-be-null? #:init-keyword #:may-be-null?)
  (is-caller-allocate? #:accessor !is-caller-allocate?)
  (is-optional? #:accessor !is-optional?)
  (is-return-value? #:accessor !is-return-value?)
  (is-skip? #:accessor !is-skip?)
  (arg-pos #:accessor !arg-pos #:init-keyword #:arg-pos #:init-value -1)
  (gi-argument-in #:accessor !gi-argument-in #:init-value #f)
  (gi-argument-in-bv-pos #:accessor !gi-argument-in-bv-pos #:init-value #f)
  (gi-argument-out #:accessor !gi-argument-out #:init-value #f)
  (gi-argument-out-bv-pos #:accessor !gi-argument-out-bv-pos #:init-value #f)
  (gi-argument-field #:accessor !gi-argument-field #:init-keyword #:gi-argument-field))

(define-method (initialize (self <argument>) initargs)
  (let ((info (or (get-keyword #:info initargs #f)
                  (error "Missing #:info initarg: " initargs))))
    (case info
      ((instance)
       (receive (split-kw split-rest)
           (split-keyword-args (list #:info) initargs)
         (next-method self split-rest)))
      (else
       (next-method self '())
       (let* ((g-name (g-base-info-get-name info))
              (name (g-name->name g-name))
              (closure (g-arg-info-get-closure info))
              (destroy (g-arg-info-get-destroy info))
              (direction (g-arg-info-get-direction info))
              (transfert (g-arg-info-get-ownership-transfer info))
              (scope (g-arg-info-get-scope info))
              (type-info (g-arg-info-get-type info))
              (type-tag (g-type-info-get-tag type-info))
              (is-pointer? (g-type-info-is-pointer type-info))
              (may-be-null? (g-arg-info-may-be-null info))
              (is-caller-allocate? (g-arg-info-is-caller-allocates info))
              (is-optional? (g-arg-info-is-optional info))
              (is-return-value? (g-arg-info-is-return-value info))
              (is-skip? (g-arg-info-is-skip info))
              (forced-type (arg-info-forced-type direction type-tag is-pointer?)))
         (receive (type-desc array-type-desc)
             (type-description type-info #:type-tag type-tag)
           (g-base-info-unref type-info)
           (g-base-info-unref info)
           (mslot-set! self
                       'g-name g-name
                       'name name
                       'closure closure
                       'destroy destroy
                       'direction direction
                       'transfert transfert
                       'scope scope
                       'type-tag type-tag
                       'type-desc type-desc
                       'array-type-desc array-type-desc
                       'forced-type forced-type
                       'is-pointer? is-pointer?
                       'may-be-null? may-be-null?
                       'is-caller-allocate? is-caller-allocate?
                       'is-optional? is-optional?
                       'is-return-value? is-return-value?
                       'is-skip? is-skip?
                       ;; the gi-argument-in and gi-argument-out slots can
                       ;; only be set! at the end of
                       ;; function-arguments-and-gi-arguments, which needs
                       ;; to proccess them all before it can compute their
                       ;; respective pointer address (see
                       ;; finalize-arguments-gi-argument-pointers).
                       'gi-argument-field (gi-type-tag->field forced-type))))))))

#;(define-method* (describe (self <argument>) #:key (port #t))
  (next-method self #:port port))

(define (arg-info-forced-type direction type-tag is-pointer?)
  (if (or is-pointer?
          (eq? direction 'inout)
          (eq? direction 'out))
      'pointer
      type-tag))

(define* (type-description info #:key (type-tag #f))
  (let ((type-tag (or type-tag
                      (g-type-info-get-tag info))))
    (case type-tag
      ((interface)
       (values (type-description-interface info)
               #f))
      ((array)
       (type-description-array info))
      ((glist
        gslist)
       (values (type-description-glist info type-tag)
               #f))
      (else
       (values type-tag
               #f)))))

(define (type-description-interface info)
  (let* ((iface-info (g-type-info-get-interface info))
         (iface-type (g-base-info-get-type iface-info)))
    (case iface-type
      ((callback)
       (g-base-info-unref iface-info)
       ;; skeleton - wip
       (list iface-type #f #f #f #f))
      (else
       (if (is-registered? iface-type)
           (receive (id name gi-type confirmed?)
               (registered-type->gi-type iface-info iface-type)
             (g-base-info-unref iface-info)
             (list iface-type name gi-type id confirmed?))
           (begin
             (g-base-info-unref iface-info)
             iface-type))))))

(define (type-description-array info)
  (let* ((type (g-type-info-get-array-type info))
         (fixed-size (g-type-info-get-array-fixed-size info))
         (is-zero-terminated (g-type-info-is-zero-terminated info))
         (param-n (g-type-info-get-array-length info))
         (param-type (g-type-info-get-param-type info 0))
         (param-tag (g-type-info-get-tag param-type)))
    (case param-tag
      ((interface)
       (let ((i-desc (type-description-interface param-type)))
         (g-base-info-unref param-type)
         (values (list type
                       fixed-size
                       is-zero-terminated
                       param-n
                       param-tag)
                 i-desc)))
      (else
       (g-base-info-unref param-type)
       (values (list type
                     fixed-size
                     is-zero-terminated
                     param-n
                     param-tag)
               param-tag)))))

(define (type-description-glist info type-tag)
  (let* ((param-type (g-type-info-get-param-type info 0))
         (param-tag (g-type-info-get-tag param-type))
         (is-pointer? (g-type-info-is-pointer param-type)))
    (case param-tag
      ((interface)
       (let ((i-desc (type-description-interface param-type)))
         (g-base-info-unref param-type)
         i-desc))
      (else
       (g-base-info-unref param-type)
       (list param-tag
             #f
             #f
             is-pointer?)))))

(define (registered-type->gi-type info type)
  (let* ((g-type (g-registered-type-info-get-g-type info))
         (g-name (gi-registered-type-info-name info))
         (name (g-name->name g-name)))
    (case type
      ((enum)
       (values g-type
               name
               (or (gi-cache-ref 'enum name)
                   (gi-import-enum info))
               #t))
      ((flags)
       (values g-type
               name
               (or (gi-cache-ref 'flags name)
                   (gi-import-flags info))
               #t))
      ((struct)
       (values g-type
               name
               (or (gi-cache-ref 'boxed name)
                   (gi-import-struct info))
               #t))
      ((union)
       (values g-type
               name
               (or (gi-cache-ref 'boxed name)
                   (gi-import-union info))
               #t))
      ((object)
       (let* ((module (resolve-module '(g-golf hl-api gobject)))
              (c-name (g-name->class-name g-name))
              (c-inst (or (and (module-variable module c-name)
                               (module-ref module c-name))
                          ((@ (g-golf hl-api object) gi-import-object) info))))
         (values g-type
                 c-name
                 c-inst
                 ;; we can't rely on GI to tell us, at import time, the
                 ;; exact class name of the returned instance. As an
                 ;; example, at import time, the WebKit2 typelib pretend
                 ;; that webkit-web-view-new returned value signature
                 ;; says the returned value is a GtkWidget instance
                 ;; (which by the way is not even instantiable), but it
                 ;; should say it is a WebKitWebView.
                 ;; So, below, a boolean, initialized to #f, which
                 ;; indicates, to those procedures that will refer to
                 ;; this type-spec if it has been confirmed - that is,
                 ;; if c-name here above is equal to calling
                 ;; g-object-type-name on the instance pointer
                 ;; returned by a function call that uses this
                 ;; type-spec.
                 #f)))
      ((interface)
       (let* ((module (resolve-module '(g-golf hl-api gobject)))
              (c-name (g-name->class-name g-name))
              (c-inst (or (and (module-variable module c-name)
                               (module-ref module c-name))
                          ((@ (g-golf hl-api object) gi-import-interface) info))))
         (values g-type
                 c-name
                 c-inst
                 #t)))
      (else
       (values g-type name #f #f)))))

(define (is-registered? type-tag)
  (member type-tag
          '(enum
            flags
            interface
            object
            struct
            union)))

(define (function-arguments-and-gi-arguments info is-method? override?)
  (let* ((n-arg (g-callable-info-get-n-args info))
         (args (if is-method?
                   (list (make-instance-argument info))
                   '())))
    (let loop ((i 0)
               (arg-pos (length args))
               (arguments args)
               (n-gi-arg-in (length args))
               (args-in args)
               (n-gi-arg-inout 0)
               (n-gi-arg-out 0)
               (args-out '()))
      (if (= i n-arg)
          (let* ((arguments (reverse arguments))
                 (args-in (reverse args-in))
                 (args-out (reverse args-out))
                 (gi-args-in-bv (if (> n-gi-arg-in 0)
                                    (make-bytevector (* %gi-argument-size
                                                        n-gi-arg-in)
                                                     0)
                                    #f))
                 (gi-args-in (if gi-args-in-bv
                                 (bytevector->pointer gi-args-in-bv)
                                 %null-pointer))
                 (gi-args-out-bv (if (> n-gi-arg-out 0)
                                     (make-bytevector (* %gi-argument-size
                                                         n-gi-arg-out)
                                                      0)
                                     #f))
                 (gi-args-out (if gi-args-out-bv
                                  (bytevector->pointer gi-args-out-bv)
                                  %null-pointer)))
            (when gi-args-in-bv
              (finalize-arguments-gi-argument-pointers args-in
                                                       gi-args-in-bv
                                                       !gi-argument-in))
            (when gi-args-out-bv
              (finalize-arguments-gi-argument-pointers args-out
                                                       gi-args-out-bv
                                                       !gi-argument-out))
            (values (if is-method? (+ n-arg 1) n-arg)
                    arguments
                    n-gi-arg-in
                    args-in
                    gi-args-in
                    gi-args-in-bv
                    n-gi-arg-out
                    args-out
                    gi-args-out
                    gi-args-out-bv))
          (let* ((arg-info (g-callable-info-get-arg info i))
                 (argument (make <argument> #:info arg-info)))
            (case (!direction argument)
              ((in)
               (mslot-set! argument
                           'arg-pos (if override?
                                        arg-pos
                                        (+ (- arg-pos n-gi-arg-out) n-gi-arg-inout))
                           'gi-argument-in-bv-pos n-gi-arg-in)
               (loop (+ i 1)
                     (+ arg-pos 1)
                     (cons argument arguments)
                     (+ n-gi-arg-in 1)
                     (cons argument args-in)
                     n-gi-arg-inout
                     n-gi-arg-out
                     args-out))
              ((inout)
               (mslot-set! argument
                           'arg-pos (if override?
                                        arg-pos
                                        (+ (- arg-pos n-gi-arg-out) n-gi-arg-inout))
                           'gi-argument-in-bv-pos n-gi-arg-in
                           'gi-argument-out-bv-pos n-gi-arg-out)
               (loop (+ i 1)
                     (+ arg-pos 1)
                     (cons argument arguments)
                     (+ n-gi-arg-in 1)
                     (cons argument args-in)
                     (+ n-gi-arg-inout 1)
                     (+ n-gi-arg-out 1)
                     (cons argument args-out)))
              ((out)
               (mslot-set! argument
                           'arg-pos arg-pos
                           'gi-argument-out-bv-pos n-gi-arg-out)
               (loop (+ i 1)
                     (+ arg-pos 1)
                     (cons argument arguments)
                     n-gi-arg-in
                     args-in
                     n-gi-arg-inout
                     (+ n-gi-arg-out 1)
                     (cons argument args-out)))))))))

(define (finalize-arguments-gi-argument-pointers args gi-args-bv gi-argument-acc)
  (let loop ((args args)
             (i 0))
    (match args
      (() #t)
      ((arg . rest)
       (set! (gi-argument-acc arg)
             (bytevector->pointer gi-args-bv
                                  (* i %gi-argument-size)))
       (loop rest
             (+ i 1))))))

(define (prepare-gi-arguments function args)
  (let ((args-length (length args))
        (n-arg (!n-arg function))
        (n-arg-in (!n-gi-arg-in function))
        (override? (!override? function)))
    (if (or (and override?
                 (= args-length n-arg))
            (= args-length n-arg-in))
        (begin
          (prepare-gi-args-in function args)
          (prepare-gi-args-out function args))
        (error "Wrong number of arguments: " args))))

(define %allow-none-exceptions
  '(child-setup-data-destroy))

(define (%allow-none-exception? name)
  (memq name %allow-none-exceptions))

(define (prepare-gi-args-in function args)
  (let ((is-method? (!is-method? function))
        (n-gi-arg-in (!n-gi-arg-in function))
        (args-in (!args-in function)))
    (let loop ((i 0))
      (if (= i n-gi-arg-in)
          #t
          (let* ((arg-in (list-ref args-in i))
                 (arg-pos (!arg-pos arg-in))
                 (arg (list-ref args #;i arg-pos))
                 (type-tag (!type-tag arg-in))
                 (type-desc (!type-desc arg-in))
                 (is-pointer? (!is-pointer? arg-in))
                 (may-be-null? (!may-be-null? arg-in))
                 (forced-type (!forced-type arg-in))
                 (gi-argument-in (!gi-argument-in arg-in))
                 (field (!gi-argument-field arg-in)))
            ;; clearing the string pointer reference kept from a previous
            ;; call.
            (set! (!string-pointer arg-in) #f)
            (case type-tag
              ((interface)
               (match type-desc
                 ((type name gi-type g-type confirmed?)
                  (case type
                    ((enum)
                     (let ((e-val (enum->value gi-type arg)))
                       (if e-val
                           (gi-argument-set! gi-argument-in 'v-int e-val)
                           (error "No such symbol " arg " in " gi-type))))
                    ((flags)
                     (let ((f-val (flags->integer gi-type arg)))
                       (if f-val
                           (gi-argument-set! gi-argument-in 'v-int f-val)
                           (error "No such flag(s) " arg " in " gi-type))))
                    ((struct)
                     (case name
                       ((void
                         g-value)
                        ;; Struct for which the (symbol) name is void
                        ;; should be considerd opaque.
                        ;; Functions and methods that use GValue(s)
                        ;; should be overridden-ed/manually wrapped to
                        ;; initialize those g-value(s) - and here, arg
                        ;; is supposed to (always) be a valid pointer to
                        ;; an initialized GValue.
                        (gi-argument-set! gi-argument-in 'v-pointer arg))
                       (else
                        (gi-argument-set! gi-argument-in 'v-pointer
                                          (cond ((or (!is-opaque? gi-type)
                                                     (!is-semi-opaque? gi-type))
                                                 arg)
                                                (else
                                                 (make-c-struct (!scm-types gi-type)
                                                                arg)))))))
                    ((union)
                     (gi-argument-set! gi-argument-in 'v-pointer arg))
                    ((object
                      interface)
                     (gi-argument-set! gi-argument-in 'v-pointer
                                       (if arg
                                           (!g-inst arg)
                                           (if may-be-null?
                                               %null-pointer
                                               (error "Invalid arg: " arg)))))
                    ((callback)
                     (let ((name (!name arg-in)))
                       (if (not arg)
                           (if (or may-be-null?
                                   (%allow-none-exception? name))
                               (gi-argument-set! gi-argument-in 'v-pointer #f)
                               (error "Invalid callback argument: " name #f)))))))))
              ((array)
               (if (or (not arg)
                       (null? arg))
                   (if may-be-null?
                       (gi-argument-set! gi-argument-in 'v-pointer #f)
                       (error "Invalid array argument: " arg))
                   (match type-desc
                     ((array fixed-size is-zero-terminated param-n param-tag)
                      (let* ((param-n (case param-n
                                        ((-1) param-n)
                                        (else
                                         (if is-method? (+ param-n 1) param-n))))
                             (arg-n (list-ref args param-n)))
                      (case param-tag
                        ((utf8
                          filename)
                         (gi-argument-set! gi-argument-in 'v-pointer
                                           (if (or is-zero-terminated
                                                   (= arg-n -1))
                                               (scm->gi-strings arg)
                                               (scm->gi-n-string arg arg-n))))
                        ((gtype)
                         (gi-argument-set! gi-argument-in 'v-pointer
                                           (if (or is-zero-terminated
                                                   (= arg-n -1))
                                               (warning
                                                "Unimplemented (prepare args-in) scm->gi-gtypes."
                                                "")
                                               (scm->gi-n-gtype arg arg-n))))
                        (else
                         (warning "Unimplemented (prepare args-in) type - array;"
                                  (format #f "~S" type-desc)))))))))
              ((glist)
               (if (or (not arg)
                       (null? arg))
                   (if may-be-null?
                       (gi-argument-set! gi-argument-in 'v-pointer #f)
                       (error "Invalid glist argument: " arg))
                   (warning "Unimplemented type" (symbol->string type-tag))))
              ((gslist)
               (if (or (not arg)
                       (null? arg))
                   (if may-be-null?
                       (gi-argument-set! gi-argument-in 'v-pointer #f)
                       (error "Invalid gslist argument: " arg))
                   (match type-desc
                     ((type name gi-type g-type confirmed?)
                      (case type
                        ((object)
                         (gi-argument-set! gi-argument-in 'v-pointer
                                           (scm->gi-gslist (map !g-inst arg))))
                        (else
                         (warning "Unimplemented gslist subtype" type-desc)))))))
              ((ghash
                error)
               (if (not arg)
                   (if may-be-null?
                       (gi-argument-set! gi-argument-in 'v-pointer #f)
                       (error "Invalid " type-tag " argument: " arg))
                   (warning "Unimplemented type" (symbol->string type-tag))))
              ((utf8
                filename)
               ;; we need to keep a reference to string pointers,
               ;; otherwise the C string will be freed, which might happen
               ;; before the C call actually occurred.
               (if (not arg)
                   (if may-be-null?
                       (gi-argument-set! gi-argument-in 'v-pointer #f)
                       (error "Invalid " type-tag " argument: " #f))
                   (let ((string-pointer (string->pointer arg)))
                     (set! (!string-pointer arg-in) string-pointer)
                     ;; don't use 'v-string, which expects a string, calls
                     ;; string->pointer (and does not keep a reference).
                     (gi-argument-set! gi-argument-in 'v-pointer string-pointer))))
              (else
               ;; Here starts fundamental types. However, we still need to
               ;; check the forced-type slot-value, and when it is a
               ;; pointer, allocate mem for the type-tag, then set the
               ;; value and initialize the gi-argument to a pointer to the
               ;; alocated mem.
               (case forced-type
                 ((pointer)
                  (if (not arg)
                      (if may-be-null?
                          (gi-argument-set! gi-argument-in 'v-pointer #f)
                          (error "Invalid (pointer to) " type-tag " argument: " arg))
                      (case type-tag
                        ((int32)
                         (let* ((bv-cache (!bv-cache arg-in))
                                (s32 (or bv-cache
                                         (make-s32vector 1 0)))
                                (s32-ptr (or (!bv-cache-ptr arg-in)
                                             (bytevector->pointer s32))))
                           (unless bv-cache
                             (mslot-set! arg-in
                                         'bv-cache s32
                                         'bv-cache-ptr s32-ptr))
                           (s32vector-set! s32 0 arg)
                           (gi-argument-set! gi-argument-in
                                             (gi-type-tag->field forced-type)
                                             s32-ptr)))
                        (else
                         (warning "Unimplemented (pointer to): " type-tag)))))
                 (else
                  (gi-argument-set! gi-argument-in
                                    (gi-type-tag->field forced-type)
                                    arg)))))
            (loop (+ i 1)))))))

(define (prepare-gi-args-out function args)
  (let ((n-gi-arg-out (!n-gi-arg-out function))
        (args-out (!args-out function)))
    (let loop ((i 0))
      (if (= i n-gi-arg-out)
          #t
          (let ((arg-out (list-ref args-out i)))
            (cond ((eq? (!direction arg-out) 'inout)
                   ;; Then we 'merely' copy the content of the gi-argument-in
                   ;; to the gi-argument-out.
                   (let ((gi-argument-size %gi-argument-size)
                         (in-bv (!gi-args-in-bv function))
                         (in-bv-pos (!gi-argument-in-bv-pos arg-out))
                         (out-bv (!gi-args-out-bv function))
                         (out-bv-pos (!gi-argument-out-bv-pos arg-out)))
                     (bytevector-copy! in-bv
                                       (* in-bv-pos gi-argument-size)
                                       out-bv
                                       (* out-bv-pos gi-argument-size)
                                       gi-argument-size)))
                  ((!override? function)
                   ;; Then all 'out argument(s) must be provided, as a
                   ;; pointer, and what ever they point to must have
                   ;; been initialized - see (g-golf override gtk) for
                   ;; some exmples.
                   (let* ((arg-pos (!arg-pos arg-out))
                          (arg (list-ref args arg-pos)))
                     (gi-argument-set! (!gi-argument-out arg-out) 'v-pointer
                                       (scm->gi arg 'pointer))))
                  (else
                   (let* ((type-tag (!type-tag arg-out))
                          (type-desc (!type-desc arg-out))
                          (is-pointer? (!is-pointer? arg-out))
                          (may-be-null? (!may-be-null? arg-out))
                          (is-caller-allocate? (!is-caller-allocate? arg-out))
                          (forced-type (!forced-type arg-out))
                          (gi-argument-out (!gi-argument-out arg-out))
                          (field (!gi-argument-field arg-out)))
                     (case type-tag
                       ((interface)
                        (match type-desc
                          ((type name gi-type g-type confirmed?)
                           (case type
                             ((enum
                               flags)
                              (let ((bv (make-bytevector (sizeof int) 0)))
                                (gi-argument-set! gi-argument-out 'v-pointer
                                                  (bytevector->pointer bv))))
                             ((struct)
                              (case name
                                ((g-value)
                                 (gi-argument-set! gi-argument-out 'v-pointer
                                                   (g-value-new))) ;; an empty GValue
                                (else
                                 (if is-caller-allocate?
                                     (let* ((bv (make-bytevector (!size gi-type) 0))
                                            (bv-ptr (bytevector->pointer bv)))
                                       (mslot-set! arg-out
                                                   'bv-cache bv
                                                   'bv-cache-ptr bv-ptr)
                                       (gi-argument-set! gi-argument-out 'v-pointer bv-ptr))
                                     (let ((bv (make-bytevector (sizeof '*) 0)))
                                       (mslot-set! arg-out
                                                   'bv-cache #f
                                                   'bv-cache-ptr %null-pointer)
                                       (gi-argument-set! gi-argument-out 'v-pointer
                                                         (bytevector->pointer bv)))))))

                             ((object
                               interface)
                              (if is-pointer?
                                  (let ((bv (make-bytevector (sizeof '*) 0)))
                                    (gi-argument-set! gi-argument-out 'v-pointer
                                                      (bytevector->pointer bv)))
                                  (gi-argument-set! gi-argument-out 'v-pointer
                                                    %null-pointer)))))))
                       ((array)
                        (match type-desc
                          ((array fixed-size is-zero-terminated param-n param-tag)
                           ;; (gi-argument-set! gi-argument-out 'v-pointer %null-pointer)
                           (warning "Unimplemented (prepare args-out) type - array;"
                                    (format #f "~S" type-desc)))))
                       ((glist
                         gslist
                         ghash
                         error)
                        (warning "Unimplemented type" (symbol->string type-tag))
                        (gi-argument-set! gi-argument-out 'v-pointer %null-pointer))
                       ((utf8
                         filename)
                        ;; not sure, but this shouldn't arm.
                        (gi-argument-set! gi-argument-out 'v-pointer %null-pointer))
                       ((boolean
                         int8 uint8
                         int16 uint16
                         int32 uint32
                         int64 uint64
                         float double
                         gtype)
                        (let* ((field (gi-type-tag->field type-tag))
                               (type (assq-ref %gi-argument-desc field))
                               (bv (make-bytevector (sizeof type) 0)))
                          (gi-argument-set! gi-argument-out 'v-pointer
                                            (bytevector->pointer bv))))
                       (else
                        ;; not sure, but this shouldn't arm.
                        (warning "Unimplemented type" (symbol->string type-tag))
                        (gi-argument-set! gi-argument-out 'v-ulong 0))))))
            (loop (+ i 1)))))))

(define (arg-out->scm argument)
  (let ((type-tag (!type-tag argument))
        (type-desc (!type-desc argument))
        (gi-argument (!gi-argument-out argument))
        (forced-type (!forced-type argument))
        (is-pointer? (!is-pointer? argument)))
    (gi-argument->scm type-tag
                      type-desc
                      gi-argument
                      argument		;; the type-desc instance 'owner'
                      #:forced-type forced-type
                      #:is-pointer? is-pointer?)))

(define* (return-value->scm function #:key (args-out #f))
  (let ((type-tag (!return-type function))
        (type-desc (!type-desc function))
        (gi-argument (!gi-arg-result function)))
    (gi-argument->scm type-tag
                      type-desc
                      gi-argument
                      function		;; the type-desc instance 'owner'
                      #:args-out args-out)))

(define %gdk-event-class
  (@ (g-golf gdk events) gdk-event-class))

(define* (gi-argument->scm type-tag type-desc gi-argument funarg
                           #:key (forced-type #f) (is-pointer? #f) (args-out #f))
  ;; forced-type is only used for 'inout and 'out arguments, in which
  ;; case it is 'pointer - see 'simple' types below.

  ;; funarg is the instance that owns the type-desc, which might need to
  ;; be updated - see the comment in the 'interface/'object section of
  ;; the code below, as well as the comment in registered-type->gi-type
  ;; which explains why/when this might happen.
  (case type-tag
    ((interface)
     (match type-desc
       ((type name gi-type g-type confirmed?)
        (case type
          ((enum)
           (let ((val (case forced-type
                        ((pointer)
                         (let* ((foreign (gi-argument-ref gi-argument 'v-pointer))
                                (bv (pointer->bytevector foreign (sizeof int))))
                           (s32vector-ref bv 0)))
                        (else
                         (gi-argument-ref gi-argument 'v-int)))))
             (or (enum->symbol gi-type val)
                 (error "No such " name " value: " val))))
          ((flags)
           (let ((val (case forced-type
                        ((pointer)
                         (let* ((foreign (gi-argument-ref gi-argument 'v-pointer))
                                (bv (pointer->bytevector foreign (sizeof int))))
                           (s32vector-ref bv 0)))
                        (else
                         (gi-argument-ref gi-argument 'v-int)))))
             (integer->flags gi-type val)))
          ((struct)
           (let* ((gi-arg-val (gi-argument-ref gi-argument 'v-pointer))
                  (foreign (if is-pointer?
                               (dereference-pointer gi-arg-val)
                               gi-arg-val)))
             (case name
               ((g-value)
                (g-value-ref foreign))
               (else
                (if (or (!is-opaque? gi-type)
                        (!is-semi-opaque? gi-type))
                    (let ((bv (!bv-cache funarg))
                          (bv-ptr (!bv-cache-ptr funarg)))
                      (if bv
                          (begin
                            (g-boxed-sa-guard bv-ptr bv)
                            bv-ptr)
                          ;; when bv is #f, it (indirectly) means that
                          ;; memory was allocated by the caller.
                          (if (null-pointer? foreign)
                              #f
                              (begin
                                (g-boxed-ga-guard foreign g-type)
                                foreign))))
                    (parse-c-struct foreign (!scm-types gi-type)))))))
          ((union)
           (let ((foreign (gi-argument-ref gi-argument 'v-pointer)))
             (case name
               ((gdk-event)
                 ;; This means that we are in gdk3/gtk3 environment,
                 ;; where the <gdk-event> class and accessors are (must
                 ;; be) defined dynamically - hence (gdk-event-class)
                (and foreign
                     (make (%gdk-event-class) #:event foreign)))
               (else
                foreign))))
          ((object)
           (let* ((gi-arg-val (gi-argument-ref gi-argument 'v-pointer))
                  (foreign (if is-pointer?
                               (dereference-pointer gi-arg-val)
                               gi-arg-val)))
             (case name
               ((<g-param>) foreign)
               (else
                (and foreign
                     (not (null-pointer? foreign))
                     (receive (class name g-type)
                         (g-object-find-class foreign)
                       ;; We used to update the funarg 'type-desc
                       ;; argument when it wasn't confirmed?, but that
                       ;; actually won't work anymore, see below [1] for
                       ;; a complete description. However, I'll keep the
                       ;; code, commented, for now, until I clear all
                       ;; occurrences of the confirmed? pattern entries.
                       #;(unless confirmed?
                         (set! (!type-desc funarg)
                               (list 'object name class g-type #t)))
                       (make class #:g-inst foreign)))))))
          ((interface)
           (let ((foreign (gi-argument-ref gi-argument 'v-pointer)))
             (and foreign
                  (make gi-type #:g-inst foreign))))))))
    ((array)
     (match type-desc
       ((array fixed-size is-zero-terminated param-n param-tag)
        (case param-tag
          ((utf8
            filename)
           (gi->scm (gi-argument-ref gi-argument 'v-pointer) 'strings))
          ((gtype)
           (let ((array-ptr (gi-argument-ref gi-argument 'v-pointer)))
             (if is-zero-terminated
                 (gi->scm array-ptr 'gtypes)
                 (gi->scm array-ptr 'n-gtype (list-ref args-out param-n)))))
          (else
           (warning "Unimplemented (arg-out->scm) type - array;"
                    (format #f "~S" type-desc)))))))
    ((glist
      gslist)
     (let* ((g-first (gi-argument-ref gi-argument 'v-pointer))
            (lst (gi->scm g-first type-tag)))
       (if (null? lst)
           lst
           (match type-desc
             ((type name gi-type g-type confirmed?)
              (case type
                ((object)
                 (match lst
                   ((x . rest)
                    (receive (class name g-type)
                        (g-object-find-class x)
                      (map (lambda (item)
                             (make class #:g-inst item))
                        lst)))))
                (else
                 (warning "Unprocessed g-list/g-slist"
                          (format #f "~S" type-desc))
                 lst)))))))
    ((ghash
      error)
     (warning "Unimplemented type" (symbol->string type-tag)))
    ((utf8
      filename)
     ;; not sure, but this shouldn't arm.
     (gi->scm (gi-argument-ref gi-argument 'v-pointer) 'string))
    ((gtype)
     (let ((val (gi-argument-ref gi-argument 'v-ulong)))
       (g-type->symbol val)))
    (else
     ;; Here starts 'simple' types, but we still need to check the
     ;; forced-type: when it is 'pointer (which happens for 'inout and
     ;; 'out arguments, not for returned values), the the gi-argument
     ;; holds a pointer to the value, otherwise, it holds the value.
     (case forced-type
       ((pointer)
        (let ((foreign (gi-argument-ref gi-argument 'v-pointer)))
          (and foreign
               (case type-tag
                 ((boolean
                   int8 uint8
                   int16 uint16
                   int32 uint32
                   int64 uint64
                   float double
                   gtype)
                  (let* ((field (gi-type-tag->field type-tag))
                         (type (assq-ref %gi-argument-desc field))
                         (bv (pointer->bytevector foreign (sizeof type)))
                         (acc (gi-type-tag->bv-acc type-tag))
                         (val (acc bv 0)))
                    (case type-tag
                      ((boolean)
                       (gi->scm val 'boolean))
                      (else
                       val))))
                 (else
                  (warning "Unimplemeted (pointer to) type-tag: " type-tag))))))
       (else
        (gi-argument-ref gi-argument
                         (gi-type-tag->field type-tag)))))))

;; [1]

;; Because g-object-find-class has been updated to define (sub)classes,
;; when that is necessary, that (a) are not defined in the (their
;; parent) namespace and (b) may differ from one call to another.

;; For example, a call to webkit-web-view-get-tls-info may return, for
;; it second 'out argument, a <g-tls-certificate-gnutls> instance, but
;; (a) "GTlsCertificateGnutls" is a runtime class - that is, undefined
;; in its corresponding namespace - subclass of "GTlsCertificate" and
;; (b) a subsequent call to webkit-web-view-get-tls-info could very well
;; return another certificate subclass type.

(define (g-object-find-class foreign)
  (let* ((module (resolve-module '(g-golf hl-api gobject)))
         (g-type (g-object-type foreign))
         (g-name (g-object-type-name foreign))
         (name (g-name->class-name g-name))
         (class-var (module-variable module name))
         (class (and class-var (module-ref module name))))
    (if class
        (values class name g-type)
        (let ((class (g-object-define-class g-type g-name name module)))
          (values class name g-type)))))

(define (g-object-define-class g-type g-name c-name module)
  (let* ((parent (g-type-parent g-type))
         (g-p-name (g-type-name parent))
         (p-name (g-name->class-name g-p-name))
         (p-class-var (module-variable module p-name))
         (p-class (and p-class-var (module-ref module p-name))))
    (if p-class
        (let ((public-i (module-public-interface module))
              (c-inst (make-class `(,p-class)
                                  '()
                                  #:name c-name)))
          (module-define! module c-name c-inst)
          (module-add! public-i c-name
                       (module-variable module c-name))
          c-inst)
        (error "Undefined (parent) class: " p-name))))


;;;
;;; Registered types
;;;

;; The gi-import-* procedures must always import the info - unlike I
;; thought when initially implementing an exception mechanism to avoid
;; to import from GLib and GObject - because they are called by
;; registered-type->gi-type, to the final aim of having any 'possible'
;; argument and returned value type being fully 'described', so 'in,
;; 'inout, 'out arguments, as well as returned value(s) may be correctly
;; encoded/decoded from/to their scheme representation.

;; So, here is a 'wip step' to achieve the above goal, but wrt to info
;; that are part of GLib and GObject, the import procs will only import
;; the info itself, not its methods (because they have been or should be
;; manually binded, see (g-golf glib) and (g-golf gobject) modules.

(define* (gi-import-enum info #:key (with-methods? #t) (force? #f))
  (gi-import-registered info
                        'enum
                        gi-enum-import
                        g-enum-info-get-n-methods
                        g-enum-info-get-method
                        #:with-methods? with-methods?
                        #:force? force?))

(define* (gi-import-flags info #:key (with-methods? #t) (force? #f))
  (gi-import-registered info
                        'flags
                        gi-enum-import
                        g-enum-info-get-n-methods
                        g-enum-info-get-method
                        #:with-methods? with-methods?
                        #:force? force?))

(define* (gi-import-struct info #:key (with-methods? #t) (force? #f))
  (gi-import-registered info
                        'struct
                        gi-struct-import
                        g-struct-info-get-n-methods
                        g-struct-info-get-method
                        #:with-methods? with-methods?
                        #:force? force?))

(define* (gi-import-union info #:key (with-methods? #t) (force? #f))
  (gi-import-registered info
                        'union
                        gi-union-import
                        g-union-info-get-n-methods
                        g-union-info-get-method
                        #:with-methods? with-methods?
                        #:force? force?)
  (when (and (string=? (g-base-info-get-namespace info) "Gdk")
             (string=? (g-base-info-get-name info) "Event")
             (string=? (g-irepository-get-version "Gdk") "3.0"))
    (let ((%gi-import-by-name (@ (g-golf hl-api import) gi-import-by-name)))
      (for-each (lambda (item)
                  (%gi-import-by-name "Gdk" item #:version "3.0"))
          '("ModifierType"
            "CrossingMode"
            "NotifyType"
            "keyval_name"))
    (gdk-event-class-redefine)
    (set! %gi-strip-boolean-result
          (append '(gdk-event-get-axis
                    gdk-event-get-button
                    gdk-event-get-click-count
                    gdk-event-get-coords
                    gdk-event-get-keycode
                    gdk-event-get-keyval
                    gdk-event-get-root-coords
                    gdk-event-get-scroll-direction
                    gdk-event-get-scroll-deltas
                    gdk-event-get-state)
                  %gi-strip-boolean-result)))))

(define* (gi-import-registered info
                               type
                               import-proc
                               import-n-method-proc
                               import-get-method-proc
                               #:key (with-methods? #t)
                               (force? #f))
  (let* ((namespace (g-base-info-get-namespace info))
         (with-methods? (if (is-namespace-import-exception? namespace)
                            #f
                            with-methods?))
         (g-type (g-registered-type-info-get-g-type info))
         (g-name (gi-registered-type-info-name info))
         (name (g-name->name g-name)))
    (or (gi-cache-ref type name)
        (let ((gi-type-inst (case type
                              ((flags)
                               (import-proc info #:flags #t))
                              ((union)
                               (import-union-1 info g-type g-name))
                              (else
                               (import-proc info)))))
          (gi-cache-set! (case type
                           ((struct union) 'boxed)
                           ((interface) 'iface)
                           (else type))
                         name
                         gi-type-inst)
          (when with-methods?
            (gi-import-registered-methods info
                                          import-n-method-proc
                                          import-get-method-proc
                                          #:force? force?))
          gi-type-inst))))

(define* (gi-import-registered-methods info
                                       import-n-method-proc
                                       import-get-method-proc
                                       #:key (force? #f))
  (let ((namespace (g-base-info-get-namespace info))
        (n-method (import-n-method-proc info)))
    (do ((i 0
            (+ i 1)))
        ((= i n-method))
      (let* ((m-info (import-get-method-proc info i))
             (name (g-function-info-get-symbol m-info)))
        ;; Some methods listed here are functions: (a) their flags is an
        ;; empty list; (b) they do not expect an additional instance
        ;; argument (their GIargInfo list is complete); (c) they have a
        ;; GIFuncInfo entry in the namespace (methods do not). We do not
        ;; (re)import those here.
        (unless (g-irepository-find-by-name namespace name)
          (gi-import-function m-info #:force? force?))))))

(define (import-union-1 info g-type name)
  (let ((fields
         (map (lambda (field)
                (match field
                  ((f-name f-type-info)
                   (let ((f-desc (type-description f-type-info)))
                     (g-base-info-unref f-type-info)
                     (list f-name f-desc)))))
           (gi-union-import info))))
    (make <gi-union>
      #:g-type g-type
      #:g-name name
      ;; #:name the initialize method does that
      #:size (g-union-info-get-size info)
      #:alignment (g-union-info-get-alignment info)
      #:fields fields
      #:is-discriminated? (g-union-info-is-discriminated? info)
      #:discriminator-offset (g-union-info-get-discriminator-offset info))))


;;;
;;; Method instance argument
;;;

(define (make-instance-argument info)
  (let* ((container (g-base-info-get-container info))
         (g-name (g-base-info-get-name container))
         (name (g-name->name g-name))
         (type (g-base-info-get-type container)))
    (receive (id r-name gi-type confirmed?)
        (registered-type->gi-type container type)
      (g-base-info-unref container)
      (make <argument>
        #:info 'instance
        #:g-name g-name
        #:name name
        #:direction 'in
        #:type-tag 'interface
        #:type-desc (list type r-name gi-type id confirmed?)
        #:forced-type 'pointer
        #:is-pointer? #t
        #:may-be-null? #f
        #:arg-pos 0 ;; always the first argument
        #:gi-argument-field 'v-pointer))))


;;;
;;; Gdk (gdk-event-class-redefine)
;;;

(define (gdk-event-class-redefine)
  (let* ((module (resolve-module '(g-golf hl-api gobject)))
         (public-i (module-public-interface module))
         (c-name '<gdk-event>)
         (c-inst (make-class `(,<object>)
                             (cons (%gdk-event-slot module public-i)
                                   ;; the accessors
                                   (gdk-event-virtual-slots module public-i))
                             #:name c-name)))
    (if (module-bound? module c-name)
        (class-redefinition (module-ref module c-name) c-inst)
        (begin
          (module-define! module c-name c-inst)
          (module-add! public-i c-name
                       (module-variable module c-name))))
    c-inst))

(define %gdk-event-slot
  (@@ (g-golf gdk events) gdk-event-slot))

(define (gdk-event-virtual-slots module public-i)
  (append (map (lambda (getter)
                 (gdk-event-virtual-slot getter module public-i))
            (gdk-event-getters))
          (gdk-event-additional-virtual-slots module public-i)))

(define (gdk-event-virtual-slot getter module public-i)
  (let* ((f-name (g-name->name getter))
         ;; 14 is the length of "gdk_event_get_"
         (slot-name (g-name->name (substring getter 14)))
         (a-name (symbol-append '! slot-name))
         (a-inst (if (module-variable module a-name)
                     (module-ref module a-name)
                     (let ((a-inst (make-accessor a-name)))
                       (module-define! module a-name a-inst)
                       (module-add! public-i a-name
                                    (module-variable module a-name))
                       a-inst)))
         (f-inst (gi-cache-ref 'function f-name))
         (procedure (if (!override? f-inst)
                        (!o-func f-inst)
                        (!i-func f-inst))))
    (make <slot>
      #:name slot-name
      #:accessor a-inst
      #:allocation #:virtual
      #:slot-ref (lambda (obj)
                   (procedure (slot-ref obj 'event)))
      #:slot-set! (lambda (obj val) (values)))))

(define (gdk-event-getters)
  (let* ((info (g-irepository-find-by-name "Gdk" "Event"))
         (n-method (g-union-info-get-n-methods info)))
    (let loop ((i 0)
               (results '()))
      (if (= i n-method)
          (reverse! results)
          (let* ((m-info (g-union-info-get-method info i))
                 (name (g-function-info-get-symbol m-info)))
            (loop (+ i 1)
                  (if (gdk-event-getter? name)
                      (cons name results)
                      results)))))))

(define (gdk-event-getter? name)
  (string-contains name "gdk_event_get_"))

(define %gdk-event-additional-virtual-slots
  `((keyname ,(lambda (obj)
                (let* ((module (resolve-module '(g-golf hl-api function)))
                       (keyname (module-ref module 'gdk-keyval-name))
                       (keyval (module-ref module 'gdk-event-get-keyval)))
                  (keyname (keyval (slot-ref obj 'event))))))
    (x ,(lambda (obj)
          (let* ((module (resolve-module '(g-golf hl-api function)))
                 (coords (module-ref module 'gdk-event-get-coords)))
            (receive (x-win y-win)
                (coords (slot-ref obj 'event))
              x-win))))
    (y ,(lambda (obj)
          (let* ((module (resolve-module '(g-golf hl-api function)))
                 (coords (module-ref module 'gdk-event-get-coords)))
            (receive (x-win y-win)
                (coords (slot-ref obj 'event))
              y-win))))
    (root-x ,(lambda (obj)
               (let* ((module (resolve-module '(g-golf hl-api function)))
                      (root-coords (module-ref module 'gdk-event-get-root-coords)))
                 (receive (x-root y-root)
                     (root-coords (slot-ref obj 'event))
                   x-root))))
    (root-y ,(lambda (obj)
               (let* ((module (resolve-module '(g-golf hl-api function)))
                      (root-coords (module-ref module 'gdk-event-get-root-coords)))
                 (receive (x-root y-root)
                     (root-coords (slot-ref obj 'event))
                   y-root))))))


(define (gdk-event-additional-virtual-slots module public-i)
  (map (lambda (slot-spec)
         (gdk-event-additional-virtual-slot slot-spec module public-i))
    %gdk-event-additional-virtual-slots))

(define (gdk-event-additional-virtual-slot slot-spec module public-i)
  (match slot-spec
    ((slot-name slot-ref-proc)
     (let* ((a-name (symbol-append '! slot-name))
            (a-inst (if (module-variable module a-name)
                        (module-ref module a-name)
                        (let ((a-inst (make-accessor a-name)))
                          (module-define! module a-name a-inst)
                          (module-add! public-i a-name
                                       (module-variable module a-name))
                          a-inst))))
       (make <slot>
         #:name slot-name
         #:accessor a-inst
         #:allocation #:virtual
         #:slot-ref slot-ref-proc
         #:slot-set! (lambda (obj val) (values)))))))
