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

;;; Code:


(define-module (g-golf hl-api function)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (g-golf support)
  #:use-module (g-golf gi)
  #:use-module (g-golf glib)
  #:use-module (g-golf gobject)
  #:use-module (g-golf override)
  #:use-module (g-golf hl-api gtype)

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
            gi-import-flag
            gi-import-struct
            gi-import-union
            gi-import-interface))


(g-export describe	;; function and argument
          !g-name
          !name
          !type-desc

          !info		;; function
          !namespace
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

(define %gi-strip-boolean-result '())

(define (%i-func f-inst)
  (lambda ( . args)
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
             (apply values
                    (cons (return-value->scm f-inst)
                          (map arg-out->scm (!args-out f-inst))))))
          (case return-type
            ((void) (values))
            (else
             (return-value->scm f-inst)))))))

(define (%o-func f-inst i-func)
  (let* ((%import (@@ (g-golf hl-api import) gi-import-by-name))
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
                       (%import namespace name))))
            prereqs))
      (set! (!o-spec-pos f-inst) o-spec-pos)
      (primitive-eval o-proc))))

(define* (gi-import-function info #:key (force? #f))
  (let ((namespace (g-base-info-get-namespace info)))
    (when (or force?
              (not (is-namespace-import-exception? namespace)))
      (let* ((g-name (g-function-info-get-symbol info))
             (name (g-name->name g-name)))
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
  (let* ((parent (g-base-info-get-container (!info f-inst)))
         (type-tag (g-base-info-get-type parent)))
    (case type-tag
      ((interface
        object)
       (let* ((p-g-name (g-registered-type-info-get-type-name parent))
              (p-name (g-name->name p-g-name 'as-string))
              (n-drop (+ (string-length p-name) 1))
              (m-long-name (!name f-inst))
              (m-long-name-str (symbol->string m-long-name))
              (m-long-generic (gi-add-method-gf m-long-name))
              (m-short-name-str (string-drop m-long-name-str n-drop))
              (m-short-name (string->symbol m-short-name-str))
              (m-short-generic (gi-add-method-gf-sn m-short-name))
              (specializers (gi-add-method-specializers f-inst))
              (procedure (if (!override? f-inst)
                             (!o-func f-inst)
                             (!i-func f-inst))))
         (add-method! m-long-generic
                      (make <method>
                        #:specializers specializers
                        #:procedure procedure))
         (when m-short-generic
           (add-method! m-short-generic
                        (make <method>
                          #:specializers specializers
                          #:procedure procedure)))))
      (else
       (fallback f-inst)))))

(define* (gi-add-method-gf name #:optional (module #f))
  (let* ((module (or module
                     (resolve-module '(g-golf hl-api gobject))))
         (variable (module-variable module name))
         (value (and variable
                     (module-ref module name))))
    (if (and value
             (is-a? value <generic>))
        value
        (if value
            (begin
              (module-replace! module `(,name))
              (let ((gf (make <generic> #:name name)))
                (module-set! module name gf)
                (add-method! gf
                             (make <method>
                               #:specializers <top>
                               #:procedure (lambda ( . args)
                                             (apply value args))))
                gf))
            (begin
              (module-export! module `(,name))
              (let ((gf (make <generic> #:name name)))
                (module-set! module name gf)
                gf))))))

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
                   ((object)
                    (if gi-type gi-type <top>))
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
  (g-name #:accessor !g-name)
  (name #:accessor !name)
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
    (let* ((namespace (g-base-info-get-namespace info))
           (g-name (g-function-info-get-symbol info))
           (name (g-name->name g-name))
           (override? (gi-override? g-name))
           (flags (g-function-info-get-flags info))
           (is-method? (gi-function-info-is-method? info flags))
           (return-type-info (g-callable-info-get-return-type info))
           (return-type (g-type-info-get-tag return-type-info))
           (type-desc (type-description return-type-info #:type-tag return-type)))
      (g-base-info-unref return-type-info)
      (receive (n-arg args
                n-gi-arg-in args-in gi-args-in gi-args-in-bv
                n-gi-arg-out args-out gi-args-out gi-args-out-bv)
          (function-arguments-and-gi-arguments info is-method? override?)
        (mslot-set! self
                    'info info
                    'namespace namespace
                    'g-name g-name
                    'name name
                    'override? override?
                    'flags flags
                    'is-method? is-method?
                    'n-arg n-arg
                    'caller-owns (g-callable-info-get-caller-owns info)
                    'return-type return-type
                    'type-desc type-desc
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
        (function-finalizer self)))))

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
  (forced-type #:accessor !forced-type #:init-keyword #:forced-type)
  (string-pointer #:accessor !string-pointer)
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
              (direction (g-arg-info-get-direction info))
              (type-info (g-arg-info-get-type info))
              (type-tag (g-type-info-get-tag type-info))
              (type-desc (type-description type-info #:type-tag type-tag))
              (is-pointer? (g-type-info-is-pointer type-info))
              (forced-type (arg-info-forced-type direction type-tag is-pointer?)))
         (g-base-info-unref type-info)
         (mslot-set! self
                     'g-name g-name
                     'name name
                     'closure (g-arg-info-get-closure info)
                     'destroy (g-arg-info-get-destroy info)
                     'direction direction
                     'transfert (g-arg-info-get-ownership-transfer info)
                     'scope (g-arg-info-get-scope info)
                     'type-tag type-tag
                     'type-desc type-desc
                     'forced-type forced-type
                     'is-pointer? is-pointer?
                     'may-be-null? (g-arg-info-may-be-null info)
                     'is-caller-allocate? (g-arg-info-is-caller-allocates info)
                     'is-optional? (g-arg-info-is-optional info)
                     'is-return-value? (g-arg-info-is-return-value info)
                     'is-skip? (g-arg-info-is-skip info)
                     ;; the gi-argument-in or/and gi-argument-out slots can
                     ;; only be set!  at the end of
                     ;; function-arguments-and-gi-arguments, which needs to
                     ;; proccess them all before it can compute their
                     ;; respective pointer address (or/and because an
                     ;; argument can be 'in, 'inout or 'out). See
                     ;; finalize-arguments-gi-argument-pointers.
                     'gi-argument-field (gi-type-tag->field forced-type)))))))

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
       (type-description-interface info))
      ((array)
       (type-description-array info))
      ((glist
        gslist)
       (type-description-glist info type-tag))
      (else
       type-tag))))

(define (type-description-interface info)
  (let* ((info (g-type-info-get-interface info))
         (type (g-base-info-get-type info)))
    (case type
      ((callback)
       (g-base-info-unref info)
       ;; skeleton - wip
       (list type #f #f #f #f))
      (else
       (if (is-registered? type)
           (receive (id name gi-type confirmed?)
               (registered-type->gi-type info type)
             (g-base-info-unref info)
             (list type name gi-type id confirmed?))
           (begin
             (g-base-info-unref info)
             type))))))

(define (type-description-array info)
  (let* ((type (g-type-info-get-array-type info))
         (fixed-size (g-type-info-get-array-fixed-size info))
         (is-zero-terminated (g-type-info-is-zero-terminated info))
         (param-n (g-type-info-get-array-length info))
         (param-type (g-type-info-get-param-type info 0))
         (param-tag (g-type-info-get-tag param-type)))
    (g-base-info-unref param-type)
    (list type
          fixed-size
          is-zero-terminated
          param-n
          param-tag)))

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
  (let* ((id (g-registered-type-info-get-g-type info))
         (g-name (g-type-name id))
         (name (g-name->name g-name)))
    (case type
      ((enum)
       (values id
               name
               (or (gi-cache-ref 'enum name)
                   (gi-import-enum info))
               #t))
      ((flags)
       (values id
               name
               (or (gi-cache-ref 'flag name)
                   (gi-import-flag info))
               #t))
      ((struct)
       (values id
               name
               (or (gi-cache-ref 'boxed name)
                   (gi-import-struct info))
               #t))
      ((union)
       (values id
               name
               (or (gi-cache-ref 'boxed name)
                   (gi-import-union info))
               #t))
      ((object)
       (let ((module (resolve-module '(g-golf hl-api object)))
             (c-name (g-name->class-name g-name)))
         ;; In the code below, it is necessary to make sure c-name has
         ;; been defined before to (maybe) get its value, because it
         ;; could be that c-name hasn't been defined yet, due to the
         ;; unspecified order in which <gobject> subclasses are being
         ;; imported, and methods defined. For example, which happened
         ;; while I was working on this, importing "Cutter", it appears
         ;; that <clutter-actor> may exists, and one of its methods
         ;; requiring, let's say, a <clutter-constraint> argument, but
         ;; the <clutter-constraint> class hasn't been imported yet, and
         ;; therefore its class name is unbound at the time
         ;; <clutter-actor> methods are being defined. Ultimately, all
         ;; classes will be defined of course, and the following
         ;; returned values updated, but only after the call to
         ;; gi-import is fully completed. Now, these returned values are
         ;; used to define (gi)arguments, and those are 'permanent'
         ;; (their definition is, not their value of course), and stored
         ;; in the arguments slot of <function> instances. Therefore, it
         ;; is possible to later permanently complete missing classes,
         ;; while processing (calling) those functions/methods, and only
         ;; as part of the first call.
         (values id
                 c-name
                 (and (module-variable module c-name)
                      (module-ref module c-name))
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
       (values id
               name
               (or (gi-cache-ref 'iface name)
                   (gi-import-interface info))
               #t))
      (else
       (values id name #f #f)))))

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
            (g-base-info-unref arg-info)
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

(define ($is-an-allow-none-exception? name)
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
                     (let ((f-val (gi-gflags->integer gi-type arg)))
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
                    ((object)
                     (gi-argument-set! gi-argument-in 'v-pointer
                                       (if arg
                                           (!g-inst arg)
                                           (if may-be-null?
                                               %null-pointer
                                               (error "Invalid arg: " arg)))))
                    ((interface)
                     ;; Interfaces (class implementation) is missing,
                     ;; but g-object subclasses that implement
                     ;; interface(s) may pass their instances as
                     ;; arguments to functions that expect an interface.
                     ;; For example a <gtk-list-store> instance is a
                     ;; valid argument to the
                     ;; gtk-tree-view-new-with-model function ...  This
                     ;; means that we need, till interface classes are
                     ;; fully implemented, to check if the argument the
                     ;; function is receiving is a pointer - then we
                     ;; pass it 'blindingly', or an instance, in which
                     ;; case we must pass its g-inst slot value (which
                     ;; holds a pointer to the GObject subclass
                     ;; instance).
                     (let ((foreign (cond ((not arg) %null-pointer)
                                          ((pointer? arg) arg)
                                          (else
                                           (!g-inst arg)))))
                       (gi-argument-set! gi-argument-in 'v-pointer foreign)))
                    ((callback)
                     (let ((name (!name arg-in)))
                       (if (not arg)
                           (if (or may-be-null?
                                   ($is-an-allow-none-exception? name))
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
                      (case param-tag
                        ((utf8
                          filename)
                         (gi-argument-set! gi-argument-in 'v-pointer
                                           (if is-zero-terminated
                                               (scm->gi-strings arg)
                                               (scm->gi-n-string arg
                                                                 (list-ref args
                                                                           (if is-method?
                                                                               (+ param-n 1)
                                                                               param-n))))))
                        ((gtype)
                         (gi-argument-set! gi-argument-in 'v-pointer
                                           (if is-zero-terminated
                                               (warning
                                                "Unimplemented (prepare args-in) scm->gi-gtypes."
                                                "")
                                               (scm->gi-n-gtype arg
                                                                (list-ref args
                                                                           (if is-method?
                                                                               (+ param-n 1)
                                                                               param-n))))))
                        (else
                         (warning "Unimplemented (prepare args-in) type - array;"
                                  (format #f "~S" type-desc))))))))
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
                         (let ((s32 (make-s32vector 1 0)))
                           (s32vector-set! s32 0 arg)
                           (gi-argument-set! gi-argument-in
                                             (gi-type-tag->field forced-type)
                                             (bytevector->pointer s32))))
                        (else
                         (warning "Unimplemeted (pointer to): " type-tag)))))
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
                             ((enum)
                              (gi-argument-set! gi-argument-out 'v-int -1))
                             ((flags)
                              (gi-argument-set! gi-argument-out 'v-int -1))
                             ((struct)
                              (case name
                                ((g-value)
                                 (gi-argument-set! gi-argument-out 'v-pointer
                                                   (g-value-new))) ;; an empty GValue
                                (else
                                 (gi-argument-set! gi-argument-out 'v-pointer
                                                   (cond ((!is-opaque? gi-type)
                                                          %null-pointer)
                                                         ((!is-semi-opaque? gi-type)
                                                          (bytevector->pointer
                                                           (make-bytevector (!size gi-type)
                                                                            0)))
                                                         (else
                                                          (make-c-struct
                                                           (!scm-types gi-type)
                                                           (!init-vals gi-type))))))))
                             ((object
                               interface)
                              (gi-argument-set! gi-argument-out 'v-pointer
                                                %null-pointer))))))
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
                       ((int32)
                        (gi-argument-set! gi-argument-out 'v-pointer
                                          (bytevector->pointer
                                           (make-s32vector 1 0))))
                       (else
                        ;; not sure, but this shouldn't arm.
                        (gi-argument-set! gi-argument-out 'v-ulong 0))))))
            (loop (+ i 1)))))))

(define (arg-out->scm argument)
  (let ((type-tag (!type-tag argument))
        (type-desc (!type-desc argument))
        (gi-argument (!gi-argument-out argument))
        (forced-type (!forced-type argument)))
    (gi-argument->scm type-tag
                      type-desc
                      gi-argument
                      argument		;; the type-desc instance 'owner'
                      #:forced-type forced-type)))

(define (return-value->scm function)
  (let ((type-tag (!return-type function))
        (type-desc (!type-desc function))
        (gi-argument (!gi-arg-result function)))
    (gi-argument->scm type-tag
                      type-desc
                      gi-argument
                      function)))	;; the type-desc instance 'owner'

(define* (gi-argument->scm type-tag type-desc gi-argument funarg
                           #:key (forced-type #f))
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
           (let ((val (gi-argument-ref gi-argument 'v-int)))
             (or (enum->symbol gi-type val)
                 (error "No such " name " value: " val))))
          ((flags)
           (let ((val (gi-argument-ref gi-argument 'v-int)))
             (gi-integer->gflags gi-type val)))
          ((struct)
           (let ((foreign (gi-argument-ref gi-argument 'v-pointer)))
             (case name
               ((g-value)
                (g-value-ref foreign))
               (else
                (if (or (!is-opaque? gi-type)
                        (!is-semi-opaque? gi-type))
                    foreign
                    (parse-c-struct foreign (!scm-types gi-type)))))))
          ((object)
           (let ((foreign (gi-argument-ref gi-argument 'v-pointer)))
             (case name
               ((<g-param>) foreign)
               (else
                (and foreign
                     ;; See the comment in registered-type->gi-type which
                     ;; describes the role of confirmed? in the pattern.
                     (if confirmed?
                         (make gi-type #:g-inst foreign)
                         (receive (class name g-type)
                             (g-object-find-class foreign)
                           (set! (!type-desc funarg)
                                 (list 'object name class g-type #t))
                           (make class #:g-inst foreign))))))))
          ((interface)
           (gi-argument-ref gi-argument 'v-pointer))))))
    ((array)
     (match type-desc
       ((array fixed-size is-zero-terminated param-n param-tag)
        (case param-tag
          ((utf8
            filename)
           (gi->scm (gi-argument-ref gi-argument 'v-pointer) 'strings))
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
                 (if confirmed?
                     (map (lambda (item)
                            (make gi-type #:g-inst item))
                       lst)
                     (match lst
                       ((x . rest)
                        (receive (class name g-type)
                            (g-object-find-class x)
                          (set! (!type-desc funarg)
                                (list 'object name class g-type #t))
                          (map (lambda (item)
                                 (make class #:g-inst item))
                            lst))))))
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
                 ((int32)
                  (let ((s32 (pointer->bytevector foreign (sizeof int))))
                    (s32vector-ref s32 0)))
                 (else
                  (warning "Unimplemeted (pointer to) type-tag: " type-tag))))))
       (else
        (gi-argument-ref gi-argument
                         (gi-type-tag->field type-tag)))))))

(define (g-object-find-class foreign)
  (let* ((module (resolve-module '(g-golf hl-api object)))
         (g-type (g-object-type foreign))
         (g-name (g-object-type-name foreign))
         (name (g-name->class-name g-name))
         (class (module-ref module name)))
    (values class name g-type)))


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

(define* (gi-import-flag info #:key (with-methods? #t) (force? #f))
  (gi-import-registered info
                        'flag
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
                        #:force? force?))

(define* (gi-import-interface info #:key (with-methods? #t) (force? #f))
  (gi-import-registered info
                        'interface
                        gi-interface-import
                        g-interface-info-get-n-methods
                        g-interface-info-get-method
                        #:with-methods? with-methods?
                        #:force? force?))

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
         (g-name (g-type-name g-type))
         (name (g-name->name g-name)))
    (or (gi-cache-ref type name)
        (let ((gi-type-inst (case type
                              ((flag)
                               (import-proc info #:flag #t))
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
