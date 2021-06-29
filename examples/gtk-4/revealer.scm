#! /bin/sh
# -*- mode: scheme; coding: utf-8 -*-
exec guile -e main -s "$0" "$@"
!#


(eval-when (expand load eval)
  (use-modules (oop goops))

  (default-duplicate-binding-handler
    '(merge-generics replace warn-override-core warn last))

  (use-modules (g-golf))

  (g-irepository-require "Gtk" #:version "4.0")
  (for-each (lambda (name)
              (gi-import-by-name "Gtk" name))
      '("Application"
        "ApplicationWindow"
        "Builder"
        "Revealer")))

(define (get-revealers builder)
  (let loop ((i 0)
             (result '()))
    (if (= i 10)
        (reverse! result)
        (let ((name (string-append "revealer"
                                   (number->string i))))
          (loop (+ i 1)
                (cons (get-object builder name)
                      result))))))

(define (change-direction revealer)
  (when (get-mapped revealer)
    (set-reveal-child revealer
                      (not (get-child-revealed revealer)))))

(define (activate app)
  (let ((builder (make <gtk-builder>))
        (ui (string-append (getcwd) "/ui/revealer.ui")))
    (case (add-from-file builder ui)
      ((0)
       (error "<gtk-builder> - add-from-file failed: " ui))
      (else
       (letrec* ((window (get-object builder "window"))
                 (revealers (get-revealers builder))
                 (count 0))
         (g-timeout-add 690
                        (lambda ()
                          (let ((revealer (list-ref revealers count)))
                            (set-reveal-child revealer #t)
                            (connect revealer
                                     'notify::child-revealed
                                     (lambda (r param)
                                       (change-direction r)))
                            (set! count (1+ count))
                            (if (>= count 9)
                                #f
                                #t))))
         (add-window app window)
         (show window))))))

(define (main args)
  (let ((app (make <gtk-application>
               #:application-id "org.gtk.example")))
    (connect app 'activate activate)
    (let ((status (g-application-run app (length args) args)))
      (exit status))))
