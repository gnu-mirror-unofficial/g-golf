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
              (gi-import-by-name "Gdk" name))
      '("Display"))

  (for-each (lambda (name)
              (gi-import-by-name "Gtk" name))
      '("Application"
        "ApplicationWindow"
        "Box"
        "Label"
        "Entry"
        "Button"
        "Image")))


(define (activate app)
  (let* ((cwd (getcwd))
         (display (gdk-display-get-default))
         (clipboard (get-clipboard display))
         (window (make <gtk-application-window>
                   #:title "Clipboard"
                   #:default-width 660
                   #:default-height 420
                   #:application app))
         (box (make <gtk-box>
                #:orientation 'vertical
                #:margin-top 24
                #:margin-start 24
                #:margin-bottom 24
                #:margin-end 24
                #:halign 'center
                #:valign 'center
                #:spacing 24))
         (title-1 (make <gtk-label>
                    #:label "Text"
                    #:halign 'start
                    #:css-classes '("title-2")))
         (box-1 (make <gtk-box>
                  #:orientation 'horizontal
                  #:halign 'center
                  #:spacing 24))
         (entry-from (make <gtk-entry>
                       #:placeholder-text "Type text to copy"))
         (copy-1 (make <gtk-button> #:label "Copy"))
         (entry-to (make <gtk-entry>))
         (paste-1 (make <gtk-button> #:label "Paste"))
         (title-2 (make <gtk-label>
                    #:label "Texture"
                    #:halign 'start
                    #:css-classes '("title-2")))
         (box-2 (make <gtk-box>
                  #:orientation 'horizontal
                  #:halign 'center
                  #:spacing 24))
         (g-file (g-file-new-for-path (string-append cwd
                                                     "/levitating-gnu.png")))
         (texture (gdk-texture-new-from-file g-file))
         (image-from (make <gtk-image>
                       #:pixel-size 96
                       #:paintable texture))
         (copy-2 (make <gtk-button>
                   #:label "Copy"
                   #:valign 'center))
         (image-to (make <gtk-image>
                     #:pixel-size 96
                     #:icon-name "image-missing"))
         (paste-2 (make <gtk-button>
                    #:label "Paste"
                    #:valign 'center)))

    (connect copy-1
             'clicked
             (lambda (b)
               (set-value clipboard (!text entry-from))))

    (connect paste-1
             'clicked
             (lambda (b)
               (let* ((content-provider (get-content clipboard))
                      (value (and content-provider
                                  (get-value content-provider))))
                 (and value
                      (if (is-a? value <string>)
                          (set-text entry-to value)
                          (warning "Can't paste, expecting a <string>, got " value))))))

    (connect copy-2
             'clicked
             (lambda (b)
               (set-value clipboard (!paintable image-from))))

    (connect paste-2
             'clicked
             (lambda (b)
               (let* ((content-provider (get-content clipboard))
                      (value (and content-provider
                                  (get-value content-provider))))
                 (and value
                      (if (is-a? value <gdk-texture>)
                          (set! (!paintable image-to) value)
                          (warning "Can't paste, expecting a <gdk-texture>, got " value))))))

    (set-child window box)
    (append box title-1)
    (append box box-1)
    (append box-1 entry-from)
    (append box-1 copy-1)
    (append box-1 entry-to)
    (append box-1 paste-1)
    (append box title-2)
    (append box box-2)
    (append box-2 image-from)
    (append box-2 copy-2)
    (append box-2 image-to)
    (append box-2 paste-2)
    (show window)))


(define (main args)
  (let ((app (make <gtk-application>
               #:application-id "org.gtk.example")))
    (connect app 'activate activate)
    (let ((status (g-application-run app (length args) args)))
      (exit status))))
