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
        "HeaderBar"
        "SearchBar"
        "SearchEntry"
        "Box"
        "Label"
        "ToggleButton")))


(define (activate app)
  (let* ((window (make <gtk-application-window>
                   #:title "Search Bar"
                   #:default-width 400
                   #:default-height 400
                   #:application app))
         (header-bar (make <gtk-header-bar>))
         (search-button (make <gtk-toggle-button>
                          #:icon-name "system-search-symbolic"))
         (box (make <gtk-box>
                #:orientation 'vertical
                #:spacing 6))
         (search-bar (make <gtk-search-bar>
                       #:valign 'start
                       #:key-capture-widget window))
         (search-entry (make <gtk-search-entry>
                         #:hexpand #t))
         (label  (make <gtk-label>
                   #:label "Type to start search"
                   #:hexpand #t
                   #:vexpand #t
                   #:halign 'center
                   #:valign 'center
                   #:css-classes '("large-title"))))

    (bind-property search-button "active"
                   search-bar "search-mode-enabled"
                   '(sync-create bidirectional))

    (connect search-entry
             'search-started
             (lambda (widget)
               (set-active search-button #t)))

    (connect search-entry
             'stop-search
             (lambda (widget)
               (set-active search-button #f)))

    (connect search-entry
             'search-changed
             (lambda (widget)
               (set-text label (!text search-entry))))

    (set-titlebar window header-bar)
    (pack-end header-bar search-button)
    (set-child window box)
    (set-child search-bar search-entry)
    (append box search-bar)
    (append box label)
    (show window)))


(define (main args)
  (let ((app (make <gtk-application>
               #:application-id "org.gtk.example")))
    (connect app 'activate activate)
    (let ((status (g-application-run app (length args) args)))
      (exit status))))
