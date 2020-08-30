;; -*- mode: scheme; coding: utf-8 -*-

;;;;
;;;; Copyright (C) 2016 - 2020
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


(define-module (g-golf support module)
  #:export (re-export-public-interface))


(define-macro (re-export-public-interface . args)
  "Re-export the public interface of a module or modules. Invoked as
@code{(re-export-public-interface (mod1) (mod2)...)}."
  (if (null? args)
      '(if #f #f)
      `(begin
	 ,@(map (lambda (mod)
		  (or (list? mod)
		      (error "Invalid module specification" mod))
		  `(module-use! (module-public-interface (current-module))
				(resolve-interface ',mod)))
		args))))
