#|
  This file is a part of CL-Locale package.
  URL: http://github.com/arielnetworks/cl-locale
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  CL-Locale is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage cl-locale-syntax
  (:use :cl)
  (:import-from :cl-locale.reader
                :locale-syntax-reader))
(in-package :cl-locale-syntax)

(syntax:define-package-syntax :cl-locale
  (:merge :standard)
  (:dispatch-macro-char #\# #\i #'locale-syntax-reader))
