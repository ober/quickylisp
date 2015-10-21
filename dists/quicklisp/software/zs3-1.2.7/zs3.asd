;;;;
;;;; Copyright (c) 2008 Zachary Beane, All Rights Reserved
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;;
;;;;   * Redistributions of source code must retain the above copyright
;;;;     notice, this list of conditions and the following disclaimer.
;;;;
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;
;;;; zs3.asd

(asdf:defsystem #:zs3
  :depends-on (#:drakma
               #:cxml
               #:ironclad
               #:puri
               #:cl-base64)
  :version "1.2.7"
  :description "A Common Lisp library for working with Amazon's Simple
  Storage Service (S3) and CloudFront content delivery service."
  :author "Zach Beane <xach@xach.com>"
  :license "BSD"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "crypto")
               (:file "xml-binding")
               (:file "xml-output")
               (:file "credentials")
               (:file "post")
               (:file "redirects")
               (:file "request")
               (:file "response")
               (:file "objects")
               (:file "bucket-listing")
               (:file "errors")
               (:file "acl")
               (:file "logging")
               (:file "location")
               (:file "interface")
               (:file "lifecycle")
               (:file "cloudfront")))
