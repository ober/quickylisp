(asdf:defsystem :daemon
  :version "0.0.3"
  :author "Masatoshi SANO"
  :description "Process daemonization for some common lisp."
  :licence "MIT"
  :components ((:file "daemon"))
  :depends-on (#+sbcl :sb-posix))
