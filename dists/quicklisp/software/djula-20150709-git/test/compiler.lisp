(in-package #:djula-test)

(in-suite djula-test)

(test compiler
  (let ((fn (djula::compile-string
             "{% ifequal day \"Thursday\" %}
I never could get the hang of {{ day }}s.
{% endifequal %}")
          ))
    (is (string= "
I never could get the hang of Thursdays.
"
                 (djula::render-template* fn nil :day "Thursday")))))
