(asdf:defsystem #:colordiff
  :description "A library for rendering colorful syntax highlighted diffs in terminal."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :depends-on (#:cl-colorist
               #:colorlisp)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "model")
                             (:file "highlighting")
                             (:file "diff")
                             (:file "source")
                             (:file "terminal"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:colordiff/tests))))

(asdf:defsystem #:colordiff/tests
  :description "Tests for Colordiff."
  :depends-on (#:colordiff)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:colordiff/tests '#:run-tests)))
