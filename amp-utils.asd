 
(defsystem "amp-utils"
  :description "My toolbelt."
  :version "0.0.1"
  :author "Albus M Piroglu <mattapiroglu@gmail.com>"
  :licence "MIT"
  :depends-on ("alexandria" "closer-mop" "cl-containers" "local-time" "osicat")
  :serial t
  :components ((:file "packages"
                :description "Creates the packages:
                                AMP-UTILS (UTILS)
                                AMP-PATHNAMES
                                AMP-SCRIPTS
                                AMP-DEV (DEV)
                                AMP: Package AMP to export all external symbols in this library. Instead of
                                     the multiple packages that this library creates, this is to have a single
                                     package to reach all items from one package.
                              ")

               (:file "dev-base"
                :description "Base dev helpers. Things like alternatives to defun, etc.")

               (:file "utils"
                :description "General utility collection. Code gathered form multiple places and added
                              as needed for other libraries.")

               (:file "pathnames"
                :description "Pathname helpers (probably) not found in some other libs.")

               (:file "io"
                :description "Some IO helpers")

               (:file "scripts"
                :description "")

               (:file "bottom-up-oc"
                :description "Help with explorative development, adding tests etc.")

               (:file "top-down-dev"
                :description "Tools to help with understanding an existing library or with building
                              an architecture code top-down. E.g. code visualisers / UML generators."))

;               (:file "ptolemy-sources-generic" :depends-on ("ptolemy-base"))))

  :in-order-to ((test-op (test-op :amp-utils/test)))
  )

(defsystem "amp-utils/test"
  :description "amp-utils tests."
  :version "0.1.0"
  :author "Albus M Piroglu <mattapiroglu@gmail.com>"
  :maintainer "Albus M Piroglu <mattapiroglu@gmail.com>"
  :licence "MIT"
  :depends-on ("amp-utils"
               "fiveam")
  :components ((:module "test"
                :pathname ""
                :serial t
                :components ((:file "packages-test")
                             (:file "utils-test")
                             (:file "bottom-up-oc-test")
                             (:file "top-down-dev-test"))))
  :perform (test-op (c d) 
                    (uiop/package:symbol-call :5am :run! :utils-test)
                    (uiop/package:symbol-call :5am :run! :amp-dev-tests)))

