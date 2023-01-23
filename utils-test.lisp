;; amp-utils/utils-test.lisp


(in-package #:amp-utils/test)
(in-suite :utils-test)


(test get-new-item-name-cases ()
      (is (equal (get-new-item-name '(a b c) 'c)
                 "C1"))

      (is (equal (get-new-item-name '(a b a1) 'a)
                 "A2"))

      (is (equal (get-new-item-name '(a32 b a a33) 'a)
                 "A34"))

      (is (equal (get-new-item-name '("a32" "b" "a" "a33") "a")
                 "a34"))

)


