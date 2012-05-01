(ns fancy-split.core-test
  (:use clojure.test
        fancy-split.core))

(def mail01 {:to "foo@bar.com"
             :from "bar@bar.com"
             :subject "Foo Bar Baz"})

(defn- split [msg rule]
  ((compile-rule rule) msg))

(deftest evaluation
  (testing "Simple field splitting"
    (is (= #{"result"}
           (split mail01 '"result")))
    (is (= #{"result"}
           (split mail01 '(:to "foo@bar.com" "result")))))
  
  (testing "Regex field splitting"
    (is (= #{"result"}
           (split mail01 '(:to #".*@bar.com" "result"))))
    (is (= #{}
           (split mail01 '(:to #".*@baz.com" "result")))))
  
  ;; regexp with replacement
  ;; TODO

  (testing "Multiple field splitting"
    (is (= #{"result"}
           (split mail01 '([:to :from] "bar@bar.com" "result"))))
    (is (= #{"result"}
           (split mail01 '([:to :from] "foo@bar.com" "result"))))
    (is (= #{}
           (split mail01 '([:to :from] "baz@bar.com" "result"))))
    (is (= #{}
           (split mail01 '([:to :from] "Foo Bar Baz" "result"))))
    (is (= #{}
           (split mail01 '([] "baz@bar.com" "result")))))
  
  (testing "Any field splitting"
    (is (= #{"result"}
           (split mail01 '(any "foo@bar.com" "result"))))
    (is (= #{}
           (split mail01 '(any "foo@baz.com" "result")))))
  
  (testing "logical operations"
    (is (= #{"result"}
           (split mail01 '(| (:to "foo@bar.com" "result")
                             "default"))))
    (is (= #{"result" "default"}
           (split mail01 '(& (:to "foo@bar.com" "result")
                             "default"))))
    (is (= #{}
           (split mail01 '(:to "foo@baz.com" "result"))))
    (is (= #{"default"}
           (split mail01 '(| (:to "foo@baz.com" "result")
                             "default"))))
    (is (= #{"default"}
           (split mail01 '(& (:to "foo@baz.com" "result")
                             "default")))))

  ;; (testing "nested logical operations"
  ;;   (is false)     ;; TODO recursive combinations of logical operations
  ;; TODO especially test (| (& r1 r2 r3))
  ;;   )
  ;; 
  (testing "nil rules"
    (is (= #{}
           (split mail01 nil)))
    (is (= #{"result"}
           (split mail01 '(| nil "result"))))
    (is (= #{"result"}
           (split mail01 '(| "result" nil))))
    (is (= #{"result"}
           (split mail01 '(& nil "result"))))
    (is (= #{"result"}
           (split mail01 '(& "result" nil))))
    (is (= #{}
           (split mail01 '(| nil nil))))
    (is (= #{}
           (split mail01 '(& nil nil)))))

  (testing "function calling"
    (is (= #{"result"}
           (split mail01 (fn [m] (if (= (:to m) "foo@bar.com") ["result"] nil)))))
    (is (= #{}
           (split mail01 (fn [m] (if (= (:to m) "foo@baz.com") ["result"] nil)))))
    (is (= #{"result"}
           (split mail01 (fn [m] ["result"]))))
    ;; TODO return a more complex rule
    )
  
  ;; (testing "function threading"
  ;;   ;; TODO (-> rule func1 func2 ...)
  ;;   )
  )