(ns functor.core-spec
  (:require [functor.core :refer :all]
            [speclj.core :refer :all]))

(describe "functor"
  (it "calculates mean"
    (should= 6 (mean [3 6 9]))))

(describe "functor-macro"
  (it "generates empty functor"
    (should=
      '(clojure.core/fn [x])
      (macroexpand-1 '(functor ([x])))))

  (it "generates degenerate functor"
    (should=
      '(clojure.core/fn [x] 1)
      (macroexpand-1 '(functor ([x] 1)))))

  (it "generates functor with one degenerate subfunction"
    (should=
      '(clojure.core/fn [x]
         (clojure.core/let [g (clojure.core/fn [this y] this)
                            this {}]
           1))
      (macroexpand-1 '(functor ([x] 1) (g [y])))))

  (it "generates functor with two degenerate methods and some calls"
    (should=
      '(clojure.core/fn [x]
         (clojure.core/let [g (clojure.core/fn [this y] this)
                            h (clojure.core/fn [this z] this)
                            this {}
                            this (g this 1)
                            this (h this 2)]
           this))
      (macroexpand-1 '(functor ([x] (g 1) (h 2) this)
                               (g [y])
                               (h [z])))))

  (it "generates functor with a setter method and a call"
    (should=
      '(clojure.core/fn [x]
         (clojure.core/let
           [g (clojure.core/fn [this y]
                (clojure.core/let
                  [this (clojure.core/assoc this :v y)]
                  this))
            this {}
            this (g this 1)
            {:keys [v]} this]
           v))
      (macroexpand-1 '(functor ([x] (g 1) v)
                               (g [y] (<- v y))))))
  )

(describe "functor utilities"
  (context "expanding functor body"
    (it "expands a degenerate body"
      (should= [[] [1]] (expand-functor-body {} [1])))
    (it "should expand method calls and body"
      (should= [['this '(g this 1)] ['this]]
               (expand-functor-body {:method-names #{'g}} ['(g 1) 'this])))
    )

  (context "expanding methods"
    (it "expands degenerate method"
      (should= ['this #{}] (expand-method-body {} [])))

    (it "should expand a setter"
      (should= ['(clojure.core/let [this (clojure.core/assoc this :v x)] this)
                #{'v}]
               (expand-method-body {} ['(<- v x)])))
    )
  )

(def degenerate (functor ([] 1)))
(def one-method (functor ([] (g 2) v) (g [x] (<- v x))))

(prn 'one-method (macroexpand-1 '(functor ([] (g 2) v) (g [x] (<- v x)))))

(describe "functor execution"
  (it "executes functors"
    (should= 1 (degenerate))
    (should= 2 (one-method))))
