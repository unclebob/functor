(ns functor.core-spec
  (:require [functor.core :refer :all]
            [speclj.core :refer :all]))

;(describe "functor"
;  (it "calculates mean"
;    (should= 6 (mean [3 6 9])))
;  )

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
                  [{:keys [v] :as this} (clojure.core/assoc this :v y)]
                  this))
            this {}
            this (g this 1)
            {:keys [v]} this]
           v))
      (macroexpand-1 '(functor ([x] (g 1) v)
                               (g [y] (<- v y))))))

  (it "generates functor with two setter methods and a call"
    (should=
      '(clojure.core/fn [x]
         (clojure.core/let
           [f (clojure.core/fn [this x]
                (clojure.core/let
                  [{:keys [x] :as this} (clojure.core/assoc this :x x)]
                  this))
            g (clojure.core/fn [this y]
                (clojure.core/let
                  [{:keys [x]} this
                   {:keys [x v] :as this} (clojure.core/assoc this :v (+ x y))]
                  this))
            this {}
            this (f this 1)
            this (g this 2)
            {:keys [x v]} this]
           (vector x v)))
      (macroexpand-1 '(functor ([x] (f 1) (g 2) (vector x v))
                               (f [x] (<- x x))
                               (g [y] (<- v (+ x y)))))))
  )

(describe "functor utilities"
  (context "expanding functor body"
    (it "expands a degenerate body"
      (should= [[] [1]] (expand-functor-body {} [1])))
    (it "should expand method calls and simple body"
      (should= [['this '(g this 1)] ['this]]
               (expand-functor-body {:method-names #{'g}} ['(g 1) 'this])))
    (it "should expand method calls and function call body"
      (should= [['this '(g this 1)] ['(f x)]]
               (expand-functor-body {:method-names #{'g}} ['(g 1) '(f x)])))
    )

  (context "expanding methods"
    (it "expands degenerate method"
      (should= ['this #{}] (expand-method-body {} [])))

    (it "expands a setter"
      (should= ['(clojure.core/let [{:keys [v] :as this} (clojure.core/assoc this :v x)] this)
                #{'v}]
               (expand-method-body {} ['(<- v x)])))

    (it "expands a setter with a var"
      (should= ['(clojure.core/let
                   [
                    {:keys [x]} this
                    {:keys [x v] :as this} (clojure.core/assoc this :v x)] this)
                #{'v}]
               (expand-method-body {:vars #{'x}} ['(<- v x)])))
    )
  )

(def degenerate (functor ([] 1)))
(def one-method (functor ([] (g 2) v) (g [x] (<- v x))))
(def mean (functor
            ([ns]
             (make-sum)
             (/ sum (count ns)))
            (make-sum []
                      (<- sum (reduce + ns)))
            ))

(prn 'one-method (macroexpand-1 '(functor ([] (g 2) v) (g [x] (<- v x)))))
(prn 'mean (macroexpand-1 '(functor
                             ([ns]
                              (make-sum)
                              (/ sum (count ns)))
                             (make-sum []
                                       (<- sum (reduce + ns)))
                             )))

(def quad (functor
            ([a b c]
             (discriminant)
             (calc-x1)
             (calc-x2)
             (if (neg? disc) 'complex
                             [x1 x2]))
            (discriminant []
                          (<- b2  (* b b))
                          (<- disc  (- b2 (* 4 a c))))
            (calc-x1 [] (<- x1 (/ (+ (- b) (Math/sqrt disc)) (* 2 a))))
            (calc-x2 [] (<- x2 (/ (- (- b) (Math/sqrt disc)) (* 2 a))))
            ))

(prn 'quad (macroexpand-1 '(functor
            ([a b c]
             (discriminant)
             (calc-x1)
             (calc-x2)
             (if (neg? disc) 'complex
                             [x1 x2]))
            (discriminant []
                          (<- b2  (* b b))
                          (<- disc  (- b2 (* 4 a c))))
            (calc-x1 [] (<- x1 (/ (+ (- b) (Math/sqrt disc)) (* 2 a))))
            (calc-x2 [] (<- x2 (/ (- (- b) (Math/sqrt disc)) (* 2 a))))
            )))

(describe "functor execution"
  (it "executes functors"
    (should= 1 (degenerate))
    (should= 2 (one-method))
    (should= 2 (mean [1 2 3]))
    (should= 'complex (quad 1 2 3))
    (should= [2.0 1.0] (quad 1 -3 2))))
