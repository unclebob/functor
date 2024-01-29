(ns functor.core-spec
  (:require [functor.core :refer :all]
            [speclj.core :refer :all]))

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
         (clojure.core/let [g (clojure.core/fn [y])]
           1))
      (macroexpand-1 '(functor ([x] 1) (g [y])))))

  (it "generates functor with two degenerate methods and some calls"
    (should=
      '(clojure.core/fn [x]
         (clojure.core/let [g (clojure.core/fn [y])
                            h (clojure.core/fn [z])]
           (g 1)
           (h 2)
           1))
      (macroexpand-1 '(functor ([x] (g 1) (h 2) 1)
                               (g [y])
                               (h [z])))))

  (it "generates functor with a setter method and a call"
    (should=
      '(clojure.core/fn [x]
         (clojure.core/let
           [v (atom nil)
            g (clojure.core/fn [y]
                (clojure.core/reset! v y))]
           (g 1)
           1))
      (macroexpand-1 '(functor ([x] (g 1) 1)
                               (g [y] (<- v y))))))

  (it "generates functor with two setter methods and a call"
    (should=
      '(clojure.core/fn [x]
         (clojure.core/let
           [x (atom nil)
            v (atom nil)
            f (clojure.core/fn [y]
                  (clojure.core/reset! x y))
            g (clojure.core/fn [y]
                   (clojure.core/reset! v (+ @x y)))]
            (f 1)
            (g 2)
           (vector @x @v)))
      (macroexpand-1 '(functor ([x] (f 1) (g 2) (vector @x @v))
                               (f [y] (<- x y))
                               (g [y] (<- v (+ @x y)))))))
  )


(def degenerate (functor ([] 1)))
(def one-method (functor ([] (g 2) @v) (g [x] (<- v x))))
(def mean (functor
            ([ns]
             (make-sum)
             (/ @sum (count ns)))
            (make-sum []
                      (<- sum (reduce + ns)))
            ))

(def quad (functor
            ([a b c]
             (discriminant)
             (if (neg? @disc)
               :complex
               (do
                 (calc-x1)
                 (calc-x2)
                 [@x1 @x2])))
            (discriminant []
                          (<- b2  (* b b))
                          (<- disc  (- @b2 (* 4 a c))))
            (calc-x1 [] (<- x1 (/ (+ (- b) (Math/sqrt @disc)) (* 2 a))))
            (calc-x2 [] (<- x2 (/ (- (- b) (Math/sqrt @disc)) (* 2 a))))
            ))

(describe "functor execution"
  (it "executes functors"
    (should= 1 (degenerate))
    (should= 2 (one-method))
    (should= 2 (mean [1 2 3]))
    (should= :complex (quad 1 2 3))
    (should= [2.0 1.0] (quad 1 -3 2))
    ))
