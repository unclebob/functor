(ns functor.core-spec
  (:require [functor.core :refer :all]
            [speclj.core :refer :all]))

(describe "functor utilities"
  (context
    "expand-method-body"
    (it "expands an empty method"
      (should= [[] #{}] (expand-forms [])))
    (it "expands a constant"
      (should= [[1] #{}]
               (expand-forms [1])))
    (it "expands a setter"
      (should= [['(clojure.core/reset! x 1)] #{'x}]
               (expand-forms ['(<- x 1)])))
    (it "expands a function"
      (should= [['(inc 1)] #{}]
               (expand-forms ['(inc 1)])))
    (it "expands a function with a setter argument"
      (should= [['(do (clojure.core/reset! x 1))] #{'x}]
               (expand-forms ['(do (<- x 1))])))
    (it "expands a function with two setter arguments"
      (should= [['(do (clojure.core/reset! x 1) (clojure.core/reset! y 2))] #{'x 'y}]
               (expand-forms ['(do (<- x 1) (<- y 2))])))
    )
  (context "dereference vars"
    (it "does not dereference if there are no vars"
      (should= ['x] (dereference-vars ['x] #{})))
    (it "dereferences a simple var"
      (should= ['@v] (dereference-vars ['v] #{'v})))
    (it "dereferences vars deep in forms"
      (should= ['(x @y) '@r] (dereference-vars ['(x y) 'r] #{'y 'r})))
    (it "does not dereference the first argument of a setter."
      (should= ['(clojure.core/reset! x y)] (dereference-vars ['(clojure.core/reset! x y)] #{'x})))
    )
  )

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
      (macroexpand-1 '(functor ([x] (f 1) (g 2) (vector x v))
                               (f [y] (<- x y))
                               (g [y] (<- v (+ x y)))))))

  (it "generates functor with a setter method and a buried call"
    (should=
      '(clojure.core/fn [x]
         (clojure.core/let
           [v (atom nil)
            g (clojure.core/fn [y]
                (do (clojure.core/reset! v y)))]
           (g 1)
           1))
      (macroexpand-1 '(functor ([x] (g 1) 1)
                               (g [y] (do (<- v y)))))))

  (it "generates functor with a setter in the main body"
    (should=
      '(clojure.core/fn [x]
         (clojure.core/let
           [y (atom nil)]
           (clojure.core/reset! y x)))
      (macroexpand-1 '(functor ([x] (<- y x))))))

  (it "generates functor with dereferenced vars"
    (should=
      '(clojure.core/fn [x]
               (clojure.core/let
                 [y (atom nil)]
                 (clojure.core/reset! y x)
                 (clojure.core/deref y)))
      (macroexpand-1 '(functor ([x] (<- y x) y)))))
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

(def quad (functor
            ([a b c]
             (calc-discriminant)
             (if (neg? discriminant)
               :complex
               (do
                 (calc-x1)
                 (calc-x2)
                 [x1 x2])))
            (calc-discriminant
              [] (<- discriminant (- (* b b) (* 4 a c))))
            (calc-x1 [] (<- x1 (/ (+ (- b) (Math/sqrt @discriminant)) (* 2 a))))
            (calc-x2 [] (<- x2 (/ (- (- b) (Math/sqrt @discriminant)) (* 2 a))))
            ))

(prn (macroexpand-1 '(functor
                       ([a b c]
                        (<- discriminant (- (* b b) (* 4 a c)))
                        (if (neg? discriminant)
                          :complex
                          (do
                            (calc-x1)
                            (calc-x2)
                            [x1 x2])))
                       (calc-x1 [] (<- x1 (/ (+ (- b) (Math/sqrt discriminant)) (* 2 a))))
                       (calc-x2 [] (<- x2 (/ (- (- b) (Math/sqrt discriminant)) (* 2 a))))
                       )))

(defn quad-normal [a b c]
  (let [discriminant (- (* b b) (* 4 a c))]
    (if (neg? discriminant)
      :complex
      (let [sqrt-desc (Math/sqrt discriminant)
            x1 (/ (+ (- b) sqrt-desc) (* 2 a))
            x2 (/ (- (- b) sqrt-desc) (* 2 a))]
        [x1 x2]))))

(describe "functor execution"
  (it "executes functors"
    (should= 1 (degenerate))
    (should= 2 (one-method))
    (should= 2 (mean [1 2 3]))
    (should= :complex (quad 1 2 3))
    (should= [2.0 1.0] (quad 1 -3 2))))

(describe "ancillary"
  (it "computes quadratic solutions"
    (should= :complex (quad-normal 1 2 3))
    (should= [2.0 1.0] (quad-normal 1 -3 2))
    )
  )
