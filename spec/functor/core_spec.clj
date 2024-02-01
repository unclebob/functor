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
      (should= ['x] (dereference-vars #{} ['x])))
    (it "dereferences a simple var"
      (should= ['@v] (dereference-vars #{'v} ['v])))
    (it "dereferences vars deep in forms"
      (should= ['(x @y) '@r] (dereference-vars #{'y 'r} ['(x y) 'r])))
    (it "does not dereference the first argument of a setter."
      (should= ['(clojure.core/reset! x y)] (dereference-vars #{'x} ['(clojure.core/reset! x y)])))
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

(prn 'one-arg (macroexpand-1 '(functor ([x] x))))

(def mean (functor
            ([ns]
             (make-sum)
             (/ sum (count ns)))
            (make-sum []
                      (<- sum (reduce + ns)))
            ))

(prn 'mean (macroexpand-1 '(functor
                             ([ns]
                              (make-sum)
                              (/ sum (count ns)))
                             (make-sum []
                                       (<- sum (reduce + ns)))
                             )))

(defn quad-normal [a b c]
  (let [discriminant (- (* b b) (* 4 a c))]
    (cond
      (zero? a)
      (/ (- c) b)

      (zero? discriminant)
      (/ (- b) (* 2 a))

      (neg? discriminant)
      (let [i-sqrt-discriminant (Math/sqrt (- discriminant))
            c-x1 [(- b) i-sqrt-discriminant]
            c-x1 (map #(/ % (* 2 a)) c-x1)
            c-x2 [(- b) (- i-sqrt-discriminant)]
            c-x2 (map #(/ % (* 2 a)) c-x2)]
        [c-x1 c-x2])
      :else
      (let [sqrt-desc (Math/sqrt discriminant)
            x1 (/ (+ (- b) sqrt-desc) (* 2 a))
            x2 (/ (- (- b) sqrt-desc) (* 2 a))]
        [x1 x2]))))

(prn 'quad (macroexpand-1 '(functor
                             ([a b c]
                              (let [discriminant (- (* b b) (* 4 a c))]
                                (cond
                                  (zero? a)
                                  (/ (- c) b)

                                  (zero? discriminant)
                                  (/ (- b) (* 2 a))

                                  (neg? discriminant)
                                  (let [i-sqrt-discriminant (Math/sqrt (- discriminant))
                                        c-x1 [(- b) i-sqrt-discriminant]
                                        c-x1 (map (fn [x] (/ x (* 2 a))) c-x1)
                                        c-x2 [(- b) (- i-sqrt-discriminant)]
                                        c-x2 (map (fn [x] (/ x (* 2 a))) c-x2)]
                                    [c-x1 c-x2])
                                  :else
                                  (let [sqrt-desc (Math/sqrt discriminant)
                                        x1 (/ (+ (- b) sqrt-desc) (* 2 a))
                                        x2 (/ (- (- b) sqrt-desc) (* 2 a))]
                                    [x1 x2]))) ([a b c]
                                                )))))

(def quad
  (functor
    ([a b c]
     (<- discriminant (make-discriminant))
     (cond
       (zero? a) (linear)
       (zero? discriminant) (one-root)
       (neg? discriminant) (complex-roots)
       :else (real-roots)))

    (linear [] (/ (- c) b))
    (one-root [] (/ (- b) (* 2 a)))

    (over-2a [c] (map (fn [x] (/ x (* 2 a))) c))

    (complex-roots
      []
      (let [i-sqrt-discriminant (Math/sqrt (- discriminant))
            c-x1 (over-2a [(- b) i-sqrt-discriminant])
            c-x2 (over-2a [(- b) (- i-sqrt-discriminant)])]
        [c-x1 c-x2]))

    (real-roots
      []
      (let [sqrt-desc (Math/sqrt discriminant)
            x1 (/ (+ (- b) sqrt-desc) (* 2 a))
            x2 (/ (- (- b) sqrt-desc) (* 2 a))]
        [x1 x2]))

    (make-discriminant [] (- (* b b) (* 4 a c)))))

(describe "functor execution"
  (it "executes functors"
    (should= 1 (degenerate))
    (should= 2 (one-method))
    (should= 2 (mean [1 2 3]))
    (should= ['(-1 1.4142135623730951) '(-1 -1.4142135623730951)] (quad 1 2 3))
    (should= [2.0 1.0] (quad 1 -3 2))
    (should= -1 (quad 0 1 1))
    (should= 2 (quad 1 -4 4))))

(describe "ancillary"
  (it "computes quadratic solutions"
    (should= ['(-1 1.4142135623730951) '(-1 -1.4142135623730951)] (quad-normal 1 2 3))
    (should= [2.0 1.0] (quad-normal 1 -3 2))
    (should= -1 (quad-normal 0 1 1))
    (should= 2 (quad-normal 1 -4 4))
    )
  )
