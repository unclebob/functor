# functor
The functor macro is an experiment in improving the cleanliness and refactorability of Clojure programs. 

Have you ever written a Clojure function and then wanted to refactor it by extracting it into parts?
Did you find that experience frustring because `letfn` is ugly and nesting `lets` is even uglier?
Me too.  In fact that's my biggest complaint about Clojure.

The problem is that Clojure does not offer a way to scope a set of variables into subfunctions. This means that it is difficult, if not impossible, to extract subfunctions that have access to the parent functions variables.  Clojure does offer `letfn`, or just `def f fn...` but these forms do not allow subfunctions to initialize variables at the outer scope.

The `functor` macro is an experiment to see if extractions can be made easier by providing
an Algol-like block structure.  This will allow programmers to create sub-functions that have access to all
the local variables and arguments used by the parent function. 

If this sounds to OO, don't worry.  I'm not proposing we add classes or mutated variables to the language.  Rather I'm proposing a mechanism for initializing outer-scope variables within subfunction.  The difference is significant.  This could be implemented by the state monad. 

##Example

Here's a simple function for calculating the roots of a quadratic equation.  

	(defn quad- [a b c]
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
			
This doesn't look to bad.  It's a nicely subdivided into the relevant parts.  Still, it's a bit large, and lacks explanation.  

Here is how it looks with the functor macro.  

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
	
Notice that now we have a set of sub-functions that are scoped within the outer `quad` function.  Notice also that those sub-functions have access to the arguments of `quad` _and_ to the `discriminant` variable.  Note the strange `<-` symbol that _sets_ the `discriminant`.  Variables that are initialized in that manner are usable _throughout the scope_ of the outer function.  