# functor

## Installation
    project.clj [com.cleancoder/functor "0.0.1-SNAPSHOT"]
    require [functor.core :refer [functor]]

## Introduction
The functor macro is an experiment in improving the cleanliness and refactorability of Clojure programs. 

Have you ever written a Clojure function and then wanted to refactor it by extracting it into parts?
Did you find that experience frustring because `letfn` is ugly and nesting `lets` is even uglier?
Me too.  In fact that's my biggest complaint about Clojure.

The problem is that Clojure does not offer a way to scope a set of variables into subfunctions. This means that it is difficult, if not impossible, to extract subfunctions that have access to the parent functions variables.  Clojure does offer `letfn`, or just `def f fn...` but these forms do not allow subfunctions to initialize variables at the outer scope.

The `functor` macro is an experiment to see if extractions can be made easier by providing
an Algol-like block structure.  This will allow programmers to create sub-functions that have access to all
the local variables and arguments used by the parent function. 

If this sounds to OO, don't worry.  I'm not proposing we add classes or mutated variables to the language.  Rather I'm proposing a mechanism for initializing outer-scope variables within subfunction.  The difference is significant.  This could be implemented by the state monad. 

## Starting Example

The following is a simple function for calculating the roots of a quadratic equation.  

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
	
Notice that now we have a set of sub-functions that are scoped within the outer `quad` function.  
Notice also that those sub-functions have access to the arguments of `quad` _and_ to the `discriminant` 
variable.  This is similar to the `label` feature of common lisp.

Note the strange `<-` symbol that _sets_ the `discriminant`.  Variables that are initialized in that manner are 
not global.  They are not known outside the `quad` function but are usable _throughout the scope_ of that function.
In that sense they are something like instance variables of a class.  Indeed, that's why I chose the name _functor_.
One definition of a functor is a class with one public method and some instance variables.

The example above is illustrative, but not motivating.  There are other, better ways to organize the `quad`
function.  But before we get into better motivations, let's look at how the `functor` macro works.

## Behind the Scenes
Consider this simple example:

    (def mean
      (functor
        ([ns]
         (make-sum)
         (/ sum (count ns)))
        (make-sum
          []
          (<- sum (reduce + ns)))))

Again, this is not a particularly motivating example; but it is small enough to illustrate _how_ the `functor`
macro works.  Here is the (slightly trimmed) code that it generates.

    (def mean 
      (fn [ns] 
        (let [sum (atom nil) 
              make-sum (fn [] (reset! sum (reduce + ns)))] 
          (make-sum) 
          (/ @sum (count ns)))))

Ew, Yuk!  An `atom`!  Allow me to explain.
The `sum` atom is scoped within the `mean` function.  All the sub-functions have access to `sum`, 
and _can set_ it.  This means that nobody outside of `mean` has access to `sum`, but all the sub-functions
within `mean` do.  That was the goal I was aiming for.

The choice to use `atom`s for the scoped variables was a pragmatic compromise.  My original goal was to thread a map
containing all the scoped variables throughout all the sub-functions.  (i.e. the State Monad).  But that
increased the complexity of the macro beyond the effort I was willing to give it.  In the end the `atom`s create
a small risk of concurrent mutation -- but I'm willing to accept that for benefit of being able to refactor
otherwise unrefactorable functions.

Also, I build this macro as a means for me to experiment with what I hope may become a new language feature.

## A Better Example
So, on to a more motivating example.  Here's a small function from `github.com/unclebob/more-speech`.

    (defn add-cross-reference [db event]
      (let [[_ _ referent] (events/get-references event)
            id (:id event)]
        (when (some? referent)
          (if (gateway/event-exists? db referent)
            (gateway/add-reference-to-event db referent id)
            (update-mem [:orphaned-replies referent] conj id)))
        (let [orphaned-replies (get-mem [:orphaned-replies id])]
          (doseq [orphan orphaned-replies]
            (gateway/add-reference-to-event db id orphan))
          (update-mem :orphaned-replies dissoc id))))

This really isn't too bad; but can we make this better with a functor?

    (def add-cross-reference
      (functor
        ([db event]
         (<- id (:id event))
         (<- parent (nth (events/get-references event) 2))
         
         (when (some? parent)
           (adopt-or-orphan-this-event))
         (un-orphan-events-that-reference-this-event))
    
        (adopt-or-orphan-this-event
          []
          (if (gateway/event-exists? db parent)
            (gateway/add-reference-to-event db parent id)
            (update-mem [:orphaned-replies parent] conj id)))
        
        (un-orphan-events-that-reference-this-event
          []
          (let [orphaned-replies (get-mem [:orphaned-replies id])]
            (doseq [orphan orphaned-replies]
              (gateway/add-reference-to-event db id orphan))
            (update-mem :orphaned-replies dissoc id)))))

This is a bit better.  I could have done it with `letfn` but this seems a bit prettier to me.
But is there a better example?

## Even Better Example

Again, from more-speech, there's this:

    (defn encrypt-if-direct-message [content tags]
      (if (re-find #"^D \#\[\d+\]" content)
        (let [reference-digits (re-find #"\d+" content)
              reference-index (Integer/parseInt reference-digits)
              p-tag (get tags reference-index)]
          (if (nil? p-tag)
            [content 1]
            (let [recipient-key (hex-string->num (second p-tag))
                  private-key (get-mem [:keys :private-key])
                  sender-key (hex-string->num private-key)
                  shared-secret (SECP256K1/calculateKeyAgreement sender-key recipient-key)
                  encrypted-content (SECP256K1/encrypt shared-secret content)]
              [encrypted-content 4])))
        [content 1]))

Nested `if`s with nested `let`s.  Urghh.  Let's see how a functor might help.

    (def encrypt-if-direct-message
      (functor
        ([content tags]
         (if (is-direct?)
           (encrypt-if-properly-referenced)
           (leave-unencrypted)))
    
        (is-direct? [] (re-find #"^D \#\[\d+\]" content))
    
        (get-p-tag-from-content
          []
          (let [reference-digits (re-find #"\d+" content)
                reference-index (Integer/parseInt reference-digits)]
            (get tags reference-index)))
    
        (encrypt-content
          []
          (let [recipient-key (hex-string->num (second p-tag))
                private-key (get-mem [:keys :private-key])
                sender-key (hex-string->num private-key)
                shared-secret (SECP256K1/calculateKeyAgreement sender-key recipient-key)]
            (SECP256K1/encrypt shared-secret content)))
    
        (encrypt-if-properly-referenced
          []
          (<- p-tag (get-p-tag-from-content))
          (if (nil? p-tag)
            [content 1]
            [(encrypt-content) 4]))
    
        (leave-unencrypted [] [content 1])))

Look at that top level function!   If direct, encrypt otherwise leave unencrypted.  Nice.

This would be tougher to do with `letfn` because you'd have to pass that `p-tag` around.  Allowing `p-tag` to
be set by one sub-function but used by another is -- convenient.

## Yet Another Example
This one's got a lot of nested `let`s.  So lets see what the functor can do to untangle it.

	(defn handle-duplicate-event [event id relays-already-sent-this-id url]
	  (do
	    (update-mem [:event-counter :dups] inc-if-nil)
	    (when (is-text-event? event)
	      (when-not (contains? relays-already-sent-this-id url)
	        (update-mem [:processed-event-ids id] add-relay-to-processed-event-ids url)
	        (gateway/add-relays-to-event (get-db) id [url])))))

	(defn validate-and-process-event [url envelope]
	  (let [[_name _subscription-id inner-event :as _decoded-msg] envelope
	        event (translate-event inner-event)
	        id (:id event)
	        relays-already-sent-this-id (get-mem [:processed-event-ids id])]
	    (update-mem [:event-counter :kinds (:kind event)] inc-if-nil)
	    (if (some? relays-already-sent-this-id)
	      (handle-duplicate-event event id relays-already-sent-this-id url)
	      (let [computed-id (compute-id inner-event)
	            ui-handler (get-mem :event-handler)]
	        (update-mem :processed-event-ids assoc id #{url})
	        (if (= id computed-id)
	          (let [event (decrypt-dm-event event)]
	            (when (not (:private event))
	              (process-event event url)
	              (when (is-text-event? event)
	                (handle-text-event ui-handler event))))
	          (log-pr 2 'id-mismatch url 'computed-id (util/num32->hex-string computed-id) envelope))))))
			  
Using the functor macro I was able to cut out most of the nested `let`s, rearrange the code to make it "saner", and combine the two related functions into a single main function with several sub-functions.  The result is, in my humble opinion, much better.

	(def validate-and-process-event
	  (functor
	    ([url envelope]
	     (init)
	     (if (is-duplicate-id?)
	       (handle-duplicate-event)
	       (handle-new-event)))

	    (handle-duplicate-event
	      []
	      (update-mem [:event-counter :dups] inc-if-nil)
	      (when (is-text-event? event)
	        (when-not (contains? relays-already-sent-this-id url)
	          (update-mem [:processed-event-ids id] add-relay-to-processed-event-ids url)
	          (gateway/add-relays-to-event (get-db) id [url]))))

	    (is-duplicate-id? [] (some? relays-already-sent-this-id))

	    (is-valid-id?
	      []
	      (<- computed-id (compute-id inner-event))
	      (= id computed-id))

	    (handle-valid-event
	      []
	      (let [decrypted-event (decrypt-dm-event event)]
	        (when (not (:private decrypted-event))
	          (process-event decrypted-event url)
	          (when (is-text-event? decrypted-event)
	            (handle-text-event ui-handler decrypted-event)))))

	    (handle-new-event
	      []
	      (update-mem :processed-event-ids assoc id (set [url]))
	      (if (is-valid-id?)
	        (handle-valid-event)
	        (log-pr 2 'id-mismatch url 'computed-id (util/num32->hex-string computed-id) envelope)))

	    (init []
	          (<- ui-handler (get-mem :event-handler))
	          (<- inner-event (nth envelope 2))
	          (<- event (translate-event inner-event))
	          (<- id (:id event))
	          (<- relays-already-sent-this-id (get-mem [:processed-event-ids id]))
	          (update-mem [:event-counter :kinds (:kind event)] inc-if-nil)
	          )
	    )
	  )

## Issues with the macro
My IDE (IntelliJ) hates this.  It sees all the symbols within the functor as undefined, and colors them all orange.  Yuk!
It also won't rename them or let me refactor them nicely.  Damn.

Also, the macro does not like `#()` and `#{}` reader shortcuts.  You have to replace them with `fn` and `set`.  I don't know why.

## Proposal
I think this would make a nice addition to the language.  It would make it much easier to pull complex functions
apart into nicely named sub-functions while allowing them to communicate with variables scoped to the outer
function.  

The language feature would not need to use `atom`s because the compiler could use vars that are scoped 
to the outer function. 

I'd like it to be part of `fn` (and therefore `defn` and `letfn`) and look something like this:

      (defn mean
		([ns]
		 (make-sum)
		 (/ sum (count ns)))
		(make-sum []
		  (<- sum (reduce + ns))))

## Argument
I expect that some will react against this by saying (or thinking) 
> _"Oh, no, OO!  We don't want to bring _objects_ into Clojure."_  

But after some thought I think those folks will agree that this is _not_ what's going on here.  We are not creating some long lived mutable object.  Rather, we are creating vars at the top level, and sub-functions that have access to those vars.  From a functional point of view this is no different from having a set of `let` vars followed by a set of expressions that use those vars. 

One might argue that the `<-` operator allows vars to be mutated.  Again, that is not necessarily the case.  In most cases the vars are simply initialized once and never mutated.  The compiler could ensure that no uninitialized var was used.  The compiler could prevent mutation by simply preventing more than one execution of `<-` on a var.  Better than that, however, would be to simulate the behavior of `let` redefinitions of a var, which are _not_ mutations.  
