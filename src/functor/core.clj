(ns functor.core
  (:require [clojure.set :as set]))

;(def mean (functor ([ns]
;  (make-sum)
;  (/ sum (count ns)))
; (make-sum []
;  (<- sum (reduce + ns)))
;  )

(def mean
  (fn [ns]
    (let [make-sum (fn [this]
                     (assoc this :sum (reduce + ns)))
          this {}
          {:keys [sum] :as this} (make-sum this)]
      (/ sum (count ns)))))


(defn expand-method-body [method-data method-body]
  (if (empty? method-body)
    ['this #{}]
    (loop [forms method-body
           expanded-forms []
           vars #{}]
      (cond
        (empty? forms)
        (let [final-let `(let [~@expanded-forms] ~'this)]
          [final-let vars])

        (not (seq? (first forms)))
        (throw (Exception. (prn-str "not a seq:" (first forms))))

        (= '<- (ffirst forms))
        (let [form (first forms)
              var (second form)
              body (drop 2 form)]
          (recur (rest forms)
                 (concat expanded-forms ['this `(assoc ~'this ~(keyword var) ~@body)])
                 (conj vars var)))

        :else
        (throw (Exception. (prn-str "horrible: " (first forms))))
        ))))

(defn- expand-method [method-desc method-data]
  (let [name (first method-desc)
        args (vec (concat ['this] (second method-desc)))
        body (drop 2 method-desc)
        [body vars] (expand-method-body method-data body)
        method-data (update method-data :methods concat [name `(fn ~args ~body)])
        method-data (update method-data :method-names conj name)
        method-data (update method-data :vars set/union vars)]
    method-data))

(defn- make-lets [method-expansions]
  (vec (concat [] method-expansions ['this {}])))

(defn expand-functor-body [{:keys [method-names] :as method-data} body-forms]
  (loop [forms body-forms
         method-calls []
         body-calls []]
    (cond
      (empty? forms)
      [method-calls body-calls]


      (not (seq? (first forms)))
      (recur (rest forms)
             method-calls
             (conj body-calls (first forms)))

      (method-names (ffirst forms))
      (let [form (first forms)
            name (first form)
            args (rest form)]
        (recur (rest forms)
               (concat method-calls ['this `(~name ~'this ~@args)])
               body-calls))

      :else
      (recur (rest forms)
             method-calls
             (concat body-calls (first body-forms)))
      )))

(defn- var-destructure [method-data]
  (if (empty? (:vars method-data))
    []
    [`{:keys [~@(:vars method-data)]} 'this]))

(defn- generate-functor [method-data functor-desc methods-desc]
  (let [arg-list (first functor-desc)
        body (rest functor-desc)]
    (if (some? methods-desc)
      (let [lets (make-lets (:methods method-data))
            [method-calls functor-body] (expand-functor-body method-data body)
            lets (vec (concat lets method-calls (var-destructure method-data)))]
        `(fn ~arg-list (let
                         ~lets
                         ~@functor-body)))
      `(fn ~arg-list ~@body))
    ))

(defmacro functor [functor-desc & methods-desc]
  (loop [methods methods-desc
         method-data {:methods [] :method-names #{} :vars #{}}]
    (if (empty? methods)
      (generate-functor method-data functor-desc methods-desc)
      (recur (rest methods)
             (expand-method (first methods) method-data))
      )))
