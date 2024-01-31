(ns functor.core
  (:require [clojure.set :as set]))

(declare expand-forms)

(defn expand-form [method-form]
  (if-not (coll? method-form)
    [method-form #{}]
    (cond
      (empty? method-form)
      [[] #{}]

      (vector? method-form)
      (let [[items vars] (expand-forms method-form)]
        [(vec items) vars])

      (= '<- (first method-form))
      (let [var (second method-form)
            expression (nth method-form 2)
            [expanded-body expanded-vars] (expand-form expression)]
        [`(reset! ~var ~expanded-body) (conj expanded-vars var)])

      :else
      (let [f-name (first method-form)
            args (rest method-form)
            [forms vars] (expand-forms args)]
        [(concat [f-name] forms) vars]))))

(defn expand-forms [method-body]
  (let [expansions (doall (map expand-form method-body))
        expanded-forms (map first expansions)
        vars (reduce #(set/union %1 (second %2)) #{} expansions)]
    [(apply vector expanded-forms) vars]))

(defn- expand-method [method-desc method-data]
  (let [name (first method-desc)
        args (second method-desc)
        body (drop 2 method-desc)
        [body vars] (expand-forms body)
        method-data (update method-data :methods conj [name args body])
        method-data (update method-data :method-names conj name)
        method-data (update method-data :vars set/union vars)]
    method-data))

(defn dereference-vars [forms vars]
  (loop [forms forms
         dereferenced-forms []]
    (cond
      (empty? forms)
      dereferenced-forms

      (not (coll? (first forms)))
      (let [form (first forms)]
        (if (vars form)
          (recur (rest forms) (concat dereferenced-forms [`@~form]))
          (recur (rest forms) (concat dereferenced-forms [form]))))

      (vector? (first forms))
      (recur (rest forms)
             (concat dereferenced-forms [(vec (dereference-vars (first forms) vars))]))

      (= 'clojure.core/reset! (ffirst forms))
      (let [[_ name expression] (first forms)]
        (recur (rest forms)
               (conj (vec dereferenced-forms)
                     (list 'clojure.core/reset! name (first (dereference-vars [expression] vars))))))

      :else
      (recur (rest forms) (concat dereferenced-forms [(dereference-vars (first forms) vars)])))
    )
  )

(defn make-atoms [vars]
  (reduce #(concat %1 [%2 '(atom nil)]) [] vars))

(defn make-method-let [[name args body] vars]
  (let [body (dereference-vars body vars)
        fn-decl (if (empty? body) `(fn ~args) `(fn [~@args] ~@body))]
    [name fn-decl]))

(defn- make-lets [{:keys [methods vars]}]
  (let [atoms (make-atoms vars)
        method-lets (map #(make-method-let % vars) methods)
        method-lets (reduce concat method-lets)]
    (vec (concat atoms method-lets))))

(defn- generate-functor [method-data functor-desc methods-desc]
  (let [arg-list (first functor-desc)
        [body vars] (expand-forms (rest functor-desc))
        method-data (update method-data :vars set/union vars)
        vars (:vars method-data)
        body (dereference-vars body vars)]
    (if (or (some? methods-desc) (not (empty? (:vars method-data))))
      (let [lets (make-lets method-data)]
        `(fn ~arg-list (let
                         [~@lets]
                         ~@body)))
      `(fn ~arg-list ~@body))))

(defmacro functor [functor-desc & methods-desc]
  (loop [methods methods-desc
         method-data {:methods [] :method-names #{} :vars #{}}]
    (if (empty? methods)
      (generate-functor method-data functor-desc methods-desc)
      (recur (rest methods)
             (expand-method (first methods) method-data)))))
