(ns functor.core
  (:require [clojure.set :as set]))

(declare expand-method-body)

(defn expand-method-form [method-form]
  (if-not (coll? method-form)
    [method-form #{}]
    (cond
      (empty? method-form)
      [[] #{}]

      (= '<- (first method-form))
      (let [var (second method-form)
            expression (nth method-form 2)
            [expanded-body expanded-vars] (expand-method-form expression)]
        [`(reset! ~var ~expanded-body) (conj expanded-vars var)])

      :else
      (let [f-name (first method-form)
            args (rest method-form)
            [forms vars] (expand-method-body args)]
        [(concat [f-name] forms) vars]))))

(defn expand-method-body [method-body]
  (let [expansions (doall (map expand-method-form method-body))
        expanded-forms (map first expansions)
        vars (reduce #(set/union %1 (second %2)) #{} expansions)]
    [(apply vector expanded-forms) vars]))

(defn- expand-method [method-desc method-data]
  (let [name (first method-desc)
        args (vec (second method-desc))
        body (drop 2 method-desc)
        [body vars] (expand-method-body body)
        fn-decl (if (empty? body) `(fn ~args) `(fn ~args ~@body))
        method-data (update method-data :methods concat [name fn-decl])
        method-data (update method-data :method-names conj name)
        method-data (update method-data :vars set/union vars)]
    method-data))

(defn make-atoms [vars]
  (reduce #(concat %1 [%2 '(atom nil)]) [] vars))

(defn- make-lets [{:keys [methods vars]}]
  (let [atoms (make-atoms vars)]
    (vec (concat atoms methods))))

(defn- generate-functor [method-data functor-desc methods-desc]
  (let [arg-list (first functor-desc)
        body (rest functor-desc)]
    (if (some? methods-desc)
      (let [lets (make-lets method-data)]
        `(fn ~arg-list (let
                         ~lets
                         ~@body)))
      `(fn ~arg-list ~@body))))

(defmacro functor [functor-desc & methods-desc]
  (loop [methods methods-desc
         method-data {:methods [] :method-names #{} :vars #{}}]
    (if (empty? methods)
      (generate-functor method-data functor-desc methods-desc)
      (recur (rest methods)
             (expand-method (first methods) method-data)))))
