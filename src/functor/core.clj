(ns functor.core
  (:require [clojure.set :as set]))

(defn expand-method-body [method-body]
  (if (empty? method-body)
    [nil #{}]
    (loop [forms method-body
           expanded-forms []
           vars #{}]
      (cond
        (empty? forms)
        [expanded-forms vars]

        (not (seq? (first forms)))
        (throw (Exception. (prn-str "not a <- setter:" (first forms))))

        (= '<- (ffirst forms))
        (let [form (first forms)
              var (second form)
              body (drop 2 form)]
          (recur (rest forms)
                 (concat expanded-forms [`(reset! ~var ~@body)])
                 (conj vars var)))

        :else
        (throw (Exception. (prn-str "only <- setters allowed: " (first forms))))))))

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
