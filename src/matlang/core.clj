(ns matlang.core
  (:require [instaparse.core :as insta]
            [rhizome.viz]
            [clojure.core.match :refer [match]])
  (:gen-class))

(def parser
  (insta/parser
   "
   prog = (spaces expr spaces <';'> spaces)*
   <expr> = assig | add-sub
   assig = varname spaces <'='> spaces expr
   <add-sub> = mult-div | add | sub
   sub = add-sub spaces <'-'> spaces mult-div
   add = add-sub spaces <'+'> spaces mult-div
   <mult-div> = factor | mult | div | mod
   mult = mult-div spaces <'*'> spaces factor
   div = mult-div spaces <'/'> spaces factor
   mod = mult-div spaces <'%'> spaces factor
   <factor> = number | <'('> expr <')'> | varget | assig | mat
   mat = <'['> spaces (_vec spaces <';'>)* spaces _vec? spaces  <']'> | transp
   _vec = (spaces number <#'\\s'> spaces)* (number)?
   <spaces> = <#'\\s*'>
   transp = (mat | varget) <'\\''>
   number = #'-?[0-9]+'
   varget = varname | argument
   varname = #'[a-zA-Z]\\w*'
   argument = <'%'>#'[0-9]+'"))
;(insta/visualize (const-parser "-123"))
;(prn (const-parser "   -123    "))


(defn args-to-env [args] (into {} (map-indexed #(vector (keyword (str "%" %1)) %2) args)))

(defn make-interpreting [make-instr-interpreting init-env]
  {:prog (fn [& instrs] (:_ret (reduce
                                (fn [env instr]
                                  (insta/transform (make-instr-interpreting env) instr))
                                init-env
                                instrs)))})

(defn dynamic-eval-args [make-interpreter]
  (fn [ast]
    (fn [& args]
      (insta/transform (make-interpreting make-interpreter (assoc (args-to-env args) :_ret 0)) ast))))

(defn make-matrix [& args]
  (let [mat (map :_ret args)
        rows (count mat)
        cols (if (> rows 0) (count (first mat)) 0)]
    (do
      (assert (every? #(= (count %) cols) mat) "invalid matrix size"))
    (assoc (apply merge args) :_ret {:type :matrix
                                     :val {:shape (list rows cols)
                                           :mat (map #(map :val %) mat)}})))

(defn make-number [env n] (assoc env :_ret {:type :scalar :val (Integer/parseInt n)}))

(defn make-type-lhs-rhs [f t]
  (fn [{lhs :_ret :as env1} {rhs :_ret :as env2}]
    (assoc
     (merge env1 env2)
     :_ret (assoc
            {}
            :type t
            :val (f (:val lhs) (:val rhs))))))

(defn ms-op [f]
  (fn [matrix scalar]
    {:shape (:shape matrix)
     :mat (map #(map (partial f scalar) %) (:mat matrix))}))

(defn mm-op [f]
  (fn [m1 m2]
    (do
      (assert (= (:shape m1) (:shape m2)) "shapes must  be equal for matrix addition")
      {:shape (:shape m1)
       :mat (map #(doall
                   (map f (first %) (second %)))
                 (map vector (:mat m1) (:mat m2)))})))

(def add-dispatch {:ss (make-type-lhs-rhs + :scalar)
                   :ms (make-type-lhs-rhs (ms-op +) :matrix)
                   ;swapping arguments here
                   :sm #((make-type-lhs-rhs (ms-op +) :matrix) %2 %1)
                   :mm (make-type-lhs-rhs (mm-op +) :matrix)})

(def sub-dispatch {:ss (make-type-lhs-rhs - :scalar)
                   :ms (make-type-lhs-rhs (ms-op -) :matrix)
                   ;broadcasting here is weird
                   ;:sm #((make-type-lhs-rhs (ms-op -) :matrix) %2 %1)
                   :sm (fn [& _] (assert false "broadcasting not working"))
                   :mm (make-type-lhs-rhs (mm-op -) :matrix)})

(def mul-dispatch {:ss (make-type-lhs-rhs * :scalar)
                   :ms (make-type-lhs-rhs (ms-op *) :matrix)
                   :sm #((make-type-lhs-rhs (ms-op *) :matrix) %2 %1)
                   :mm (make-type-lhs-rhs (mm-op -) :matrix)})

(defn transpose [mat]
  (do
    (println mat)
    (assert (= (:type mat) :matrix) "Only matrix can be transposed")
    (let [{shape :shape mat :mat} (:val mat)]
      {:type :matrix
       :val {:shape `((second shape) (first shape))
             :mat (apply map vector mat)}})))

(defn op-dispatch [fs]
  (fn [lhs rhs]
    (match [((comp :type :_ret) lhs) ((comp :type :_ret) rhs)]
      [:scalar :scalar] ((:ss fs) lhs rhs)
      [:matrix :scalar] ((:ms fs) lhs rhs)
      [:scalar :matrix] ((:sm fs) lhs rhs)
      [:matrix :matrix] ((:mm fs) lhs rhs))))

(defn make-lang0-instr-interpreting [env]
  {:assig (fn [{varname :_ret :as env1} {value :_ret :as env2}] (assoc (merge env1 env2) varname value :_ret value))
   :add (op-dispatch add-dispatch)
   :sub (op-dispatch sub-dispatch)
   :mult (op-dispatch mul-dispatch)
   :div (fn [{lhs :_ret :as env1} {rhs :_ret :as env2}] (assoc (merge env1 env2) :_ret (/ lhs rhs)))
   :mat make-matrix
   :_vec (fn [& elems] (assoc (apply merge elems) :_ret (map :_ret elems)))
   :number #(make-number env %)
   :varname #(assoc env :_ret (keyword %))
   :mod (fn [{lhs :_ret :as env1} {rhs :_ret :as env2}] (assoc (merge env1 env2) :_ret (rem lhs rhs)))
   :argument #(assoc env :_ret (keyword (str "%" %)))
   :transp #(assoc env :_ret (transpose (:_ret %)))
   :varget (fn [{varname :_ret :as env1}] (assoc env1 :_ret (varname env1)))})

(def interpreter (dynamic-eval-args  make-lang0-instr-interpreting))

(def prg "[1 1]';")

(defn _print [ret]
  (match [(:type ret)]
    [:scalar] (:val ret)
    [:matrix] ((comp :mat :val) ret)
    :else (println "undefined return type" ret)))

(def evaluator (->> prg parser interpreter))

;(insta/visualize (parser prg))
(evaluator 1 2)
