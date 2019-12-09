(ns matlang.core
  (:require [instaparse.core :as insta] [rhizome.viz])
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
   mat = <'['> spaces (_vec spaces <';'>)* spaces _vec? spaces  <']'>
   _vec = (spaces number <#'\\s'> spaces)* (number)?
   <spaces> = <#'\\s*'>
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
        cols (if (> 0 rows) (count (first mat)) 0)]
    (do
      (assert (every? #((= (count %) cols)) mat) "invalid matrix size"))
    (assoc (apply merge args) :_ret {:type :matrix
                                     :shape (list rows cols)
                                     :mat mat})))

(defn make-lang0-instr-interpreting [env]
  {:assig (fn [{varname :_ret :as env1} {value :_ret :as env2}] (assoc (merge env1 env2) varname value :_ret value))
   :add (fn [{lhs :_ret :as env1} {rhs :_ret :as env2}] (assoc (merge env1 env2) :_ret (+ lhs rhs)))
   :sub (fn [{lhs :_ret :as env1} {rhs :_ret :as env2}] (assoc (merge env1 env2) :_ret (- lhs rhs)))
   :mult (fn [{lhs :_ret :as env1} {rhs :_ret :as env2}] (assoc (merge env1 env2) :_ret (* lhs rhs)))
   :div (fn [{lhs :_ret :as env1} {rhs :_ret :as env2}] (assoc (merge env1 env2) :_ret (/ lhs rhs)))
   :mat make-matrix
   :_vec (fn [& elems] (assoc (apply merge elems) :_ret (map :_ret elems)))
   :number #(assoc env :_ret (Integer/parseInt %))
   :varname #(assoc env :_ret (keyword %))
   :mod (fn [{lhs :_ret :as env1} {rhs :_ret :as env2}] (assoc (merge env1 env2) :_ret (rem lhs rhs)))
   :argument #(assoc env :_ret (keyword (str "%" %)))
   :varget (fn [{varname :_ret :as env1}] (assoc env1 :_ret (varname env1)))})

(def interpreter (dynamic-eval-args  make-lang0-instr-interpreting))

(def prg "vec = [ 1 1 4  12   3; 13 11];")

(def const-eval-test (->> prg parser interpreter))

;(insta/visualize (parser prg))
(const-eval-test 2 3)
