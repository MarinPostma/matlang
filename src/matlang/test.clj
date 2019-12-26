(ns matlang.test)

(ns matlang.core
  (:require [instaparse.core :as insta]
            [rhizome.viz]
            [clojure.core.match :refer [match]])
  (:gen-class))

(def lang0-parser
  (instaparse.core/parser
   "prog = (spaces expr spaces <';'> spa:ces)*
    <expr> = assig | add-sub
    assig = varname spaces <'='> spaces expr
    <add-sub> = mult-div | add | sub
    add = add-sub spaces <'+'> spaces mult-div
    sub = add-sub spaces <'-'> spaces mult-div
    <mult-div> = factor | mult |div
    mult = mult-div spaces <'*'> spaces factor
    div = mult-div spaces <'/'> spaces factor
    <factor> = number | <'('> spaces expr spaces <')'> | varget |assig
    <spaces> = <#'\\s*'>
    number = #'-?[0-9]+'
    varget = varname
    varname = #'[a-zA-Z]\\w*'"))

(-> "a=1+1*3;b=a-2; a+b;" lang0-parser (instaparse.core/visualize :output-file :buffered-image))

(defn make-interpreting [make-instr-interpreting init-env]
  {:prog (fn [& instrs] (:_ret (reduce
                                (fn [env instr]
                                  (instaparse.core/transform (make-instr-interpreting env) instr))
                                init-env
                                instrs)))})

(defn make-lang0-instr-interpreting [env]
  {:assig (fn [{varname :_ret :as env1} {value :_ret :as env2}]
            (assoc (merge env1 env2) varname value :_ret value))
   :add (fn [{v1 :_ret :as env1} {v2 :_ret :as env2}]
          (assoc (merge env1 env2) :_ret (+ v1 v2)))
   :sub (fn [{v1 :_ret :as env1} {v2 :_ret :as env2}]
          (assoc (merge env1 env2) :_ret (- v1 v2)))
   :mult (fn [{v1 :_ret :as env1} {v2 :_ret :as env2}]
           (assoc (merge env1 env2) :_ret (* v1 v2)))
   :div (fn [{v1 :_ret :as env1} {v2 :_ret :as env2}]
          (assoc (merge env1 env2) :_ret (quot v1 v2)))
   :vec (fn [& args]
          (assoc (merge args) :_ret {:type :vec :value (map :_ret args)}))
   :number #(assoc env :_ret (Integer/parseInt %))
   :varname #(assoc env :_ret (keyword %))
   :varget (fn [{varname :_ret :as env1}]
             (assoc env1 :_ret (varname env1)))})

(def lang0-interpret (dynamic-eval (make-interpreting make-lang0-instr-interpreting {:_ret 0})))

(def lang0-interpret-test (->> "a=1+1*3;b=a-2; a+b;" lang0-parser lang0-interpret))
(lang0-interpret-test)
