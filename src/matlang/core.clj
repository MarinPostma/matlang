(ns matlang.core
  (:require [instaparse.core :as insta]
            [rhizome.viz]
            [matlang.env :refer [make-env-fn init-env]]
            [clojure.core.match :refer [match]])
  (:gen-class))

(def parser
  (insta/parser
   "
   prog = (spaces form spaces)*
   <form> = (block | control_flow | ( expr spaces <';'> ))
   <expr> = assig | declare | add-sub | logical_statement
   <control_flow> = if_statement | while_statement
   declare = <'let'> spaces varname (spaces <'='> spaces expr)?
   assig = spaces varname spaces <'='> spaces expr
   <add-sub> = mult-div | add | sub
   sub = add-sub spaces <'-'> spaces mult-div
   add = add-sub spaces <'+'> spaces mult-div
   <mult-div> = factor | mult | div | mod
   mult = mult-div spaces <'*'> spaces factor
   div = mult-div spaces <'/'> spaces factor
   mod = mult-div spaces <'%'> spaces factor
   <factor> = number | <'('> expr <')'> | varget | assig | mat
   <mat> = _mat | transp
   _mat = <'['> spaces (_vec spaces <';'>)* spaces _vec? spaces  <']'>
   _vec = (spaces number <#'\\s'> spaces)* (number)?
   <spaces> = <#'\\s*'>
   transp = (mat | varget) <'\\''>
   number = float | integer
   float = #'-?[0-9]+.[0-9]+'
   integer = #'-?[0-9]+'
   varget = varname | argument
   varname = #'[a-zA-Z]\\w*'
   block = <'{'>(spaces form spaces)*<'}'>
   if_statement = <#'if '> spaces logical_statement spaces block (spaces <#'else'> spaces block)?
   while_statement = <#'while '> spaces logical_statement spaces block
   <boolean> = true | false
   <logical_statement> = more_than | less_than | equal | more_equal | less_equal | boolean
    more_than = expr spaces <'>'> spaces expr
    less_than = expr spaces <'<'> spaces expr
    equal = expr spaces <'=='> spaces expr
    less_equal = expr spaces <'<='> spaces expr
    more_equal = expr spaces <'>='> spaces expr
   true = <#'true'>
   false = <#'false'>
   argument = <'%'>#'[0-9]+'"))

;; define empty env
(def env (init-env {}))

;; generate functions to manipulate env  all functions to manipulate env are now in scope :
;; push-env
;; pop-env
;; push-env
;; set-env-value
;; get-env-value
;; declare-env-value

(make-env-fn env)

;; inject program args
;; TODO

(defn parse-num
  [args]
  (let [args (first args)
        t (first args)
        v (second args)]
    {:type t
     :value (if (= t :integer)
              (Integer/parseInt v)
              (Float/parseFloat v))}))

(defn do-add
  [args]
  (let [lhs (exec (first args))
        rhs (exec (second args))]
    (match [(:type lhs) (:type rhs)]
      [:integer :integer] {:type :integer :value (+ (:value lhs) (:value rhs))}
      [:float :integer] {:type :float :value (+ (:value lhs) (:value rhs))}
      [:integer :float] {:type :float :value (+ (:value lhs) (:value rhs))}
      [:float :float] {:type :float :value (+ (:value lhs) (:value rhs))}
      :else (throw (Exception. (str "Unsuported add with types " (:type lhs) " and " (:type rhs)))))))

(defn do-sub
  [args]
  (let [lhs (exec (first args))
        rhs (exec (second args))]
    (match [(:type lhs) (:type rhs)]
      [:integer :integer] {:type :integer :value (- (:value lhs) (:value rhs))}
      [:float :integer] {:type :float :value (- (:value lhs) (:value rhs))}
      [:integer :float] {:type :float :value (- (:value lhs) (:value rhs))}
      [:float :float] {:type :float :value (- (:value lhs) (:value rhs))}
      :else (throw (Exception. (str "Unsuported sub with types " (:type lhs) " and " (:type rhs)))))))

(defn do-mult
  [args]
  (let [lhs (exec (first args))
        rhs (exec (second args))]
    (match [(:type lhs) (:type rhs)]
      [:integer :integer] {:type :integer :value (* (:value lhs) (:value rhs))}
      [:float :integer] {:type :float :value (* (:value lhs) (:value rhs))}
      [:integer :float] {:type :float :value (* (:value lhs) (:value rhs))}
      [:float :float] {:type :float :value (* (:value lhs) (:value rhs))}
      :else (throw (Exception. (str "Unsuported mult with types " (:type lhs) " and " (:type rhs)))))))

(defn do-div
  [args]
  (let [lhs (exec (first args))
        rhs (exec (second args))]
    (match [(:type lhs) (:type rhs)]
      [:integer :integer] {:type :integer :value (/ (:value lhs) (:value rhs))}
      [:float :integer] {:type :float :value (/ (:value lhs) (:value rhs))}
      [:integer :float] {:type :float :value (/ (:value lhs) (:value rhs))}
      [:float :float] {:type :float :value (/ (:value lhs) (:value rhs))}
      :else (throw (Exception. (str "Unsuported mult with types " (:type lhs) " and " (:type rhs)))))))

(defn do-mod
  [args]
  (let [lhs (exec (first args))
        rhs (exec (second args))]
    (match [(:type lhs) (:type rhs)]
      [:integer :integer] {:type :integer :value (mod (:value lhs) (:value rhs))}
      :else (throw (Exception. (str "Unsuported mod with types " (:type lhs) " and " (:type rhs)))))))

(defn do-if
  [args]
  (let [predicate (exec (first args))
        true-branch (second args)
        false-branch (nth 3 args)]
    (if (= :boolean (:type predicate))
      (do
        (push-env)
        (if (:value predicate)
          (exec-block true-branch)
          (if false-branch
            (exec-block false-branch)))
        (pop-env)
        :none)
      (throw (Exception. (str "Expected predicate to be of type boolean, found"))))))

(defn do-declare
  [args]
  (let [varname (exec (first args))]
    (declare-env-value varname)
    (if (second args)
      (set-env-value varname (exec (second args))))
    :none))

(defn exec
  [inst]
  (let [tok-type (first inst)
        args (rest inst)]
    (do
      (match tok-type
        :block (set-env-value :_ret (exec-block args))
        :varname (set-env-value :_ret (keyword (first args)))
        :declare (set-env-value :_ret (do-declare args))
        :if (set-env-value :_ret (do-if args))
        :add (set-env-value :_ret (do-add args))
        :sub (set-env-value :_ret (do-sub args))
        :mult (set-env-value :_ret (do-mult args))
        :div (set-env-value :_ret (do-div args))
        :mod (set-env-value :_ret (do-mod args))
        :number (set-env-value :_ret (parse-num args))
        :true (set-env-value :_ret {:type :boolean :value true})
        :false (set-env-value :_ret {:type :boolean :value false})
        :else (throw (Exception. "unknown token")))
      (get-env-value :_ret))))

(defn exec-block
  [block]
  (loop [cur-inst (first block)
         remaining (rest block)]
    (do
      (exec cur-inst)
      (if (> (count remaining) 0)
        (recur (first remaining) (rest remaining))))))

(defn run-program
  "runs a program string"
  [program]
  (let [ast (parser program)]
    (if (= (first ast) :prog)
      (do
        (exec-block (rest ast))
        (println env))
      (throw (Exception. "Invalid program")))))

(def prgm "{let hello = 12; let tutu = 11;}")
(parser prgm)
;;(run-program prgm)
