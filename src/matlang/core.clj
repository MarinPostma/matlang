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
   <expr> = assig | declare | add-sub | logical_statement | defn | fncall
   <control_flow> = if_statement | while_statement
   declare = <'let'> spaces varname (spaces <'='> spaces expr)?
   defn = <'fn'> spaces <'('> (spaces varname spaces <','> spaces)* ( varname )? spaces <')'> spaces block
   fncall = varname <'('> (spaces expr spaces <','> spaces)* ( expr )? spaces <')'>
   assig = spaces varname spaces <'='> spaces expr
   <add-sub> = mult-div | add | sub
   sub = add-sub spaces <'-'> spaces mult-div
   add = add-sub spaces <'+'> spaces mult-div
   <mult-div> = factor | mult | div | mod
   mult = mult-div spaces <'*'> spaces factor
   div = mult-div spaces <'/'> spaces factor
   mod = mult-div spaces <'%'> spaces factor
   <factor> = number | <'('> expr <')'> | varget | assig | mat
   <mat> = _mat
   _mat = <'['> spaces (_vec spaces <';'>)* spaces _vec? spaces  <']'>
   _vec = (spaces number <#'\\s'> spaces)* (number)?
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
   <spaces> = <#'\\s*'>
   argument = <'%'>#'[0-9]+'"))

(declare exec exec-block)

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
        false-branch (nth args 2 nil)]
    (println false-branch)
    (if (= :boolean (:type predicate))
      (do
        (if (:value predicate)
          (exec true-branch)
          (if false-branch
            (exec false-branch))))
      (throw (Exception. (str "Expected predicate to be of type boolean, found"))))))

(defn do-declare
  [args]
  (let [varname (exec (first args))]
    (declare-env-value varname)
    (if (second args)
      (let [value (exec (second args))]
        (set-env-value varname value)
        value)
      :none)))

(defn do-assig
  [args]
  (let [value (exec (second args))
        varname (exec (first args))]
    (set-env-value varname value)))

(defn do-while
  [args]
  (loop [predicate (exec (first args))]
    (if (= :boolean (:type predicate))
      (when (:value predicate)
        (exec (second args))
        (recur (exec (first args))))
      (throw (Exception. (str "Expected predicate to be of type boolean, found: " (:type predicate)))))))

(defn do-less-than
  [args]
  (let [lhs (exec (first args))
        rhs (exec (second args))]
    (match [(:type lhs) (:type rhs)]
      [:integer :integer] {:type :boolean :value (< (:value lhs) (:value rhs))}
      [:float :integer] {:type :boolean :value (< (:value lhs) (:value rhs))}
      [:integer :float] {:type :boolean :value (< (:value lhs) (:value rhs))}
      [:float :float] {:type :boolean :value (< (:value lhs) (:value rhs))}
      :else (throw (Exception. (str "Unsuported LT with types " (:type lhs) " and " (:type rhs)))))))

(defn do-defn
  [args]
  (let [params (drop-last args)
        body (last args)]
    {:type :function :value {:params (map exec params)
                             :body body}}))

(defn exec-fn
  [function params]
  (do
    (println function)
    ;; push new env
    (push-env)
        ;; setup function environement
    (if (> (count params) 0)
      (doseq [pair (map vector (:params function) params)]
        (declare-env-value (first pair))
        (set-env-value (first pair) (exec (second pair)))))
        ;; execute function body:
    (exec-block (rest (:body function)))
        ; return block value and pop function env
    (pop-env)))

(defn do-fncall
  [args]
  (let [function-name (exec (first args))
        params (rest args)
        function (get-env-value function-name)]
    (if (not= (:type function) :function)
      (throw (Exception. (str  function-name " is not a function."))))
    (let [function (:value function)]
      (if (not= (count params) (count (:params function)))
        (throw (Exception. "wrong number of arguments for " function-name)))
      (exec-fn function params))))

(defn exec
  [inst]
  (let [tok-type (first inst)
        args (rest inst)]
    (do
      (match tok-type
        :defn (set-env-value :_ret (do-defn args))
        :fncall (set-env-value :_ret (do-fncall args))
        :block (set-env-value :_ret (do (push-env) (exec-block args) (pop-env)))
        :varget (set-env-value :_ret (get-env-value (exec (first args))))
        :assig (set-env-value :_ret (do-assig args))
        :varname (set-env-value :_ret  (keyword (first args)))
        :declare (set-env-value :_ret  (do-declare args))
        :while_statement (set-env-value :_ret (do-while args))
        :if_statement (set-env-value :_ret  (do-if args))
        :add (set-env-value :_ret  (do-add args))
        :sub (set-env-value :_ret  (do-sub args))
        :mult (set-env-value :_ret  (do-mult args))
        :div (set-env-value :_ret  (do-div args))
        :mod (set-env-value :_ret  (do-mod args))
        :less_than (set-env-value :_ret (do-less-than args))
        :number (set-env-value :_ret  (parse-num args))
        :true (set-env-value :_ret  {:type :boolean :value true})
        :false (set-env-value :_ret  {:type :boolean :value false})
        :else (throw (Exception. (str "unknown token: " tok-type))))
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
        ;; parse the program, exec global scope
        (exec-block (rest ast))
        ;; run main
        (if-let [main (get-env-value :main)]
          (if (= (:type main) :function)
            (exec-fn (:value main) [])
            (throw (Exception. "main is not a function")))
          (throw (Exception. "no main found")))
        (println env))
      (throw (Exception. "Invalid program")))))

(def prgm "let hello = 0; let main = fn() {hello = hello + 1;};")
;(def prgm "let machin =   1231; machin = 0;")
(run-program prgm)
