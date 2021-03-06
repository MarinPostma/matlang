(ns matlang.core
  (:require [instaparse.core :as insta]
            [instaparse.failure :as fail]
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
   defn = <'fn'> spaces <'('> spaces (spaces varname spaces <','> spaces)* spaces varname? spaces <')'> spaces block
   fncall = varname <'('> (spaces expr spaces <','> spaces)* ( expr )? spaces <')'>
   assig = spaces varname spaces <'='> spaces expr
   <add-sub> = mult-div | add | sub
   sub = add-sub spaces <'-'> spaces mult-div
   add = add-sub spaces <'+'> spaces mult-div
   <mult-div> = factor | mult | div | mod
   mult = mult-div spaces <'*'> spaces factor
   div = mult-div spaces <'/'> spaces factor
   mod = mult-div spaces <'%'> spaces factor
   <factor> = number | <'('> expr <')'> | varget | assig | mat | string | expr
   mat = <'['> spaces (_vec spaces <';'>)* spaces _vec? spaces  <']'>
   _vec = (spaces expr <#'\\s'> spaces)* expr?
   number = float | integer
   float = #'-?[0-9]+\\.[0-9]+'
   integer = #'-?[0-9]+'
   varget = varname | argument
   varname = #'[a-zA-Z]\\w*'
   block = <'{'> (spaces form spaces)* <'}'>
   if_statement = <#'if '> spaces logical_statement spaces block (spaces <#'else'> spaces block)?
   while_statement = <#'while '> spaces logical_statement spaces block
   <boolean> = true | false
   <logical_statement> = more_than | less_than | equal | more_equal | less_equal | boolean
    more_than = expr spaces <'>'> spaces expr
    less_than = expr spaces <'<'> spaces expr
    equal = expr spaces <'=='> spaces expr
    not_equal = expr spaces <'!='> spaces expr
    less_equal = expr spaces <'<='> spaces expr
    more_equal = expr spaces <'>='> spaces expr
   true = <#'true'>
   false = <#'false'>
   string = <'\"'> #'[^\"]*' <'\"'>
   <spaces> = <#'\\s*'>
   argument = <'%'>#'[0-9]+'"))

(declare exec exec-block)

(defmacro make-builtin
  [builtin-name & builtin-args]
  `{~builtin-name {:type :function
                   :value {:params '(~@builtin-args)
                           :body [:block [:builtin ~builtin-name
                                          ~@(map (fn [k] [:varget [:varname k]])
                                                 builtin-args)]]}}})
(def stdlib (merge (make-builtin :print :string)
                   (make-builtin :println :string)
                   (make-builtin :len :matrix)))

;; define empty env


(def env (init-env (merge {} stdlib)))

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
      [:string :string] {:type :string :value (str (:value lhs) (:value rhs))}
      [:string :float] {:type :string :value (str (:value lhs) (:value rhs))}
      [:string :integer] {:type :string :value (str (:value lhs) (:value rhs))}
      [:float :string] {:type :string :value (str (:value lhs) (:value rhs))}
      [:integer :string] {:type :string :value (str (:value lhs) (:value rhs))}
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
        (push-env)
        (exec (second args))
        (pop-env)
        (recur (exec (first args))))
      (throw (Exception. (str "Expected predicate to be of type boolean, found: " (:type predicate)))))))

(defn make-bool-fn
  [op]
  (fn [args]
    (let [lhs (exec (first args))
          rhs (exec (second args))]
      (match [(:type lhs) (:type rhs)]
        [:integer :integer] {:type :boolean :value (op (:value lhs) (:value rhs))}
        [:float :integer] {:type :boolean :value (op (:value lhs) (:value rhs))}
        [:integer :float] {:type :boolean :value (op (:value lhs) (:value rhs))}
        [:float :float] {:type :boolean :value (op (:value lhs) (:value rhs))}
        :else (throw (Exception. (str "Unsuported LT with types " (:type lhs) " and " (:type rhs))))))))

(def do-less-than (make-bool-fn <))
(def do-equal (make-bool-fn =))
(def do-leq (make-bool-fn <=))
(def do-more-than (make-bool-fn >))
(def do-geq (make-bool-fn >=))
(def do-neq (make-bool-fn not=))

(defn do-defn
  [args]
  (let [params (drop-last args)
        body (last args)]
    {:type :function :value {:params (map exec params)
                             :body body}}))

(defn exec-fn
  [function params]
  (do
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

(defn do-builtin-len
  [args]
  (let [item (exec (first args))]
    (match (:type item)
      :string {:type :integer :value (count (:value item))}
      :else (throw (Exception. (str "Can't take lenght of " (:type item)))))))

;; TODO; same logic, abstact.
(defn do-builtin-println
  [args]
  (let [value (->> args first exec)]
    (when (some #(= (:type value) %1) [:integer :float :string :boolean])
      (->> value :value println))
    :none))

(defn do-builtin-print
  [args]
  (do
    (->> args first exec :value print)
    :none))

(defn do-builtin
  [args]
  (let [builtin (first args)
        params (rest args)]
    (match builtin
      :print (do-builtin-print)
      :println (do-builtin-println params)
      :len (do-builtin-len params)
      :else (throw (Exception. (str "not a builtin: " builtin))))))

(defn parse-string
  [args]
  {:type :string
   :value (first args)})

(defn make-vec
  [args]
  (let  [values (map exec args)]
    {:type :vector
     :value {:size (count values)
             :type (:type (first values))
             :values (if (every? #(= (:type %1) (:type (first values))) values)
                       (vec values)
                       (throw (Exception. "All elements in a matrix must have the same type")))}}))

(defn make-mat
  [args]
  (let [values (map exec args)]
    (if (and (every? #(= (:type %1) :vector) values)
             (every? #(= (:type (:value %1)) (:type (:value (first values)))) values))
      (if (every? #(= (:size (:value %1)) (:size (:value (first values)))) values)
        {:type :matrix
         :value {:size (list (count values) (:size (:value (first values))))
                 :type (:type (:value (first values)))
                 :values (vec (apply concat (map #(:values (:value %1)) values)))}}

        (throw (Exception. "All rows in a matrix must have the same size.")))
      (throw (Exception. "All elements in a matrix must have the same type")))))

(defn exec
  [inst]
  (let [tok-type (first inst)
        args (rest inst)]
    (do
      (match tok-type
        :_vec (set-env-value :_ret (make-vec args))
        :mat (set-env-value :_ret (make-mat args))
        :builtin (set-env-value :_ret (do-builtin args))
        :defn (set-env-value :_ret (do-defn args))
        :fncall (set-env-value :_ret (do-fncall args))
        :block (set-env-value :_ret (do (push-env) (exec-block args) (pop-env)))
        :varget (set-env-value :_ret (get-env-value (exec (first args))))
        :assig (set-env-value :_ret (do-assig args))
        :more_than (set-env-value :_ret (do-more-than args))
        :less_than (set-env-value :_ret (do-less-than args))
        :equal (set-env-value :_ret (do-equal args))
        :not_equal (set-env-value :_ret (do-neq args))
        :less_equa (set-env-value :_ret (do-leq args))
        :more_equa (set-env-value :_ret (do-geq args))
        :varname (set-env-value :_ret  (keyword (first args)))
        :declare (set-env-value :_ret  (do-declare args))
        :while_statement (set-env-value :_ret (do-while args))
        :if_statement (set-env-value :_ret  (do-if args))
        :add (set-env-value :_ret  (do-add args))
        :sub (set-env-value :_ret  (do-sub args))
        :mult (set-env-value :_ret  (do-mult args))
        :div (set-env-value :_ret  (do-div args))
        :mod (set-env-value :_ret  (do-mod args))
        :string (set-env-value :_ret (parse-string args))
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
      (if cur-inst (exec cur-inst))
      (if (> (count remaining) 0)
        (recur (first remaining) (rest remaining))))))

(defn run-program
  "runs a program string"
  [program]
  (let [ast (parser program)]
    (if (not (instance? clojure.lang.PersistentVector ast))
      (println ast)
      (if (= (first ast) :prog)
        (do
        ;; parse the program, exec global scope
          (exec-block (rest ast))
        ;; run main
          (if-let [main (get-env-value :main)]
            (if (= (:type main) :function)
              (exec-fn (:value main) [])
              (throw (Exception. "main is not a function")))
            (throw (Exception. "no main found"))))
        (throw (Exception. "Invalid program"))))))

(defn -main
  [& args]
  (run-program (slurp (first args))))


