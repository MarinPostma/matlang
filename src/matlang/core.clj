(ns matlang.core
  (:require [instaparse.core :as insta]
            [rhizome.viz]
            [clojure.core.match :refer [match]])
  (:gen-class))

(def parser
  (insta/parser
   "
   prog = (spaces form spaces)*
   <form> = (block | control_flow | ( expr spaces <';'> ))
   <expr> = assig | add-sub | logical_statement
   <control_flow> = if_statement | while_statement
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
   number = #'-?[0-9]+'
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
      (println args)
      (assert (every? #(= (count %) cols) mat) "invalid matrix size"))
    (assoc (apply merge args) :_ret {:type :matrix
                                     :val {:shape (list rows cols)
                                           :mat (map #(map :val %) mat)}})))

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

(defn transpose [mat]
  (do
    (println "this is the mat" mat)
    (assert (= (:type mat) :matrix) "Only matrix can be transposed")
    (let [{shape :shape mat :mat} (:val mat)]
      {:type :matrix
       :val {:shape (list (second shape) (first shape))
             :mat (apply map vector mat)}})))

(defn op-dispatch [fs]
  (fn [lhs rhs]
    (match [((comp :type :_ret) lhs) ((comp :type :_ret) rhs)]
      [:scalar :scalar] ((:ss fs) lhs rhs)
      [:matrix :scalar] ((:ms fs) lhs rhs)
      [:scalar :matrix] ((:sm fs) lhs rhs)
      [:matrix :matrix] ((:mm fs) lhs rhs))))

(defn apply-op
  "applies `op` the args (isn the fort [lhs rhs]) and return the resulting environement"
  [env op args]
  (let [[lhs rhs] (map (partial evaluate env) args)]
    (op lhs rhs)))

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
                   ; TODO: implement matrix multiplication
                   :mm (make-type-lhs-rhs (mm-op -) :matrix)})

(def div-dispatch {:ss (make-type-lhs-rhs / :scalar)
                   :ms (make-type-lhs-rhs (ms-op /) :matrix)
                   :sm #((make-type-lhs-rhs (ms-op /) :matrix) %2 %1)
                   ; TODO: matrix matrix should not be allowed
                   :mm (make-type-lhs-rhs (mm-op /) :matrix)})

(def mod-dispatch {:ss (make-type-lhs-rhs mod :scalar)
                   :ms (make-type-lhs-rhs (ms-op mod) :matrix)
                   :sm #((make-type-lhs-rhs (ms-op mod) :matrix) %2 %1)
                   ; TODO: matrix matrix should not be allowed
                   :mm (make-type-lhs-rhs (mm-op mod) :matrix)})

(defn op-dispatch [fs]
  (fn [lhs rhs]
    (match [((comp :type :_ret) lhs) ((comp :type :_ret) rhs)]
      [:scalar :scalar] ((:ss fs) lhs rhs)
      [:matrix :scalar] ((:ms fs) lhs rhs)
      [:scalar :matrix] ((:sm fs) lhs rhs)
      [:matrix :matrix] ((:mm fs) lhs rhs))))

(defn make-if
  ([env _test block]
   ; side effects in the test statement affect the outer scope
   ; if blocks
   (let [{ret :_ret :as env1} (evaluate env _test)]
     (if (:val ret) (evaluate env1 block) env1)))
  ; if else blocks
  ([env _test if-block else-block]
   (let [{ret :_ret :as env1} (evaluate env _test)]
     (if (:val ret) (evaluate env1 if-block) (evaluate env1 else-block)))))

(defn evaluate [env ast]
  (let [token (first ast) params (rest ast)]
    (match [token]
      [:prog] (reduce evaluate env params)
          ; block have internal scoping, when merging their environement, only the values
          ; of the parent env should be updated the other can be discarded...
          ; also a block should not return value
      [:block] (merge env (select-keys (assoc (reduce evaluate env params) :_ret nil) (keys env)))
      [:assig] (let [[{varname :_ret :as env1} {value :_ret :as env2}] (map (partial evaluate env) params)] (assoc (merge env1 env2) varname value :_ret value))
      [:varget] (let [{varname :_ret :as env1} (evaluate env (second ast))] (assoc env1 :_ret varname))
          ; operations
      [:add] (apply-op env (op-dispatch add-dispatch) params)
      [:sub] (apply-op env (op-dispatch sub-dispatch) params)
      [:mult] (apply-op env (op-dispatch mul-dispatch) params)
      [:div] (apply-op env (op-dispatch div-dispatch) params)
      [:mod] (apply-op env (op-dispatch mod-dispatch) params)
      [:_mat] (apply make-matrix (map (partial evaluate env) params))
      [:_vec] (let [elems (map (partial evaluate env) params)] (assoc (apply merge elems) :_ret (map :_ret elems)))
          ; boolean stuff
      [:equal] (apply-op env (make-type-lhs-rhs = :boolean) params)
          ; straightforward parsing
      [:number] (assoc env :_ret {:type :scalar :val (Integer/parseInt (second ast))})
      [:if_statement] (apply make-if env params)
      [:varname] (assoc env :_ret (keyword (second ast)))
      [:true] (assoc env :_ret {:type :boolean :value true})
      [:false] (assoc env :_ret {:type :boolean :value false})
      :else (println "Unknown operation" ast))))

(def prg "b = 17; if 12 == 13 {b = 12;} else {b = 31;}")

(def evaluator (->> prg parser interpreter))
;((-> prg parser (partial execute {})))
(evaluate {} (parser prg))
;(evaluator 1 7)
