(ns matlang.types.macros)

(defmacro make-type
  [t]
  `(defn ~(symbol (str "make-" t))
     [~(symbol "value")]
     {:type ~(keyword t)
      :value ~(symbol "value")}))

