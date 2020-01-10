(ns matlang.types.ops)

(def op-matrix
  {:op-plus {:integer {:integer +
                       :float +
                       :matrix
                       :string
                       :boolean}
             :float {:integer +
                     :float +
                     :matrix
                     :string
                     :boolean}
             :matrix {:integer +
                      :float +
                      :matrix
                      :string
                      :boolean}
             :string {:integer +
                      :float +
                      :matrix
                      :string
                      :boolean}
             :boolean {:integer +
                       :float +
                       :matrix
                       :string
                       :boolean}}

   :op-minus {:integer {:integer +
                        :float +
                        :matrix
                        :string
                        :boolean}
              :float {:integer +
                      :float +
                      :matrix
                      :string
                      :boolean}
              :matrix {:integer +
                       :float +
                       :matrix
                       :string
                       :boolean}
              :string {:integer +
                       :float +
                       :matrix
                       :string
                       :boolean}
              :boolean {:integer +
                        :float +
                        :matrix
                        :string
                        :boolean}}

   :op-prod {:integer {:integer +
                       :float +
                       :matrix
                       :string
                       :boolean}
             :float {:integer +
                     :float +
                     :matrix
                     :string
                     :boolean}
             :matrix {:integer +
                      :float +
                      :matrix
                      :string
                      :boolean}
             :string {:integer +
                      :float +
                      :matrix
                      :string
                      :boolean}
             :boolean {:integer +
                       :float +
                       :matrix
                       :string
                       :boolean}}

   :op-div {:integer {:integer +
                      :float +
                      :matrix
                      :string
                      :boolean}
            :float {:integer +
                    :float +
                    :matrix
                    :string
                    :boolean}
            :matrix {:integer +
                     :float +
                     :matrix
                     :string
                     :boolean}
            :string {:integer +
                     :float +
                     :matrix
                     :string
                     :boolean}
            :boolean {:integer +
                      :float +
                      :matrix
                      :string
                      :boolean}}

   :op-mod {:integer {:integer +
                      :float +
                      :matrix
                      :string
                      :boolean}
            :float {:integer +
                    :float +
                    :matrix
                    :string
                    :boolean}
            :matrix {:integer +
                     :float +
                     :matrix
                     :string
                     :boolean}
            :string {:integer +
                     :float +
                     :matrix
                     :string
                     :boolean}
            :boolean {:integer +
                      :float +
                      :matrix
                      :string
                      :boolean}}})

(defn get-op
  [lhs rhs op]
  (let [type-lhs (:type lhs)
        type-rhs (:type rhs)
        operation (op op-matrix)]
    (if (type-lhs operation)
      (if (->> operation type-rhs type-lhs)
        (->> operation type-rhs type-lhs))
      (throw (Exception
              (str "No operation "
                   (name operation)
                   " for types"
                   (name type-lhs)
                   " and "
                   (name type-rhs)))))))
