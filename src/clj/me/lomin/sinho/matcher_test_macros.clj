(ns me.lomin.sinho.matcher-test-macros
  (:require [cljs.test]))

(defmethod cljs.test/assert-expr '=*
  [env msg form]
  (let [[_ expected] form]
    `(let [result# ~form]
       (cljs.test/do-report {:type (if (= ~expected result#) :pass :fail)
                             :message ~msg
                             :expected '~form
                             :actual result#})
       result#)))