(ns arrangement.core)

(defn rank
  "Compares two vectors lexicographically for sorting.
   Returns negative, zero, or positive number based on comparison.
   Handles heterogeneous types by falling back to string comparison."
  [a b]
  (try
    (compare a b)
    (catch #?(:clj Exception :cljs js/Error) _
      (compare (str a) (str b)))))