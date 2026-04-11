# sinho

*sinho* provides a matcher called ```=*``` for writing tests with a high signal-to-noise ratio.

Works with Clojure, ClojureScript, and Babashka.

## Rationale

See this notebook about [Signal-To-Noise Ratio of Software Tests](https://nextjournal.com/lomin/signal-to-noise-ratio-of-software-tests).

## Getting Started

*sinho* is available from Clojars. Add the following dependency to your *deps.edn* or *project.clj*:

[![Current Version](https://clojars.org/me.lomin/sinho/latest-version.svg)](https://clojars.org/me.lomin/sinho)

For Babashka, add to your `bb.edn`:

```edn
{:deps {me.lomin/sinho {:mvn/version "2.1.2"}}}
```

### Examples

#### Direct usage
```clojure
(ns me.lomin.sinho.example-test
  (:require [clojure.test :refer :all]
            [me.lomin.sinho.matcher :refer [=*]]))

(defn query-twitter-for [_user] ; fake implementation
  {:name           "Steven Collins"
   :twitter        "@lomin"
   :following      169
   :followers      122
   :person-of-note {{:name     "Borussia Dortmund"
                     :twitter  "@BlackYellow"
                     :folowing 146
                     :folowers 4579000}   :favorite
                    {:name      "FC Bayern English"
                     :twitter   "@FCBayernEN"
                     :following 49
                     :followers 11000000} :blocked}})

(deftest blocks-all-evil-test
         (is (=* {:twitter        "@lomin"
                  :person-of-note {{:twitter "@FCBayernEN"} :blocked}}
                 (query-twitter-for "@lomin"))))

(deftest failing-blocks-all-evil-test
         (is (=* {:twitter        "@lomin"
                  :person-of-note {{:instagram "@FCBayernEN"} :blocked}}
                 (query-twitter-for "@lomin"))))
```

#### As a [nubank/matcher-combinator](https://github.com/nubank/matcher-combinators)
```clojure
(ns me.lomin.sinho.example-matcher-combinator-test
  (:require
   [clojure.test :as t :refer [deftest is]]
   [me.lomin.sinho.matcher-combinator :refer [=*]]))

(defn query-twitter-for [_user] ; fake implementation
  {:name "Steven Collins"
   :twitter "@lomin"
   :following 169
   :followers 122
   :person-of-note {{:name "Borussia Dortmund"
                     :twitter "@BlackYellow"
                     :folowing 146
                     :folowers 4579000} :favorite
                    {:name "FC Bayern English"
                     :twitter "@FCBayernEN"
                     :following 49
                     :followers 11000000} :blocked}})

(deftest blocks-all-evil-test-mc
  (is (match? (=* {:twitter "@lomin"
                   :person-of-note {{:twitter "@FCBayernEN"} :blocked}})
              (query-twitter-for "@lomin"))))

(deftest failing-blocks-all-evil-test-mc
    (is (match? (=* {:twitter "@lomin"
                     :person-of-note {{:instagram "@FCBayernEN"} :blocked}})
                (query-twitter-for "@lomin"))))
```

## Platform Notes

The `=*` matcher API is identical across Clojure, ClojureScript, and Babashka. There are a few differences to be aware of:

- **Babashka** requires version 0.9.159 or later.
- **Test output:** On Clojure with [Kaocha](https://github.com/lambdaisland/kaocha), `=*` failures render as [deep-diff2](https://github.com/lambdaisland/deep-diff2) diffs. On ClojureScript, deep-diff2 diffs are available but without Kaocha integration. On Babashka, you get standard `clojure.test` failure output (no deep-diff2 or Kaocha).

## Predicate Matching with `pred`

*New in 3.0.* Use `(pred f)` to match values by predicate instead of equality:

```clojure
(require '[me.lomin.sinho.pred :refer [pred named-pred]])

;; Type check in a map
(=* {:name (pred string?)
     :age  (pred #(< 0 % 200))}
    {:name "Alice" :age 30 :email "a@b.com"})
;; => match (returns expected)

;; Named predicate for readable failure messages
(=* (named-pred "positive?" pos?) -5)
;; => #Mismatch{:expected #Pred{:label "positive?"}, :actual -5}
```

**Bare functions** in expected positions are compared by value equality, not
interpreted as predicates. This eliminates the ambiguity between
"predicate-on-actual" and "literal function-value equality."

## Migration from 2.x to 3.0

1. **Wrap inline predicates with `pred`.** If you previously used bare functions
   in expected positions expecting predicate behavior, wrap them:
   ```clojure
   ;; 2.x (ambiguous)
   (=* string? actual)

   ;; 3.0 (unambiguous)
   (=* (pred string?) actual)
   ```

2. **Diff shapes may differ on ambiguous sets.** If your tests pinned specific
   diff shapes for sets of structurally-similar compound elements, the 3.0
   greedy matching may produce a different (but still correct) diff. Update
   those assertions or switch to verdict-level assertions (`(= expected (=* ...))`).

3. **Performance is dramatically better.** Map comparison is `O(n)` for atom
   keys (down from `O(n!)` worst case). Set comparison is `O(n^3)` for verdicts
   (down from factorial). Sequential comparison is `O(m*n)` (down from exponential).

## About

Sinho (신호) means signal in Korean.
