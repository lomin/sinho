# sinho

*sinho* provides a matcher called ```=*``` for writing tests with a high signal-to-noise ratio.

## Rationale

See this notebook about [Signal-To-Noise Ratio of Software Tests](https://nextjournal.com/lomin/signal-to-noise-ratio-of-software-tests).

### Alternatives

* [nubank/matcher-combinators](https://github.com/nubank/matcher-combinators)

## Getting Started

*sinho* is available from Clojars. Add the following dependency to your *deps.edn* or *project.clj*:

[![Current Version](https://clojars.org/me.lomin/sinho/latest-version.svg)](https://clojars.org/me.lomin/sinho)

### Example
```clojure
(ns me.lomin.sinho.example-test
  (:require [clojure.test :refer :all]
            [me.lomin.sinho.matcher :refer [=*]]))

(defn query-twitter-for [user] ; fake implementation
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

## About

Sinho (신호) means signal in Korean.

## License

Copyright © 2020 Steven Collins

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.