(ns me.lomin.sinho.pred-test
  "Test suite for the (pred f) wrapper — v3 predicate escape hatch."
  (:require [clojure.test :as t :refer [deftest is testing]]
            [me.lomin.sinho.matcher :refer [=*]]
            [me.lomin.sinho.pred :as pred]
            [matcher-combinators.model :refer [->Mismatch]]))

;; ── Derive-hierarchy check ──────────────────────────────────────

(derive ::circle ::shape)
(derive ::square ::shape)

(deftest pred-hierarchy-test
  (testing "pred with isa? hierarchy check — match returns expected"
    (let [p (pred/pred #(isa? % ::shape))]
      (is (= p (=* p ::circle)))
      (is (= p (=* p ::square)))))
  (testing "pred with isa? hierarchy check — mismatch"
    (let [p (pred/pred #(isa? % ::shape))]
      (is (= (->Mismatch p :not-a-shape)
             (=* p :not-a-shape))))))

;; ── Type predicate ──────────────────────────────────────────────

(deftest pred-type-check-test
  (testing "pred with string? type predicate — match"
    (let [p (pred/pred string?)]
      (is (= p (=* p "hello")))))
  (testing "pred with string? type predicate — mismatch"
    (let [p (pred/pred string?)]
      (is (= (->Mismatch p 42)
             (=* p 42))))))

;; ── Range predicate ─────────────────────────────────────────────

(deftest pred-range-test
  (testing "pred with range predicate — match"
    (let [p (pred/pred #(< 0 % 100))]
      (is (= p (=* p 50)))))
  (testing "pred with range predicate — mismatch"
    (let [p (pred/pred #(< 0 % 100))]
      (is (= (->Mismatch p 200)
             (=* p 200))))))

;; ── Named predicates ────────────────────────────────────────────

(deftest named-pred-test
  (testing "named-pred produces readable labels"
    (let [p (pred/named-pred "positive?" pos?)]
      (is (= p (=* p 5)))
      (is (= "positive?" (pred/pred-label-for-diff p))))))

;; ── Pred in nested structures ───────────────────────────────────

(deftest pred-nested-test
  (testing "pred inside a map value — match returns expected"
    (let [expected {:name (pred/pred string?)
                    :age (pred/pred #(< 0 % 200))}
          actual {:name "Alice" :age 30 :email "a@b.com"}]
      (is (= expected (=* expected actual)))))

  (testing "pred inside a vector — match returns expected"
    (let [expected [1 (pred/pred string?) 3]]
      (is (= expected (=* expected [1 "hello" 3])))))

  (testing "pred mismatch in nested map"
    (let [p (pred/pred pos?)
          result (=* {:x p} {:x -1})]
      (is (= {:x (->Mismatch p -1)} result)))))

;; ── Bare functions are NOT treated as predicates ────────────────

(deftest bare-fn-equality-test
  (testing "bare function compared by value equality, not as predicate"
    (is (= string? (=* string? string?)))
    (is (= (->Mismatch string? pos?) (=* string? pos?)))))
