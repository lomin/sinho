# Changelog

## 3.0.0

### New feature: `(pred f)` predicate wrapper

The `pred` wrapper provides an unambiguous predicate escape hatch at the value
level. Wrap a 1-arg function with `pred` to match against the actual value at
that position:

```clojure
(require '[me.lomin.sinho.pred :refer [pred named-pred]])

;; Type check
(=* {:name (pred string?)} {:name "Alice" :age 30})
;; => {:name #Pred{...}, :age 30}  (match)

;; Range check
(=* (pred #(< 0 % 100)) 50)
;; => #Pred{...}  (match)

;; Named predicate for readable failure diffs
(=* (named-pred "shape?" #(isa? % ::shape)) :not-a-shape)
;; => #Mismatch{:expected #Pred{:label "shape?"}, :actual :not-a-shape}
```

**Important:** bare functions in expected positions are compared by value
equality, not interpreted as predicates. `(=* string? string?)` compares the
function objects. Use `(pred string?)` to match by predicate.

### Coinductive IR decision engine (internal rewrite)

The A\*-based matcher from sinho 2.2 has been replaced with a coinductive IR
decision engine that walks the expected structure directly:

| Structure | sinho 2.2 | sinho 3.0 |
|---|---|---|
| Map with `n` keys | `O(n!)` worst case | `O(n)` for atom keys; greedy bipartite fallback for complex keys |
| Set of `m` elements in `n`-element actual | `O(n!/(n-m)!)` | `O(n^3)` (Kuhn's augmenting-paths for verdicts; greedy `O(n^2 log n)` for diffs) |
| Sequence `m` vs `n` | `O(3^(m+n))` | `O(m*n)` edit-distance DP |
| Nested structure, depth `d`, width `w` | `O((w!)^d)` | `O(w*d)` amortised via seen-set |
| Atoms | `O(1)` | `O(1)` |

### Semantic compatibility with sinho 2.2

Every `=*` call that returned truthy in 2.2 returns truthy in 3.0, and every
falsy in 2.2 returns falsy in 3.0. The **diff shape** may differ in two cases:

1. **Ambiguous sets of structurally-similar compound elements:** The greedy
   bipartite matching (strategy 2) may produce a non-minimum diff on adversarial
   inputs. The diff is still correct (it accurately identifies structural
   differences), but may not be the smallest possible diff. Verdicts use exact
   bipartite matching (Kuhn's augmenting-paths, `O(n^3)`); only the diff
   generation uses greedy (`O(n^2 log n)`).

2. **Sequential elements with edit-distance alignment:** The DP-based edit
   distance may choose a different alignment than the A\* search when multiple
   alignments have the same cost.

### Deterministic tie-breaking in set matching

Set matching breaks ties deterministically by `(hash (str (pr-str l) (pr-str r)))`.
This ensures golden-master tests do not flake across JVM runs.

### No new dependencies

sinho 3.0 keeps the same dependency graph as 2.2: `specter`,
`matcher-combinators`, `lambdaisland/deep-diff2`, optional `kaocha`.
The cljc + bb + cljs build matrix is preserved.

## 2.2.0

Previous release. See git history for details.
