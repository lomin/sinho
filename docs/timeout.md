# Adaptive Self-Clocking Timeout Guard (CLJ/CLJS)

## 1. Executive Summary

A portable, watchdog-free way to **cooperatively** stop CPU-bound computations after a timeout on **Clojure (JVM)** and **ClojureScript (Node & Browser)**.

* You create a **`timeout?`** function with a deadline.
* Call **`(timeout?)`** at natural boundaries. It returns **`true`** iff you must stop **now**.
* Overhead is minimized by **adaptive stride-based sampling** (check the clock once every *k* calls; adapt *k* using EWMA/EWVAR + a **decayed max** hedge).
* No partial/result storage or copying; user code returns its own current value when `timeout?` is `true`.

**Target overshoot envelope** (with reasonable call sites):

```
p99 overshoot ≤ max(S, min(5% × timeout, 10ms))
```

where **S** is the p99 cost of a single atomic step between checks.

---

## 2. Contract & Constraints

* **Cooperative model:** Timeouts are only observed at `timeout?` call-sites.
* **Immediate exit:** When `(timeout?) => true`, return your current value immediately.
* **Thread confinement:** One guard per compute thread/worker.
* **Single implementation:** Only input is `timeout-ms`.

---

## 3. Public API (final)

```clojure
;; Factory — the ONLY public function
(make-timeout timeout-ms) ; => timeout? IFn

;; The returned `timeout?` function supports:
;;  - 0-arity:   ([]      -> boolean)   ; true => stop now
;;  - 1-arity:   ([:timeout/debug] -> debug-map)

;; Debug map schema (see §8):
;; {:timeout/timeout?  true|false
;;  :timeout/debug     { ... namespaced keys ... }}
```

**Typical usage**

```clojure
(let [timeout? (make-timeout 200)]          ;; 200 ms
  (reduce (fn [acc x]
            (if (timeout?)
              (reduced acc)                ;; return current value
              (step acc x)))
          init
          xs))
```

---

## 4. High-Level Design

A `Guard` tracks time and simple stats to choose stride `k`—how many `timeout?` calls to skip before reading the clock again.

```
┌───────────────────────────────────────────┐
│               Timeout Guard               │
│  deadline_us, target_overshoot_us,        │
│  start_us (immutable fields)              │
│  last_sample_us, steps_since, stride_k    │
│  mean_us (EWMA), var_us (EWVAR),          │
│  decayed_max_us (tail hedge), timed_out,  │
│  detected_at_us                           │
│  debug counters (check/sample/panic/...)  │
│                                           │
│  timeout?()                               │
│    ├─ Fast path: steps_since < stride_k   │
│    └─ Sample: clock→stats→stride→deadline │
└───────────────────────────────────────────┘
```

The implementation uses a protocol-based approach (`IGuard`) with getters/setters for field access, providing better encapsulation while maintaining performance.

---

## 5. Platform Shims (tiny, unavoidable)

Single `.cljc` with exactly two small `#?` islands:

1. **Monotonic time (`now-us`)** (JVM vs Node vs Browser)
2. **Unchecked long math** on JVM hot path (CLJS uses plain `inc`/`-`)

```clojure
#?(:clj
   (defn now-us [] 
     (quot (System/nanoTime) 1000))
   :cljs
   (defn now-us []
     (if (and (exists? js/process)
              (.-hrtime js/process)
              (.-bigint (.-hrtime js/process)))
       ;; Node: process.hrtime.bigint() -> ns BigInt
       (let [bn ((.-bigint (.-hrtime js/process)))]
         (js/Number (/ bn (js/BigInt 1000))))
       ;; Browser: performance.now() (ms double) -> µs Number
       (let [ms (if (exists? js/performance)
                  (js/performance.now)
                  (.now js/Date))]
         (js/Math.floor (* 1000 ms))))))
```

---

## 6. Algorithm

### 6.1 Fixed constants

* `Z95 = 1.64485` (normal 95th percentile)
* `ALPHA = 0.05` (EWMA/EWVAR smoothing)
* `SAFETY = 2.0` (conservative stride)
* `DECAY = 0.98` (per-sample decayed max)
* `BURN-RATIO = 0.33` (panic if a window burns >33% of remaining time)
* `KMAX = 1 << 20` (maximum stride value)
* `SAMPLE-MAX-SPACING-MS = 10.0` (time-based ceiling for next sample in milliseconds)
* `TARGET-OVERSHOOT-FRACTION = 0.05` (5% of timeout)
* `TARGET-OVERSHOOT-CAP-MS = 10.0` (10ms maximum overshoot target)

Note: `target_overshoot_us = min(10ms, 5% × timeout)` is computed using the fraction and cap constants.

### 6.2 Stats on sample steps

Let `x = elapsed_us / steps_since`:

* **EWMA:** `μ' = μ + α·(x − μ)`
* **EWVAR:** `σ²' = (1 − α)·σ² + α·(x − μ)²`
* **Decayed hedge:** `decayed_max' = max(x, decayed_max · DECAY)`

### 6.3 Stride

```
p95_proxy          = μ + Z95 * sqrt(max(σ², 0))
effective_step_us  = max(p95_proxy, decayed_max, 1.0)

remaining_us       = deadline_us − now_us()
budget_us          = max(remaining_us − target_overshoot_us, target_overshoot_us)

k_base             = floor(budget_us / (SAFETY * effective_step_us))

# Time-based ceiling: never plan next sample farther than SAMPLE_MAX_SPACING_MS
tmax_us            = 1000 * SAMPLE_MAX_SPACING_MS
k_time             = floor(tmax_us / (SAFETY * effective_step_us))

stride_k           = clamp( min(k_base, k_time), 1, KMAX)
```

The time-based ceiling ensures the system samples at least every 10ms of wall-clock time, preventing excessively long periods without checking the deadline.

### 6.4 Panic mode

If `(elapsed / remaining) > BURN_RATIO` in the current window ⇒ set `stride_k = 1`.

---

## 7. Thread Confinement Model

Each timeout guard is designed for single-threaded use (one guard per compute thread). Multiple guards can run concurrently in different threads without coordination. The guard uses mutable fields internally but is not thread-safe - each thread should create its own guard instance.

---

## 8. Debug Payload (for tests)

`(timeout? :timeout/debug)` returns:

```clojure
{:timeout/timeout?  <boolean>     ; true iff algorithm declared timeout (detected)
 :timeout/debug     {             ; gauges & counters for assertions
   :timeout.debug/start-us
   :timeout.debug/now-us
   :timeout.debug/deadline-us
   :timeout.debug/detected-at-us  ; 0 if not yet detected
   :timeout.debug/overshoot-us
   :timeout.debug/target-overshoot-us

   :timeout.debug/check-count
   :timeout.debug/sample-count
   :timeout.debug/panic-count

   :timeout.debug/stride-final
   :timeout.debug/stride-min
   :timeout.debug/stride-max
   :timeout.debug/stride-shrink-count

   :timeout.debug/mean-step-us
   :timeout.debug/var-step-us
   :timeout.debug/p95-proxy-us
   :timeout.debug/decayed-max-us
   :timeout.debug/effective-step-us
   :timeout.debug/burn-max }}
```

These let you assert, e.g.:

* Detection latency: `max 0 (detected-at-us − deadline-us)` within envelope.
* Efficiency: `sample-count / check-count` is small when far from the deadline.
* Adaptation wasn't pinned: reasonable `stride-min/max`, decayed-max behavior, limited panic.

---

## 9. Performance & Limits

* **Fast path:** counter inc + compare; zero alloc; JVM uses unchecked ops.
* **Sample path:** one monotonic time read + a few FP ops; panic forces `k=1`.
* **Limits:** Overshoot is probabilistic; very long atomic steps or GC/STW pauses can exceed the envelope. Insert `timeout?` at natural boundaries.

---

## 10. Testing & Acceptance

The library includes comprehensive tests demonstrating the timeout behavior:

* **Property tests:** Statistical verification of overshoot bounds across varying workloads
* **Platform tests:** Cross-platform compatibility (CLJ/CLJS) validation
* **Acceptance tests:** Integration scenarios showing real-world usage patterns

Tests verify that the implementation maintains the target overshoot envelope under various conditions while minimizing overhead through adaptive sampling.