(ns intro-to-ec.heuristic_search
  (:require [clojure.set :as cset]
            [clojure.data.priority-map :as cpm]
            ;[intro-to-ec.grid-problem-with-walls :as gpw]
            ;[intro-to-ec.heuristic_search :as hs]
            ))

;(require '[clojure.data.priority-map :as cpm]) (require '[intro-to-ec.heuristic_search :as hs]) (require '[intro-to-ec.grid-problem-with-walls :as gpw])
;use cpm/priority-map to call priority-map
;(hs/search hs/breadth-first-search (gpw/make-grid-problem -10 10 gpw/low-vertical-wall)[4 4] 100)
;(hs/heuristic-search hs/heu (gpw/make-grid-problem -10 10 gpw/low-vertical-wall)[4 4] 50)
;(hs/A-star-search hs/a-star (gpw/make-grid-problem -10 10 gpw/low-vertical-wall hs/a-star)[4 4] 50)

(defn remove-previous-states
  [new-states frontier visited]
  (remove (cset/union (set frontier) (set visited)) new-states))

(def depth-first-search
  {:get-next-node first
   :add-children concat})

(def breadth-first-search
  {:get-next-node first
   :add-children #(concat %2 %1)})

(def random-search
  {:get-next-node rand-nth
   :add-children concat})

(defn generate-path
  [came-from node]
  (if (= :start-node (get came-from node))
    [node]
    (conj (generate-path came-from (get came-from node)) node)))

(def heu
  {:get-next-node #(first (first %))
  :add-children #(reduce (fn [first child] (assoc first child (%1 child nil nil))) %2 %3)})


(def a-star
  {:get-next-node #(first (first %))
    :add-children #(reduce (fn [first child] (assoc first child ( + (%1 child) %4 ))) %2 %3)})

  (defn search
    [{:keys [get-next-node add-children]}
     {:keys [goal? make-children]}
     start-node max-calls]
    (loop [frontier [start-node]
           came-from {start-node :start-node}
           num-calls 0]
      (println num-calls ": " frontier)
      (println came-from)
      (let [current-node (get-next-node frontier)]
        (cond
          (goal? current-node) (generate-path came-from current-node)
          (= num-calls max-calls) :max-calls-reached
          :else
          (let [kids (remove-previous-states
                      (make-children current-node) frontier (keys came-from))]
            (recur
             (add-children
              kids
              (rest frontier))
             (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
             (inc num-calls)))))))

(defn heuristic-search
  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children heuristic]}
   start-node max-calls]
  (loop [frontier (cpm/priority-map start-node (heuristic start-node nil nil))
         came-from {start-node :start-node}
         num-calls 0]
    (println num-calls ": " frontier)
    (println came-from)
    (let [current-node (get-next-node frontier)]
      (cond
        (goal? current-node) (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (remove-previous-states
                    (make-children current-node) frontier (keys came-from))]
          (recur
           (add-children heuristic (pop frontier) kids)
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (inc num-calls)))))))

 (defn A-star-search
  [{:keys [get-next-node add-children]}
   {:keys [goal? make-children heuristic]}
   start-node max-calls]
  (loop [frontier (cpm/priority-map start-node (heuristic start-node) nil nil)
         cost-so-far {start-node 0}
         came-from {start-node :start-node}
         num-calls 0]
    (println num-calls ": " frontier)
    (println came-from)
    (println "The cost is: " cost-so-far)
    (let [current-node (get-next-node frontier)
          new-cost (+ (get cost-so-far current-node) 1)]
      (cond
        (goal? current-node) (generate-path came-from current-node)
        (= num-calls max-calls) :max-calls-reached
        :else
        (let [kids (remove-previous-states
                    (make-children current-node) frontier (keys came-from))]
          (recur
          (add-children heuristic (pop frontier) kids new-cost)
          (reduce (fn [c child] (assoc c child (heuristic child nil nil))) cost-so-far kids)
          (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
          (inc num-calls)))))))
