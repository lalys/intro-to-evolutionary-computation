(ns intro-to-ec.heuristic_search
  (:require [clojure.set :as cset]
            [clojure.data.priority-map :as cpm]))

;(require '[clojure.data.priority-map :as cpm])
;use cpm/priority-map to call priority-map
;(require '[intro-to-ec.heuristic_search :as hs])
;(require '[intro-to-ec.grid-problem-with-walls :as gpw])
;(hs/search hs/breadth-first-search (gpw/make-grid-problem -10 10 gpw/low-vertical-wall)[4 4] 100)

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

(def hs
  {:get-next-node #(first (first %))
  :add-children #(reduce (fn [front child] (assoc front child (%1 child))) )})

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
  (loop [frontier (cpm/priority-map first (heuristic first)) ;; Make priority map
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
            heuristic
            (pop frontier)
            kids
            (rest frontier))
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids)
           (inc num-calls)))))))
