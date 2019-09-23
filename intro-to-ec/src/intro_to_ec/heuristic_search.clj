(ns intro-to-ec.heuristic_search
  (:require [clojure.set :as cset]
            [clojure.data.priority-map :as cpm]))

;;(require '[clojure.data.priority-map :as cpm])
;;use cpm/priority-map to call priority-map

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

(defn children
  {reduce (fn [cf child] (assoc cf child current-node)) came-from kids
    })

(defn hs
  {:get-next-node first
  :add-children children})


(defn abs [n] (max n (- n)))

(def h-search
  {
   return abs(a.x - b.x) + abs(a.y - b.y)})

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
   {:keys [goal? make-children]}
   start-node max-calls]
  (loop [frontier [:a :b :c] ;; Make priority map
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
           (reduce (fn [cf child] (assoc cf child current-node)) came-from kids) ;; Add heuristic-search
           (inc num-calls)))))))
