(ns com.manigfeald.match-test
  (:require [clojure.test :refer :all]
            [com.manigfeald.match :refer :all]))


;; example extension
(declare klass)

(defmethod to-pattern-seq 'com.manigfeald.match-test/klass [row]
  (assert (= 2 (count row)))
  (let [row (second row)
        value-patterns (into {} (for [[k v] row] [k (to-pattern v)]))
        access (fn [value methodName]
                 `(let [class# (class ~value)
                        method# (try
                                  (.getMethod class# ~methodName nil)
                                  (catch Exception e#))]
                    (when method#
                      (try
                        (.invoke method# ~value nil)
                        (catch Exception e#)))))]
    (reify
      Pattern
      (guard [pattern value]
        `(and ~@(for [[k v] value-patterns]
                  (guard v (access value (name k))))))
      (bindings [pattern value]
        (for [[k v] value-patterns
              i (bindings v (access value (name k)))]
          i)))))


(deftest a-test
  (is (match 1
             1 true
             _  false))
  (is (nil? (match 2
                   1 true)))
  (is (= 1 (match [1]
                  [x] x)))
  (is (= 2 (match {:a 2}
                  {:a x} x)))
  (is (match 0
             (x (zero? x)) true
             _ false))
  (is (not (match 1
                  (x (zero? x)) true
                  _ false)))
  (is (= (match 'a
                'a true)))
  (is (= [2] (match [1 2]
                    (cons _ y) y)))
  (is (= 1 (match [1 2]
                  [x 2] x)))
  (is (= nil (match [1 2]
                    [x] x)))
  (is (match [1 1]
             [x x] true))
  (is (match [1 2]
             [x x] false
             _ true))
  (is  (match {:a 1 :b 2}
              ({:a x :b y} (= (* x 3) y)) false
              ({:a x :b y} (= (* x 2) y)) true))
  (is  (match (list 1 2 3)
              (cons x (cons x (cons 3 nil))) false
              ((cons x (cons y (cons z nil))) (= z (+ x y))) true))
  ;; use the example extension
  (is (match [107 #inst "2008"]
             ([y (klass {:getYear x})] (not= x y)) false
             [x (klass {:getYear x})] true)))

;; (defmethod to-pattern-seq 'instant [row]
;;   (assert (= 3 (count row)))
;;   (to-pattern-seq `(cons ~(nth row 1) ~(nth row 2))))

#_(prn (macroexpand '(match (list 1 2 3)
              (cons x (cons x (cons 3 nil))) false
              ((cons x (cons y (cons z nil))) (= z (+ x y))) true)))

