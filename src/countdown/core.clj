(ns countdown.core
  (:require [clojure.math.combinatorics :as combo]))

(defn pow
  "Takes a power"
  [x y]
  (Math/pow x y))

(defn generate-expressions
  "Generates all postfix expressions using given operands.  Expressions not guaranteed valid."
  [operands]
  (let [operators (list + - * / pow)
        op-selections (combo/selections operators (dec (count operands)))
        unshuffled (map #(concat operands %) op-selections)
        ]
    (apply concat (map combo/permutations unshuffled))))

(defn eval-postfix
  "Evaluate a postfix expression."
  ([expression] (eval-postfix expression (list)))
  ([expression stack]
   (if (empty? expression)
     ; base case
     (if (= 1 (count stack))
       (first stack)
       "bad expression")
     ; evaluate next piece of expression
     (if (or (= (type 2.0) (type (first expression)))
             (= (type 2) (type (first expression))))
       ; push an operand
       (recur (rest expression) (cons (first expression) stack))
       ; for operator, pop top two of stack, push result
       (if (and (< 1 (count stack))
                (or (not= (first expression) /)
                    (and (not (== 0 (first stack))))))
         (recur (rest expression)
                (cons ((first expression)
                       (nth stack 1)
                       (first stack))
                      (rest (rest stack))))
         "bad expression")))))

(defn expression-to-str
  "Return an expression as a printable string."
  [expression]
  (map #(if (= % +)
          "+"
          (if (= % -)
            "-"
            (if (= % *)
              "*"
              (if (= % /)
                "/"
                (if (= % pow)
                  "^"
                  %)))))
       expression))

(defn find-expressions
  "Find all postfix expressions using these operands that result in target."
  [target operands]
  (filter #(= (eval-postfix %) target) (generate-expressions operands)))
