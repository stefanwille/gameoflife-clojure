(ns gameoflife.core-test
  (:require [clojure.test :refer :all]
            [gameoflife.core :refer :all]))


(defn cell-at [board x y]
  (let [row (get board y)
        cell (get row x)
        cell-string (str cell)]
    (if (= cell-string "") " " cell-string)))

(defn neighbours [board x y]
  [(cell-at board (+ x -1) (+ y -1))
   (cell-at board (+ x 0) (+ y -1))
   (cell-at board (+ x +1) (+ y -1))
   (cell-at board (+ x +1) (+ y 0))
   (cell-at board (+ x -1) (+ y 0))
   (cell-at board (+ x -1) (+ y +1))
   (cell-at board (+ x 0) (+ y +1))
   (cell-at board (+ x +1) (+ y +1))])

(defn neighbour-count [board x y] 0
  (count (filter #(= "O" %) (neighbours board x y))))

(defn next-cell [board x y]
  (let [live-neighbours (neighbour-count board x y)
        is-live-cell (= "O" (cell-at board x y))
        is-dead-cell (not is-live-cell)]
    (cond
      (and is-live-cell (< live-neighbours 2)) " "
      (and is-live-cell (or (= live-neighbours 2) (= live-neighbours 3))) "O"
      (and is-live-cell (> live-neighbours 3)) " "
      (and is-dead-cell (= live-neighbours 3)) "O"
      (and is-dead-cell (not= live-neighbours 3)) " "
      :else "UNEXPECTED CASE")))

(defn next-generation [board]
  (let [width (count (get board 0))
        height (count board)
        new-cells (for [y (range height) x (range width)] (next-cell board x y))
        rows-of-cells (partition width new-cells)
        rows (map #(apply str %) rows-of-cells)]
    rows))

(deftest cell-at-test
  (let [board ["  " " O " "O  "]]
    (testing "returns the cell at the given coordinates"
      (is (= " " (cell-at board 0 0)))
      (is (= "O" (cell-at board 1 1))))
    (testing "returns an empty cell when the coordinates lie outside the board"
      (is (= " " (cell-at board -1 0)))
      (is (= " " (cell-at board 0 -1)))
      (is (= " " (cell-at board 5 0)))
      (is (= " " (cell-at board 0 5))))))

(deftest neighbours-test
  (testing "returns the list of a cell's neighbour cells"
    (let [board [" O " " O " "O  "]]
      (is (= [" "  "O"  " "  " "  " "  "O" " " " "] (neighbours board 1 1))))))

(deftest neighbours-count-test
  (testing "returns the number of neighbours of a given cell"
    (is (= 0 (neighbour-count ["  " " O " "   "] 1 1)))
    (is (= 1 (neighbour-count ["  " " OO" "   "] 1 1)))))

(deftest next-cell-test
  (testing "Any live cell with fewer than two live neighbours dies, as if by underpopulation"
    (is (= " " (next-cell ["   " " O " "   "] 1 1)))
    (is (= " " (next-cell ["   " " OO" "   "] 1 1))))
  (testing "Any live cell with two or three live neighbours lives on to the next generation"
    (is (= "O" (next-cell ["   " "OOO" "   "] 1 1)))
    (is (= "O" (next-cell [" O " "OOO" "   "] 1 1))))
  (testing "Any live cell with more than three live neighbours dies, as if by overpopulation"
    (is (= " " (next-cell ["OO " "OOO" "   "] 1 1)))
    (is (= " " (next-cell ["OOO" "OOO" "   "] 1 1))))
  (testing "Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction"
    (is (= "O" (next-cell [" O " "O O" "   "] 1 1))))
  (testing "Any dead cell with not exactly three live neighbours stays a dead cell"
    (is (= " " (next-cell ["   " "   " "   "] 1 1)))
    (is (= " " (next-cell ["   " "  O" "   "] 1 1)))
    (is (= " " (next-cell ["OO " "O O" "   "] 1 1)))
    (is (= " " (next-cell ["OOO" "O O" "   "] 1 1)))))

(deftest next-generation-test
  (testing "returns a new board with the next generation"

    (let [board [
                 " OO" 
                 " O " 
                 "O  "
                 ]]
      (is (= [
              " OO"  
              "OOO"  
              "   "] (next-generation board)))
      
      )))
