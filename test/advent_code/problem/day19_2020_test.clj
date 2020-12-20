(ns advent-code.problem.day19-2020-test
  (:require [clojure.test :refer :all]
            [advent-code.interfaces :as ifaces]
            [advent-code.data-helpers :as dh]
            [advent-code.problem.day19-2020 :refer :all]))

(def raw-example-1 "0: 1 2
1: \"a\"
2: 1 3 | 3 1
3: \"b\"
")

(def consume-a (make-leaf-consume \a))
(def consume-b (make-leaf-consume \b))

(deftest test-consume-a
  (is (consume-a [\a])))

(deftest test-consume-a-false
  (is (not (consume-a [\b]))))

(def comp-a-b (make-compose [consume-a consume-b]))

(deftest test-comp-a-b
  (is (comp-a-b [\a \b])))

(def a-or-b? (make-or-fn [consume-a consume-b]))

(deftest test-a-or-b
  (is (= '(\c)
         (a-or-b? [\a \c]))))

(deftest test-function-from-tree
  (is (= [\b]
         ((make-function-for-index (build-tree raw-example-1) 1) [\a \b]))))

(deftest test-example-1-1
  (is (= '()
         ((make-function-for-index (build-tree raw-example-1) 0) [\a \a \b]))))

(deftest test-example-1-2
  (is (= '()
         ((make-function-for-index (build-tree raw-example-1) 0) [\a \b \a]))))

(deftest test-example-1-3
  (is (nil? ((make-function-for-index (build-tree raw-example-1) 0) [\a \b \b]))))

(def raw-example-2-rules "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"
")

(def raw-example-2-lines "ababbb
bababa
abbbab
aaabbb
aaaabbb
")

(def example-2-lines (map seq (dh/split-lines raw-example-2-lines)))

(deftest test-example-2
  (let [rule-tree (build-tree raw-example-2-rules)
        zero-fn (make-function-for-index rule-tree 0)]
    (is (= 2
           (count (filter #(= '() %) (map zero-fn example-2-lines)))))))

(def raw-example-2 "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb
")

(deftest actual-problem-on-example-2
  (is (= 2
         (ifaces/run-problem "day19-2020" "1" raw-example-2))))

(deftest actual-problem-1
  (is (= 208
         (ifaces/run-problem "day19-2020" "1" (slurp "resources/2020-day19.input")))))

(def prob2-example "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
")

(def split-prob2 (dh/split-paragraphs prob2-example))

(def prob2-rules (first split-prob2))
(def prob2-testlines (second split-prob2))

(def example-problem-2-lines (map seq (dh/split-lines prob2-testlines)))

(deftest test-unaltered-prob2-example
  (is (= 3
         (ifaces/run-problem "day19-2020" "1" prob2-example))))

(deftest test-reduce-compose-a-b
  (let [make-ab-fn #(make-compose [consume-a % consume-b])
        ab-fns (rest (iterate make-ab-fn identity))]
    (are [x y] (= x y)
      ((first ab-fns) '(\a \b)) '()
      ((second ab-fns) '(\a \a \b \b)) '()
      ((first ab-fns) '(\a \a \b \b)) nil)))

(deftest test-altered-prob2-examples
  (are [x y] (= y (validate-line (build-tree prob2-rules) (seq x)))
   "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa" nil
   "bbabbbbaabaabba" '()
   "babbbbaabbbbbabbbbbbaabaaabaaa" '()
   "aaabbbbbbaaaabaababaabababbabaaabbababababaaa" '()
   "bbbbbbbaaaabbbbaaabbabaaa" '()
   "bbbababbbbaaaaaaaabbababaaababaabab" '()
   "ababaaaaaabaaab" '()
   "ababaaaaabbbaba" '()
   "baabbaaaabbaaaababbaababb" '()
   "abbbbabbbbaaaababbbbbbaaaababb" '()
   "aaaaabbaabaaaaababaa" '()
   "aaaabbaaaabbaaa" nil
   "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa" '()
   "babaaabbbaaabaababbaabababaaab" nil
   "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba" '()))

(deftest test-actual-example-2
  (is (= 12
         (ifaces/run-problem "day19-2020" "2" prob2-example))))

(deftest test-actual-prob-2
  (is (= 316
         (ifaces/run-problem "day19-2020" "2" (slurp "resources/2020-day19.input")))))
