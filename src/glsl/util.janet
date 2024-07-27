(use judge)

# like each, but the final form will not be evaluated on the last element
(defmacro each-last [name ind & exprs]
  (def always (drop -1 exprs))
  (def not-last (last exprs))
  (with-syms [$i $ind]
    ~(let [,$ind ,ind]
      (var ,$i (next ,$ind))
      (while ,$i
        (let [,name (in ,$ind ,$i)]
          ,;always
          (set ,$i (next ,$ind ,$i))
          (when ,$i ,not-last))))))

(test-stdout (each-last x [1 2 3] (print x) (print "hello") (print "sep")) `
  1
  hello
  sep
  2
  hello
  sep
  3
  hello
`)

(defn intercalate [ind el]
  (def result @[])
  (each-last x ind
    (array/push result x)
    (array/push result el))
  result)

(test (intercalate [1 2 3] ",") @[1 "," 2 "," 3])

(defmacro wrap-when [bool before & rest]
  (def after (last rest))
  (def during (drop -1 rest))
  (with-syms [$bool]
    ~(let [,$bool ,bool]
      (when ,$bool ,before)
      ,;during
      (when ,$bool ,after))))

(test-stdout (wrap-when true (prin "(") (prin "body") (prin ")")) `
  (body)
`)
(test-stdout (wrap-when false (prin "(") (prin "body") (prin ")")) `
  body
`)


(defn ptuple? [x] (and (tuple? x) (= (tuple/type x) :parens)))
(defn btuple? [x] (and (tuple? x) (= (tuple/type x) :brackets)))
