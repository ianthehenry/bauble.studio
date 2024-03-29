(use judge)
(use /src/infix-syntax)

(defmacro* test-pipe [form & expectations]
  ~(test-macro (pipe-syntax ,form) ,;expectations))

(deftest "pipe-syntax syntax"
  (test-pipe (box 50 | scale 2)
    (-> (box 50) (scale 2))))

(deftest "first argument is a single atom"
  (test-pipe (box | scale 2)
    (-> box (scale 2))))

(deftest "intermediate argument is a single atom"
  (test-pipe (box | foo | bar 2)
    (-> box (foo) (bar 2))))

(deftest "final argument is a single atom"
  (test-pipe (box | foo 2 | bar)
    (-> box (foo 2) (bar))))

(deftest "nested pipes"
  (test-pipe (box | morph (sphere 50 | scale) | rotate | scale 2)
    (-> box (morph (-> (sphere 50) (scale))) (rotate) (scale 2))))

(deftest "infix operators"
  (test-pipe (x + y)
    (-> x (+ y))))

(deftest "prefix math still works normally"
  (test-pipe (+ x y z)
    (+ x y z)))

(deftest "infix operators will invoke multiple arguments"
  (test-pipe (x + sin y)
    (-> x (+ (sin y)))))

(deftest "infix operators will not invoke single arguments"
  (test-pipe (x + (sin y) + z)
    (-> x (+ (sin y)) (+ z))))

(deftest "infix operators chain with pipe"
  (test-pipe (x + sin y | abs)
    (-> x (+ (sin y)) (abs))))

(deftest "you can end with an infix operator"
  (test-pipe (x + sin y -)
    (-> x (+ (sin y)) (-))))

(deftest "no operator precedence"
  (test-pipe (x + y * z)
    (-> x (+ y) (* z))))
