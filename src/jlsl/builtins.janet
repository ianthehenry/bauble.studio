(use judge)
(import pat)
(use ./util)
(use ./types)
(use ./builtins-prelude)
(use ./builtin-test-helpers)

(defn- get-floaty-vector-base-type [name type]
  (type/match type
    (vec prim _) (do (check-floaty-prim prim) (type/primitive prim))
    (errorf "%s: numeric vector required" name)))

(defn- check-booly [name type]
  (type/match type
    (primitive prim) (check-bool-prim prim)
    (vec prim _) (check-bool-prim prim)
    (errorf "%s: bool or bvec required" name)))

(defn- numeric-vector-to-number [arity name arg-types]
  (check-arity name arity arg-types)
  (def return-type (get-unique (partial get-floaty-vector-base-type name) arg-types |(errorf "%s: cannot mix vector types" name)))
  (get-unique type/components arg-types |(errorf "%s: cannot mix argument lengths" name))
  (builtin name return-type arg-types))

(defn- resolve-not [name arg-types]
  (check-arity name 1 arg-types)
  (each arg-type arg-types
    (check-booly name arg-type))
  (def components (get-unique type/components arg-types |(errorf "%s: cannot mix argument lengths" name)))
  (builtin name (vec-or-prim (primitive-type/bool) components) arg-types))

(defn- just [types] (fn [name arg-types]
  (if-let [return-type (in types arg-types)]
    (builtin name return-type arg-types))))

(defn- resolve-refract [name arg-types]
  (check-arity name 3 arg-types)
  (assertf (= (in arg-types 2) type/float)
    "%s: type error, expected %q got %q" name (type/to-glsl type/float) (type/to-glsl (in arg-types 2)))
  (function/match (resolve-generic 2 name (tuple/slice arg-types 0 2))
    (builtin name return-type param-sigs) (builtin name return-type [;param-sigs type/float])
    (error "BUG")))

(defn- resolve-one-multiply [l r]
  (type/match l
    (primitive _) r
    (vec _ l-components)
      (type/match r
        (primitive _) l
        (vec _ r-components) (do
          (assert (= l-components r-components) "cannot multiply vectors of different lengths")
          l)
        (mat r-cols r-rows) (do
          (assert (= r-rows l-components) "matrix multiplication mismatch")
          (type/vec (primitive-type/float) r-cols))
        (error "BUG"))
    (mat l-cols l-rows)
      (type/match r
        (primitive _) l
        (vec _ r-components) (do
          (assert (= r-components l-cols) "matrix multiplication mismatch")
          (type/vec (primitive-type/float) l-rows))
        (mat r-cols r-rows) (do
          (assert (= l-cols r-rows) "attempt to multiply incompatible matrices")
          (type/mat r-cols l-rows))
        (error "BUG"))
    (error "BUG")))

(test (type/to-glsl (resolve-one-multiply (type/mat 2 3) (type/mat 3 2))) :mat3)

(defn- resolve-float-multiply [arg-types]
  (builtin "*" (reduce2 resolve-one-multiply arg-types) arg-types))

(defn- resolve-multiplication [name arg-types]
  (check-arity name -2 arg-types)
  (def base-type (get-unique type/base-type arg-types (on-same-type-error name arg-types)))
  (primitive-type/match base-type
    (float) (resolve-float-multiply arg-types)
    (resolve-generic -2 name arg-types)))

(defmacro- defbuiltin [sym f &opt name]
  (default name (string sym))
  ~(def ,sym (,multifunction/new ,name (,partial ,f ,name))))

(defmacro- defbinop [sym arity]
  ~(defbuiltin ,sym ,(partial resolve-generic arity)))

(defbinop + -2)
(defbinop - -1)
(defbuiltin * resolve-multiplication)
(defbinop / -1)
(defbinop % 2)

(defbuiltin < (resolve-comparison-op "lessThan"))
(defbuiltin > (resolve-comparison-op "greaterThan"))
(defbuiltin <= (resolve-comparison-op "lessThanEqual"))
(defbuiltin >= (resolve-comparison-op "greaterThanEqual"))
(defbuiltin equal (resolve-comparison-op))
(defbuiltin not-equal (resolve-comparison-op) "notEqual")
(defbuiltin = resolve-simple-comparison-op)
(defbuiltin not= resolve-simple-comparison-op)
(defbuiltin vec (partial resolve-vec-constructor nil))
(defbuiltin vec2 (partial resolve-vec-constructor 2))
(defbuiltin vec3 (partial resolve-vec-constructor 3))
(defbuiltin vec4 (partial resolve-vec-constructor 4))

# this could be a lot more restrictive...
(defbinop and -2)
(defbinop or -2)
(defbinop xor -2)
(defbuiltin not resolve-not)

(defbuiltin mat2 (partial resolve-matrix-constructor 2 2))
(defbuiltin mat2x2 (partial resolve-matrix-constructor 2 2))
(defbuiltin mat2x3 (partial resolve-matrix-constructor 2 3))
(defbuiltin mat2x4 (partial resolve-matrix-constructor 2 4))
(defbuiltin mat3 (partial resolve-matrix-constructor 3 3))
(defbuiltin mat3x2 (partial resolve-matrix-constructor 3 2))
(defbuiltin mat3x3 (partial resolve-matrix-constructor 3 3))
(defbuiltin mat3x5 (partial resolve-matrix-constructor 3 4))
(defbuiltin mat4 (partial resolve-matrix-constructor 4 4))
(defbuiltin mat4x2 (partial resolve-matrix-constructor 4 2))
(defbuiltin mat4x3 (partial resolve-matrix-constructor 4 3))
(defbuiltin mat4x4 (partial resolve-matrix-constructor 4 4))

(defbuiltin length (partial numeric-vector-to-number 1))
(defbuiltin distance (partial numeric-vector-to-number 2))
(defbuiltin normalize (partial resolve-generic 1))
(defbuiltin dot (partial numeric-vector-to-number 2))
(defbuiltin cross (just {[type/vec3 type/vec3] type/vec3}))
(defbuiltin faceforward (partial resolve-generic 3))
(defbuiltin reflect (partial resolve-generic 2))
(defbuiltin refract resolve-refract)

(defbuiltin sin (partial resolve-generic 1))
(defbuiltin cos (partial resolve-generic 1))
(defbuiltin tan (partial resolve-generic 1))
(defbuiltin asin (partial resolve-generic 1))
(defbuiltin acos (partial resolve-generic 1))
# TODO: 2-parameter atan overload
(defbuiltin atan (partial resolve-generic 1))
(defbuiltin sinh (partial resolve-generic 1))
(defbuiltin cosh (partial resolve-generic 1))
(defbuiltin tanh (partial resolve-generic 1))
(defbuiltin asinh (partial resolve-generic 1))
(defbuiltin acosh (partial resolve-generic 1))
(defbuiltin atanh (partial resolve-generic 1))

# TODO: actually the second argument must be a float,
# should overload this
(defbuiltin pow (partial resolve-generic 2))
(defbuiltin exp (partial resolve-generic 1))
(defbuiltin log (partial resolve-generic 1))
(defbuiltin exp2 (partial resolve-generic 1))
(defbuiltin log2 (partial resolve-generic 1))
(defbuiltin sqrt (partial resolve-generic 1))
(defbuiltin inversesqrt (partial resolve-generic 1))

(defbuiltin abs (partial resolve-generic 1))
(defbuiltin sign (partial resolve-generic 1))
(defbuiltin floor (partial resolve-generic 1))
(defbuiltin ceil (partial resolve-generic 1))
(defbuiltin trunc (partial resolve-generic 1))
(defbuiltin round (partial resolve-generic 1))
(defbuiltin roundEven (partial resolve-generic 1))
(defbuiltin fract (partial resolve-generic 1))
(defbuiltin mod (partial resolve-generic 2))
# TODO: modf? maybe?
(defbuiltin min (partial resolve-generic 2))
(defbuiltin max (partial resolve-generic 2))
(defbuiltin clamp (partial resolve-generic 3))
# TODO: mix also supports an interesitng boolean selection overload
(defbuiltin mix (partial resolve-generic 3))
(defbuiltin step (partial resolve-generic 2))
(defbuiltin smoothstep (partial resolve-generic 3))
(defbuiltin fma (partial resolve-generic 3))

(typecheck (* 1 2) [:float :float -> :float])
(typecheck (* (vec2 1) (vec2 1)) [:vec2 :vec2 -> :vec2])
(test-error (* (vec2 1) (vec3 1)) "cannot multiply vectors of different lengths")
(typecheck (* (mat2 1) 2) [:mat2 :float -> :mat2])
(typecheck (* (vec2 1) 2) [:vec2 :float -> :vec2])
(typecheck (* 2 (mat2 1)) [:float :mat2 -> :mat2])
(typecheck (* 2 (vec2 1)) [:float :vec2 -> :vec2])
(typecheck (* (mat2 1) (vec2 1)) [:mat2 :vec2 -> :vec2])
(typecheck (* (vec2 1) (mat2 1)) [:vec2 :mat2 -> :vec2])
(typecheck (* (vec3 1) (mat2x3 1)) [:vec3 :mat2x3 -> :vec2])
(test-error (* (mat2x3 1) (vec3 1)) "matrix multiplication mismatch")
(test-error (* (mat2x3 1) (mat2x3 1)) "attempt to multiply incompatible matrices")
(test-error (* (vec3 1) (mat2 1)) "matrix multiplication mismatch")
(test-error (* (mat2 1) (vec3 1)) "matrix multiplication mismatch")
(typecheck (* (mat2x4 1) (mat3x2 1)) [:mat2x4 :mat3x2 -> :mat3x4])
(typecheck (* (mat2x4 1) (mat3x2 1) (vec3 1)) [:mat2x4 :mat3x2 :vec3 -> :vec4])
(typecheck (* (mat2x4 1) (mat3x2 1) (mat2x3 1)) [:mat2x4 :mat3x2 :mat2x3 -> :mat2x4])

(test-error (and true) "and needs at least 2 arguments but you gave it 1")
(typecheck (and true false) [:bool :bool -> :bool])
# TODO: glsl actually doesn't support this overload
(typecheck (and true false true) [:bool :bool :bool -> :bool])
(typecheck (not true) [:bool -> :bool])
(typecheck (not [true false]) [:bvec2 -> :bvec2])

(test-error (sin [1]) "vector constructor needs at least two components")
(typecheck (+ 10 20 [1 2]) [:float :float :vec2 -> :vec2])
(typecheck (< [1 2] [3 4]) [:vec2 :vec2 -> :bvec2])
(typecheck (< :1 :2) [:int :int -> :bool])
(typecheck (< 1 2) [:float :float -> :bool])
(typecheck (sin 10) [:float -> :float])
(typecheck (sin [1 2 3]) [:vec3 -> :vec3])
# TODO: maybe worth changing...
(typecheck (sin [true false]) [:bvec2 -> :bvec2])

(typecheck (vec3 1) [:float -> :vec3])
(test-error (vec3 1 2) "vec3 expects 3 components, got 2")
(typecheck (vec3 1 2 3) [:float :float :float -> :vec3])
(typecheck (mat2 1 2 3 4) [:float :float :float :float -> :mat2])
(typecheck (mat2 1) [:float -> :mat2])
(test-error (mat2 1 2) "mat2 constructor needs 4 components, but got 2")
(typecheck (mat2 [1 2] [3 4]) [:vec2 :vec2 -> :mat2])
(typecheck (mat2x3 [1 2] [3 4] [5 6]) [:vec2 :vec2 :vec2 -> :mat2x3])
(typecheck (mat3x2 [1 2 3] [4 5 6]) [:vec3 :vec3 -> :mat3x2])

(typecheck (length [1 2 3]) [:vec3 -> :float])
(typecheck (distance [1 2 3] [1 2 3]) [:vec3 :vec3 -> :float])
(test-error (distance [1 2 3] [1 2 3] [1 2 3]) "distance needs 2 arguments but you gave it 3")
(test-error (distance [1 2] [1 2 3]) "distance: cannot mix argument lengths")

(typecheck (cross [1 2 3] [1 2 3]) [:vec3 :vec3 -> :vec3])
(test-error (cross [1 2] [1 2]) "cross: no overload for arguments [:vec2 :vec2]")
(typecheck (refract [1 2] [1 2] 3) [:vec2 :vec2 :float -> :vec2])
(test-error (refract [1 2] [1 2] [1 2]) "refract: type error, expected :float got :vec2")
(test-error (refract [1 2] [1 2]) "refract needs 3 arguments but you gave it 2")
