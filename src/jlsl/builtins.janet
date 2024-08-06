(use judge)
(import pat)
(use ./util)
(use ./types)
(use ./builtins-prelude)
(use ./builtin-test-helpers)

(defmacro- defbuiltin [sym f &opt name]
  (default name (string sym))
  ~(def ,sym (,multifunction/new ,name (,partial ,f ,name))))

(defmacro- defbinop [sym arity]
  ~(defbuiltin ,sym ,(partial resolve-generic arity)))

(defbinop + -2)
(defbinop - -1)
(defbinop * -2)
(defbinop / -1)

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

(defbuiltin length (fn [name arg-types]
  (check-arity name 1 arg-types)
  (type/match (in arg-types 0)
    (vec prim _) (do (check-floaty-prim prim)
      (builtin name type/float arg-types))
    (errorf "%s: numeric vector required" name))))

(defbuiltin sin (partial resolve-generic 1))
(defbuiltin cos (partial resolve-generic 1))

(test-error (sin [1]) "vector constructor needs at least two components")
(typecheck (+ 10 20 [1 2]) [:float :float :vec2 -> :vec2])
(typecheck (< [1 2] [3 4]) [:vec2 :vec2 -> :bvec2])
(typecheck (< 1 2) [:float :float -> :bool])
(typecheck (sin 10) [:float -> :float])
(typecheck (sin [1 2 3]) [:vec3 -> :vec3])
# TODO: maybe worth changing...
(typecheck (sin [true false]) [:bvec2 -> :bvec2])

(typecheck (vec3 1 2 3) [:float :float :float -> :vec3])
(typecheck (mat2 1 2 3 4) [:float :float :float :float -> :mat2])
(typecheck (mat2 [1 2] [3 4]) [:vec2 :vec2 -> :mat2])
(typecheck (mat2x3 [1 2] [3 4] [5 6]) [:vec2 :vec2 :vec2 -> :mat2x3])
(typecheck (mat3x2 [1 2 3] [4 5 6]) [:vec3 :vec3 -> :mat3x2])
