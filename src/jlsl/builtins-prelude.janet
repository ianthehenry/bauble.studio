(use judge)
(import pat)
(use ./util)
(use ./type)

# these are the helpers that we use to define builtins.
# they exist here so that we can have the vec builtin
# separate from the builtins/ module.

(defn on-same-type-error [name arg-types]
  |(errorf "%s: arguments must have the same base type, got %q"
    name (tmap type/to-glsl arg-types)))

(defn check-arity [name arity arg-types]
  (def actual (length arg-types))
  (if (< arity 0)
    (let [min-arity (math/abs arity)]
      (when (< actual min-arity)
        (errorf "%s needs at least %d arguments but you gave it %d" name min-arity actual)))
    (assertf (= arity actual)
      "%s needs %d arguments but you gave it %d" name arity actual)))

(defn builtin [name type arg-types]
  (function/builtin name type (map |(if (param-sig? $) $ (param-sig/in $)) arg-types)))

(defn get-more-or-less-unique [f ind on-error]
  (pat/match (sort (distinct (map f ind)))
    [unique] unique
    [1 unique] unique
    (on-error)))

(defn resolve-very-generic [f arity name arg-types]
  (check-arity name arity arg-types)
  (def base-type (get-unique type/base-type arg-types (on-same-type-error name arg-types)))
  (def components (get-more-or-less-unique type/components arg-types
    |(errorf "%s: cannot mix argument lengths, got %q"
      name (tmap type/to-glsl arg-types))))
  (def what-do (f base-type components))
  (def [name type] (if (type? what-do) [name what-do] what-do))
  (builtin name type arg-types))

(defn vec-or-prim [base-type components]
  (if (= components 1)
    (type/primitive base-type)
    (type/vec base-type components)))

(def resolve-generic (partial resolve-very-generic vec-or-prim))

(defn check-numeric-prim [prim]
  (primitive-type/match prim
    (bool) (error "expected numeric type")
    (float) nil
    (double) nil
    (int) nil
    (uint) nil))

(defn check-floaty-prim [prim]
  (primitive-type/match prim
    (float) nil
    (double) nil
    (error "expected floating-point type")))

(defn check-bool-prim [prim]
  (primitive-type/match prim
    (bool) nil
    (error "expected bool type")))

(defn resolve-comparison-op [&opt alt] (partial resolve-very-generic
  (fn [base-type components]
    (when (nil? alt)
      (assert (> components 1) "you must use the operator form for scalars"))
    (check-numeric-prim base-type)
    (if (= components 1)
      (type/primitive (primitive-type/bool))
      (let [type (type/vec (primitive-type/bool) components)]
        (if alt [alt type] type))))
  2))

(def resolve-simple-comparison-op (partial resolve-very-generic
  (const type/bool)
  2))

(defn resolve-vec-constructor [expected-components name arg-types]
  (check-arity name -1 arg-types)
  (def base-type (get-unique type/base-type arg-types (on-same-type-error name arg-types)))
  (def components (sum (map type/components arg-types)))
  (if (nil? expected-components) (do
    (assert (>= components 2) "vector constructor needs at least two components")
    (assert (<= components 4) "vector constructor cannot have more than four components"))
    (assertf (or (= components 1) (= components expected-components))
      "%s expects %d components, got %d" name expected-components components))
  (def constructor (primitive-type/vec-prefix base-type))
  (def components (or expected-components components))
  (builtin
    (string constructor components)
    (type/vec base-type components)
    arg-types))

(defn resolve-matrix-constructor [cols rows name arg-types]
  (check-arity name -1 arg-types)
  (def base-type (get-unique type/base-type arg-types (on-same-type-error name arg-types)))
  (assert (= base-type (primitive-type/float)) "matrices must contain floats")
  (def components (sum (map type/components arg-types)))
  (def expected-components (* rows cols))
  (assertf (or (= components 1) (= components expected-components))
    "%s constructor needs %d components, but got %d" name expected-components components)
  (builtin
    name
    (type/mat cols rows)
    arg-types))
