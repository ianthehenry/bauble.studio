(use judge)
(use ./util)
(use ./types)

(def builtins @{})

(defmacro- defbuiltin [sym return-type & param-sigs]
  (with-syms [$param-sigs]
    ~(upscope
      (def ,$param-sigs (,tuple ,;(map param-sig/of-ast param-sigs)))
      (,put ',builtins ',sym
        (,multifunction/new ,(string sym)
          (,struct (,tmap ,param-sig/type ,$param-sigs)
            (,function/builtin ,(string sym)
              ,(type/of-ast return-type)
              ,$param-sigs)))))))

(defn- check-arity [name arity arg-types]
  (def actual (length arg-types))
  (if (< arity 0)
    (let [min-arity (math/abs arity)]
      (when (< actual min-arity)
        (errorf "%s needs at least %d arguments but you gave it %d" name min-arity actual)))
    (unless (= arity actual)
      (errorf "%s needs %d arguments but you gave it %d" name arity actual))))

(defn- builtin [name type arg-types]
  (function/builtin name type (map param-sig/in arg-types)))

(defn- resolve-very-generic [f arity name arg-types]
  (check-arity name arity arg-types)
  (def base-type (get-unique type/base-type arg-types))
  (def components (get-unique type/components arg-types))
  (def what-do (f base-type components))
  (def [name type] (if (type? what-do) [name what-do] what-do))
  (builtin name type arg-types))

(defn- vec-or-prim [base-type components]
  (if (= components 1)
    (type/primitive base-type)
    (type/vec base-type components)))

(def- resolve-generic (partial resolve-very-generic vec-or-prim))

(defn- check-numeric-prim [prim]
  (primitive-type/match prim
    (bool) (error "expected numeric type")
    (float) nil
    (double) nil
    (int) nil
    (uint) nil))

(defn- check-floaty-prim [prim]
  (primitive-type/match prim
    (bool) (error "expected floating-point type")
    (float) nil
    (double) nil
    (int) (error "expected floating-point type")
    (uint) (error "expected floating-point type")))

(defn- resolve-comparison-op [&opt alt] (partial resolve-very-generic
  (fn [base-type components]
    (when (nil? alt)
      (assert (> components 1) "you must use the operator form for scalars"))
    (check-numeric-prim base-type)
    (if (= components 1)
      (type/primitive (primitive-type/bool))
      (let [type (type/vec (primitive-type/bool) components)]
        (if alt [alt type] type))))
  2))

(def- resolve-simple-comparison-op (partial resolve-very-generic
  (const type/bool)
  2))

(defmacro- register [sym f &opt name]
  (default name (string sym))
  ~(,put ',builtins ',sym
    (,multifunction/new ,name
      (,partial ,f ,name))))

(defmacro- defbinop [sym arity]
  ~(register ,sym ,(partial resolve-generic arity)))

(defbinop + -2)
(defbinop - -1)
(defbinop * -2)
(defbinop / -1)

(register < (resolve-comparison-op "lessThan"))
(register > (resolve-comparison-op "greaterThan"))
(register <= (resolve-comparison-op "lessThanEqual"))
(register >= (resolve-comparison-op "greaterThanEqual"))
(register equal (resolve-comparison-op))
(register not-equal (resolve-comparison-op) "notEqual")
(register = resolve-simple-comparison-op)
(register not= resolve-simple-comparison-op)

(register length (fn [name arg-types]
  (check-arity name 1 arg-types)
  (type/match (in arg-types 0)
    (vec prim _) (do (check-floaty-prim prim)
      (builtin name type/float arg-types))
    (errorf "%s: numeric vector required" name))))
