(use judge)
(import pat)
(use ./util)
(use ./types)
(use ./builtins-prelude)

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
(defbuiltin vec resolve-vec-constructor)

(defbuiltin length (fn [name arg-types]
  (check-arity name 1 arg-types)
  (type/match (in arg-types 0)
    (vec prim _) (do (check-floaty-prim prim)
      (builtin name type/float arg-types))
    (errorf "%s: numeric vector required" name))))
