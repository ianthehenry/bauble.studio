(use judge)
(use module)
(use ./adt)
(use ./util)

(defadt primitive-type
  (float)
  (double)
  (int)
  (uint)
  (bool))

(defadt type
  (void)
  (primitive type)
  (vec type count)
  (mat cols rows) # this is specifically a float matrix; we don't support dmat yet
  (array type length)
  (struct name fields))

(defmodule primitive-type
  (def short-names
    {:float (primitive-type/float)
     :double (primitive-type/double)
     :int (primitive-type/int)
     :uint (primitive-type/uint)
     :bool (primitive-type/bool)})

  (defn of-ast [ast]
    (if-let [t (in short-names ast)]
      ~',t
      (errorf "%q is not a primitive type" ast)))

  (defn to-glsl [t]
    (primitive-type/match t
      (float) :float
      (double) :double
      (int) :int
      (uint) :uint
      (bool) :bool))

  (defn vec-prefix [t]
    (primitive-type/match t
      (float) "vec"
      (double) "dvec"
      (int) "ivec"
      (uint) "uvec"
      (bool) "bvec")))

(defmodule type
  (def float (type/primitive (primitive-type/float)))
  (def int (type/primitive (primitive-type/int)))
  (def uint (type/primitive (primitive-type/uint)))
  (def double (type/primitive (primitive-type/double)))
  (def bool (type/primitive (primitive-type/bool)))
  (def vec2 (type/vec (primitive-type/float) 2))
  (def vec3 (type/vec (primitive-type/float) 3))
  (def vec4 (type/vec (primitive-type/float) 4))

  (def short-names
    {:void (type/void)
     :vec2 (type/vec (primitive-type/float) 2)
     :vec3 (type/vec (primitive-type/float) 3)
     :vec4 (type/vec (primitive-type/float) 4)
     :dvec2 (type/vec (primitive-type/double) 2)
     :dvec3 (type/vec (primitive-type/double) 3)
     :dvec4 (type/vec (primitive-type/double) 4)
     :ivec2 (type/vec (primitive-type/int) 2)
     :ivec3 (type/vec (primitive-type/int) 3)
     :ivec4 (type/vec (primitive-type/int) 4)
     :uvec2 (type/vec (primitive-type/uint) 2)
     :uvec3 (type/vec (primitive-type/uint) 3)
     :uvec4 (type/vec (primitive-type/uint) 4)
     :bvec2 (type/vec (primitive-type/bool) 2)
     :bvec3 (type/vec (primitive-type/bool) 3)
     :bvec4 (type/vec (primitive-type/bool) 4)
     :mat2 (type/mat 2 2)
     :mat3 (type/mat 3 3)
     :mat4 (type/mat 4 4)
     :mat2x2 (type/mat 2 2)
     :mat2x3 (type/mat 2 3)
     :mat2x4 (type/mat 2 4)
     :mat3x2 (type/mat 3 2)
     :mat3x3 (type/mat 3 3)
     :mat3x4 (type/mat 3 4)
     :mat4x2 (type/mat 4 2)
     :mat4x3 (type/mat 4 3)
     :mat4x4 (type/mat 4 4)})

  (defn of-ast [ast]
    (if-let [t (in short-names ast)]
      ~',t
      (if (and (ptuple? ast) (= (length ast) 2))
        [type/array (of-ast (in ast 0)) (in ast 1)]
        (if-let [prim (primitive-type/of-ast ast)]
          [type/primitive prim]
          (if (keyword? ast)
            (errorf "unknown type %q" ast)
            ast)))))

  (test (of-ast :float)
    [@type/primitive [quote [<1> float]]])
  (test (eval (of-ast :float))
    [<1> primitive [<2> float]])
  (test (eval (of-ast '(:float 3)))
    [<1>
     array
     [<1> primitive [<2> float]]
     3])

  (defn to-glsl [t]
    (type/match t
      (void) :void
      (primitive t) (primitive-type/to-glsl t)
      (struct name _) (symbol name)
      (mat col row) (if (= col row) (keyword "mat" col) (keyword "mat" col "x" row))
      (array type length) [(to-glsl type) length]
      (vec type count) (keyword (primitive-type/vec-prefix type) count)))

  (defn components [t]
    (type/match t
      (void) (error "vector cannot contain void")
      (primitive _) 1
      (vec _ count) count
      (mat cols rows) (* cols rows)
      (array _ _) (error "you can't construct vectors from arrays")
      (struct _ _) (error "vectors cannot contain compound entries")))

  (defn base-type [t]
    (type/match t
      (void) nil
      (primitive t) t
      (vec t _) t
      (mat _ _) (primitive-type/float)
      (array _ _) nil
      (struct _ _) nil))

  (defn- is-vector-field? [field]
    (or (string/check-set "xyzw" field)
        (string/check-set "rgba" field)
        (string/check-set "stpq" field)))

  (defn field-type [t field]
    (type/match t
      (void) (errorf "cannot access field %q of void" field)
      (primitive t) (errorf "cannot access field %q of primitive type" field)
      (mat _ _) (errorf "cannot access field %q of a matrix" field)
      (array _ _) (errorf "cannot access field %q of an array" field)
      (vec t count)
        (if (is-vector-field? field)
          (let [len (length field)]
            (cond
              (= len 1) (type/primitive t)
              (and (>= len 2) (<= len 4)) (type/vec t len)
              (errorf "cannot create a vector with %d components" len)))
          (errorf "unknown vector field %q" field))
      (struct name fields)
        (or (in fields field)
          (errorf "%s: unknown field %q" name field))))

  (defn element-type [t]
    (type/match t
      (void) (error "cannot index into void")
      (primitive t) (error "cannot index into primitive type")
      (mat _ rows) (type/vec (primitive-type/float) rows)
      (array type _) type
      (vec t _) (type/primitive t)
      (struct name fields) (error "cannot index into struct")))
  )
