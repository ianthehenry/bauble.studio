(use judge)
(use module)
(use ./adt)
(use ./util)
(import pat)

(defadt variable
  (dynamic id name type)
  (lexical id name type))

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

(defadt free-vars
  (unscanned)
  (unresolved free-variables function-references)
  (resolved free-variables))

(defadt function
  (builtin name return-type param-sigs)
  (custom impl))

(defadt expr
  (literal type value)
  (identifier variable)
  (call function args)
  (crement op value)
  (dot expr field)
  (in expr index))

(defadt statement
  (declaration const? variable expr)
  (assign l-value r-value)
  (update op l-value r-value)
  (break)
  (continue)
  (discard)
  (return expr)
  (do body)
  (with bindings body)
  (if cond then else)
  (case value cases)
  (while cond body)
  (do-while cond body)
  (for init cond update body)
  (expr expr))

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


(defmodule variable
  (defn new [name type] (variable/lexical (gensym) name type))
  (defn dyn [name type] (variable/dynamic (gensym) name type))

  (defn name [t]
    (variable/match t
      (dynamic _ name _) name
      (lexical _ name _) name))

  (defn type [t]
    (variable/match t
      (dynamic _ _ type) type
      (lexical _ _ type) type)))

(defmodule param-sig
  (defn new [type access] [type access])
  (defn type [t] (in t 0))
  (defn access [t] (in t 1))
  (defn in [type] (new type :in))

  (defn to-glsl [t]
    (def type (type/to-glsl (type t)))
    (match (access t)
      :in type
      :out (tuple/brackets 'out type)
      :inout (tuple/brackets 'inout type)
      (error "BUG: unknown access type")))

  (defn of-ast [ast]
    (if (btuple? ast)
      (match ast
        ['in type] [new (type/of-ast type) :in]
        ['out type] [new (type/of-ast type) :out]
        ['inout type] [new (type/of-ast type) :inout]
        (errorf "unknown parameter signature %q" ast))
      [new (type/of-ast ast) :in]))
  )

(defmodule param
  (defn new [lexical-variable sig]
    (assert (= (param-sig/type sig) (variable/type lexical-variable)) "BUG: parameter signature type mismatch")
    [lexical-variable sig])
  (defn var [t] (in t 0))
  (defn sig [t] (in t 1))
  (defn type [t] (param-sig/type (sig t)))
  (defn access [t] (param-sig/access (sig t))))

(var function/param-sigs nil)

# an impl is an implementation of a custom function
(defmodule impl
  (defn new [name return-type param-sigs]
    {:name name
     :declared-return-type return-type
     :declared-param-sigs param-sigs
     :params (ref/new)
     :body (ref/new)
     :scan-ref (ref/new (free-vars/unscanned))
     :free-var-access-ref (ref/new)
     :implicit-params-ref (ref/new)})

  (defn return-type [{:declared-return-type declared-return-type}] declared-return-type)

  (defn- /name [x] (in x :name))
  (def name /name)
  (defn param-sigs [{:declared-param-sigs param-sigs}] param-sigs)

  (defn implement [t return-type params body]
    (def {:name name
          :declared-return-type declared-return-type
          :declared-param-sigs declared-param-sigs
          :params current-params
          :body current-body} t)

    (assertf (empty? current-body) "%s: cannot implement a function multiple times" name)
    (assertf (not (empty? body)) "%s: cannot implement with empty body" name)
    (def implemented-param-sigs (map param/sig params))
    (assertf (contents= declared-param-sigs implemented-param-sigs)
      "%s: parameter mismatch, declared as %q implemented as %q"
      name
      declared-param-sigs
      implemented-param-sigs)
    (assertf (= declared-return-type return-type)
      "%s: return type mismatch, declared as %q implemented as %q"
      name
      declared-return-type
      return-type)
    (array/concat current-body body)
    (array/concat current-params params)
    t)

  (defn root-identifier [expr]
    (expr/match expr
      (literal _ _) nil
      (identifier variable) variable
      (call function args) nil
      (dot expr _) (root-identifier expr)
      (in expr index) (root-identifier expr)
      (crement _ expr) nil))

  # returns free variables and all referenced functions
  (defn- scan [name body params]
    (assertf (not (empty? body)) "%s: cannot find free variables of a function that has not been implemented yet" name)

    (var scope (tabseq [param :in params] (param/var param) true))
    (var functions-called @{})

    # a map from variables to an array of @[read? written?]
    (def free-vars @{})
    (defn free-entry [k] (get-or-put free-vars k @[false false]))
    (defn mark [variable rw]
      (case rw
        :read (put (free-entry variable) 0 true)
        :write (put (free-entry variable) 1 true)
        (error "BUG")))

    (defn see-expr [expr rw]
      (expr/match expr
        (literal _ _) nil
        (identifier variable)
          (unless (in scope variable)
            (mark variable rw))
        (call function args) (do
          (put functions-called function (table/proto-flatten scope))
          (def args-and-params (try
            (zip args (function/param-sigs function))
            ([_ _]
              (errorf "wrong number of arguments to function %s, expected %q, got %q"
                (/name function)
                (length (function/param-sigs function))
                (length args)
                ))))
          (each [arg param-sig] args-and-params
            (match (param-sig/access param-sig)
              :in (see-expr arg :read)
              :out (see-expr arg :write)
              :inout (do (see-expr arg :read) (see-expr arg :write))
              access (errorf "BUG: unknown access qualifier %q" access))))
        (dot expr _) (do
          # okay so technically if the field is the entire thing,
          # e.g. if you have `foo.xyz = vec3(1, 2, 3)`, then the
          # read here is unnecessary. But just... just set `foo`
          # instead.
          (see-expr expr :read)
          (see-expr expr rw))
        (in expr index) (do (see-expr expr :read) (see-expr expr rw) (see-expr index :read))
        (crement _ expr) (do (see-expr expr :read) (see-expr expr :write))
        ))

    (var visit nil)
    (defn in-block [f]
      (set scope (table/setproto @{} scope))
      (f)
      (set scope (table/getproto scope)))

    (defn block [statements]
      (in-block (fn []
        (each statement statements
          (visit statement)))))

    (set visit (fn visit [statement]
      (statement/match statement
        (declaration const? variable expr) (do
          (see-expr expr :read)
          (put scope variable true))
        (assign l-value r-value) (do
          (see-expr l-value :write)
          (see-expr r-value :read)
          # it's not actually in scope, but it's no longer
          # exactly free. if we read the value after this,
          # we don't need to mark it as read-free, and we've
          # already marked it write-free
          (put scope (root-identifier l-value) true))
        (update op l-value r-value) (do
          (see-expr l-value :read)
          (see-expr l-value :write)
          (see-expr r-value :read))
        (break) nil
        (continue) nil
        (discard) nil
        (return expr) (see-expr expr :read)
        (do body) (block body)
        (with bindings body)
          (block [
            ;(seq [[variable expr] :in bindings]
              (statement/declaration false variable expr))
            ;body])
        (if cond then else) (do
          (see-expr cond :read)
          (visit then)
          (visit else))
        (case value cases) (do
          (see-expr value :read)
          (each case cases
            (pat/match case
              [body] (visit body)
              [expr body] (do (see-expr expr :read) (visit body)))))
        (while cond body) (do
          (see-expr cond :read)
          (block body))
        (do-while cond body) (do
          (see-expr cond :read)
          (block body))
        (for init cond update body) (in-block (fn []
          (visit init)
          (see-expr cond :read)
          (visit update)
          (each statement body
            (visit statement))))
        (expr expr) (see-expr expr nil))))
    (block body)

    [free-vars functions-called])

  (defn- memoized-scan [{:name name :params params :body body :scan-ref scan-ref}]
    (free-vars/match (ref/get scan-ref)
      (unscanned) (do
        (def [free-vars functions] (scan name body params))
        (ref/set scan-ref (free-vars/unresolved free-vars functions))
        [free-vars functions])
      (unresolved free-vars functions) [free-vars functions]
      (resolved free-vars) [free-vars {}]))

  (var- free-var-accesses nil)

  (defn- union-variable [into variable [read? written?]]
    (if (has-key? into variable)
      (let [[old-read? old-written?] (in into variable)]
        (put into variable [(or read? old-read?) (or written? old-written?)]))
      (put into variable [read? written?])))

  (defn- compute-free-var-accesses [t]
    (def result @{})
    (def [free-vars functions] (memoized-scan t))
    (eachp [variable access-types] free-vars
      (union-variable result variable access-types))
    (eachp [function bound-vars] functions
      (function/match function
        (builtin _ _ _) nil
        (custom impl) (do
          (def free-var-set (free-var-accesses impl))
          (loop [[free-var access-types] :pairs free-var-set
                 :unless (in bound-vars free-var)]
            (union-variable result free-var access-types)))))
    result)

  (set free-var-accesses (fn impl/free-var-accesses [t]
    (def {:free-var-access-ref free-var-access-ref} t)
    (when (= (ref/get free-var-access-ref) :computing)
      (break @{}))
    (ref/set free-var-access-ref :computing)
    (def result (compute-free-var-accesses t))
    (ref/set free-var-access-ref result)
    result))

  (defn implicit-params [t]
    (def {:implicit-params-ref implicit-params-ref} t)
    (ref/get-or-put implicit-params-ref
      (sort (seq [[variable [read? write?]] :pairs (free-var-accesses t)]
        (param/new variable (param-sig/new (variable/type variable) (cond
          (and read? write?) :inout
          read? :in
          write? :out
          (error "BUG: free variable not actually used"))))))))
  )

(def- multifunction-proto @{:type 'function})
(defn multifunction? [t] (and (table? t) (= (table/getproto t) multifunction-proto)))
(defmodule multifunction
  (defn new [name overloads]
    (table/setproto @{:name name :overloads overloads} multifunction-proto))

  (defn single [name return-type param-sigs]
    (new name
      {(tmap param-sig/type param-sigs)
        (function/custom
          (impl/new name return-type param-sigs))}))

  # multifunction -> function
  (defn resolve [{:name name :overloads overloads} arg-types]
    (or (overloads arg-types)
      (errorf "%s: no overload for arguments %q" name (tuple/brackets ;(map type/to-glsl arg-types)))))

  (defn param-sigs [t arg-types]
    (impl/param-sigs (resolve t arg-types)))

  (defn name [{:name name}] name))

(defmodule function
  (defn return-type [t]
    (function/match t
      (builtin _ return-type _) return-type
      (custom impl) (impl/return-type impl)))

  (defn name [t]
    (function/match t
      (builtin name _ _) name
      (custom impl) (impl/name impl)))

  (defn implicit-params [t]
    (function/match t
      (builtin _ _ _) []
      (custom impl) (impl/implicit-params impl))))

(set function/param-sigs (fn function/param-sigs [t]
  (function/match t
    (builtin _ _ param-sigs) param-sigs
    (custom impl) (impl/param-sigs impl))))
