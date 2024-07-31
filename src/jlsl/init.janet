(use judge)
(use module)
(import pat)
(use ./util)
(use ./adt)
(import ../glsl)

(defadt variable
  (dynamic id name type)
  (lexical id name type))

(defadt primitive-type
  (float)
  (double)
  (int)
  (uint)
  (bool))

(defmodule primitive-type
  (defn of-ast [ast]
    (case ast
      :float ~',(primitive-type/float)
      :double ~',(primitive-type/double)
      :int ~',(primitive-type/int)
      :uint ~',(primitive-type/uint)
      :bool ~',(primitive-type/bool)))

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

# TODO: matrices, arrays
(defadt type
  (void)
  (primitive type)
  (vec type count)
  (struct name fields))

(defadt free-vars
  (unscanned)
  (unresolved free-variables function-references)
  (resolved free-variables))

(defmodule type
  (def float (type/primitive (primitive-type/float)))
  (def int (type/primitive (primitive-type/int)))
  (def uint (type/primitive (primitive-type/uint)))
  (def double (type/primitive (primitive-type/double)))
  (def bool (type/primitive (primitive-type/bool)))
  (def vec2 (type/vec (primitive-type/float) 2))
  (def vec3 (type/vec (primitive-type/float) 3))
  (def vec4 (type/vec (primitive-type/float) 4))

  (defn of-ast [ast]
    (case ast
      :void ~',(type/void)
      :vec2 ~',(type/vec (primitive-type/float) 2)
      :vec3 ~',(type/vec (primitive-type/float) 3)
      :vec4 ~',(type/vec (primitive-type/float) 4)
      :dvec2 ~',(type/vec (primitive-type/double) 2)
      :dvec3 ~',(type/vec (primitive-type/double) 3)
      :dvec4 ~',(type/vec (primitive-type/double) 4)
      :ivec2 ~',(type/vec (primitive-type/int) 2)
      :ivec3 ~',(type/vec (primitive-type/int) 3)
      :ivec4 ~',(type/vec (primitive-type/int) 4)
      :uvec2 ~',(type/vec (primitive-type/uint) 2)
      :uvec3 ~',(type/vec (primitive-type/uint) 3)
      :uvec4 ~',(type/vec (primitive-type/uint) 4)
      :bvec2 ~',(type/vec (primitive-type/bool) 2)
      :bvec3 ~',(type/vec (primitive-type/bool) 3)
      :bvec4 ~',(type/vec (primitive-type/bool) 4)
      (if-let [prim (primitive-type/of-ast ast)]
        [type/primitive prim]
        (if (keyword? ast)
          (errorf "unknown type %q" ast)
          ast))))

  (test (of-ast :float)
    [@type/primitive [quote [<1> float]]])
  (test (eval (of-ast :float))
    [<1> primitive [<2> float]])

  (defn to-glsl [t]
    (type/match t
      (void) :void
      (primitive t) (primitive-type/to-glsl t)
      (struct name _) (symbol name)
      (vec type count) (keyword (primitive-type/vec-prefix type) count)))

  (defn components [t]
    (type/match t
      (void) (error "vector cannot contain void")
      (primitive _) 1
      (vec _ count) count
      (struct _ _) (error "vectors cannot contain compound entries")))

  (defn base-type [t]
    (type/match t
      (void) nil
      (primitive t) t
      (vec t _) t
      (struct _ _) nil))

  (defn- is-vector-field? [field]
    (or (string/check-set "xyzw" field)
        (string/check-set "rgba" field)
        (string/check-set "stpq" field)))

  (defn field-type [t field]
    (type/match t
      (void) (error "cannot access field of void")
      (primitive t) (errorf "cannot access field %q of primitive type" field)
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

# TODO: I might want to separate a function and a function
# implementation into different types.
(defadt function
  (builtin name return-type param-sigs)
  (defined
    name
    return-type
    param-sigs
    params
    body
    scan-ref
    free-var-access-ref
    implicit-params-ref))

(defadt expr
  (literal type value)
  (identifier variable)
  (call function args)
  (crement op value)
  (dot expr field))

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
  (call function arguments)
  (crement op expr))

(defmacro defbuiltin [sym return-type & param-sigs]
  ~(def ,(symbol "builtins/" sym)
    (,function/builtin ,(string sym) ,(type/of-ast return-type)
      ,(tuple/brackets ;(map param-sig/of-ast param-sigs)))))

(defbuiltin + :float :float :float)
(defbuiltin * :float :float :float)
(defbuiltin - :float :float :float)
(defbuiltin < :bool :float :float)
(defbuiltin > :bool :float :float)
(defbuiltin vec- :vec3 :vec3 :vec3)
(defbuiltin length :float :vec3)

(defmodule function
  (defn new [name return-type param-sigs]
    (function/defined
      name
      return-type
      param-sigs
      @[] # params
      @[] # body
      (ref/new (free-vars/unscanned)) # scan-ref
      (ref/new) # free-var-access-ref
      (ref/new) # implicit-params-ref
      ))

  (defn of-ast [sym]
    (case sym
      '+ ['quote builtins/+]
      '* ['quote builtins/*]
      '- ['quote builtins/-]
      '< ['quote builtins/<]
      '> ['quote builtins/>]
      'length ['quote builtins/length]
      sym))

  (defn to-glsl [t]
    (function/match t
      (builtin name _ _) (symbol name)
      (defined name _ _ _ _ _ _ _) (symbol name)))

  (defn param-sigs [t]
    (function/match t
      (builtin _ _ param-sigs) param-sigs
      (defined _ _ param-sigs _ _ _ _ _) param-sigs))

  (defn- /name [t]
    (function/match t
      (builtin name _ _) name
      (defined name _ _ _ _ _ _ _) name))
  (def name /name)

  (defn return-type [t]
    (function/match t
      (builtin _ type _) type
      (defined _ type _ _ _ _ _ _) type))

  # TODO: theoretically a single function can have multiple overloads,
  # so we should be able to implement it multiple times. but we're not there yet.
  (defn implement [t return-type params body]
    (function/match t
      (builtin _ _ _) (error "BUG: attempting to implement builtin function")
      (defined name declared-return-type declared-param-sigs current-params current-body _ _ _) (do
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
        (array/concat current-params params)))
    t)

  # returns free variables and all referenced functions
  (defn scan [name body params]
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
            (zip args (param-sigs function))
            ([_ _]
              (errorf "wrong number of arguments to function %s, expected %q, got %q"
                (/name function)
                (length (param-sigs function))
                (length args)
                ))))
          (each [arg param-sig] args-and-params
            (match (param-sig/access param-sig)
              :in (see-expr arg :read)
              :out (see-expr arg :write)
              :inout (do (see-expr arg :read) (see-expr arg :write))
              access (errorf "BUG: unknown access qualifier %q" access))))
        (dot expr _) (see-expr expr rw)
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
          (see-expr r-value :read))
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
        (call function args) (see-expr (expr/call function args) nil)
        (crement op expr) (see-expr (expr/crement op expr) nil))))
    (block body)

    [free-vars functions-called])

  (defn memoized-scan [t]
    (function/match t
      (builtin _ _ _) [[] {}]
      (defined name _ _ params body scan-ref _ _)
        (free-vars/match (ref/get scan-ref)
          (unscanned) (do
            (def [free-vars functions] (scan name body params))
            (ref/set scan-ref (free-vars/unresolved free-vars functions))
            [free-vars functions])
          (unresolved free-vars functions) [free-vars functions]
          (resolved free-vars) [free-vars {}])))

    (var free-var-accesses nil)

    (defn union-variable [into variable [read? written?]]
      (if (has-key? into variable)
        (let [[old-read? old-written?] (in into variable)]
          (put into variable [(or read? old-read?) (or written? old-written?)]))
        (put into variable [read? written?])))

    (defn compute-free-var-accesses [t]
      (def result @{})
      (def [free-vars functions] (memoized-scan t))
      (eachp [variable access-types] free-vars
        (union-variable result variable access-types))
      (eachp [function bound-vars] functions
        (def free-var-set (free-var-accesses function))
        (loop [[free-var access-types] :pairs free-var-set
               :unless (in bound-vars free-var)]
          (union-variable result free-var access-types)))
      result)

  (set free-var-accesses (fn free-var-accesses [t]
    (function/match t
      (builtin _ _ _) @{}
      (defined name _ _ _ _ _ free-var-access-ref _) (do
        (if (= (ref/get free-var-access-ref) :computing) @{}
          (do
            (ref/set free-var-access-ref :computing)
            (def result (compute-free-var-accesses t))
            (ref/set free-var-access-ref result)
            result))))))

  (defn implicit-params [t]
    (function/match t
      (builtin _ _ _) []
      (defined name _ _ _ _ _ _ implicit-params-ref)
        (ref/get-or-put implicit-params-ref
          (sort (seq [[variable [read? write?]] :pairs (free-var-accesses t)]
            (param/new variable (param-sig/new (variable/type variable) (cond
              (and read? write?) :inout
              read? :in
              write? :out
              (error "BUG: free variable not actually used")))))))))
  )

(defmodule expr
  (defn type [t]
    (expr/match t
      (literal type _) type
      (identifier variable) (variable/type variable)
      (call function _) (function/return-type function)
      (dot expr field) (type/field-type (type expr) field)
      (crement _ expr) (type expr)))

  (defn vector [& exprs]
    (assert (not (empty? exprs)) "vector cannot be empty")
    (def base-type (get-unique (>> type type/base-type) exprs))
    (def components (sum (map (>> type type/components) exprs)))
    (def constructor (primitive-type/vec-prefix base-type))
    # TODO: this should really return a function call node...
    # we can share this type resolution across other generic functions
    # i think
    (expr/call
      (function/builtin
        (symbol constructor components)
        (type/vec base-type components)
        (seq [expr :in exprs] (param-sig/new (type expr) :in)))
      exprs))

  (defn of-ast [expr-ast]
    (pat/match expr-ast
      |keyword? [expr/literal ['quote type/int] expr-ast]
      |boolean? [expr/literal ['quote type/bool] expr-ast]
      |number? [expr/literal ['quote type/float] expr-ast]
      |symbol? [expr/identifier expr-ast]
      |btuple? [vector ;(map of-ast expr-ast)]
      ['. expr key] [expr/dot (of-ast expr) ['quote key]]
      [(and op (or '++ '-- '_++ '_--)) expr] (expr/crement ['quote op] (of-ast expr))
      # TODO
      # ['if cond then else]
      # ['in expr key]
      [f & args] [expr/call (function/of-ast f) (map of-ast args)]
      )))

(defmodule statement
  (var of-ast nil)

  # takes a list of ASTs and returns code that you can evaluate
  # to return an array of statements
  (defn of-asts [asts]
    (with-syms [$statements]
      ~(let [,$statements @[]]
        ,;(seq [statement :in asts]
          [array/push $statements (of-ast statement)])
        ,$statements)))

  # takes the AST of a statement and returns code that
  # creates a first-class statement
  (varfn of-ast [ast]
    (assertf (ptuple? ast) "%q is not a statement" ast)

    (pat/match ast
      # TODO: we have no way to declare a variable without any initial value
      [(and declaration (or 'def 'var)) name value]
        (with-syms [$expr $type $statement]
          (def const? (= declaration 'def))
          ~(upscope
            (def ,$expr ,(expr/of-ast value))
            (def ,$type (,expr/type ,$expr))
            (def ,name (,variable/new ,(string name) ,$type))
            (,statement/declaration ,const? ,name ,$expr)))
      ['set dest value] [statement/assign (expr/of-ast dest) (expr/of-ast value)]
      ['return value] [statement/return (expr/of-ast value)]
      ['break] [statement/break]
      ['continue] [statement/continue]
      [(and op (or
        '+= '*= '/= '-= '%=
        'blshift= 'brshift=
        'bxor= 'band= 'bor=)) dest expr]
        [statement/update op (expr/of-ast dest) (expr/of-ast expr)]
      ['do & body] [statement/do (of-asts body)]
      ['with bindings & body] [statement/with
        (tuple/brackets ;(seq [[variable expr] :in (partition 2 bindings)]
          (tuple/brackets variable (expr/of-ast expr))))
        (of-asts body)]
      ['if cond then & else] [statement/if (expr/of-ast cond) (of-ast then) (if else (of-ast else))]
      ['case value & cases]
        [statement/case
          (expr/of-ast value)
          (tuple/brackets ;(seq [case :in (partition 2 cases)]
            (pat/match case
              [body] [(of-ast body)]
              [value body] [(expr/of-ast value) (of-ast body)])))]
      ['while cond & body] [statement/while (expr/of-ast cond) (of-asts body)]
      ['do-while cond & body] [statement/do-while (expr/of-ast cond) (of-asts body)]
      ['for init check advance & body]
        (with-syms [$init]
          ~(let [,$init ,(of-ast init)]
            ,[statement/for $init
              (expr/of-ast check)
              (of-ast advance)
              (of-asts body)]))
      [(and op (or '++ '-- '_++ '_--)) expr] [statement/crement ['quote op] (expr/of-ast expr)]
      [function & args] [statement/call (function/of-ast function) (map expr/of-ast args)]
    )))

(defmacro- jlsl/stub [return-type name param-sigs]
  ~(,function/new ,name ,(type/of-ast return-type) [,;(map param-sig/of-ast param-sigs)]))

(defmacro- jlsl/declare [return-type name param-sigs]
  ['def name (call jlsl/stub return-type (string name) param-sigs)])

(defmacro- jlsl/implement [return-type name params & body]
  (def $return-type (gensym))
  (def $params (gensym))
  (def $body (gensym))

  (def <params> (seq [[sig name] :in (partition 2 params)]
    (def <sig> (param-sig/of-ast sig))
    (with-syms [$sig]
      ~(upscope
        (def ,$sig ,<sig>)
        (def ,name (,variable/new ,(string name) (,param-sig/type ,$sig)))
        (,param/new ,name ,$sig)))))

  (def <body> (seq [statement-ast :in body]
    ~(,array/push ,$body ,(statement/of-ast statement-ast))))

  ~(do
    (def ,$return-type ,(type/of-ast return-type))
    (def ,$params [,;<params>])
    (def ,$body @[])
    ,;<body>
    (,function/implement ,name ,$return-type ,$params ,$body)))

(defmacro- jlsl/fn [return-type name params & body]
  (call jlsl/implement return-type (call jlsl/stub return-type name (map 0 (partition 2 params))) params ;body))

(defmacro- jlsl/defn [return-type name params & body]
  ['upscope
    (call jlsl/declare return-type name (map 0 (partition 2 params)))
    (call jlsl/implement return-type name params ;body)])

(test-macro (jlsl/declare :float incr [:float])
  (def incr (@new "incr" (@type/primitive (quote (<1> float))) [(@new (@type/primitive (quote (<1> float))) :in)])))

(test-macro (jlsl/implement :float incr [:float x] (return x))
  (do
    (def <1> (@type/primitive (quote (<2> float))))
    (def <3> [(upscope (def <4> (@new (@type/primitive (quote (<2> float))) :in)) (def x (@new "x" (@type <4>))) (@new x <4>))])
    (def <5> @[])
    (@array/push <5> (@statement/return (@expr/identifier x)))
    (@implement incr <1> <3> <5>)))
(test-macro (jlsl/defn :float incr [:float x] (return x))
  (upscope
    (def incr (@new "incr" (@type/primitive (quote (<1> float))) [(@new (@type/primitive (quote (<1> float))) :in)]))
    (do
      (def <2> (@type/primitive (quote (<1> float))))
      (def <3> [(upscope (def <4> (@new (@type/primitive (quote (<1> float))) :in)) (def x (@new "x" (@type <4>))) (@new x <4>))])
      (def <5> @[])
      (@array/push <5> (@statement/return (@expr/identifier x)))
      (@implement incr <2> <3> <5>))))

(test-macro (jlsl/defn :void foo [:float x :float y]
  (var x 1)
  (return [x 2 3]))
  (upscope
    (def foo (@new "foo" (quote (<1> void)) [(@new (@type/primitive (quote (<2> float))) :in) (@new (@type/primitive (quote (<2> float))) :in)]))
    (do
      (def <3> (quote (<1> void)))
      (def <4> [(upscope (def <5> (@new (@type/primitive (quote (<2> float))) :in)) (def x (@new "x" (@type <5>))) (@new x <5>)) (upscope (def <6> (@new (@type/primitive (quote (<2> float))) :in)) (def y (@new "y" (@type <6>))) (@new y <6>))])
      (def <7> @[])
      (@array/push <7> (upscope (def <8> (@expr/literal (quote (<1> primitive (<2> float))) 1)) (def <9> (@type <8>)) (def x (@new "x" <9>)) (@statement/declaration false x <8>)))
      (@array/push <7> (@statement/return (@vector (@expr/identifier x) (@expr/literal (quote (<1> primitive (<2> float))) 2) (@expr/literal (quote (<1> primitive (<2> float))) 3))))
      (@implement foo <3> <4> <7>))))

(test (jlsl/defn :void foo [:float x :float y]
  (var z 1)
  (return (+ x y z)))
  [<1>
   defined
   "foo"
   [<2> void]
   [[[<2> primitive [<3> float]] :in]
    [[<2> primitive [<3> float]] :in]]
   @[[[<4>
       lexical
       <5>
       "x"
       [<2> primitive [<3> float]]]
      [[<2> primitive [<3> float]] :in]]
     [[<4>
       lexical
       <6>
       "y"
       [<2> primitive [<3> float]]]
      [[<2> primitive [<3> float]] :in]]]
   @[[<7>
      declaration
      false
      [<4>
       lexical
       <8>
       "z"
       [<2> primitive [<3> float]]]
      [<9>
       literal
       [<2> primitive [<3> float]]
       1]]
     [<7>
      return
      [<9>
       call
       [<1>
        builtin
        "+"
        [<2> primitive [<3> float]]
        [[[<2> primitive [<3> float]] :in]
         [[<2> primitive [<3> float]] :in]]]
       @[[<9>
          identifier
          [<4>
           lexical
           <5>
           "x"
           [<2> primitive [<3> float]]]]
         [<9>
          identifier
          [<4>
           lexical
           <6>
           "y"
           [<2> primitive [<3> float]]]]
         [<9>
          identifier
          [<4>
           lexical
           <8>
           "z"
           [<2> primitive [<3> float]]]]]]]]
   @[[<10> unscanned]]
   @[]
   @[]])

(test (jlsl/defn :void foo [:float x :float y]
  (var z 0)
  (for (var i 0) (< i 10) (++ i)
    (+= z i))
  (return (+ x y z)))
  [<1>
   defined
   "foo"
   [<2> void]
   [[[<2> primitive [<3> float]] :in]
    [[<2> primitive [<3> float]] :in]]
   @[[[<4>
       lexical
       <5>
       "x"
       [<2> primitive [<3> float]]]
      [[<2> primitive [<3> float]] :in]]
     [[<4>
       lexical
       <6>
       "y"
       [<2> primitive [<3> float]]]
      [[<2> primitive [<3> float]] :in]]]
   @[[<7>
      declaration
      false
      [<4>
       lexical
       <8>
       "z"
       [<2> primitive [<3> float]]]
      [<9>
       literal
       [<2> primitive [<3> float]]
       0]]
     [<7>
      for
      [<7>
       declaration
       false
       [<4>
        lexical
        <10>
        "i"
        [<2> primitive [<3> float]]]
       [<9>
        literal
        [<2> primitive [<3> float]]
        0]]
      [<9>
       call
       [<1>
        builtin
        "<"
        [<2> primitive [<3> bool]]
        [[[<2> primitive [<3> float]] :in]
         [[<2> primitive [<3> float]] :in]]]
       @[[<9>
          identifier
          [<4>
           lexical
           <10>
           "i"
           [<2> primitive [<3> float]]]]
         [<9>
          literal
          [<2> primitive [<3> float]]
          10]]]
      [<7>
       crement
       ++
       [<9>
        identifier
        [<4>
         lexical
         <10>
         "i"
         [<2> primitive [<3> float]]]]]
      @[[<7>
         update
         @+=
         [<9>
          identifier
          [<4>
           lexical
           <8>
           "z"
           [<2> primitive [<3> float]]]]
         [<9>
          identifier
          [<4>
           lexical
           <10>
           "i"
           [<2> primitive [<3> float]]]]]]]
     [<7>
      return
      [<9>
       call
       [<1>
        builtin
        "+"
        [<2> primitive [<3> float]]
        [[[<2> primitive [<3> float]] :in]
         [[<2> primitive [<3> float]] :in]]]
       @[[<9>
          identifier
          [<4>
           lexical
           <5>
           "x"
           [<2> primitive [<3> float]]]]
         [<9>
          identifier
          [<4>
           lexical
           <6>
           "y"
           [<2> primitive [<3> float]]]]
         [<9>
          identifier
          [<4>
           lexical
           <8>
           "z"
           [<2> primitive [<3> float]]]]]]]]
   @[[<11> unscanned]]
   @[]
   @[]])

# ----------

(defdyn *identifier-map*)
(def core/dyn dyn)
(defn dyn [dynvar]
  (or (core/dyn dynvar) (errorf "%q is not set" dynvar)))

(defmodule bimap
  (defn new [&opt proto]
    (if proto
      [(table/setproto @{} (in proto 0))
       (table/setproto @{} (in proto 1))]
       [@{} @{}]))
  (def- core/in in)
  (def- core/put put)
  (def- core/has-key? has-key?)
  (defn in [[forward _] key] (core/in forward key))
  (defn out [[_ backward] value] (core/in backward value))
  (defn put [[forward backward] k v]
    (core/put forward k v)
    (core/put backward v k)
    v)
  (defn has-key? [[forward _] k] (core/has-key? forward k))
  (defn has-value? [[_ backward] v] (core/has-key? backward v)))

(defn render/expr [t] # and *identifier-map*
  (expr/match t
    (literal _ value) value
    (identifier variable)
      (or (bimap/in (dyn *identifier-map*) variable)
        (errorf "BUG: variable %q is not in scope. This shouldn't happen, but it happened. How did it happen?"
          variable))
    (call function args)
      [(function/to-glsl function)
        ;(map render/expr args)
        ;(map |(render/expr (expr/identifier (param/var $))) (function/implicit-params function))]
    (dot expr field) ['. (render/expr expr) field]
    (crement op expr) [op (render/expr expr)]))

(defmacro subscope [& exprs]
  ~(with-dyns [,*identifier-map* (,bimap/new (,dyn ,*identifier-map*))]
    ,;exprs))

(defn new-identifier [variable] # and *identifier-map*
  (def identifier-map (dyn *identifier-map*))
  (var identifier (symbol (variable/name variable)))
  # there might be a more efficient way to do this, but who cares
  (var i 1)
  (while (bimap/has-value? identifier-map identifier)
    (set identifier (symbol (variable/name variable) i))
    (++ i))
  (bimap/put identifier-map variable identifier))

(defn render/statement [t] # and *identifier-map*
  (statement/match t
    (declaration const? variable expr) (do
      # render before we allocate the identifier
      (def rendered-expr (render/expr expr))
      [(if const? 'def 'var)
        (type/to-glsl (variable/type variable))
        (new-identifier variable)
        rendered-expr])
    (assign l-value r-value)
      ['set (render/expr l-value) (render/expr r-value)]
    (update op l-value r-value)
      [op (render/expr l-value) (render/expr r-value)]
    (break) ['break]
    (discard) ['discard]
    (continue) ['continue]
    (return expr) ['return (render/expr expr)]
    (do body) (subscope ['do ;(map render/statement body)])
    (with bindings body) (subscope ['do
      ;(seq [[variable expr] :in bindings]
        (render/statement (statement/declaration false variable expr)))
      ;(map render/statement body)])
    (if cond then else)
      ['if (render/expr cond) (render/statement then) ;(if else [(render/statement else)] [])]
    (case value cases)
      ['case ;(catseq [case :in cases]
        (pat/match case
          [body] [(render/statement body)]
          [value body] [(render/expr value) (render/statement body)]))]
    (while cond body) (subscope ['while (render/expr cond) ;(map render/statement body)])
    (do-while cond body) ['do-while (render/expr cond) ;(subscope (map render/statement body))]
    (for init cond update body)
      (subscope
        ['for (render/statement init) (render/expr cond) (render/statement update) ;(map render/statement body)])
    (call function arguments)
      [(function/to-glsl function) ;(map render/expr arguments)]
    (crement op expr) [op (render/expr expr)])
  )

(defn render/param [param] # and *identifier-map*
  [(param-sig/to-glsl (param/sig param)) (new-identifier (param/var param))])

(defn render/function [function]
  (def forwards @{})
  (def results @[])
  (def in-progress @{})
  (def finished @{})

  (visit function (fn [node visiting? stack]
    (unless (function? node) (break))

    (when visiting?
      # we don't need a forward declaration for a direct recursive call
      (unless (= node (find-last function? stack))
        (put forwards node true))
      (break))

    (function/match node
      (builtin _ _ _) nil
      (defined name return-type _ params body _ _ _)
        (with-dyns [*identifier-map* (bimap/new)]
          (assertf (not (empty? body)) "%s: unimplemented function" name)
          (def implicit-params (function/implicit-params node))
          # TODO: we should make sure we're actually generating a unique name
          (def glsl-name (symbol name))
          # TODO: hoist free variables
          # TODO: we need to come up with preferred glsl names for these variables
          (def glsl ~(defn ,(type/to-glsl return-type) ,glsl-name [,;(mapcat render/param [;params ;implicit-params])]
            ,;(map render/statement body)))
          (array/push results glsl)))))

  (array/concat
    (seq [function :keys forwards]
      (function/match function
        (builtin _ _ _) (error "BUG: cannot forward-declare a builtin function")
        (defined name return-type _ params _ _ _ _)
          (with-dyns [*identifier-map* (bimap/new)]
            ~(defn ,(type/to-glsl return-type) ,name [,;(mapcat render/param params)]))))
    results))


(defmacro* test-function [expr & results]
  ~(test-stdout (,prin (,glsl/render-program (,render/function ,expr))) ,;results))

(test (render/function (jlsl/defn :float incr [:float x]
  (return (+ x 1))))
  @[[defn
     :float
     incr
     [:float x]
     [return [+ x 1]]]])

(deftest "only referenced functions are included"
  (test-function (do
    (jlsl/defn :float square [:float x]
      (return (* x x)))

    (jlsl/defn :float cube [:float x]
      (return (* x x x)))

    (jlsl/defn :float foo [:float x]
      (return (+ (square x) 1)))) `
    float square(float x) {
      return x * x;
    }
    
    float foo(float x) {
      return square(x) + 1.0;
    }
  `))

(deftest "recursive functions"
  (test-function
    (jlsl/defn :float foo [:float x]
      (return (foo x))) `
    float foo(float x) {
      return foo(x);
    }
  `))

(deftest "mutually recursive functions generate forward declarations"
  (test-function (do
    (jlsl/declare :float bar [:float])

    (jlsl/defn :float foo [:float x]
      (return (bar x)))

    (jlsl/implement :float bar [:float x]
      (return (foo x)))) `
    float bar(float x);
    
    float foo(float x) {
      return bar(x);
    }
    
    float bar(float x) {
      return foo(x);
    }
  `))

(deftest "anonymous functions"
  (test-function
    (jlsl/fn :float "foo" [:float x]
      (return (+ x 1))) `
    float foo(float x) {
      return x + 1.0;
    }
  `))

(deftest "function with out and inout parameters"
  (test-function
    (jlsl/defn :float foo [:float x [in :float] y [out :float] z [inout :float] w]
      (return (foo x y z w))) `
    float foo(float x, float y, out float z, inout float w) {
      return foo(x, y, z, w);
    }
  `))

(defn show-implicit-params [function]
  (map |[(type/to-glsl (param/type $)) (variable/name (param/var $)) (param/access $)]
    (function/implicit-params function)))

(deftest "function with no free variables"
  (test (show-implicit-params
    (jlsl/fn :float "name" [:float x]
      (return (+ x 1))))
    @[]))

(deftest "function with simple free variable"
  (def free (variable/new "free" type/float))

  (test (show-implicit-params
    (jlsl/fn :float "name" [:float x]
      (return (+ x free))))
    @[[:float "free" :in]]))

(deftest "function with out free variable"
  (def free (variable/new "free" type/float))

  (test (show-implicit-params
    (jlsl/fn :float "name" [:float x]
      (set free 100)
      (return x)))
    @[[:float "free" :out]]))

(deftest "function with inout free variable"
  (def free (variable/new "free" type/float))

  (test (show-implicit-params
    (jlsl/fn :float "name" [:float x]
      (+= free 100)
      (return x)))
    @[[:float "free" :inout]]))

(deftest "outness projects through field and array access"
  (def free1 (variable/new "free1" type/vec3))
  (def free2 (variable/new "free2" type/vec3))

  (test (show-implicit-params
    (jlsl/fn :float "name" [:float x]
      (set (. free1 x) 100)
      (+= (. free2 xyz) 100)
      # TODO: add arrays here, once we add arrays
      # (set ((in free2 0) x) 100)
      (return x)))
    @[[:vec3 "free1" :out]
      [:vec3 "free2" :inout]]))

(deftest "function that calls another function with a free variable"
  (def free (variable/new "free" type/float))

  (test (show-implicit-params (do
    (jlsl/defn :float foo [:float x]
      (return (+ x free)))

    (jlsl/fn :float "name" [:float x]
      (return (foo x)))))
    @[[:float "free" :in]]))

(deftest "recursive functions with free variables"
  (def free (variable/new "free" type/float))
  (test (show-implicit-params (do
    (jlsl/defn :float foo [:float x]
      (return (foo (+ x free))))))
    @[[:float "free" :in]]))

(deftest "mutually recursive functions with free variables"
  (def free (variable/new "free" type/float))
  (test (show-implicit-params (do
    (jlsl/declare :float bar [:float])

    (jlsl/defn :float foo [:float x]
      (return (bar (+ x free))))

    (jlsl/implement :float bar [:float x]
      (return (foo x)))))
    @[[:float "free" :in]])

  (def free2 (variable/new "free2" type/float))
  (test (show-implicit-params (do
    (jlsl/declare :float bar [:float])

    (jlsl/defn :float foo [:float x]
      (return (bar (+ x free2))))

    (jlsl/implement :float bar [:float x]
      (return (foo (+ x free))))))
    @[[:float "free" :in]
      [:float "free2" :in]])
  )

(deftest "variables should be reference-unique"
  (def free1 (variable/new "free" type/float))
  (def free2 (variable/new "free" type/float))
  (test (show-implicit-params
    (jlsl/defn :float foo [:float x]
      (return (+ x (+ free1 free2)))))
    @[[:float "free" :in]
      [:float "free" :in]]))

(deftest "function invocations only pick up free variables that are free at the function's callsite"
  (test (show-implicit-params
    (jlsl/defn :float foo [:float x]
      (return ((jlsl/fn :float "bar" [:float y] (return (* x y))) x))))
    @[]))

# TODO: we should support overloads and multiple implementations.
# also have to think about e.g. matrix multiplication
(deftest "builtins are variadic"
  (def free1 (variable/new "free" type/float))
  (def free2 (variable/new "free" type/float))
  (test-error (show-implicit-params (do
    (jlsl/defn :float foo [:float x]
      (return (+ x free1 free2)))))
    "wrong number of arguments to function +, expected 2, got 3"))

(deftest "variables always get unique identifiers"
  (def free1 (variable/new "free" type/float))
  (def free2 (variable/new "free" type/float))
  (def free3 (variable/new "x" type/float))
  (def free4 (variable/new "y" type/float))
  (test-function
    (jlsl/defn :float foo [:float x]
      (return (+ (+ free1 free2) (+ free3 free4)))) `
    float foo(float x, float free, float free1, float x1, float y) {
      return (free + free1) + (x1 + y);
    }
  `))

# TODO: we could optimize this, and realize that the shadow is allowed if the other identifier
# is not referenced again in the current scope. But... that is a micro-optimization of the
# aesthetics of the generated code.
(deftest "variables get unique identifiers even if they're shadowing another lexical variable"
  (test-function
    (jlsl/defn :float foo [:float x]
      (var x 10)
      (do
        (var x 20))
      (return 1)) `
    float foo(float x) {
      float x1 = 10.0;
      {
        float x2 = 20.0;
      }
      return 1.0;
    }
  `))

(deftest "function calls automatically forward free variables"
  (test-function
    (jlsl/defn :float foo [:float x]
      (return ((jlsl/fn :float "bar" [:float y] (return (* x y))) x))) `
    float bar(float y, float x) {
      return x * y;
    }
    
    float foo(float x) {
      return bar(x, x);
    }
  `))

(deftest "free variable forwarding happens through function calls"
  (def free (variable/new "free" type/float))
  (test-function (do
    (jlsl/defn :float qux [:float x]
      (return (+ x free)))

    (jlsl/defn :float bar [:float x]
      (return (qux x)))

    (jlsl/defn :float foo [:float x]
      (return (bar x)))) `
    float qux(float x, float free) {
      return x + free;
    }
    
    float bar(float x, float free) {
      return qux(x, free);
    }
    
    float foo(float x, float free) {
      return bar(x, free);
    }
  `))

(deftest "free variable forwarding happens even with mutual recursion"
  (def free (variable/new "free" type/float))
  (test-function (do
    (jlsl/declare :float foo [:float])

    (jlsl/defn :float bar [:float x]
      (return (foo x)))

    (jlsl/defn :float qux [:float x]
      (return (+ x free)))

    (jlsl/implement :float foo [:float x]
      (return (+ (bar x) (qux x))))) `
    float foo(float x);
    
    float bar(float x, float free) {
      return foo(x, free);
    }
    
    float qux(float x, float free) {
      return x + free;
    }
    
    float foo(float x, float free) {
      return bar(x, free) + qux(x, free);
    }
  `))

(deftest "with statements"
  (def p (variable/new "p" type/vec3))
  (test-function
    (jlsl/defn :float distance []
      (with [p [0 0 0]]
        (return p))) `
    float distance() {
      {
        vec3 p = vec3(0.0, 0.0, 0.0);
        return p;
      }
    }
  `))

(deftest "dynamic variable"
  (def p (variable/new "p" type/vec3))
  (test-function (do
    (jlsl/defn :float sphere [:float r]
      (return (- (length p) r)))

    # TODO: rename vec- to -
    (jlsl/defn :float translated []
      (with [p (builtins/vec- p [10 20 30])]
        (return (sphere 20))))

    (jlsl/defn :float distance []
      (with [p [0 0 0]]
        (return (translated))))) `
    float sphere(float r, vec3 p) {
      return length(p) - r;
    }
    
    float translated(vec3 p) {
      {
        vec3 p1 = vec_(p, vec3(10.0, 20.0, 30.0));
        return sphere(20.0, p1);
      }
    }
    
    float distance() {
      {
        vec3 p = vec3(0.0, 0.0, 0.0);
        return translated(p);
      }
    }
  `))

(deftest "for loop"
  (def p (variable/new "p" type/vec3))
  (test-function
    (jlsl/fn :float "distance" []
      (for (var i 10) (< i 10) (++ i)
        (break))) `
    float distance() {
      for (float i = 10.0; i < 10.0; ++i) {
        break;
      }
    }
  `))
