(use judge)
(use module)
(import pat)
(use ./util)
(use ./adt)

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

(defadt type
  (void)
  (primitive type)
  (vec type count)
  (struct name fields))

(defmodule type
  (def float (type/primitive (primitive-type/float)))
  (def int (type/primitive (primitive-type/int)))
  (def uint (type/primitive (primitive-type/uint)))
  (def double (type/primitive (primitive-type/double)))
  (def bool (type/primitive (primitive-type/bool)))

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
      (primitive t) (primitive-type/to-glsl t)
      (struct name _) (symbol name)
      (vec type count) (keyword (primitive-type/vec-prefix type) count)))

  (defn components [t]
    (type/match t
      (primitive _) 1
      (vec _ count) count
      (struct _ _) (error "vectors cannot contain compound entries")))

  (defn base-type [t]
    # TODO: arrays, probably
    (type/match t
      (primitive t) t
      (vec t _) t
      (struct _ _) nil))
  )

(defmodule variable
  (defn new [name type] (variable/lexical (gensym) name type))
  (defn dyn [name type] (variable/dynamic (gensym) name type))

  (defn to-glsl [t]
    (variable/match t
      (dynamic _ name _) (symbol name)
      (lexical _ name _) (symbol name)))
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
  (defined name return-type param-sigs params body scan-ref free-vars-ref))

(defadt expr
  (literal type value)
  (identifier variable)
  (call function args))

(defadt statement
  (declaration const? variable expr)
  (assign l-value r-value)
  (update op l-value r-value)
  (break)
  (continue)
  (return expr)
  (do body)
  (if cond then else)
  (case value cases)
  (while cond body)
  (do-while cond body)
  (for init cond update body)
  (call function arguments))

(defmacro defbuiltin [sym return-type & param-sigs]
  ~(def ,(symbol "builtins/" sym)
    (,function/builtin ,(string sym) ,return-type
      ,(tuple/brackets ;(map param-sig/of-ast param-sigs)))))

(defbuiltin + :float :float :float)
(defbuiltin * :float :float :float)

(defmodule function
  (defn new [name return-type param-sigs]
    (function/defined name return-type param-sigs @[] @[] (ref/new) (ref/new)))

  (defn of-ast [sym]
    (case sym
      '+ ['quote builtins/+]
      '* ['quote builtins/*]
      sym))

  (defn to-glsl [t]
    (function/match t
      (builtin name _ _) (symbol name)
      (defined name _ _ _ _ _ _) (symbol name)))

  (defn param-sigs [t]
    (function/match t
      (builtin _ _ param-sigs) param-sigs
      (defined _ _ param-sigs _ _ _ _) param-sigs))

  (defn return-type [t]
    (function/match t
      (builtin _ type _) type
      (defined _ type _ _ _ _ _) type))

  # TODO: theoretically a single function can have multiple overloads,
  # so we should be able to implement it multiple times. but we're not there yet.
  (defn implement [t return-type params body]
    (function/match t
      (builtin _ _ _) (error "BUG: attempting to implement builtin function")
      (defined name declared-return-type declared-param-sigs current-params current-body _ _) (do
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
      # TODO: this should consider a variable written
      # to if it occurs on the left-hand side of a field
      # or array access
      (expr/match expr
        (literal _ _) nil
        (identifier variable)
          (unless (in scope variable)
            (mark variable rw))
        (call function args) (do
          (put functions-called function true)
          (each [arg param-sig] (zip args (param-sigs function))
            (match (param-sig/access param-sig)
              :in (see-expr arg :read)
              :out (see-expr arg :write)
              :inout (do (see-expr arg :read) (see-expr arg :write))
              access (errorf "BUG: unknown access qualifier %q" access))))))

    (var visit nil)
    (defn block [statements]
      (set scope (table/setproto @{} scope))
      (each statement statements
        (visit statement))
      (set scope (table/getproto scope)))

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
          (see-expr r-value :write))
        (break) nil
        (continue) nil
        (return expr) (see-expr expr :read)
        (do body) (block body)
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
        (for init cond update body) (do
          (see-expr cond :read)
          (block [init update ;body]))
        (call function args) (see-expr (expr/call function args)))))
    (block body)

    [free-vars (keys functions-called)])

  (defn memoized-scan [t]
    (function/match t
      (builtin _ _ _) [[] []]
      (defined name _ _ params body scan-ref _)
        (ref/get-or-put scan-ref (scan name body params))))

  (defn compute-free-variables [t]
    (def result @{})

    (defn union-variable [variable [read? written?]]
      (if (has-key? result variable)
        (let [[old-read? old-written?] (in result variable)]
          (put result variable [(or read? old-read?) (or written? old-written?)]))
        (put result variable [read? written?])))

    (visit t
      (fn [function visiting? _]
        (when visiting? (break))
        (def [vars _] (memoized-scan function))
        (eachp [variable access-types] vars
          (union-variable variable access-types)))
      (fn [f function]
        (def [_ functions] (memoized-scan function))
        (each function functions
          (f function))))
    # TODO: this ignores read-writeness of the variables
    (keys result))

  (defn free-variables [t]
    (function/match t
      (builtin _ _ _) []
      (defined name _ _ _ _ _ free-vars-ref)
        (ref/get-or-put free-vars-ref (compute-free-variables t))))

  )

(defmodule expr
  (defn to-glsl [t]
    (expr/match t
      (literal _ value) value
      (identifier variable) (variable/to-glsl variable)
      (call function args) [(function/to-glsl function) ;(map to-glsl args)]))

  (defn type [t]
    (expr/match t
      (literal type _) type
      (identifier variable) (variable/type variable)
      (call function _) (function/return-type function)))

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
        [])
      exprs))

  (defn of-ast [expr-ast]
    (pat/match expr-ast
      |keyword? [expr/literal ['quote type/int] expr-ast]
      |boolean? [expr/literal ['quote type/bool] expr-ast]
      |number? [expr/literal ['quote type/float] expr-ast]
      |symbol? [expr/identifier expr-ast]
      |btuple? [vector ;(map of-ast expr-ast)]
      [f & args] [expr/call (function/of-ast f) (map of-ast args)]
      # TODO
      # [(and op (or '++ '--)) expr]
      # [(and op (or '_++ '_--)) expr]
      # ['if cond then else]
      # ['in expr key]
      # ['. expr key]
      )))

(defmodule statement
  (defn to-glsl [t]
    (statement/match t
      (declaration const? variable expr)
        [(if const? 'def 'var)
          (type/to-glsl (variable/type variable))
          (variable/to-glsl variable)
          (expr/to-glsl expr)]
      (assign l-value r-value)
        ['set (expr/to-glsl l-value) (expr/to-glsl r-value)]
      (update op l-value r-value)
        [op (expr/to-glsl l-value) (expr/to-glsl r-value)]
      (break) ['break]
      (continue) ['continue]
      (return expr) ['return (expr/to-glsl expr)]
      (do body) ['do ;(map to-glsl body)]
      (if cond then else)
        ['if (expr/to-glsl cond) (to-glsl then) ;(if else [(to-glsl else)] [])]
      (case value cases)
        ['case ;(catseq [case :in cases]
          (pat/match case
            [body] [(to-glsl body)]
            [value body] [(expr/to-glsl value) (to-glsl body)]))]
      (while cond body) ['while (expr/to-glsl cond) ;(map to-glsl body)]
      (do-while cond body) ['do-while (expr/to-glsl cond) ;(map to-glsl body)]
      (for init cond update body)
        ['for (expr/to-glsl init) (expr/to-glsl cond) (to-glsl update) ;(map to-glsl body)]
      (call function arguments)
        [(function/to-glsl function) ;(map expr/to-glsl arguments)]))

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
      [(map {'def true 'var false} const?) name value]
        (with-syms [$expr $type $statement]
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
        :float
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
       @<
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
       call
       @++
       @[[<9>
          identifier
          [<4>
           lexical
           <10>
           "i"
           [<2> primitive [<3> float]]]]]]
      @[[<7>
         declaration
         nil
         [<4>
          lexical
          <11>
          "z"
          [<2> primitive [<3> float]]]
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
        :float
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
   @[]
   @[]])

# ----------

(defn render-param [param]
  [(param-sig/to-glsl (param/sig param)) (variable/to-glsl (param/var param))])

(defn render-function [function]
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
      (defined name return-type _ params body _ _) (do
        (assertf (not (empty? body)) "%s: unimplemented function" name)
        # TODO: we should make sure we're actually generating a unique name
        (def glsl-name (symbol name))
        # TODO: hoist free variables
        # TODO: we need to come up with preferred glsl names for these variables
        (def glsl ~(defn ,(type/to-glsl return-type) ,glsl-name [,;(mapcat render-param params)]
          ,;(map statement/to-glsl body)))
        (array/push results glsl)))))

  (array/concat
    (seq [function :keys forwards]
      (function/match function
        (builtin _ _ _) (error "BUG: cannot forward-declare a builtin function")
        (defined name return-type _ params _ _ _)
        ~(defn ,(type/to-glsl return-type) ,name [,;(mapcat render-param params)])))
    results))

(test (render-function (jlsl/defn :float incr [:float x]
  (return (+ x 1))))
  @[[defn
     :float
     incr
     [:float x]
     [return [+ x 1]]]])

(deftest "only referenced functions are included"
  (test (render-function (do
    (jlsl/defn :float square [:float x]
      (return (* x x)))

    (jlsl/defn :float cube [:float x]
      (return (* x x x)))

    (jlsl/defn :float foo [:float x]
      (return (+ (square x) 1)))))
    @[[defn
       :float
       square
       [:float x]
       [return [* x x]]]
      [defn
       :float
       foo
       [:float x]
       [return [+ [square x] 1]]]]))

(deftest "recursive functions"
  (test (render-function (do
    (jlsl/defn :float foo [:float x]
      (return (foo x)))))
    @[[defn
       :float
       foo
       [:float x]
       [return [foo x]]]]))

(deftest "mutually recursive functions generate forward declarations"
  (test (render-function (do
    (jlsl/declare :float bar [:float])

    (jlsl/defn :float foo [:float x]
      (return (bar x)))

    (jlsl/implement :float bar [:float x]
      (return (foo x)))))
    @[[defn :float "bar" [:float x]]
      [defn
       :float
       foo
       [:float x]
       [return [bar x]]]
      [defn
       :float
       bar
       [:float x]
       [return [foo x]]]]))

(deftest "anonymous functions"
  (test (render-function
    (jlsl/fn :float "foo" [:float x]
      (return (+ x 1))))
    @[[defn
       :float
       foo
       [:float x]
       [return [+ x 1]]]]))

(deftest "function with out and inout parameters"
  (test (render-function (do
    (jlsl/defn :float foo [:float x [in :float] y [out :float] z [inout :float] w]
      (return (foo x)))))
    @[[defn
       :float
       foo
       [:float
        x
        :float
        y
        [out :float]
        z
        [inout :float]
        w]
       [return [foo x]]]]))

(deftest "function with no free variables"
  (test (function/free-variables
    (jlsl/fn :float "name" [:float x]
      (return (+ x 1))))
    @[]))

(deftest "function with simple free variable"
  (def free (variable/new "free" type/float))

  (test (function/free-variables
    (jlsl/fn :float "name" [:float x]
      (return (+ x free))))
    @[[<1>
       lexical
       <2>
       "free"
       [<3> primitive [<4> float]]]]))

(deftest "function that calls another function with a free variable"
  (def free (variable/new "free" type/float))

  (test (function/free-variables (do
    (jlsl/defn :float foo [:float x]
      (return (+ x free)))

    (jlsl/fn :float "name" [:float x]
      (return (foo x)))))
    @[[<1>
       lexical
       <2>
       "free"
       [<3> primitive [<4> float]]]]))

(deftest "recursive functions with free variables"
  (def free (variable/new "free" type/float))
  (test (function/free-variables (do
    (jlsl/defn :float foo [:float x]
      (return (foo (+ x free))))))
    @[[<1>
       lexical
       <2>
       "free"
       [<3> primitive [<4> float]]]]))

(deftest "mutually recursive functions with free variables"
  (def free (variable/new "free" type/float))
  (test (function/free-variables (do
    (jlsl/declare :float bar [:float])

    (jlsl/defn :float foo [:float x]
      (return (bar (+ x free))))

    (jlsl/implement :float bar [:float x]
      (return (foo x)))))
    @[[<1>
       lexical
       <2>
       "free"
       [<3> primitive [<4> float]]]])

  (def free2 (variable/new "free" type/float))
  (test (function/free-variables (do
    (jlsl/declare :float bar [:float])

    (jlsl/defn :float foo [:float x]
      (return (bar (+ x free2))))

    (jlsl/implement :float bar [:float x]
      (return (foo (+ x free))))))
    @[[<1>
       lexical
       <2>
       "free"
       [<3> primitive [<4> float]]]
      [<1>
       lexical
       <5>
       "free"
       [<3> primitive [<4> float]]]])
  )

(deftest "variables should be reference-unique"
  (def free1 (variable/new "free" type/float))
  (def free2 (variable/new "free" type/float))
  (test (function/free-variables (do
    (jlsl/defn :float foo [:float x]
      (return (+ x (+ free1 free2))))))
    @[[<1>
       lexical
       <2>
       "free"
       [<3> primitive [<4> float]]]
      [<1>
       lexical
       <5>
       "free"
       [<3> primitive [<4> float]]]]))

# TODO: we should support different implementations
(deftest "builtins are variadic"
  (def free1 (variable/new "free" type/float))
  (def free2 (variable/new "free" type/float))
  (test-error (function/free-variables (do
    (jlsl/defn :float foo [:float x]
      (return (+ x free1 free2)))))
    "zip length mismatch"))

# TODO: alright, this is the one to beat
(deftest "first-class functions automatically forward free variables"
  (test (render-function
    (jlsl/defn :float foo [:float x]
      (return ((jlsl/fn :float "bar" [:float y] (return (* x y))) x))))
    @[[defn
       :float
       bar
       [:float y]
       [return [* x y]]]
      [defn
       :float
       foo
       [:float x]
       [return [bar x]]]]))
