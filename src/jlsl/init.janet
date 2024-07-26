(use judge)
(use module)
(import pat)
(use ./util)
(use ./adt)

(defadt variable
  (dynamic name type)
  (lexical name type))

(defadt primitive-type
  (float)
  (double)
  (int)
  (uint)
  (bool))

# TODO: I might want to separate a function and a function
# implementation into different types.
(defadt function
  (builtin name return-type param-types)
  (defined name return-type param-types params body scan-ref free-vars-ref))

(defadt type
  (void)
  (primitive type)
  (vec type count)
  (struct name fields))

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

(defmodule variable
  (defn to-glsl [t]
    (variable/match t
      (dynamic name _) (symbol name)
      (lexical name _) (symbol name)))
  (defn type [t]
    (variable/match t
      (dynamic _ type) type
      (lexical _ type) type)))

(defmacro defbuiltin [sym return-type & arg-types]
  ~(def ,(symbol "builtins/" sym)
    (,function/builtin ,(string sym) ,return-type ,(tuple/brackets ;arg-types))))

(defbuiltin + :float :float :float)
(defbuiltin * :float :float :float)

(defmodule function
  (defn new [name return-type arg-types]
    (function/defined name return-type arg-types @[] @[] (ref/new) (ref/new)))

  (defn of-ast [sym]
    (case sym
      '+ ['quote builtins/+]
      '* ['quote builtins/*]
      sym))

  (defn to-glsl [t]
    (function/match t
      (builtin name _ _) (symbol name)
      (defined name _ _ _ _ _ _) (symbol name)))

  (defn return-type [t]
    (function/match t
      (builtin _ type _) type
      (defined _ type _ _ _ _ _) type))

  # TODO: theoretically a single function can have multiple overloads,
  # so we should be able to implement it multiple times. but we're not there yet.
  (defn implement [t return-type params body]
    (function/match t
      (builtin _ _ _) (error "BUG: attempting to implement builtin function")
      (defined name declared-return-type declared-arg-types current-params current-body _ _) (do
        (assertf (empty? current-body) "%s: cannot implement a function multiple times" name)
        (assertf (not (empty? body)) "%s: cannot implement with empty body" name)
        (def implemented-arg-types (map variable/type params))
        (assertf (contents= declared-arg-types implemented-arg-types)
          "%s: argument type mismatch, declared as %q implemented as %q"
          name
          declared-arg-types
          implemented-arg-types)
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

    (var scope (tabseq [variable :in params] variable true))
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
          # TODO: oh, this is interesting... if we
          # call a function with inout arguments, we
          # actually need to record that here
          (each arg args (see-expr arg :read)))))

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
        (call function args) (do
          # TODO: this is redundant with the expression code that does the same thing...
          # ...and also should take into account inout parameters
          (put functions-called function true)
          (each arg args (see-expr arg :read))))))
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
    (keys result))

  (defn free-variables [t]
    (function/match t
      (builtin _ _ _) []
      (defined name _ _ _ _ _ free-vars-ref)
        (ref/get-or-put free-vars-ref (compute-free-variables t))))

  )

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
      |stuple? [vector ;(map of-ast expr-ast)]
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
            (def ,name (,variable/lexical ,(string name) ,$type))
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

(defmacro- jlsl/stub [return-type name arg-types]
  ~(,function/new ,name ,(type/of-ast return-type) [,;(map type/of-ast arg-types)]))

(defmacro- jlsl/declare [return-type name arg-types]
  ['def name (call jlsl/stub return-type (string name) arg-types)])

(defmacro- jlsl/implement [return-type name params & body]
  (def $return-type (gensym))
  (def $params (gensym))
  (def $body (gensym))

  (def <params> (seq [[type name] :in (partition 2 params)]
    ~(def ,name (,variable/lexical ,(string name) ,(type/of-ast type)))))

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
  (def incr (@new "incr" (@type/primitive (quote (<1> float))) [(@type/primitive (quote (<1> float)))])))

(test-macro (jlsl/implement :float incr [:float x] (return x))
  (do
    (def <1> (@type/primitive (quote (<2> float))))
    (def <3> [(def x (@variable/lexical "x" (@type/primitive (quote (<2> float)))))])
    (def <4> @[])
    (@array/push <4> (@statement/return (@expr/identifier x)))
    (@implement incr <1> <3> <4>)))
(test-macro (jlsl/defn :float incr [:float x] (return x))
  (upscope
    (def incr (@new "incr" (@type/primitive (quote (<1> float))) [(@type/primitive (quote (<1> float)))]))
    (do
      (def <2> (@type/primitive (quote (<1> float))))
      (def <3> [(def x (@variable/lexical "x" (@type/primitive (quote (<1> float)))))])
      (def <4> @[])
      (@array/push <4> (@statement/return (@expr/identifier x)))
      (@implement incr <2> <3> <4>))))

(test-macro (jlsl/defn :void foo [:float x :float y]
  (var x 1)
  (return [x 2 3]))
  (upscope
    (def foo (@new "foo" (quote (<1> void)) [(@type/primitive (quote (<2> float))) (@type/primitive (quote (<2> float)))]))
    (do
      (def <3> (quote (<1> void)))
      (def <4> [(def x (@variable/lexical "x" (@type/primitive (quote (<2> float))))) (def y (@variable/lexical "y" (@type/primitive (quote (<2> float)))))])
      (def <5> @[])
      (@array/push <5> (upscope (def <6> (@expr/literal (quote (<1> primitive (<2> float))) 1)) (def <7> (@type <6>)) (def x (@variable/lexical "x" <7>)) (@statement/declaration false x <6>)))
      (@array/push <5> (@statement/return (@vector (@expr/identifier x) (@expr/literal (quote (<1> primitive (<2> float))) 2) (@expr/literal (quote (<1> primitive (<2> float))) 3))))
      (@implement foo <3> <4> <5>))))

(test (jlsl/defn :void foo [:float x :float y]
  (var z 1)
  (return (+ x y z)))
  [<1>
   defined
   "foo"
   [<2> void]
   [[<2> primitive [<3> float]]
    [<2> primitive [<3> float]]]
   @[[<4>
      lexical
      "x"
      [<2> primitive [<3> float]]]
     [<4>
      lexical
      "y"
      [<2> primitive [<3> float]]]]
   @[[<5>
      declaration
      false
      [<4>
       lexical
       "z"
       [<2> primitive [<3> float]]]
      [<6>
       literal
       [<2> primitive [<3> float]]
       1]]
     [<5>
      return
      [<6>
       call
       [<1> builtin "+" :float [:float :float]]
       @[[<6>
          identifier
          [<4>
           lexical
           "x"
           [<2> primitive [<3> float]]]]
         [<6>
          identifier
          [<4>
           lexical
           "y"
           [<2> primitive [<3> float]]]]
         [<6>
          identifier
          [<4>
           lexical
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
   [[<2> primitive [<3> float]]
    [<2> primitive [<3> float]]]
   @[[<4>
      lexical
      "x"
      [<2> primitive [<3> float]]]
     [<4>
      lexical
      "y"
      [<2> primitive [<3> float]]]]
   @[[<5>
      declaration
      false
      [<4>
       lexical
       "z"
       [<2> primitive [<3> float]]]
      [<6>
       literal
       [<2> primitive [<3> float]]
       0]]
     [<5>
      for
      [<5>
       declaration
       false
       [<4>
        lexical
        "i"
        [<2> primitive [<3> float]]]
       [<6>
        literal
        [<2> primitive [<3> float]]
        0]]
      [<6>
       call
       @<
       @[[<6>
          identifier
          [<4>
           lexical
           "i"
           [<2> primitive [<3> float]]]]
         [<6>
          literal
          [<2> primitive [<3> float]]
          10]]]
      [<5>
       call
       @++
       @[[<6>
          identifier
          [<4>
           lexical
           "i"
           [<2> primitive [<3> float]]]]]]
      @[[<5>
         declaration
         nil
         [<4>
          lexical
          "z"
          [<2> primitive [<3> float]]]
         [<6>
          identifier
          [<4>
           lexical
           "i"
           [<2> primitive [<3> float]]]]]]]
     [<5>
      return
      [<6>
       call
       [<1> builtin "+" :float [:float :float]]
       @[[<6>
          identifier
          [<4>
           lexical
           "x"
           [<2> primitive [<3> float]]]]
         [<6>
          identifier
          [<4>
           lexical
           "y"
           [<2> primitive [<3> float]]]]
         [<6>
          identifier
          [<4>
           lexical
           "z"
           [<2> primitive [<3> float]]]]]]]]
   @[]
   @[]])

# ----------

(defn render-arg [variable] [(type/to-glsl (variable/type variable)) (variable/to-glsl variable)])

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
        (def glsl ~(defn ,(type/to-glsl return-type) ,glsl-name [,;(mapcat render-arg params)]
          ,;(map statement/to-glsl body)))
        (array/push results glsl)))))

  (array/concat
    (seq [function :keys forwards]
      (function/match function
        (builtin _ _ _) (error "BUG: cannot forward-declare a builtin function")
        (defined name return-type _ params _ _ _)
        ~(defn ,(type/to-glsl return-type) ,name [,;(mapcat render-arg params)])))
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

(deftest "function with no free variables"
  (test (function/free-variables
    (jlsl/fn :float "name" [:float x]
      (return (+ x 1))))
    @[]))

(deftest "function with simple free variable"
  (def free (variable/lexical "free" type/float))

  (test (function/free-variables
    (jlsl/fn :float "name" [:float x]
      (return (+ x free))))
    @[[<1>
       lexical
       "free"
       [<2> primitive [<3> float]]]]))

(deftest "function that calls another function with a free variable"
  (def free (variable/lexical "free" type/float))

  (test (function/free-variables (do
    (jlsl/defn :float foo [:float x]
      (return (+ x free)))

    (jlsl/fn :float "name" [:float x]
      (return (foo x)))))
    @[[<1>
       lexical
       "free"
       [<2> primitive [<3> float]]]]))

(deftest "recursive functions with free variables"
  (def free (variable/lexical "free" type/float))
  (test (function/free-variables (do
    (jlsl/defn :float foo [:float x]
      (return (foo (+ x free))))))
    @[[<1>
       lexical
       "free"
       [<2> primitive [<3> float]]]]))

(deftest "mutually recursive functions with free variables"
  (def free (variable/lexical "free" type/float))
  (test (function/free-variables (do
    (jlsl/declare :float bar [:float])

    (jlsl/defn :float foo [:float x]
      (return (bar (+ x free))))

    (jlsl/implement :float bar [:float x]
      (return (foo x)))))
    @[[<1>
       lexical
       "free"
       [<2> primitive [<3> float]]]])

  (def free2 (variable/lexical "free2" type/float))
  (test (function/free-variables (do
    (jlsl/declare :float bar [:float])

    (jlsl/defn :float foo [:float x]
      (return (bar (+ x free2))))

    (jlsl/implement :float bar [:float x]
      (return (foo (+ x free))))))
    @[[<1>
       lexical
       "free2"
       [<2> primitive [<3> float]]]
      [<1>
       lexical
       "free"
       [<2> primitive [<3> float]]]])
  )

# TODO: this is a problem; we need variables to have reference-equality
(deftest "variables should be reference-unique"
  (def free1 (variable/lexical "free" type/float))
  (def free2 (variable/lexical "free" type/float))
  (test (function/free-variables (do
    (jlsl/defn :float foo [:float x]
      (return (+ x free1 free2)))))
    @[[<1>
       lexical
       "free"
       [<2> primitive [<3> float]]]]))

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
