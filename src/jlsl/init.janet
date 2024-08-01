(use judge)
(use module)
(import pat)
(use ./util)
(import ../glsl)
(use ./types)
(use ./builtins)

(defn resolve-function [function-or-multifunction arg-types]
  (assertf (tuple? arg-types) "arg-types must be a tuple, got %q" arg-types)
  (if (multifunction? function-or-multifunction)
    (multifunction/resolve function-or-multifunction arg-types)
    (do
      (assertf (function? function-or-multifunction)
        "%q is not a function" function-or-multifunction)
      function-or-multifunction)))

(defn resolve-impl [t arg-types]
  (function/match (resolve-function t arg-types)
    (custom impl) impl
    (builtin name _ _) (errorf "cannot implement builtin %s" name)))

(defmodule expr
  (defn type [t]
    (expr/match t
      (literal type _) type
      (identifier variable) (variable/type variable)
      (call function _) (function/return-type function)
      (dot expr field) (type/field-type (type expr) field)
      (in expr _) (type/element-type (type expr))
      (crement _ expr) (type expr)))

  (defn- call [general-function args]
    (expr/call (resolve-function general-function (tmap type args)) args))

  (defn- parse-callable [ast]
    (or (if-let [f (in builtins ast)] ~',f ast)))

  (defn of-ast [expr-ast]
    (pat/match expr-ast
      |keyword? [expr/literal ['quote type/int] expr-ast]
      |boolean? [expr/literal ['quote type/bool] expr-ast]
      |number? [expr/literal ['quote type/float] expr-ast]
      |symbol? [expr/identifier expr-ast]
      |btuple? [call ~',(in builtins 'vec) (map of-ast expr-ast)]
      ['. expr field] [expr/dot (of-ast expr) ['quote field]]
      ['in expr index] [expr/in (of-ast expr) (of-ast index)]
      [(and op (or '++ '-- '_++ '_--)) expr] [expr/crement ['quote op] (of-ast expr)]
      # TODO
      # ['if cond then else]
      [f & args] [call (parse-callable f) (map of-ast args)]
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
  (set of-ast (fn of-ast [ast]
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
        [statement/update ~',op (expr/of-ast dest) (expr/of-ast expr)]
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
      other [statement/expr (expr/of-ast other)]
    )))
  )

(defmacro- jlsl/stub [return-type name param-sigs]
  ~(,multifunction/single ,name ,(type/of-ast return-type) [,;(map param-sig/of-ast param-sigs)]))

# declare returns a multifunction
(defmacro- jlsl/declare [return-type name param-sigs]
  ['def name (call jlsl/stub return-type (string name) param-sigs)])

# implement takes a multifunction and returns a single function
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
    (,function/custom (,impl/implement (,resolve-impl ,name (,tmap ,param/type ,$params)) ,$return-type ,$params ,$body))))

# fn returns a single function
(defmacro- jlsl/fn [return-type name params & body]
  (call jlsl/implement return-type (call jlsl/stub return-type name (map 0 (partition 2 params))) params ;body))

# defn returns a single function
(defmacro- jlsl/defn [return-type name params & body]
  ['upscope
    (call jlsl/declare return-type name (map 0 (partition 2 params)))
    (call jlsl/implement return-type name params ;body)])

(test-macro (jlsl/declare :float incr [:float])
  (def incr (@single "incr" (@type/primitive (quote (<1> float))) [(@new (@type/primitive (quote (<1> float))) :in)])))

(test-macro (jlsl/implement :float incr [:float x] (return x))
  (do
    (def <1> (@type/primitive (quote (<2> float))))
    (def <3> [(upscope (def <4> (@new (@type/primitive (quote (<2> float))) :in)) (def x (@new "x" (@type <4>))) (@new x <4>))])
    (def <5> @[])
    (@array/push <5> (@statement/return (@expr/identifier x)))
    (@function/custom (@implement (@resolve-impl incr (@tmap @type <3>)) <1> <3> <5>))))
(test-macro (jlsl/defn :float incr [:float x] (return x))
  (upscope
    (def incr (@single "incr" (@type/primitive (quote (<1> float))) [(@new (@type/primitive (quote (<1> float))) :in)]))
    (do
      (def <2> (@type/primitive (quote (<1> float))))
      (def <3> [(upscope (def <4> (@new (@type/primitive (quote (<1> float))) :in)) (def x (@new "x" (@type <4>))) (@new x <4>))])
      (def <5> @[])
      (@array/push <5> (@statement/return (@expr/identifier x)))
      (@function/custom (@implement (@resolve-impl incr (@tmap @type <3>)) <2> <3> <5>)))))

(test-macro (jlsl/defn :void foo [:float x :float y]
  (var x 1)
  (return [x 2 3]))
  (upscope
    (def foo (@single "foo" (quote (<1> void)) [(@new (@type/primitive (quote (<2> float))) :in) (@new (@type/primitive (quote (<2> float))) :in)]))
    (do
      (def <3> (quote (<1> void)))
      (def <4> [(upscope (def <5> (@new (@type/primitive (quote (<2> float))) :in)) (def x (@new "x" (@type <5>))) (@new x <5>)) (upscope (def <6> (@new (@type/primitive (quote (<2> float))) :in)) (def y (@new "y" (@type <6>))) (@new y <6>))])
      (def <7> @[])
      (@array/push <7> (upscope (def <8> (@expr/literal (quote (<1> primitive (<2> float))) 1)) (def <9> (@type <8>)) (def x (@new "x" <9>)) (@statement/declaration false x <8>)))
      (@array/push <7> (@statement/return (@call (quote @{:name "vec" :overloads "<function 0x1>"}) @[(@expr/identifier x) (@expr/literal (quote (<1> primitive (<2> float))) 2) (@expr/literal (quote (<1> primitive (<2> float))) 3)])))
      (@function/custom (@implement (@resolve-impl foo (@tmap @type <4>)) <3> <4> <7>)))))

(test (jlsl/defn :void foo [:float x :float y]
  (var z 1)
  (return (+ x y z)))
  [<1>
   custom
   {:body @[[<7>
             declaration
             false
             [<2>
              lexical
              <8>
              "z"
              [<4> primitive [<5> float]]]
             [<9>
              literal
              [<4> primitive [<5> float]]
              1]]
            [<7>
             return
             [<9>
              call
              [<1>
               builtin
               "+"
               [<4> primitive [<5> float]]
               @[[[<4> primitive [<5> float]] :in]
                 [[<4> primitive [<5> float]] :in]
                 [[<4> primitive [<5> float]] :in]]]
              @[[<9>
                 identifier
                 [<2>
                  lexical
                  <3>
                  "x"
                  [<4> primitive [<5> float]]]]
                [<9>
                 identifier
                 [<2>
                  lexical
                  <6>
                  "y"
                  [<4> primitive [<5> float]]]]
                [<9>
                 identifier
                 [<2>
                  lexical
                  <8>
                  "z"
                  [<4> primitive [<5> float]]]]]]]]
    :declared-param-sigs [[[<4> primitive [<5> float]] :in]
                          [[<4> primitive [<5> float]] :in]]
    :declared-return-type [<4> void]
    :free-var-access-ref @[]
    :implicit-params-ref @[]
    :name "foo"
    :params @[[[<2>
                lexical
                <3>
                "x"
                [<4> primitive [<5> float]]]
               [[<4> primitive [<5> float]] :in]]
              [[<2>
                lexical
                <6>
                "y"
                [<4> primitive [<5> float]]]
               [[<4> primitive [<5> float]] :in]]]
    :scan-ref @[[<10> unscanned]]}])

(test (jlsl/defn :void foo [:float x :float y]
  (var z 0)
  (for (var i 0) (< i 10) (++ i)
    (+= z i))
  (return (+ x y z)))
  [<1>
   custom
   {:body @[[<7>
             declaration
             false
             [<2>
              lexical
              <8>
              "z"
              [<4> primitive [<5> float]]]
             [<9>
              literal
              [<4> primitive [<5> float]]
              0]]
            [<7>
             for
             [<7>
              declaration
              false
              [<2>
               lexical
               <10>
               "i"
               [<4> primitive [<5> float]]]
              [<9>
               literal
               [<4> primitive [<5> float]]
               0]]
             [<9>
              call
              [<1>
               builtin
               "<"
               [<4> primitive [<5> bool]]
               @[[[<4> primitive [<5> float]] :in]
                 [[<4> primitive [<5> float]] :in]]]
              @[[<9>
                 identifier
                 [<2>
                  lexical
                  <10>
                  "i"
                  [<4> primitive [<5> float]]]]
                [<9>
                 literal
                 [<4> primitive [<5> float]]
                 10]]]
             [<7>
              expr
              [<9>
               crement
               ++
               [<9>
                identifier
                [<2>
                 lexical
                 <10>
                 "i"
                 [<4> primitive [<5> float]]]]]]
             @[[<7>
                update
                +=
                [<9>
                 identifier
                 [<2>
                  lexical
                  <8>
                  "z"
                  [<4> primitive [<5> float]]]]
                [<9>
                 identifier
                 [<2>
                  lexical
                  <10>
                  "i"
                  [<4> primitive [<5> float]]]]]]]
            [<7>
             return
             [<9>
              call
              [<1>
               builtin
               "+"
               [<4> primitive [<5> float]]
               @[[[<4> primitive [<5> float]] :in]
                 [[<4> primitive [<5> float]] :in]
                 [[<4> primitive [<5> float]] :in]]]
              @[[<9>
                 identifier
                 [<2>
                  lexical
                  <3>
                  "x"
                  [<4> primitive [<5> float]]]]
                [<9>
                 identifier
                 [<2>
                  lexical
                  <6>
                  "y"
                  [<4> primitive [<5> float]]]]
                [<9>
                 identifier
                 [<2>
                  lexical
                  <8>
                  "z"
                  [<4> primitive [<5> float]]]]]]]]
    :declared-param-sigs [[[<4> primitive [<5> float]] :in]
                          [[<4> primitive [<5> float]] :in]]
    :declared-return-type [<4> void]
    :free-var-access-ref @[]
    :implicit-params-ref @[]
    :name "foo"
    :params @[[[<2>
                lexical
                <3>
                "x"
                [<4> primitive [<5> float]]]
               [[<4> primitive [<5> float]] :in]]
              [[<2>
                lexical
                <6>
                "y"
                [<4> primitive [<5> float]]]
               [[<4> primitive [<5> float]] :in]]]
    :scan-ref @[[<11> unscanned]]}])

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
      # TODO: need to allocate GLSL names
      [(symbol (function/name function))
        ;(map render/expr args)
        ;(map |(render/expr (expr/identifier (param/var $))) (function/implicit-params function))]
    (dot expr field) ['. (render/expr expr) field]
    (in expr index) ['in (render/expr expr) (render/expr index)]
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
    (expr expr) (render/expr expr))
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
      (custom {:name name :declared-return-type return-type :params params :body body})
        (with-dyns [*identifier-map* (bimap/new)]
          (assertf (not (empty? body)) "%s: unimplemented function" name)
          (def implicit-params (function/implicit-params node))
          # TODO: we should make sure we're actually generating a unique name
          (def glsl-name (symbol name))
          (def glsl ~(defn ,(type/to-glsl return-type) ,glsl-name [,;(mapcat render/param [;params ;implicit-params])]
            ,;(map render/statement body)))
          (array/push results glsl)))))

  (array/concat
    (seq [function :keys forwards]
      (function/match function
        (builtin _ _ _) (error "BUG: cannot forward-declare a builtin function")
        (custom {:name name :declared-return-type return-type :params params})
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
  (def free3 (variable/new "free3" (type/array type/vec3 5)))

  (test (show-implicit-params
    (jlsl/fn :float "name" [:float x]
      (set (. free1 x) 100)
      (+= (. free2 xyz) 100)
      (set (. (in free3 0) x) 100)
      (return x)))
    @[[:vec3 "free1" :inout]
      [:vec3 "free2" :inout]
      [[:vec3 5] "free3" :inout]]))

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
  (def free1 (variable/new "free1" type/float))
  (def free2 (variable/new "free2" type/float))
  (test (show-implicit-params (do
    (jlsl/defn :float foo [:float x]
      (return (+ x free1 free2)))))
    @[[:float "free1" :in]
      [:float "free2" :in]]))

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

    (jlsl/defn :float translated []
      (with [p (- p [10 20 30])]
        (return (sphere 20))))

    (jlsl/defn :float distance []
      (with [p [0 0 0]]
        (return (translated))))) `
    float sphere(float r, vec3 p) {
      return length(p) - r;
    }
    
    float translated(vec3 p) {
      {
        vec3 p1 = p - vec3(10.0, 20.0, 30.0);
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


(deftest "increment/decrement statements"
  (test-function
    (jlsl/fn :float "foo" [:float x]
      (++ x)
      (_-- x)
      (return x)) `
    float foo(float x) {
      ++x;
      x--;
      return x;
    }
  `))

(deftest "for loop"
  (test-function
    (jlsl/fn :float "distance" []
      (for (var i :10) (< i :10) (_++ i)
        (break))) `
    float distance() {
      for (int i = 10; i < 10; i++) {
        break;
      }
    }
  `))

(deftest "arrays"
  (test-function
    (jlsl/fn :float "foo" [(:float 10) foos]
      (var total 0)
      (for (var i :0) (< i :10) (++ i)
        (+= total (in foos i)))
      (return total)) `
    float foo(float[10] foos) {
      float total = 0.0;
      for (int i = 0; i < 10; ++i) {
        total += foos[i];
      }
      return total;
    }
  `))

(deftest "reading a free variable after setting it does not mark it inout"
  (test-function
    (jlsl/fn :float "foo" []
      (var x 0)
      (return ((jlsl/fn :float "bar" []
        (set x 0)
        (+= x 1))))) `
    float bar(out float x) {
      x = 0.0;
      x += 1.0;
    }
    
    float foo() {
      float x = 0.0;
      return bar(x);
    }
  `))

(deftest "setting a component of a vector has to be an inout parameter"
  (test-function
    (jlsl/fn :float "foo" []
      (var foo [1 2 3])
      (return ((jlsl/fn :float "bar" []
        (set (. foo x) 0))))) `
    float bar(inout vec3 foo) {
      foo.x = 0.0;
    }
    
    float foo() {
      vec3 foo = vec3(1.0, 2.0, 3.0);
      return bar(foo);
    }
  `))

(deftest "there is a difference between setting a vector and setting all of its components"
  (test-function
    (jlsl/fn :float "foo" []
      (var foo [0 0 0])
      (var bar [0 0 0])
      (return ((jlsl/fn :float "helper" []
        (set (. foo xyz) [1 2 3])
        (set bar [1 2 3]))))) `
    float helper(inout vec3 foo, out vec3 bar) {
      foo.xyz = vec3(1.0, 2.0, 3.0);
      bar = vec3(1.0, 2.0, 3.0);
    }
    
    float foo() {
      vec3 foo = vec3(0.0, 0.0, 0.0);
      vec3 bar = vec3(0.0, 0.0, 0.0);
      return helper(foo, bar);
    }
  `))

(deftest "builtin overloads"
  (test-function
    (jlsl/fn :float "foo" []
      (return (= [0 0 0] [1 2 3]))
      (return (equal [0 0 0] [1 2 3]))
      (return (equal [0 0 0] [1 2 3]))
      (return (not-equal [0 0 0] [1 2 3]))
      (return (< [0 0 0] [1 2 3]))
      (return (< 1 2))) `
    float foo() {
      return vec3(0.0, 0.0, 0.0) == vec3(1.0, 2.0, 3.0);
      return equal(vec3(0.0, 0.0, 0.0), vec3(1.0, 2.0, 3.0));
      return equal(vec3(0.0, 0.0, 0.0), vec3(1.0, 2.0, 3.0));
      return notEqual(vec3(0.0, 0.0, 0.0), vec3(1.0, 2.0, 3.0));
      return lessThan(vec3(0.0, 0.0, 0.0), vec3(1.0, 2.0, 3.0));
      return 1.0 < 2.0;
    }
  `)
  (test-error
    (jlsl/fn :float "foo" []
      (return (equal 1 2)))
    "you must use the operator form for scalars")

  )

(deftest "vector constructors"
  (test-function
    (jlsl/fn :float "foo" []
      (return [1 2])
      (return [1 2 3])
      (return [1 2 3 4])
      (return (vec 1 2 3 4))
      (return (vec (. [1 2 3] xy) 3 4))
      ) `
    float foo() {
      return vec2(1.0, 2.0);
      return vec3(1.0, 2.0, 3.0);
      return vec4(1.0, 2.0, 3.0, 4.0);
      return vec4(1.0, 2.0, 3.0, 4.0);
      return vec4(vec3(1.0, 2.0, 3.0).xy, 3.0, 4.0);
    }
  `)
  (test-error
    (jlsl/fn :float "foo" []
      (return [1]))
    "vector constructor needs at least two components")
  (test-error
    (jlsl/fn :float "foo" []
      (return []))
    "vec needs at least 1 arguments but you gave it 0")
  )
