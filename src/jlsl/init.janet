(use judge)
(use module)
(import pat)
(use ./util)
(use ./adt)

(defadt function
  (builtin name return-type arg-types)
  (defined name return-type args body))

(defmacro defbuiltin [sym return-type & arg-types]
  ~(def ,(symbol "builtins/" sym)
    (,function/builtin ',sym ,return-type ,(tuple/brackets ;arg-types))))

(defbuiltin + :float :float :float)

(defmodule function
  (defn of-ast [sym]
    (case sym
      '+ ['quote builtins/+]
      sym))

  (defn to-glsl [t]
    (function/match t
      (builtin name _ _) name
      (defined name _ _ _) name))

  (defn return-type [t]
    (function/match t
      (builtin _ type _) type
      (defined _ type _ _) type)))

(defadt variable
  (dynamic name type)
  (lexical name type))

(defmodule variable
  (defn to-glsl [t]
    (variable/match t
      (dynamic name _) name
      (lexical name _) name))
  (defn type [t]
    (variable/match t
      (dynamic _ type) type
      (lexical _ type) type)))

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

(defadt expr
  (literal type value)
  (identifier variable)
  (call function args))

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
      # [(and op (or '++ '--)) expr]
      # [(and op (or '_++ '_--)) expr]
      # ['if cond then else]
      # ['in expr key]
      # ['. expr key]
      )))

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

(defmacro- jlsl/defn [return-type name args & body]
  (def $return-type (gensym))
  (def $args (gensym))
  (def $body (gensym))

  (def <args> (seq [[type name] :in (partition 2 args)]
    ~(def ,name (,variable/lexical ,(string name) ,(type/of-ast type)))))

  (def <body> (seq [statement-ast :in body]
    ~(,array/push ,$body ,(statement/of-ast statement-ast))))

  ~(do
    (def ,$return-type ,(type/of-ast return-type))
    (def ,$args [,;<args>])
    (def ,$body @[])
    ,;<body>
    (,function/defined ,(string name) ,$return-type ,$args ,$body)))

(test-macro (jlsl/defn :void foo [:float x :float y]
  (var x 1)
  (return [x 2 3]))
  (do
    (def <1> (quote (<2> void)))
    (def <3> [(def x (@variable/lexical "x" (@type/primitive (quote (<4> float))))) (def y (@variable/lexical "y" (@type/primitive (quote (<4> float)))))])
    (def <5> @[])
    (@array/push <5> (upscope (def <6> (@expr/literal (quote (<2> primitive (<4> float))) 1)) (def <7> (@type <6>)) (def x (@variable/lexical "x" <7>)) (@statement/declaration false x <6>)))
    (@array/push <5> (@statement/return (@vector (@expr/identifier x) (@expr/literal (quote (<2> primitive (<4> float))) 2) (@expr/literal (quote (<2> primitive (<4> float))) 3))))
    (@function/defined "foo" <1> <3> <5>)))

(test (jlsl/defn :void foo [:float x :float y]
  (var z 1)
  (return (+ x y z)))
  [<1>
   defined
   "foo"
   [<2> void]
   [[<3>
     lexical
     "x"
     [<2> primitive [<4> float]]]
    [<3>
     lexical
     "y"
     [<2> primitive [<4> float]]]]
   @[[<5>
      declaration
      false
      [<3>
       lexical
       "z"
       [<2> primitive [<4> float]]]
      [<6>
       literal
       [<2> primitive [<4> float]]
       1]]
     [<5>
      return
      [<6>
       call
       [<1> builtin + :float [:float :float]]
       @[[<6>
          identifier
          [<3>
           lexical
           "x"
           [<2> primitive [<4> float]]]]
         [<6>
          identifier
          [<3>
           lexical
           "y"
           [<2> primitive [<4> float]]]]
         [<6>
          identifier
          [<3>
           lexical
           "z"
           [<2> primitive [<4> float]]]]]]]]])

(test (jlsl/defn :void foo [:float x :float y]
  (var z 0)
  (for (var i 0) (< i 10) (++ i)
    (+= z i))
  (return (+ x y z)))
  [<1>
   defined
   "foo"
   [<2> void]
   [[<3>
     lexical
     "x"
     [<2> primitive [<4> float]]]
    [<3>
     lexical
     "y"
     [<2> primitive [<4> float]]]]
   @[[<5>
      declaration
      false
      [<3>
       lexical
       "z"
       [<2> primitive [<4> float]]]
      [<6>
       literal
       [<2> primitive [<4> float]]
       0]]
     [<5>
      for
      [<5>
       declaration
       false
       [<3>
        lexical
        "i"
        [<2> primitive [<4> float]]]
       [<6>
        literal
        [<2> primitive [<4> float]]
        0]]
      [<6>
       call
       @<
       @[[<6>
          identifier
          [<3>
           lexical
           "i"
           [<2> primitive [<4> float]]]]
         [<6>
          literal
          [<2> primitive [<4> float]]
          10]]]
      [<5>
       call
       @++
       @[[<6>
          identifier
          [<3>
           lexical
           "i"
           [<2> primitive [<4> float]]]]]]
      @[[<5>
         declaration
         nil
         [<3>
          lexical
          "z"
          [<2> primitive [<4> float]]]
         [<6>
          identifier
          [<3>
           lexical
           "i"
           [<2> primitive [<4> float]]]]]]]
     [<5>
      return
      [<6>
       call
       [<1> builtin + :float [:float :float]]
       @[[<6>
          identifier
          [<3>
           lexical
           "x"
           [<2> primitive [<4> float]]]]
         [<6>
          identifier
          [<3>
           lexical
           "y"
           [<2> primitive [<4> float]]]]
         [<6>
          identifier
          [<3>
           lexical
           "z"
           [<2> primitive [<4> float]]]]]]]]])
