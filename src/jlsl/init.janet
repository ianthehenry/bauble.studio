(use judge)
(use module)
(import pat)
(use ./util)
(use ./adt)

(defmacro deftype [name]
  (with-syms [$proto $args]
    ~(upscope
      (def- ,$proto {:type ',(gensym)})
      (defn ,(symbol name "/new") [& ,$args]
        (struct/with-proto ,$proto ;,$args))
      (defn ,(symbol name "?") [t]
        (and (struct? t) (= (struct/getproto t) ,$proto))))))

(defmacro getter [field]
  ~(defn ,field [t] (,in t ,(keyword field))))

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

  (defn return-type [t]
    (function/match t
      (builtin _ type _) type
      (defined _ type _ _) type)))

# TODO: is there actually any difference between lexical and dynamic variables? do i need two concepts?
(deftype dynamic-var)
(defmodule dynamic-var
  (defn new [name type] (dynamic-var/new :name name :type type))
  (getter name)
  (getter type))

(deftype lexical-var)
(defmodule lexical-var
  (def- proto @{})
  (defn new [name type] (lexical-var/new :name name :type type))
  (getter name)
  (getter type))

# TODO: this should be an ADT
(deftype type)
(defmodule type
  (defn prim [type] (type/new :tag :primitive :type type))

  (def float (prim :float))
  (def double (prim :double))
  (def int (prim :int))
  (def uint (prim :uint))
  (def bool (prim :bool))

  (defn of [type-or-keyword]
    (if (keyword? type-or-keyword)
      (prim type-or-keyword)
      type-or-keyword))

  (defn vec [type count]
    (assert (and (>= count 2) (<= count 4)) "vectors only come in size 2, 3, or 4")
    (type/new :tag :vector :type (of type) :count count))

  (defn struct [name fields]
    (type/new :tag :struct :name name :fields fields))

  (defn to-glsl [t]
    (pat/match t
      {:tag :primitive :type &} type
      {:tag :vector :type & :count &} (do
        (def vec (match type
          :float "vec"
          :bool "bvec"
          :double "dvec"
          :int "ivec"
          :uint "uvec"))
        (keyword vec count))
      {:tag :struct :name &} name))

  (defn components [t]
    (pat/match t
      {:tag :primitive} 1
      {:tag :vector :count &} count
      nil))

  (test (components float) 1)
  (test (components (vec :float 3)) 3)

  (defn base-type [t]
    # TODO: arrays, probably
    (pat/match t
      {:tag :primitive} t
      {:tag :vector :type &} type
      nil))

  (test (base-type float) {:tag :primitive :type :float})
  (test (= float (base-type float)) true)
  (test (base-type (vec :float 3)) {:tag :primitive :type :float})
  (test (= float (base-type (vec :float 3))) true))

(defadt expr
  (literal type value)
  (identifier variable)
  (call function args))

# TODO: no, we can't resolve it to glsl... we need to hold onto identifiers
# and variables and such. hrm.
(defmodule expr
  (defn type [t]
    (expr/match t
      (literal type _) type
      (identifier variable)
        # TODO: do we need to distinguish lexical and dynamic vars at all?
        (in variable :type)
      (call function _) (function/return-type function)))

  (defn vector [& exprs]
    (assert (not (empty? exprs)) "vector cannot be empty")
    (def base-type (get-unique (>> type type/base-type) exprs))
    (def components (sum (map (>> type type/components) exprs)))

    # TODO: there's some duplication here...
    (def constructor
      (pat/match base-type
        (= type/float) "vec"
        (= type/int) "ivec"
        (= type/bool) "bvec"
        (= type/double) "dvec"
        (= type/uint) "uvec"))
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
      |keyword? [expr/literal type/int expr-ast]
      |boolean? [expr/literal type/bool expr-ast]
      |number? [expr/literal type/float expr-ast]
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
  (do statements)
  (if cond then else)
  (case value cases)
  (while cond statements)
  (do-while cond statements)
  (for init cond update statements)
  (call function arguments))

# this takes the AST of a statement and returns code that
# creates a first-class statement
(defn statement/of-ast [statement-ast]
  (pat/match statement-ast
    # TODO: we have no way to declare a variable without any initial value
    [(map {'def true 'var false} const?) name value]
      (with-syms [$expr $type $statement]
        ~(upscope
          (def ,$expr ,(expr/of-ast value))
          (def ,$type (,expr/type ,$expr))
          (def ,name (,lexical-var/new ',name ,$type))
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
    ['do & statements]
      # TODO: accumulate the statements and evaluate them one after another
      (with-syms [$statements]
        ~(do
          (def ,$statements @[])
          ,;(seq [statement :in statements]
            [array/push $statements (statement/of-ast statement)])
          [statement/do ,$statements]))
    ['if cond then & else] [statement/if (expr/of-ast cond) (expr/of-ast then) (if else (expr/of-ast else))]
    ['case value & cases]
      [statement/case
        (expr/of-ast value)
        (tuple/brackets ;(seq [case :in (partition 2 cases)]
          (match (length case)
            1 (let [[body] case] (statement/of-ast body))
            2 (let [[value body] case] [(expr/of-ast value) (statement/of-ast body)])
            (error "impossible"))))]
    ['while cond & body]
      [statement/while (expr/of-ast cond) (map statement/of-ast body)]
    ['do-while cond & body]
      [statement/do-while (expr/of-ast cond) (map statement/of-ast body)]
    ['for init check advance & body]
      [statement/for (statement/of-ast init)
        (expr/of-ast check)
        (statement/of-ast advance)
        (map statement/of-ast body)]
    [function & args] [statement/call (function/of-ast function) (map expr/of-ast args)]
  ))

(defmacro- glsl/defn [return-type name args & body]
  (def args
    (seq [[type name] :in (partition 2 args)]
      (def type (type/of type))
      (assertf (type? type) "parameter %q needs a type, got %q" name type)
      (lexical-var/new name type)))

  # so something kinda funny about this is that
  # if you evaluate the returned `defn` multiple times,
  # each time it will actually append to the *same* statements array.
  # which is weird, and not what you really want.

  (def $statements (gensym))

  (def body (seq [statement-ast :in body]
    ~(,array/push ,$statements ,(statement/of-ast statement-ast))))

  ~(let [,$statements @[]
         ,;(catseq [arg :in args] [(arg :name) ~',arg])]
    ,;body
    ,$statements))

(test-macro (glsl/defn :void foo [:float x :float y]
  (var x 1)
  (return [x 2 3]))
  (let [<1> @[] x (quote {:name x :type {:tag :primitive :type :float}}) y (quote {:name y :type {:tag :primitive :type :float}})]
    (@array/push <1> (upscope (def <2> (@expr/literal {:tag :primitive :type :float} 1)) (def <3> (@type <2>)) (def x (@new (quote x) <3>)) (@statement/declaration false x <2>)))
    (@array/push <1> (@statement/return (@vector (@expr/identifier x) (@expr/literal {:tag :primitive :type :float} 2) (@expr/literal {:tag :primitive :type :float} 3))))
    <1>))

(test (glsl/defn :void foo [:float x :float y]
  (var z 1)
  (return (+ x y z)))
  @[[<1>
     declaration
     false
     {:name z
      :type {:tag :primitive :type :float}}
     [<2>
      literal
      {:tag :primitive :type :float}
      1]]
    [<1>
     return
     [<2>
      call
      [<3> builtin + :float [:float :float]]
      @[[<2>
         identifier
         {:name x
          :type {:tag :primitive :type :float}}]
        [<2>
         identifier
         {:name y
          :type {:tag :primitive :type :float}}]
        [<2>
         identifier
         {:name z
          :type {:tag :primitive :type :float}}]]]]])
