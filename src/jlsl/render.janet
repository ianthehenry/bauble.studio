(use judge)
(use module)
(import pat)
(use ./util)
(import ../glsl)
(use ./expr)
(use ./types)
(use ./dsl)

(def- *glsl-identifier-map* (gensym))
(def- *glsl-function-name-map* (gensym))

(defn resolve-glsl-identifier [variable] # and *glsl-identifier-map*
  (or (bimap/in (dyn *glsl-identifier-map*) variable)
    (errorf "BUG: variable %q is not in scope. This shouldn't happen, but it happened. How did it happen?"
      variable)))

(defn resolve-glsl-function-name-aux [function] # and *glsl-function-name-map*
  (function/match function
    (builtin name _ _) (symbol name)
    (custom _) (bimap/in (dyn *glsl-function-name-map*) function)))

(defn resolve-glsl-function-name [function] # and *glsl-function-name-map*
  (or (resolve-glsl-function-name-aux function)
    (errorf "BUG: function %q is not in scope. This shouldn't happen, but it happened. How did it happen?"
      function)))

(defn- allocate-glsl-identifier [variable] # and *glsl-identifier-map*
  (def identifier-map (dyn *glsl-identifier-map*))
  (var identifier (symbol (variable/name variable)))
  # there might be a more efficient way to do this, but who cares
  (var i 1)
  (while (bimap/has-value? identifier-map identifier)
    (set identifier (symbol (variable/name variable) i))
    (++ i))
  (bimap/put identifier-map variable identifier))

(defn- collides-with-builtin? [name]
  # TODO: it's illegal to define a GLSL function with the same name
  # as a builtin function. we should detect that and generate something
  # else
  false)

(defn- allocate-glsl-function-name [function] # and *glsl-function-name-map*
  (def function-name-map (dyn *glsl-function-name-map*))
  (def base-name
    (if (collides-with-builtin? (function/name function))
      (string (function/name function) "_")
      (function/name function)))
  (var function-name (symbol base-name))

  # there might be a more efficient way to do this, but who cares
  (var i 1)
  (while (bimap/has-value? function-name-map function-name)
    (set function-name (symbol base-name i))
    (++ i))
  (bimap/put function-name-map function function-name))

(defn- allocate-or-resolve-glsl-function-name [function] # and *glsl-function-name-map*
  (or (resolve-glsl-function-name-aux function) (allocate-glsl-function-name function)))

(defn- render/expr [t] # and *glsl-identifier-map*
  (expr/match t
    (literal _ value) value
    (identifier variable) (resolve-glsl-identifier variable)
    (call function args)
      [(resolve-glsl-function-name function)
        ;(map render/expr args)
        ;(map |(render/expr (expr/identifier (param/var $))) (function/implicit-params function))]
    (dot expr field) ['. (render/expr expr) field]
    (in expr index) ['in (render/expr expr) (render/expr index)]
    (crement op expr) [op (render/expr expr)]))

(defn inherit-scope [] (bimap/new (dyn *glsl-identifier-map*)))
(defn new-scope [] (bimap/new))

(defmacro- subscope [& exprs]
  ~(with-dyns [',*glsl-identifier-map* (,inherit-scope)]
    ,;exprs))

(defn- render/statement [t] # and *glsl-identifier-map*
  (statement/match t
    (declaration const? variable expr) (do
      # render before we allocate the identifier
      (def rendered-expr (render/expr expr))
      [(if const? 'def 'var)
        (type/to-glsl (variable/type variable))
        (allocate-glsl-identifier variable)
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
          [statement] [(render/statement statement)]
          [value statement] [(render/expr value) (render/statement statement)]))]
    (while cond body) (subscope ['while (render/expr cond) ;(map render/statement body)])
    (do-while cond body) ['do-while (render/expr cond) ;(subscope (map render/statement body))]
    (for init cond update body)
      (subscope
        ['for (render/statement init) (render/expr cond) (render/statement update) ;(map render/statement body)])
    (expr expr) (render/expr expr))
  )

(defn- render/param [param] # and *glsl-identifier-map*
  [(param-sig/to-glsl (param/sig param)) (allocate-glsl-identifier (param/var param))])

(defn render/function [root-function &opt root-variables]
  (assert (function? root-function) "render/function called with a non-function. did you forget to resolve a multifunction?")
  (default root-variables [])
  (def forwards @{})
  (def results @[])
  (def in-progress @{})
  (def finished @{})

  (visit root-function (fn [node visiting? stack]
    (unless (function? node) (break))

    (when visiting?
      # we don't need a forward declaration for a direct recursive call
      (unless (= node (find-last function? stack))
        (put forwards node true)
        (allocate-glsl-function-name node))
      (break))

    # We inherit in the root function so that it has uniforms, inputs, and outputs
    # in scope already. But for non-root functions, we're going to forward uniforms
    # etc. as normal implicit arguments. We could revisit this at some points in
    # the future, but I expect that GLSL compilers are smart enough that it doesn't
    # make a difference.
    (def function-scope (if (= node root-function) (inherit-scope) (new-scope)))
    (def root-variable-set (tabseq [variable :in root-variables] variable true))

    (function/match node
      (builtin _ _ _) nil
      (custom {:name name :declared-return-type return-type :params params :body body})
        (with-dyns [*glsl-identifier-map* function-scope]
          (assertf (not (empty? body)) "%s: unimplemented function" name)
          (def implicit-params
            (if (= node root-function)
              (filter |(not (has-key? root-variable-set (param/var $)))
                (function/implicit-params node))
              (function/implicit-params node)))

          # we might have already allocated an identifier due to a forward declaration
          (def glsl-name (allocate-or-resolve-glsl-function-name node))
          (def glsl ~(defn ,(type/to-glsl return-type) ,glsl-name [,;(mapcat render/param [;params ;implicit-params])]
            ,;(map render/statement body)))
          (array/push results glsl)))))

  (array/concat
    (seq [function :keys forwards]
      (function/match function
        (builtin _ _ _) (error "BUG: cannot forward-declare a builtin function")
        (custom {:name name :declared-return-type return-type :params params})
          (with-dyns [*glsl-identifier-map* (bimap/new)]
            ~(defn ,(type/to-glsl return-type) ,name [,;(mapcat render/param params)]))))
    results))

(defn- validate-program [program]
  (def {:main main :inputs inputs :outputs outputs :uniforms uniforms} program)
  (def return-type (function/return-type main))
  (def implicit-params (function/implicit-params main))
  (type/match return-type
    (void) nil
    (errorf "main must return void" return-type))

  (each param implicit-params
    (var variable (param/var param))
    (match (param/access param)
      :in (unless (or (has-value? inputs variable) (has-value? uniforms variable))
        (errorf "main reads from a free variable %s which is not declared as an input or uniform. Did you forget to set a dynamic variable?" (variable/name variable)))
      :out (unless (has-value? outputs variable)
        (errorf "main writes to free variable %s which is not declared as an output" (variable/name variable)))
      :inout (errorf "main contains a free variable %s which is read and written to" (variable/name variable))
      x (errorf "BUG: illegal access type %q" x)))
  program)

(defmodule program
  (defmacro new [& body]
    (def $uniforms (gensym))
    (def $inputs (gensym))
    (def $outputs (gensym))
    (def $precisions (gensym))

    (var defined-main? false)

    (def <statements>
      (seq [form :in body]
        (pat/match form
          ['defn _ 'main &] (set defined-main? true)
          nil)
        (pat/match form
          ['uniform type name] ~(,array/push ,$uniforms (def ,name (,variable/new ,(string name) ,(type/of-ast type))))
          ['in type name] ~(,array/push ,$inputs (def ,name (,variable/new ,(string name) ,(type/of-ast type))))
          ['out type name] ~(,array/push ,$outputs (def ,name (,variable/new ,(string name) ,(type/of-ast type))))
          ['defn & args] ~(as-macro ,jlsl/defn ,;args)
          ['declare & args] ~(as-macro ,jlsl/declare ,;args)
          ['implement & args] ~(as-macro ,jlsl/implement ,;args)
          ['unquote & args] args
          ['precision &] ~(,array/push ,$precisions ',form))))
    (unless defined-main?
      (error "program with no main function"))
    ~(do
      (def ,$uniforms @[])
      (def ,$inputs @[])
      (def ,$outputs @[])
      (def ,$precisions @[])
      ,;<statements>
      (,validate-program
        {:uniforms ,$uniforms
         :inputs ,$inputs
         :outputs ,$outputs
         :precisions ,$precisions
         :main (,multifunction/resolve-function main [])}))))

(defmacro declare [thing]
  # TODO: allocate a decent name for this uniform??
  (with-syms [$var]
    ~(fn [,$var] [',thing (,type/to-glsl (,variable/type ,$var)) (,allocate-glsl-identifier ,$var)])))

(defn render/program [{:precisions precisions :uniforms uniforms :inputs inputs :outputs outputs :main main}]
  (def global-scope (bimap/new))
  (def root-variables (array/concat @[] uniforms inputs outputs))
  (with-dyns [*glsl-identifier-map* (new-scope)
              *glsl-function-name-map* (bimap/new)]
    [;precisions
     ;(map (declare in) inputs)
     ;(map (declare out) outputs)
     ;(map (declare uniform) uniforms)
     ;(render/function main root-variables)
    ]))

(use ./builtins)
(use ./flexins)

(test-macro (program/new
  (uniform :float t)
  (defn :void main []
    (return 10)))
  (do
    (def <1> @[])
    (def <2> @[])
    (def <3> @[])
    (def <4> @[])
    (@array/push <1> (def t (@new "t" (@type/primitive (quote (<5> float))))))
    (as-macro @jlsl/defn :void main [] (return 10))
    (@validate-program {:inputs <2> :main (@resolve-function main []) :outputs <3> :precisions <4> :uniforms <1>})))

(deftest "various sorts of illegal programs"
  (test-error (program/new
    (defn :void main [:float x]
      (return 10)))
    "main: no overload for arguments []")
  (test-error (macex '(program/new))
    "program with no main function")
  (test-error (program/new
    (defn :float main []
      (return 10)))
    "main must return void")
  (def free (variable/new "foo" type/float))
  (test-error (program/new
    (defn :void main []
      (return free)))
    "main reads from a free variable foo which is not declared as an input or uniform. Did you forget to set a dynamic variable?")

  (test-error (program/new
    (out :float x)
    (defn :void main []
      (return x)))
    "main reads from a free variable x which is not declared as an input or uniform. Did you forget to set a dynamic variable?")

  (test-error (program/new
    (uniform :float x)
    (defn :void main []
      (+= x 10)
      (return x)))
    "main contains a free variable x which is read and written to")

  (test-error (program/new
    (uniform :float x)
    (defn :void main []
      (set x 10)
      (return 1)))
    "main writes to free variable x which is not declared as an output")

  (test-error (program/new
    (in :float x)
    (defn :void main []
      (+= x 10)
      (return x)))
    "main contains a free variable x which is read and written to")

  (test-error (program/new
    (in :float x)
    (defn :void main []
      (set x 10)
      (return 1)))
    "main writes to free variable x which is not declared as an output")

  )

(test (render/program (program/new
  (uniform :float t)
  (defn :void main []
    (return 10))))
  [[uniform :float t]
   [defn :void main [] [return 10]]])

(test (render/program (program/new
  (uniform :float t)
  (in :float t)
  (out :float t)
  (defn :void main []
    (return 10))))
  [[in :float t]
   [out :float t1]
   [uniform :float t2]
   [defn :void main [] [return 10]]])

(defmacro*- test-function [expr & results]
  ~(test-stdout (,prin (,glsl/render-program
    (as-macro ,with-dyns [',*glsl-identifier-map* (,new-scope)
                          ',*glsl-function-name-map* (,bimap/new)]
      (,render/function ,expr)))) ,;results))

(defmacro*- test-program [expr & results]
  ~(test-stdout (,prin (,glsl/render-program (,render/program ,(call program/new ;expr)))) ,;results))

(deftest "trivial program"
  (test-program [
    (uniform :float t)
    (defn :void main []
      (return 10))
    ] `
    uniform float t;
    
    void main() {
      return 10.0;
    }
  `))

(deftest "you can reference uniforms or input/output variables in main without them counting as free variables"
  (test-program [
    (uniform :float t)
    (defn :void main []
      (return t))
    ] `
    uniform float t;
    
    void main() {
      return t;
    }
  `))

(deftest "uniforms are forwarded as normal free variables to calling functions"
  (test-program [
    (uniform :float t)
    (defn :void foo [:float x]
      (return (+ x t)))
    (defn :void main []
      (return (foo 10)))
    ] `
    uniform float t;
    
    void foo(float x, float t) {
      return x + t;
    }
    
    void main() {
      return foo(10.0, t);
    }
  `))

(deftest "with can be used to shadow uniforms"
  (test-program [
    (uniform :float t)
    (defn :void foo [:float x]
      (return (+ x t)))
    (defn :void main []
      (with [t 100]
        (return (foo 10))
        ))
    ] `
    uniform float t;
    
    void foo(float x, float t) {
      return x + t;
    }
    
    void main() {
      {
        float t1 = 100.0;
        return foo(10.0, t1);
      }
    }
  `))

(deftest "setting uniforms or inputs is disallowed even across function calls"
  (test-error (program/new
    (uniform :float t)
    (defn :void foo [:float x]
      (set t 10)
      (return x))
    (defn :void main []
      (return (foo 10))))
    "main writes to free variable t which is not declared as an output"))

# TODO: I think that maybe we should just disallow setting dynamic variables altogether
(deftest "you can set uniforms if you've shadowed them using with"
  (test-program [
    (uniform :float t)
    (defn :void foo [:float x]
      (set t 10)
      (return x))
    (defn :void main []
      (with [t 100]
        (return (foo 10))
        ))
    ] `
    uniform float t;
    
    void foo(float x, out float t) {
      t = 10.0;
      return x;
    }
    
    void main() {
      {
        float t1 = 100.0;
        return foo(10.0, t1);
      }
    }
  `))

(deftest "uniforms with the same name get unique lexical identifiers"
  (test-program [
    (uniform :float t)
    (defn :float foo [:float x]
      (return (+ x t)))
    (uniform :float t)
    (defn :void main []
      (return (+ t (foo 10))))
    ] `
    uniform float t;
    uniform float t1;
    
    float foo(float x, float t) {
      return x + t;
    }
    
    void main() {
      return t1 + foo(10.0, t);
    }
  `))

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

(deftest "dynamic variables are only set during a with call"
  (def p (variable/new "p" type/vec3))
  (test-function
    (jlsl/defn :float main []
      (with [p [0 0 0]]
        (return p))
      (return p)) `
    float main(vec3 p) {
      {
        vec3 p1 = vec3(0.0, 0.0, 0.0);
        return p1;
      }
      return p;
    }
  `))

# this test is here so that we don't naively remove the creation of a new subscope in with.
# we have to do something more complicated and distinguish "logical" and "physical" scopes
# when we're rendering a function
(deftest "with reserves symbols in the current glsl lexical scope even though it does not create a block"
  (def p (variable/new "p" type/vec3))
  (test-function
    (jlsl/defn :float main []
      (with [p [0 0 0]]
        (return p))
      (with [p [0 0 0]]
        (return p))
      (return p)) `
    float main(vec3 p) {
      {
        vec3 p1 = vec3(0.0, 0.0, 0.0);
        return p1;
      }
      {
        vec3 p1 = vec3(0.0, 0.0, 0.0);
        return p1;
      }
      return p;
    }
  `))

(deftest "functions with the same name are allocated unique identifiers"
  (test-function (do
    (jlsl/defn :float foo []
      (return 1))
    (def foo1 foo)
    (jlsl/defn :float foo [] (return (foo1)))) `
    float foo() {
      return 1.0;
    }
    
    float foo1() {
      return foo();
    }
  `))

(deftest "do expressions"
  (test-function
    (jlsl/defn :float main []
      (return (do (var x 10) (+= x 5) x))) `
    float do() {
      float x = 10.0;
      x += 5.0;
      return x;
    }
    
    float main() {
      return do();
    }
  `)

  (test-function
    (jlsl/defn :float main []
      (var x 10)
      (return (do (+= x 5) x))) `
    float do(inout float x) {
      x += 5.0;
      return x;
    }
    
    float main() {
      float x = 10.0;
      return do(x);
    }
  `))

(deftest "single expression do does not produce an iife"
  (test-function
    (jlsl/defn :float main []
      (var x 10)
      (return (do x))) `
    float main() {
      float x = 10.0;
      return x;
    }
  `))

(deftest "with expressions"
  (jlsl/defdyn q :vec2)
  (test-function (do
    (jlsl/defn :float circle [:float r]
      (return (- (length q) r)))

    (jlsl/defn :float distance []
      (return (with [q (- q [10 20])] (circle 10)))

      (return (with [q (- q [10 20])]
        (min (circle 10) (circle 20))))

      (var x 0)
      (return (with [q (- q [10 20])]
        (set x 1)
        (circle 20)))
      )) `
    float circle(float r, vec2 q) {
      return length(q) - r;
    }
    
    float with_outer(vec2 q) {
      {
        vec2 q1 = q - vec2(10.0, 20.0);
        return circle(10.0, q1);
      }
    }
    
    float with_outer1(vec2 q) {
      {
        vec2 q1 = q - vec2(10.0, 20.0);
        return min(circle(10.0, q1), circle(20.0, q1));
      }
    }
    
    float with_inner(vec2 q, out float x) {
      x = 1.0;
      return circle(20.0, q);
    }
    
    float with_outer2(vec2 q, out float x) {
      {
        vec2 q1 = q - vec2(10.0, 20.0);
        return with_inner(q1, x);
      }
    }
    
    float distance(vec2 q) {
      return with_outer(q);
      return with_outer1(q);
      float x = 0.0;
      return with_outer2(q, x);
    }
  `))

(deftest "case statement"
  (test-function
    (jlsl/defn :float main []
      (case 1
        2 (return 1)
        (return 2))) `
    float main() {
      switch (2.0) {
      case return(1.0): return 2.0;
      }
    }
  `))

(deftest "do expressions restrict control flow"
  (test-error
    (jlsl/defn :float main []
      (return (do (var x 10) (return 1) (+= x 5) x)))
    "cannot return out of a do expression")
  (test-error
    (jlsl/defn :float main []
      (return (do (break) 1)))
    "cannot break out of a do expression")
  (test-error
    (jlsl/defn :float main []
      (return (do (continue) 1)))
    "cannot continue out of a do expression")
  (test-function
    (jlsl/defn :float main []
      (return (do (case 1 2 (break)) 1))) `
    float do() {
      switch (2.0) {
      default: break;
      }
      return 1.0;
    }
    
    float main() {
      return do();
    }
  `)
  (test-error
    (jlsl/defn :float main []
      (return (do (case 1 2 (continue)) 1)))
    "cannot continue out of a do expression")
  (test-function
    (jlsl/defn :float main []
      (return (do (while true (break)) 1))) `
    float do() {
      while (true) {
        break;
      }
      return 1.0;
    }
    
    float main() {
      return do();
    }
  `)
  (test-function
    (jlsl/defn :float main []
      (return (do (do-while true (continue)) 1))) `
    float do() {
      do {
        continue;
      } while (true);
      return 1.0;
    }
    
    float main() {
      return do();
    }
  `)
  )

(deftest "do expressions can set variables"
  (test-function
    (jlsl/defn :float main []
      (var x 10)
      (return (do (+= x 1) x))) `
    float do(inout float x) {
      x += 1.0;
      return x;
    }
    
    float main() {
      float x = 10.0;
      return do(x);
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

(deftest "if statements"
  (test-function
    (jlsl/fn :float "sign" [:float x]
      (if (= x 0)
        (return 0))
      (if (< x 0)
        (return -1)
        (return 1))) `
    float sign(float x) {
      if (x == 0.0) return 0.0;
      if (x < 0.0) return -1.0;
      else return 1.0;
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
      (return (+ 1 [0 0 0]))
      (return (< 1 2))) `
    float foo() {
      return vec3(0.0, 0.0, 0.0) == vec3(1.0, 2.0, 3.0);
      return equal(vec3(0.0, 0.0, 0.0), vec3(1.0, 2.0, 3.0));
      return equal(vec3(0.0, 0.0, 0.0), vec3(1.0, 2.0, 3.0));
      return notEqual(vec3(0.0, 0.0, 0.0), vec3(1.0, 2.0, 3.0));
      return lessThan(vec3(0.0, 0.0, 0.0), vec3(1.0, 2.0, 3.0));
      return 1.0 + vec3(0.0, 0.0, 0.0);
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
