(use judge)
(use module)
(import pat)
(use ./util)
(use ./types)
(use ./builtins)

(defmodule expr
  (defn- call [general-function args]
    (expr/call (multifunction/resolve-function general-function (tmap expr/type args)) args))

  # TODO: get rid of this
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
