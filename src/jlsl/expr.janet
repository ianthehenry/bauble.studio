(use judge)
(use module)
(import pat)
(use ./util)
(use ./types)
(import ./builtins)

(defmodule expr
  (defn- call [general-function args]
    (expr/call (multifunction/resolve-function general-function (tmap expr/type args)) args))

  (defn of-ast [ast]
    (pat/match ast
      |keyword? [expr/literal ['quote type/int] ast]
      |boolean? [expr/literal ['quote type/bool] ast]
      |number? [expr/literal ['quote type/float] ast]
      |symbol? [expr/identifier ast]
      |btuple? [call ~',builtins/vec (map of-ast ast)]
      ['. expr field] [expr/dot (of-ast expr) ['quote field]]
      ['in expr index] [expr/in (of-ast expr) (of-ast index)]
      [(and op (or '++ '-- '_++ '_--)) expr] [expr/crement ['quote op] (of-ast expr)]
      # TODO
      # ['if cond then else]
      [f & args] [call f (map of-ast args)]
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
      [(or 'def 'var) &] (errorf "illegal form %q" ast)
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
      ['if cond then & else] (do
        (assert (<= (length else) 1) "too many arguments to if")
        (def else (get else 0))
        [statement/if (expr/of-ast cond) (of-ast then) (if else (of-ast else))])
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
