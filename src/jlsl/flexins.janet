(use judge)
(use ./util)
(use ./types)
(use ./type)
(use ./expr)
(import ./builtins)

# "flexins" are functions that *either* behave as builtins, if
# invoked with expression arguments, or behave as regular Janet
# functions, if invoked without any

(defmacro- defflex [sym &opt alt]
  (default alt sym)
  ~(def ,sym (multifunction/register-wrapper (fn [& args]
    # TODO: this isn't really right. Consider
    # (+ 1 [1 2 p.x]). The tuple isn't an expression,
    # but it contains one. Similarly variables
    # aren't expressions, but they could be.
    ((if (,some ,expr? args) ,(symbol "builtins/" sym) ,alt) ;args))
    ,(symbol "builtins/" sym))))

(defflex +)
(defflex -)
(defflex *)
(defflex /)
(defflex >)
(defflex >=)
(defflex <=)
(defflex =)
(defflex not=)
# TODO: the CPU version of this should be a different thing
(defflex length)

(test (+ 1 2) 3)
(test (+ (expr/literal type/float 1) 2)
  [<1>
   call
   [<2>
    builtin
    "+"
    [<3> primitive [<4> float]]
    @[[[<3> primitive [<4> float]] :in]
      [[<3> primitive [<4> float]] :in]]]
   @[[<1>
      literal
      [<3> primitive [<4> float]]
      1]
     [<1>
      literal
      [<3> primitive [<4> float]]
      2]]])

(def- foo (variable/new "name" type/vec2))
(test (+ (expr/literal type/float 1) [1 foo 3])
  [<1>
   call
   [<2>
    builtin
    "+"
    [<3> vec [<4> float] 4]
    @[[[<3> primitive [<4> float]] :in]
      [[<3> vec [<4> float] 4] :in]]]
   @[[<1>
      literal
      [<3> primitive [<4> float]]
      1]
     [<1>
      call
      [<2>
       builtin
       "vec4"
       [<3> vec [<4> float] 4]
       @[[[<3> primitive [<4> float]] :in]
         [[<3> vec [<4> float] 2] :in]
         [[<3> primitive [<4> float]] :in]]]
      @[[<1>
         literal
         [<3> primitive [<4> float]]
         1]
        [<1>
         identifier
         [<5>
          lexical
          <6>
          "name"
          [<3> vec [<4> float] 2]]]
        [<1>
         literal
         [<3> primitive [<4> float]]
         3]]]]])
