(use judge)
(use ./util)
(use ./types)
(use ./expr)

(defmacro- jlsl/stub [return-type name param-sigs]
  ~(,multifunction/single ,name ,(type/of-ast return-type) [,;(map param-sig/of-ast param-sigs)]))

# declare returns a multifunction
(defmacro jlsl/declare [return-type name param-sigs]
  ['def name (call jlsl/stub return-type (string name) param-sigs)])

# implement takes a multifunction and returns a single function
(defmacro jlsl/implement [return-type name params & body]
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
(defmacro jlsl/fn [return-type name params & body]
  (call jlsl/implement return-type (call jlsl/stub return-type name (map 0 (partition 2 params))) params ;body))

# defn defines a multifunction but returns a single function
(defmacro jlsl/defn [return-type name params & body]
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
