(use judge)
(use module)
(import pat)
(use ./util)

# this is a very primitive helper for defining sum types.
# it provides a macro that allows you to match on all
# cases of a sum type exhaustively, and will check that you
# have the right numbers of arguments to each constructor.
#
# but it only allows one pattern per constructor. so e.g. this is
# not allowed:
#
# (defadt result
#  (ok value)
#  (error error))
#
# (result/match
#   (ok 0)
#   (ok other)
#   (error foo))
#
# maybe one day we will learn about proper exhaustivity. but it is
# not this day.

# TODO: defadt-

(defn- defadt-helper [private? name cases]
  (def unique-type-tag (gensym))
  (def prefix (string name "/"))

  (def cases (tabseq [[constructor & args] :in cases] constructor args))

  (defn match-macro [value branches]
    (def cases-missing (table/clone cases))

    (def cases-seen @{})
    (var allow-non-exhaustive-match? false)

    (defn fix-pattern [pattern-ast]
      (assertf (and
        (tuple? pattern-ast)
        (= (tuple/type pattern-ast) :parens)
        (symbol? (first pattern-ast)))
        "%q is not a valid ADT pattern" pattern-ast)
      (def [constructor & args] pattern-ast)

      (when (in cases-seen constructor)
        (errorf "duplicate constructor %q" constructor))

      (if-let [expected-args (in cases-missing constructor)]
        (assertf (= (length expected-args) (length args))
          "wrong number of arguments to constructor %q, expected %d got %d"
          constructor
          (length expected-args)
          (length args))
        (errorf "unknown constructor %q" constructor))

      (put cases-seen constructor true)
      (put cases-missing constructor nil)

      ~[_ ',constructor ,;args])

    (def pat-branches
      (catseq [branch :in (partition 2 branches)]
        (case (length branch)
          1 (do (set allow-non-exhaustive-match? true) branch)
          2 (let [[pattern body] branch] [(fix-pattern pattern) body])
          (error "impossible"))))

    (unless (or allow-non-exhaustive-match? (= (div (length branches) 2) (length cases)))
      (error "non-exhaustive match"))

    (with-syms [$value]
      ~(let [,$value ,value]
        (,assertf (and (tuple? ,$value) (= (first ,$value) ',unique-type-tag)) "%q is not a %s" ,$value ',name)
        (as-macro ,pat/match ,$value
          ,;pat-branches))))

  (def $defn (if private? 'defn- 'defn))
  (def $defmacro (if private? 'defmacro- 'defmacro))

  (with-syms [$value $branches]
    ~(upscope
      (,$defn ,(symbol name "?") [value]
        (and (tuple? value) (= (first value) ',unique-type-tag)))
      (,$defmacro ,(symbol prefix "match") [value & branches]
        (,match-macro value branches))
      ,;(catseq [[constructor args] :pairs cases]
        [~(,$defn ,(symbol prefix constructor) [,;args]
          [',unique-type-tag ',constructor ,;args])
         ~(,$defmacro ,(symbol "@" prefix constructor) [,;args]
          ['quote (,tuple/brackets '',unique-type-tag '',constructor ,;args)])]))))

(defmacro defadt- [name & cases] (defadt-helper true name cases))
(defmacro defadt [name & cases] (defadt-helper false name cases))

(test-macro (defadt result
  (ok value)
  (error value etc))
  (upscope
    (defn result?
      [value]
      (and (tuple? value) (= (first value) (quote <1>))))
    (defmacro result/match
      [value & branches]
      (@match-macro value branches))
    (defn result/error
      [value etc]
      [(quote <1>) (quote error) value etc])
    (defmacro @result/error
      [value etc]
      [(quote quote) (@tuple/brackets (quote (quote <1>)) (quote (quote error)) value etc)])
    (defn result/ok
      [value]
      [(quote <1>) (quote ok) value])
    (defmacro @result/ok
      [value]
      [(quote quote) (@tuple/brackets (quote (quote <1>)) (quote (quote ok)) value)])))

(defadt- result
  (ok value)
  (error value))

(test-macro (@result/ok x)
  (quote [(quote <1>) (quote ok) x]))

(defn- gensymy? [x]
  (and (symbol? x) (string/has-prefix? "_" x)))

(test (gensymy? (gensym)) true)
(test (gensymy? 'x) false)

(defn- ungensym [structure] (prewalk |(if (gensymy? $) '<> $) structure))

(test (ungensym (@result/ok x))
  [[quote <>] [quote ok] x])

(test-stdout
  (result/match (result/ok 123)
    (error _) "oh no"
    (ok x) (printf "ok %q" x)) `
  ok 123
`)

(test-stdout
  (pat/match (result/ok 123)
    ,(@result/error _) "oh no"
    ,(@result/ok x) (printf "ok %q" x)) `
  ok 123
`)

(test (result? (result/ok 123)) true)
(test (result? [:ok 123]) false)
