(use judge)
(use ./util)
(use ./types)
(use ./type)
(use ./expr)
(import ./builtins)
(use ./builtin-test-helpers)

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
(defflex sin math/sin)
(defflex cos math/cos)

(defn- float [x] (expr/literal type/float x))

(test (sin 0) 0)
(typecheck (sin (float 0)) [:float -> :float])
(test (+ 1 2) 3)
(typecheck (+ (float 1) 2) [:float :float -> :float])

(def- foo (variable/new "name" type/vec2))
(typecheck (+ (float 1) [1 foo 3]) [:float :vec4 -> :vec4])
