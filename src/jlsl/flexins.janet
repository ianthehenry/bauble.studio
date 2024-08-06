(use judge)
(use ./util)
(use ./types)
(use ./type)
(use ./expr)
(import ./builtins)
(use ./builtin-test-helpers)

# "flexins" are functions that *either* behave as builtins, if
# invoked with expression arguments, or behave as regular Janet
# functions, if invoked without any.
#
# however the janet versions are not overloaded, such that
# e.g. (sin [1 2 3]) does not work

(defmacro- defflex [sym &opt alt]
  (default alt sym)
  ~(def ,sym (multifunction/register-wrapper (fn [& args]
    # TODO: this isn't really right. Consider
    # (+ 1 [1 2 p.x]). The tuple isn't an expression,
    # but it contains one. Similarly variables
    # aren't expressions, but they could be.
    ((if (,some ,expr? args) ,(symbol "builtins/" sym) ,alt) ;args))
    ,(symbol "builtins/" sym))))

(defn- vector-length [vec] (math/sqrt (reduce (fn [acc v] (+ acc (* v v))) 0 vec)))

(defflex +)
(defflex -)
(defflex *)
(defflex /)
(defflex <)
(defflex >)
(defflex >=)
(defflex <=)
(defflex =)
(defflex not=)
(defflex max)
(defflex min)
(defflex mod)

(def @length length)
(defflex length vector-length)

(defn- non-flexible []
  (seq [[key entry] :pairs (require "./builtins")
        :when (and (symbol? key) (table? entry))
        :unless (in entry :private)
        :unless (table/rawget (curenv) key)]
    key))

(eval (seq [sym :in (non-flexible)
            :let [math-equivalent (symbol "math/" sym)]
            :when (has-key? (curenv) math-equivalent)]
  (call defflex sym math-equivalent)))

(deftest "builtins without flexin equivalents"
  (test (sort (seq [key :in (non-flexible)
                    :unless (string/has-prefix? "mat" key)
                    :unless (string/has-prefix? "vec" key)]
    (if (or (has-key? (curenv) key) (has-key? (curenv) (symbol "math/" key)))
      key
      [key])))
    @[[clamp]
      [cross]
      [distance]
      [dot]
      [equal]
      [faceforward]
      [fma]
      [fract]
      [inversesqrt]
      [mix]
      [normalize]
      [not-equal]
      [reflect]
      [refract]
      [resolve-refract]
      [roundEven]
      [sign]
      [smoothstep]
      [step]]))

(defn- float [x] (expr/literal type/float x))

(test (sin 0) 0)
(typecheck (sin (float 0)) [:float -> :float])
(test (+ 1 2) 3)
(typecheck (+ (float 1) 2) [:float :float -> :float])

(test (length [3 4]) 5)
(test (@length [1 2 3]) 3)

(def- foo (variable/new "name" type/vec2))
(typecheck (+ (float 1) [1 foo 3]) [:float :vec4 -> :vec4])
