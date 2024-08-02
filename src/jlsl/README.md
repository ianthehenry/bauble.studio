# `jlsl`

A higher-level language for generating GLSL expressions in Janet.

If you're writing raw `glsl` code, you might have code like this:

```janet
(defn :float square [:float x]
  (return (pow x x)))

(defn :void main []
  (set frag-color (vec3 (square 0.5))))
```

But `'square` is just a symbol. `glsl` attaches no meaning to any of its symbols. There's nothing stopping you from writing:

```janet
(defn :float square [:float x]
  (return (pow x 2)))

(defn :void main []
  (set frag-color (vec3 (cube 0.5))))
```

And referencing functions that don't actually exist.

`jlsl`, meanwhile, has a notion of scope. `(jlsl/defn square ...)` is a macro that creates a Janet binding called `square`. When you later write something like:

```janet
(jlsl/defn main []
  (set frag-color (vec3 (square 0.5))))
```

`defn` will actually lookup and evaluate the symbol `square` in the current environment.

Similarly,

```janet
(defn :float square [:float x]
  (return (pow x 2)))
```

In that expression, `x` is a lexically-scoped variable. This not only makes it harder to make typos, but it also allows us to track the types of identifiers. `jlsl` uses that type to do some simple typechecking and type-driven overloading.

# vector punning

In `jlsl`, you can use square brackets as shorthand for the `vec*` functions. For example:

```janet
[1 0.5 1 1]
# becomes
vec4(1 0.5 1 1)
```

And:

```janet
[p.xy z]
# becomes
(vec3 p.xy z)
```

It uses type-directed disambiguation to know that you wanted the result to be a `vec3`. (This is syntax sugar for `(vec 1 2 3)`, which will compile to one of the vector constructors depending on what you pass it.)

# overloads

Some builtins have more general overloads than in native GLSL. For example, the JLSL expression `(< x y)` compiles to either the `x < y` (a `bool`) or `lessThan(x, y)` (a `bvec`) depending on the type of its arguments.

`(= x y)` and `(not= x y)` are not overloaded this way; you need to explicitly use `(equal x y)` and `(not-equal x y)` if you want the `bvec` forms (because it's perfectly reasonable to compare vectors for equality).

Additionally `pow([1 2 3], 4)` compiles to `pow(vec3(1.0, 2.0, 3.0), vec3(4.0))`.

# dynamic variables

The most important addition to `jlsl` is the existence of dynamic variables. GLSL doesn't have any native concept of a dynamic variable, so `jlsl` translates dynamic variables into implicitly-passed function arguments. For example:

```janet
(defn :float box [:vec3 size]
  (var :vec3 q (- (abs p) size))
  (return (+ (length (max q 0)) (min (max q.x (max q.y q.z)) 0))))

(defn :float distance []
  (return (box (vec3 50 100 50))))
```

Notice a reference to the symbol `p` in the `box` function, which is not an argument to the function. If `p` is a dynamic variable, then when you compile those functions, it will actually produce `glsl` that looks like this:

```janet
(defn :float box [:vec3 p :vec3 size]
  (var :vec3 q (- (abs p) size))
  (return (+ (length (max q 0)) (min (max q.x (max q.y q.z)) 0))))

(defn :float distance [:vec3 p]
  (return (box p (vec3 50 100 50))))
```

Any invocation of a function that references dynamic variables will automatically forward those dynamic variables along.

You can also *modify* dynamic variables using `with`. For example:

```janet
(defn :float box [:vec3 size]
  (var :vec3 q (- (abs p) size))
  (return (+ (length (max q 0)) (min (max q.x (max q.y q.z)) 0))))

(defn :float distance []
  (with [p (- p [100 0 0])]
    (return (box (vec3 50 100 50)))))

# TODO: is `with` an expression or a statement? which is easier for me? probably it should be a statement...?
(defn :float distance []
  (return
    (with [p (- p [100 0 0])]
      (box (vec3 50 100 50)))))
```

That will produce code like this:

```
(defn :float box [:vec3 p :vec3 size]
  (var :vec3 q (- (abs p) size))
  (return (+ (length (max q 0)) (min (max q.x (max q.y q.z)) 0))))

(defn :float distance [:vec3 p]
  (var p1 (- p (vec3 100 0 0)))
  (return (box p (vec3 50 100 50))))
```

# builtin function name resolution

So there's this slighty awkward thing where some names like `+` and `length` are both Janet and GLSL functions. But we don't want to shadow Janet's built-in `length` with a GLSL alternative, so when Janet resolves function identifiers, it first checks if it's the name of a builtin, and only if it's not will JLSL look for an identifier with that name in scope. So, for example:

```
(defn :float length [:vec3 v]
  (return (pow (sum (pow v 3)) (/ 3))))

(defn :float foo []
  (return (length [1 2 3])))
```

Even though the Janet identifier `length` is bound to a JLSL function in this case, the identifier resolution process resolves the symbol `length` to the GLSL builtin and doesn't even look at the lexical binding of the `length` identifier.

The only workaround here is to pick a different name for the custom function.

(It would be *possible* to make an identifier that can behave as both a native Janet callable and a GLSL function by defining an abstract type, but that would require a native Janet module, and `jlsl` wants to remain in pure Janet.)

# future work

`jlsl` associates types with every expression, and it uses those types for resolving function overloads. It could do a lot more, though, and statically typecheck your entire program. But it doesn't really do that yet.
