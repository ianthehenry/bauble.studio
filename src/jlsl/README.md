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

It uses type-directed disambiguation to know that you wanted the result to be a `vec3`.

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

# coming soon

`jlsl` also has a notion of typechecking. Except... that doesn't actually work yet.
