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

Some builtins have more general overloads than in native GLSL. For example, the `jlsl` expression `(< x y)` compiles to either the `x < y` (a `bool`) or `lessThan(x, y)` (a `bvec`) depending on the type of its arguments.

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

```janet
# TODO: it would be better to show the compiled GLSL code here
(defn :float box [:vec3 p :vec3 size]
  (var :vec3 q (- (abs p) size))
  (return (+ (length (max q 0)) (min (max q.x (max q.y q.z)) 0))))

(defn :float distance [:vec3 p]
  (var p1 (- p (vec3 100 0 0)))
  (return (box p (vec3 50 100 50))))
```

# `do` expressions

`jlsl` supports `do` expressions, which are compiled to immediately-invoked function expressions. For example:

```janet
(jlsl/defn :float main []
  (return (do (var x 10) (+= x 5) x)))
```

Produces:

```glsl
float do() {
  float x = 10.0;
  x += 5.0;
  return x;
}

float main() {
  return do();
}
```

Because of the existence of `out` and `inout` parameters, and `jlsl`'s notion of dynamic scope, you can capture and even modify variables inside `do` expressions. For example, this slight variation:

```janet
(jlsl/defn :float main []
  (var x 10)
  (return (do (+= x 5) x)))
```

Produces the following code:

```glsl
float do(inout float x) {
  x += 5.0;
  return x;
}

float main() {
  float x = 10.0;
  return do(x);
}
```

# `with` expressions

Similarly, `jlsl` supports `with` expressions, which behave much like `with` statements wrapped in `do`. These two formulations produce equivalent code:

```janet
(defn :float distance []
  (with [p (- p [100 0 0])]
    (return (box (vec3 50 100 50)))))

(defn :float distance []
  (return
    (with [p (- p [100 0 0])]
      (box (vec3 50 100 50)))))
```

# builtin function name resolution

So there's this slighty awkward thing where some names like `+` and `length` are both Janet and GLSL functions. We shadow these functions with functions that first check their arguments to see if you're calling them with any `jlsl` expressions (or `jlsl` variables, which are implicitly converted to `jlsl` identifier expressions). If so, the result is a `jlsl` expression. Otherwise, the function is normal Janet function is invoked.

So for example, `(+ 1 2)` will return `3`, but `(+ 1 p)` will return an expression.

## beware: `length`

The function called `length` defined in `jlsl/builtins` and `jlsl/flexins` is equivalent to the GLSL `length` function that returns the Euclidean of a vector length, *not* the Janet length function that returns the number of elements in a data structure. If you want to count elements, use `@length`, which is an alias for Janet's builtin `length` function.

## beware: `and` and `or`

The `jlsl/flexins` module defines `and` and `or` *functions* that shadow Janet's native `and` and `or` *macros*. This means that they are not short-circuiting! If you want to use `and` and `or` for control flow, use `@and` and `@or`, which are aliases for the native macros.

# future work

`jlsl` associates types with every expression, and it uses those types for resolving function overloads. It could do a lot more, though, and statically typecheck your entire program. But it doesn't really do that yet.
