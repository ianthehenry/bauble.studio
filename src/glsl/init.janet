(use judge)
(use module)
(import pat)
(use ./util)

(defmodule- printer
  (defn new []
    @{:indent 0
      :indented false
      :buf (buffer/new 256)})

  (defn indent [t]
    (repeat (t :indent)
      (buffer/push-string (t :buf) "  "))
    (put t :indented true))

  (defn newline [t]
    (buffer/push-byte (t :buf) (chr "\n"))
    (put t :indented false))

  (defn prin [t & things]
    (unless (t :indented) (indent t))
    (each thing things
      (buffer/push-string (t :buf) thing)))

  (defn print [t & things]
    (prin t ;things)
    (newline t))

  (defn to-string [t]
    (string (t :buf)))

  (defn indent-body* [t f]
    (++ (t :indent))
    (f)
    (-- (t :indent)))

  (defmacro indent-body [t & body]
    ~(,indent-body* ,t (fn [] ,;body)))

  (defmacro bracket-body [t start end & body]
    ~(do
      (,print ,t ,start)
      (,indent-body* ,t (fn [] ,;body))
      (,prin ,t ,end))))

(deftest "printer works"
  (def p (printer/new))
  (printer/print p "hello")
  (printer/print p "start " "{")
  (printer/indent-body p
    (printer/print p "one")
    (printer/print p "two"))
  (printer/print p "}")
  (test-stdout (print (printer/to-string p)) `
    hello
    start {
      one
      two
    }
    
  `))

(deftest "bracketing works"
  (def p (printer/new))
  (printer/print p "hello")
  (printer/prin p "start")
  (printer/bracket-body p " {" "} "
    (printer/print p "one")
    (printer/print p "two"))
  (printer/print p "hello")
  (test-stdout (print (printer/to-string p)) `
    hello
    start {
      one
      two
    } hello
    
  `))

(defn identifier [name]
  (string/replace-all "-" "_" name))

(var render-statement nil)
(var render-expression nil)

(defn- to-float [num]
  (def str (string num))
  (if (string/find "." str) str (string str ".0")))

(varfn render-expression [p expression &named needs-parens?]
  (default needs-parens? false)
  (pat/match expression
    [(and op (or '++ '--)) expr] (do
      (printer/prin p op)
      (render-expression p expr :needs-parens? true))
    [(and op (or '_++ '_--)) expr] (do
      (render-expression p expr :needs-parens? true)
      (printer/prin p (string/slice op 1)))
    [(and op (or '- 'not 'bnot)) expr] (do
      (def op (case op
        'not "!"
        'bnot "~"
        op))
      (printer/prin p op)
      (render-expression p expr :needs-parens? true))
    ['/ expr] (render-expression p ~(/ 1 ,expr) :needs-parens? needs-parens?)
    [(and op (or '+ '/ '- '* '%
      'and 'or 'xor
      'blshift 'brshift 'band 'bor 'bxor
      '< '> '<= '>=
      '= 'not=)) & args] (do
      (def op (case op
        'blshift "<<"
        'brshift ">>"
        'band "&"
        'bor "|"
        'bxor "^"
        'and "&&"
        'or "||"
        'xor "^^"
        'not= "!="
        '= "=="
        op))
      (wrap-when needs-parens?
        (printer/prin p "(")
        (each-last arg args
          (render-expression p arg :needs-parens? true)
          (printer/prin p " " op " "))
        (printer/prin p ")")))
    ['if cond then else] (do
      (wrap-when needs-parens?
        (printer/prin p "(")
        (render-expression p cond :needs-parens? true)
        (printer/prin p " ? ")
        (render-expression p then :needs-parens? true)
        (printer/prin p " : ")
        (render-expression p else :needs-parens? true)
        (printer/prin p ")")))
    ['in expr key] (do
      (render-expression p expr)
      (printer/prin p "[")
      (render-expression p key)
      (printer/prin p "]"))
    ['. expr key] (do
      (render-expression p expr :needs-parens? true)
      (printer/prin p "." (identifier key)))
    [f & args] (do
      (printer/prin p (identifier f) "(")
      (each-last arg args
        (render-expression p arg)
        (printer/prin p ", "))
      (printer/prin p ")"))
    |symbol? (printer/prin p (identifier expression))
    |keyword? (printer/prin p expression)
    |boolean? (printer/prin p (string expression))
    |number? (printer/prin p (to-float expression))))

(varfn render-statement [p statement &named newline-after-block? one-line? semicolon?]
  (default newline-after-block? true)
  (default semicolon? true)
  (default one-line? false)
  (var semicolon? semicolon?)
  (pat/match statement
    ['def type name value] (do
      (printer/prin p "const " type " " (identifier name) " = ")
      (render-expression p value))
    ['var type name] (do
      (printer/prin p type " " (identifier name)))
    ['var type name value] (do
      (printer/prin p type " " (identifier name) " = ")
      (render-expression p value))
    ['set dest value] (do
      (render-expression p dest)
      (printer/prin p " = ")
      (render-expression p value))
    ['return value] (do
      (printer/prin p "return ")
      (render-expression p value))
    ['break] (printer/prin p "break")
    ['continue] (printer/prin p "continue")
    [(and op (or
      '+= '*= '/= '-= '%=
      'blshift= 'brshift=
      'bxor= 'band= 'bor=)) dest expr] (do
      (def op (case op
        'bxor= "^="
        'band= "&="
        'bor= "|="
        'brshift= ">>="
        'blshift= "<<="
        op))
      (render-expression p dest)
      (printer/prin p " " op " ")
      (render-expression p expr))
    ['do & statements] (do
      (printer/bracket-body p "{" "}"
        (each statement statements
          (render-statement p statement)))
      (when newline-after-block? (printer/newline p))
      (set semicolon? false))
    ['if cond then & else] (do
      (assert (<= (length else) 1) "too many arguments to if")
      (printer/prin p "if (")
      (render-expression p cond)
      (printer/prin p ") ")
      (render-statement p then :newline-after-block? (empty? else))
      (unless (empty? else)
        (printer/prin p " else ")
        (render-statement p (first else)))
      (set semicolon? false))
    ['case value & cases] (do
      (printer/prin p "switch (")
      (render-expression p value)
      (printer/print p ") {")
      (each case (partition 2 cases)
        (def body (match (length case)
          1 (do (printer/prin p "default: ") (first case))
          2 (let [[value body] case]
            (printer/prin p "case ")
            (render-expression p value)
            (printer/prin p ": ")
            body)
          (error "impossible")))
        (render-statement p body))
      (printer/print p "}")
      (set semicolon? false))
    ['while cond & body] (do
      (printer/prin p "while (")
      (render-expression p cond)
      (printer/prin p ") ")
      (render-statement p ~(do ,;body))
      (set semicolon? false))
    ['do-while cond & body] (do
      (printer/prin p "do ")
      (render-statement p ~(do ,;body) :newline-after-block? false)
      (printer/prin p " while (")
      (render-expression p cond)
      (printer/prin p ")"))
    ['for init check advance & body] (do
      (printer/prin p "for (")
      (render-statement p init :one-line? true)
      (printer/prin p " ")
      (render-expression p check)
      (printer/prin p "; ")
      (render-statement p advance :semicolon? false)
      (printer/prin p ") ")
      (render-statement p ~(do ,;body))
      (set semicolon? false))
    (render-expression p statement))
  (when semicolon?
    ((if one-line? printer/prin printer/print) p ";")))

(defn- format-args [args]
  (->
    (seq [[type arg] :in (partition 2 args)]
      (string/join [;(string/split ":" type) (identifier arg)] " "))
    (string/join ", ")))

(test (format-args [:foo 'name :bar 'other]) "foo name, bar other")

(defn render-toplevel [p toplevel]
  (pat/match toplevel
    ['struct name & fields] (do
      (printer/prin p "struct " (identifier name))
      (printer/bracket-body p " {" "}"
        (each field (partition 2 fields)
          (render-statement p ~(var ,;field))))
      (printer/newline p))
    ['defn type name args & body] (do
      (printer/prin p type " " (identifier name) "(" (format-args args) ")")
      (if (empty? body)
        (printer/print p ";")
        (do
          (printer/bracket-body p " {" "}"
            (each statement body
              (render-statement p statement)))
          (printer/newline p))))
    ['def type name value] (do
      (printer/prin p "const " type " " (identifier name) " = ")
      (render-expression p value)
      (printer/print p ";"))
    [(and (or 'uniform 'out 'in) decl) type name] (printer/print p decl " " type " " (identifier name) ";")
    ['precision & rest] (printer/print p "precision " ;(intercalate rest " ") ";")))

(defn render-program [program &opt version]
  (def p (printer/new))
  (when version
    (printer/print p "#version " version))
  (var last-head nil)
  (each toplevel program
    (var new-head (first toplevel))
    # group forward declarations together
    (when (and (= new-head 'defn) (= (length toplevel) 4))
      (set new-head :forward-declaration))

    (when (and last-head
        (or (= new-head 'defn)
        (not= last-head new-head)))
      (printer/newline p))

    (render-toplevel p toplevel)

    (set last-head new-head))
  (printer/to-string p))

(defn uniforms [program]
  (def uniforms @{})
  (each toplevel program
    (match toplevel
      ['uniform type name] (put uniforms (identifier name) type)))
  uniforms)

(defmacro*- test-program [program & args]
  ~(test-stdout (print (,render-program ',program)) ,;args))

(defmacro*- test-program* [program & args]
  ~(test-stdout (print (,render-program ,program)) ,;args))

(defmacro*- test-statements [statements & args]
  ~(test-program [(defn :void main [] ,;statements)] ,;args))

(deftest "post- and pre-crements"
  (test-statements [
    (++ i)
    (_++ i)
    (-- i)
    (_-- i)
    ] `
    void main() {
      ++i;
      i++;
      --i;
      i--;
    }
    
  `))

(deftest "out parameters"
  (test-program [
    (defn :void hello [out:float foo]
      (set foo 10))
    ] `
    void hello(out float foo) {
      foo = 10.0;
    }
    
  `))

(deftest "dot syntax"
  (test-statements [
    (. foo xyy)
    (. (+ (vec2 1 2) (vec2 3 4)) xyy)
    ] `
    void main() {
      foo.xyy;
      (vec2(1.0, 2.0) + vec2(3.0, 4.0)).xyy;
    }
    
  `))

(deftest "ternary conditional expressions"
  (test-statements [
    (def :float x (if (> y 10) 10 20))
    (def :float x (if (> y 10) (+ 5 5) (+ 10 10)))
    (def :float x (if (> y 10) (if (> y 20) 1 2) 3))
    (def :float x (if (> y 10) 1 (if (> y 20) 2 3)))
    (def :float x (if (if (> y 10) false true) (if (> y 20) 1 2) 3))
    ] `
    void main() {
      const float x = (y > 10.0) ? 10.0 : 20.0;
      const float x = (y > 10.0) ? (5.0 + 5.0) : (10.0 + 10.0);
      const float x = (y > 10.0) ? ((y > 20.0) ? 1.0 : 2.0) : 3.0;
      const float x = (y > 10.0) ? 1.0 : ((y > 20.0) ? 2.0 : 3.0);
      const float x = ((y > 10.0) ? false : true) ? ((y > 20.0) ? 1.0 : 2.0) : 3.0;
    }
    
  `))

(deftest "array access"
  (test-statements [
    (def :float x (in lights :0))
    (def :float x (in lights (+ i :1)))
    ] `
    void main() {
      const float x = lights[0];
      const float x = lights[i + 1];
    }
    
  `))

(deftest "field access"
  (test-statements [
    (def :float x (. foo xyz))
    (def :float x (. (f) xyz))
    (def :float x (. (. (in lights :0) incidence) brightness))
    ] `
    void main() {
      const float x = foo.xyz;
      const float x = f().xyz;
      const float x = lights[0].incidence.brightness;
    }
    
  `))

(deftest "loops"
  (test-statements [
    (while (> i 10) (-- i))
    (do-while (> i 10) (-- i))
    (for (var :int i :0) (< i :10) (++ i) (f))
    ] `
    void main() {
      while (i > 10.0) {
        --i;
      }
      do {
        --i;
      } while (i > 10.0);
      for (int i = 0; i < 10; ++i) {
        f();
      }
    }
    
  `))

(deftest "struct declaration"
  (test-program [
    (struct LightIncidence
      :vec3 direction
      :vec3 color)
    ] `
    struct LightIncidence {
      vec3 direction;
      vec3 color;
    }
    
  `))

(deftest "if formats correctly with and without else"
  (test-statements [
    (if x (set y 10))
    (set x 10)
    (if x (do (set y 10)))
    (set x 10)
    (if x (do (set y 10)) (set y 20))
    (set x 10)
    (if x (do (set y 10)) (do (set y 20)))
    (set x 10)
    ] `
    void main() {
      if (x) y = 10.0;
      x = 10.0;
      if (x) {
        y = 10.0;
      }
      x = 10.0;
      if (x) {
        y = 10.0;
      } else y = 20.0;
      x = 10.0;
      if (x) {
        y = 10.0;
      } else {
        y = 20.0;
      }
      x = 10.0;
    }
    
  `))

(deftest "switch indentation"
  (test-statements [
    (case x
      10 (do (f) (continue) (g))
      20 (h)
      30 (do (f) (g) (break))
      (do (h)))
    ] `
    void main() {
      switch (x) {
      case 10.0: {
        f();
        continue;
        g();
      }
      case 20.0: h();
      case 30.0: {
        f();
        g();
        break;
      }
      default: {
        h();
      }
      }
    }
    
  `))

(deftest "assignment to arbitrary expression"
  (test-statements [
    (set (. foo x) 10)
    (set (in foo :0) 10)
    (+= (in foo :0) 10)
    (*= (f foo) 10)
    ] `
    void main() {
      foo.x = 10.0;
      foo[0] = 10.0;
      foo[0] += 10.0;
      f(foo) *= 10.0;
    }
    
  `))

(deftest "forward function declarations"
  (test-program [
    (defn :float foo [:float x])
    (defn :float bar [:float x])
    (defn :float foo [:float x]
      (return (+ (bar x) 1)))
    (defn :float bar [:float x]
      (return (+ (foo x) 1)))
    ] `
    float foo(float x);
    float bar(float x);
    
    float foo(float x) {
      return bar(x) + 1.0;
    }
    
    float bar(float x) {
      return foo(x) + 1.0;
    }
    
  `))

(def example-program ~[
  (precision highp float)
  (uniform :vec3 camera-origin)
  (uniform :vec3 camera-rotation)
  (uniform float t)
  (uniform vec4 viewport)
  (out vec4 frag_color)

  (def :int MAX_STEPS :64)
  (def :float MINIMUM_HIT_DISTANCE 0.1)
  (def :float NORMAL_OFFSET 0.005)
  (def :float MAXIMUM_TRACE_DISTANCE (* 64 1024))

  (def :float PI 3.14159265359)

  (defn :mat3 rotate-x [:float angle]
    (def :float s (sin angle))
    (def :float c (cos angle))
    (return (mat3
      1 0 0
      0 c (- s)
      0 s c)))

  (defn :float s3d-ellipsoid [:vec3 p :vec3 size]
    (def :float k0 (length (/ p size)))
    (def :float k1 (length (/ p (* size size))))
    (return (/ (* k0 (- k0 1)) k1)))

  (defn :void main []
    (def :float gamma 2.2)

    (def :vec2 local_coord (- gl_FragCoord.xy viewport.xy))
    (def :vec2 resolution viewport.zw)
    (def :vec3 dir (*
      (rotate_x camera_rotation.x)
      (rotate_y camera_rotation.y)
      (rotate_z camera_rotation.z)
      (perspective 27.0 resolution local_coord)))
    (def :vec3 hit (march camera-origin dir))
    (var :vec3 color)
    (def :float depth (distance camera-origin hit))
    (var :float alpha 1)
    (if (>= depth MAXIMUM_TRACE_DISTANCE) (do
      (def :vec3 light (pow (/ (vec3 69 72 79) (vec3 255)) (vec3 gamma)))
      (def :vec3 dark (pow (/ (vec3 40 42 46) (vec3 255)) (vec3 gamma)))
      (set color (vec3 (mix dark light (/ (+ local_coord.x local_coord.y) (+ resolution.x resolution.y))))))
      (do
        (set color (nearest-color hit))))

    (if (or (>= depth MAXIMUM_TRACE_DISTANCE) (< (+ hit.x hit.z) 10.0)) (do
      (def :float r (length (- (/ local-coord resolution) (vec2 0.5))))
      (if (> r 0.45) (do
        (set color (vec3 0.6 0.8 0.6))
        (set alpha 1)))
      (if (> r 0.47) (do
        (set alpha 0)))))
    (set frag-color (vec4 (pow color (vec3 (/ gamma))) alpha)))
  ])

(test (uniforms example-program)
  @{"camera_origin" :vec3
    "camera_rotation" :vec3
    "t" float
    "viewport" vec4})

(test-program* example-program `
  precision highp float;
  
  uniform vec3 camera_origin;
  uniform vec3 camera_rotation;
  uniform float t;
  uniform vec4 viewport;
  
  out vec4 frag_color;
  
  const int MAX_STEPS = 64;
  const float MINIMUM_HIT_DISTANCE = 0.1;
  const float NORMAL_OFFSET = 0.005;
  const float MAXIMUM_TRACE_DISTANCE = 64.0 * 1024.0;
  const float PI = 3.14159265359;
  
  mat3 rotate_x(float angle) {
    const float s = sin(angle);
    const float c = cos(angle);
    return mat3(1.0, 0.0, 0.0, 0.0, c, -s, 0.0, s, c);
  }
  
  float s3d_ellipsoid(vec3 p, vec3 size) {
    const float k0 = length(p / size);
    const float k1 = length(p / (size * size));
    return (k0 * (k0 - 1.0)) / k1;
  }
  
  void main() {
    const float gamma = 2.2;
    const vec2 local_coord = gl_FragCoord.xy - viewport.xy;
    const vec2 resolution = viewport.zw;
    const vec3 dir = rotate_x(camera_rotation.x) * rotate_y(camera_rotation.y) * rotate_z(camera_rotation.z) * perspective(27.0, resolution, local_coord);
    const vec3 hit = march(camera_origin, dir);
    vec3 color;
    const float depth = distance(camera_origin, hit);
    float alpha = 1.0;
    if (depth >= MAXIMUM_TRACE_DISTANCE) {
      const vec3 light = pow(vec3(69.0, 72.0, 79.0) / vec3(255.0), vec3(gamma));
      const vec3 dark = pow(vec3(40.0, 42.0, 46.0) / vec3(255.0), vec3(gamma));
      color = vec3(mix(dark, light, (local_coord.x + local_coord.y) / (resolution.x + resolution.y)));
    } else {
      color = nearest_color(hit);
    }
    if ((depth >= MAXIMUM_TRACE_DISTANCE) || ((hit.x + hit.z) < 10.0)) {
      const float r = length((local_coord / resolution) - vec2(0.5));
      if (r > 0.45) {
        color = vec3(0.6, 0.8, 0.6);
        alpha = 1.0;
      }
      if (r > 0.47) {
        alpha = 0.0;
      }
    }
    frag_color = vec4(pow(color, vec3(1.0 / gamma)), alpha);
  }
  
`)
