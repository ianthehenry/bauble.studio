(import jaylib)
(import cmd)
(use module)
(import ./ray)
(import ../src :as bauble)
(import ../src/jlsl)

(defdyn *verbose*)
(setdyn *verbose* true)

(defn compile-shader [source]
  (let [[expr env] (bauble/bauble-evaluator/evaluate source)
        [animated? shader-source] (bauble/shade/compile-shape expr env "330")]
    shader-source))

(defn init-jaylib []
  (jaylib/set-trace-log-level :warning)
  (jaylib/set-config-flags :window-hidden)
  (jaylib/init-window 0 0 "Bauble"))

(defmodule shader
  (defn new [jlsl-program]
    (def shader-source (jlsl/render-program jlsl-program "330"))
    (when (dyn *verbose*)
      (eprint "```glsl")
      (eprint shader-source)
      (eprint "```"))

    (def shader (jaylib/load-shader-from-memory nil shader-source))
    (def uniforms (jlsl/uniforms jlsl-program))
    (def uniform-locations (tabseq [name :keys uniforms]
      name (jaylib/get-shader-location shader name)))
    (def unset-uniforms (tabseq [name :keys uniforms] name true))
    (var last-resolution nil)
    (var fbo nil)

    (defn set-uniform-values [& new-values]
      (each [name value] (partition 2 new-values)
        (def name (jlsl/identifier name))
        (match (in uniforms name)
          nil (errorf "unknown uniform %s" name)
          type (do
            (put unset-uniforms name nil)
            (jaylib/set-shader-value shader (in uniform-locations name) value type)))))

    (defn render [resolution & uniform-values]
      (when (not= resolution last-resolution)
        (set-uniform-values 'viewport [0 0 ;resolution])
        (when fbo (jaylib/unload-render-texture fbo))
        (set fbo (jaylib/load-render-texture ;resolution)))

      (set-uniform-values ;uniform-values)
      (unless (empty? unset-uniforms)
        (errorf "must set values for uniforms %s" (string/join (keys unset-uniforms) ", ")))

      (ray/do-texture fbo
        (ray/do-shader shader
          (jaylib/draw-rectangle-v [0 0] resolution :red)))
      (def image (jaylib/load-image-from-texture (jaylib/get-render-texture-texture2d fbo)))
      (jaylib/image-flip-vertical image)
      image)
    {:render render})

  (defn render [{:render render} & args]
    (render ;args))
  )

(defn render-image [shader-source &named resolution]
  (def frame-buffer (ray/make-fbo resolution :point))
  (def shader (jaylib/load-shader-from-memory nil shader-source))

  (defn set-uniform [name type value]
    (jaylib/set-shader-value shader (jaylib/get-shader-location shader name) value type))
  (defn set-uniform-matrix [name value]
    (jaylib/set-shader-value-matrix shader (jaylib/get-shader-location shader name) value))

  (set-uniform "camera_origin" :vec3 [256 362 256])
  (set-uniform "camera_orientation" :vec3 [(* 0.125 math/pi 2) (* -0.125 math/pi 2) 0])

  (set-uniform "t" :float 0)
  (set-uniform "viewport" :vec4 [0 0 ;resolution])
  (set-uniform "render_type" :int 0)

  (ray/do-texture frame-buffer
    (ray/do-shader shader
      (jaylib/draw-rectangle-v [0 0] resolution :red)))
  (def image (jaylib/load-image-from-texture (jaylib/get-render-texture-texture2d frame-buffer)))
  (jaylib/image-flip-vertical image)
  image)

(def arg/resolution (cmd/peg "WxH" ~(/ (* (number :d+) "x" (number :d+) -1) ,|[$0 $1])))

(cmd/defn render "render a bauble to a png"
  [infile (required :file)
   outfile (required :file)
   --resolution (optional arg/resolution [512 512]) "default 512x512"]
  (def shader-source (compile-shader (slurp infile)))
  (init-jaylib)
  (def image (render-image shader-source :resolution resolution))
  (jaylib/export-image image outfile))

(cmd/defn print-source "print fragment shader source to stdout"
  [infile (required :file)]
  (print (compile-shader (slurp infile))))

(cmd/defn jlsl-test "testing" []
  (init-jaylib)
  (def program ~[
    (precision highp float)
    (uniform :vec3 camera-origin)
    (uniform :vec3 camera-orientation)
    (uniform :float t)
    (uniform :vec4 viewport)
    (out :vec4 frag_color)

    (def :int MAX_STEPS :64)
    (def :float MINIMUM_HIT_DISTANCE 0.1)
    (def :float NORMAL_OFFSET 0.005)
    (def :float MAXIMUM_TRACE_DISTANCE (* 64 1024))

    (def :float PI 3.14159265359)

    (defn :mat3 rotate-x [:float angle]
      (var :float s (sin angle))
      (var :float c (cos angle))
      (return (mat3
        1 0 0
        0 c (- s)
        0 s c)))

    (defn :mat3 rotate-y [:float angle]
      (var :float s (sin angle))
      (var :float c (cos angle))
      (return (mat3
        c 0 s
        0 1 0
        (- s) 0 c)))

    (defn :mat3 rotate-z [:float angle]
      (var :float s (sin angle))
      (var :float c (cos angle))
      (return (mat3
        c (- s) 0
        s c 0
        0 0 1)))

    (defn :float s3d-ellipsoid [:vec3 p :vec3 size]
      (var :float k0 (length (/ p size)))
      (var :float k1 (length (/ p (* size size))))
      (return (/ (* k0 (- k0 1)) k1)))

    (defn :float nearest-distance [:vec3 p]
      (return (s3d-ellipsoid p (vec3 50 50 100))))

    (defn :vec3 march [:vec3 ray-origin :vec3 ray-direction]
      (var :float distance 0)

      (for (var :int steps :0) (< steps MAX_STEPS) (++ steps)
        (var :vec3 p (+ ray-origin (* distance ray-direction)))
        (var :float nearest (nearest-distance p))

        (if (or (< nearest MINIMUM_HIT_DISTANCE) (> distance MAXIMUM_TRACE_DISTANCE))
          (return (+ p (* nearest ray-direction))))

        (+= distance nearest))
      (return (+ ray-origin (* distance ray-direction))))

    (def :float DEG_TO_RAD (/ PI 180))
    (defn :vec3 perspective [:float fov :vec2 size :vec2 pos]
      (var :vec2 xy (- pos (* size 0.5)))

      (var :float cot-half-fov (tan (* (- 90 (* fov 0.5)) DEG_TO_RAD)))
      (var :float z (* (min size.x size.y) 0.5 cot-half-fov))

      (return (normalize (vec3 xy (- z)))))

    (defn :vec3 calculate-normal [:vec3 p]
      (def :vec2 step (vec2 NORMAL_OFFSET 0))
      (var :float x (nearest-distance p))

      (return (normalize (vec3
        (- x (nearest-distance (- p step.xyy)))
        (- x (nearest-distance (- p step.yxy)))
        (- x (nearest-distance (- p step.yyx)))))))

    (defn :vec3 nearest-color [:vec3 p]
      (var :vec3 normal (calculate-normal p))
      (return (+ 0.5 (* 0.5 normal))))

    (defn :void main []
      (def :float gamma 2.2)

      (var :vec2 local-coord (- gl_FragCoord.xy viewport.xy))
      (var :vec2 resolution viewport.zw)
      (var :vec3 dir (*
        (rotate-z camera-orientation.z)
        (rotate-y camera-orientation.y)
        (rotate-x camera-orientation.x)
        (perspective 45.0 resolution local-coord)))
      (var :vec3 hit (march camera-origin dir))
      (var :vec3 color)
      (var :float depth (distance camera-origin hit))
      (var :float alpha 1)
      (if (>= depth MAXIMUM_TRACE_DISTANCE) (do
        (def :vec3 light (pow (/ (vec3 69 72 79) (vec3 255)) (vec3 gamma)))
        (def :vec3 dark (pow (/ (vec3 40 42 46) (vec3 255)) (vec3 gamma)))
        (set color (vec3 (mix dark light (/ (+ local-coord.x local-coord.y) (+ resolution.x resolution.y))))))
        (do
          (set color (nearest-color hit))))

      (set frag-color (vec4 (pow color (vec3 (/ gamma))) alpha)))
    ])
  (def shader (shader/new program))
  (def image (shader/render shader [512 512]
    :camera-origin [256 362 256]
    :camera-orientation [(* 0.125 math/pi 2) (* -0.125 math/pi 2) 0]
    :t 0))
  (jaylib/export-image image "jlsl-test.png"))

(cmd/main (cmd/group
  render render
  jlsl-test jlsl-test
  compile print-source))
