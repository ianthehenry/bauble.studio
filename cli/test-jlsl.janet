(import cmd)
(import ../src/jlsl)
(import jaylib)
(use ../src/jlsl/builtins)
(use ../src/jlsl/flexins)
(use ./helpers)

(cmd/defn test-jlsl "testing" []
  (init-jaylib)

  (def MAX_STEPS :64)
  (def MINIMUM_HIT_DISTANCE 0.1)
  (def NORMAL_OFFSET 0.005)
  (def MAXIMUM_TRACE_DISTANCE (* 64 1024))
  (def PI 3.14159265359)
  (def DEG_TO_RAD (/ PI 180))

  (jlsl/jlsl/defdyn p :vec3)

  (def program (jlsl/render/program (jlsl/program/new
    (precision highp float)
    (uniform :vec3 camera-origin)
    (uniform :vec3 camera-orientation)
    (uniform :float t)
    (uniform :vec4 viewport)
    (out :vec4 frag-color)

    # TODO: i think this should be available implicitly
    (in :vec4 gl_FragCoord)

    (defn :mat3 rotate-x [:float angle]
      (var s (sin angle))
      (var c (cos angle))
      (return (mat3
        1 0 0
        0 c (- s)
        0 s c)))

    (defn :mat3 rotate-y [:float angle]
      (var s (sin angle))
      (var c (cos angle))
      (return (mat3
        c 0 s
        0 1 0
        (- s) 0 c)))

    (defn :mat3 rotate-z [:float angle]
      (var s (sin angle))
      (var c (cos angle))
      (return (mat3
        c (- s) 0
        s c 0
        0 0 1)))

    (defn :float s3d-ellipsoid [:vec3 size]
      (var k0 (length (/ p size)))
      (var k1 (length (/ p (* size size))))
      (return (/ (* k0 (- k0 1)) k1)))

    (defn :float moved []
      (with [p (- p [50 0 -50])]
        (return (s3d-ellipsoid [50 50 100]))))

    (defn :float nearest-distance []
      (return (min (moved) (s3d-ellipsoid [50 50 100]))))

    (defn :vec3 march [:vec3 ray-origin :vec3 ray-direction]
      (var distance 0)

      (for (var steps :0) (< steps MAX_STEPS) (++ steps)
        (with [p (+ ray-origin (* distance ray-direction))]
          (var nearest (nearest-distance))

          (if (or (< nearest MINIMUM_HIT_DISTANCE) (> distance MAXIMUM_TRACE_DISTANCE))
            (return (+ p (* nearest ray-direction))))

          (+= distance nearest)))
      (return (+ ray-origin (* distance ray-direction))))

    (defn :vec3 perspective [:float fov :vec2 size :vec2 pos]
      (var xy (- pos (* size 0.5)))

      (var cot-half-fov (tan (* (- 90 (* fov 0.5)) DEG_TO_RAD)))
      (var z (* (min (. size x) (. size y)) 0.5 cot-half-fov))

      (return (normalize [xy (- z)])))

    (defn :vec3 calculate-normal []
      (def step (vec2 NORMAL_OFFSET 0))
      (var x (nearest-distance))
      (var offset (vec3 0))
      (with [p (- p (. step xyy))] (set (. offset x) (nearest-distance)))
      (with [p (- p (. step yxy))] (set (. offset y) (nearest-distance)))
      (with [p (- p (. step yyx))] (set (. offset z) (nearest-distance)))
      (return (normalize (- (vec3 x) offset))))

    (defn :vec3 nearest-color []
      (var normal (calculate-normal))
      (return (+ 0.5 (* 0.5 normal))))

    (defn :void main []
      (def gamma 2.2)

      (var local-coord (- (. gl_FragCoord xy) (. viewport xy)))
      (var resolution (. viewport zw))
      (var dir (*
        (rotate-z (. camera-orientation z))
        (rotate-y (. camera-orientation y))
        (rotate-x (. camera-orientation x))
        (perspective 45.0 resolution local-coord)))
      (var hit (march camera-origin dir))
      (var color [0 0 0]) # todo: this should actually be unassigned
      (var depth (distance camera-origin hit))
      (var alpha 1)
      (if (>= depth MAXIMUM_TRACE_DISTANCE) (do
        (def light (pow (/ [69 72 79] 255) (vec3 gamma)))
        (def dark (pow (/ [40 42 46] 255) (vec3 gamma)))
        (set color (vec3 (mix dark light (/ (+ (. local-coord x) (. local-coord y))
          (+ (. resolution x) (. resolution y)))))))
        (with [p hit] (set color (nearest-color))))

      (set frag-color (vec4 (pow color (vec3 (/ gamma))) alpha)))
    )))
  (setdyn *verbose* true)
  (def shader (shader/new program))
  (def image (shader/render shader [512 512]
    :camera-origin [256 362 256]
    :camera-orientation [(* 0.125 math/pi 2) (* -0.125 math/pi 2) 0]
    :t 0))
  (jaylib/export-image image "test-jlsl.png"))
