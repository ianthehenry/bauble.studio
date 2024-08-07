(import jaylib)
(import cmd)
(use module)
(import ./ray)
(import ../src :as bauble)
(import ../src/glsl)
(use ./helpers)

(cmd/defn test-glsl "testing" []
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
  (jaylib/export-image image "test-glsl.png"))
