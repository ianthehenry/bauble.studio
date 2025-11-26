(def @= =)
(use ../environment)
(import ../../jlsl)
(use ./util)
(use judge)

(jlsl/jlsl/defdyn camera-type :int "")
(def free-camera-target-3d (jlsl/variable/new "free_camera_target" jlsl/type/vec3))
(def free-camera-target-2d (jlsl/variable/new "free_camera_target" jlsl/type/vec2))
(jlsl/jlsl/defdyn free-camera-orbit :vec2 "")
(jlsl/jlsl/defdyn free-camera-zoom :float "")
(jlsl/jlsl/defdyn crosshairs-3d :vec4 "")

# TODO: these should probably be somewhere else
(def- MAX_STEPS 64:u)
(def- MINIMUM_HIT_DISTANCE 0.1)
(def- MAXIMUM_HIT_DISTANCE 10)
(def- MAXIMUM_TRACE_DISTANCE (* 64 1024))

(def- base-zoom-distance 512)
(def- free-camera-fov 45)

# think of base zoom distance as the apothem of a triangle.
# we want to know the length of the opposite edge of this triangle.
#
#      -----
#  \   |   /
#   \  |  /
#    \ | /
#     \|/
#
# tan(45 / 2) = (x / base-zoom-distance)

(def- ortho-base-zoom-distance (sugar (* base-zoom-distance (math/tan (free-camera-fov / 360 * tau / 2)))))
(test ortho-base-zoom-distance 212.07734393502469)

(defmacro jlsl/fn [t name & rest]
  ~(as-macro ,sugar
    (as-macro ,jlsl/jlsl/fn ,t ,(string name) ,;rest)))

(defn make-sample-2d [options nearest-distance <background-color> <default-color> <color-field>]
  (def {:render-type render-type :crosshairs crosshairs} options)
  (jlsl/fn :vec4 sample []
    (with [Q (frag-coord * ortho-base-zoom-distance * free-camera-zoom + free-camera-target-2d)
           q Q
           dist (nearest-distance)
           gradient (calculate-gradient (nearest-distance))]

      ,(if (@and (@= render-type 0) <color-field>)
        (jlsl/statement
          (if (<= dist 0)
            (return [<color-field> 1])
            (return <background-color>)))
        (jlsl/statement
          (return [<default-color> 1])))

      )))

(defn- make-march [nearest-distance]
  (jlsl/fn :float march [[out :uint] steps]
    (var ray-depth 0)

    (for (set steps 0:u) (< steps MAX_STEPS) (++ steps)
      (with [depth ray-depth
             P (+ ray.origin (* ray-depth ray.direction))
             p P]
        (var dist (nearest-distance))
        # we could dynamically adjust the minimum_hit_distance when the ray
        # is farther away from the camera... it could speed up scenes with background
        # scenery for little cost, but might cause issues with reflections
        (if (or (and (>= dist 0) (< dist MINIMUM_HIT_DISTANCE)) (> ray-depth MAXIMUM_TRACE_DISTANCE))
          (return ray-depth))
        (var rate (if (> dist 0) 0.95 1.05))
        (+= ray-depth (* dist rate))
        (if (< ray-depth 0)
          (return 0))))
    (return ray-depth)))

# returns [point-on-axis distance-to-axis]
(sugar (jlsl/jlsl/defn :vec4 intersect-axis [:vec3 origin :vec3 axis Ray ray]
  (var A (mat2x3 ray.direction (- axis)))
  (var b (origin - ray.origin))
  (var At (transpose A))
  (var ts (At * A | inverse * At * b))
  (if (< ts.x 0) (return [0 0 0 10000]))
  (var p1 (ray.direction * ts.x + ray.origin))
  (var p2 (axis * ts.y + origin))
  (return [p2 (distance p1 p2)])
  ))

(defn- has-fov? [expr]
  (case (jlsl/expr/type expr)
    (jlsl/type/coerce PerspectiveCamera) true
    (jlsl/type/coerce OrthographicCamera) false
    (error "that's no camera")))

# TODO: okay, so, theoretically we have nearest-distance in the current environment already
(defn make-sample-3d [options nearest-distance <camera> <background-color> <default-color> <color-field>]
  (def {:render-type render-type :crosshairs crosshairs :dynamic-camera? dynamic-camera?} options)
  (def march (make-march nearest-distance))
  (def ortho-distance 1024)

  (jlsl/fn :vec4 sample []
    (var ray* (Ray [0 0 0] [0 0 1]))
    (var ortho-quad [0 0 0])
    (var ortho-scale 0)
    (var fov 0)
    ,(if dynamic-camera?
      (jlsl/statement
        (set ortho-quad [ortho-distance (* frag-coord ortho-base-zoom-distance free-camera-zoom)])
        (set ortho-scale (* ortho-base-zoom-distance free-camera-zoom))
        (case camera-type
          0:s ,(if <camera>
            (jlsl/statement
              (var camera <camera>)
              (set ray* (camera/ray camera))
              ,(if (has-fov? <camera>)
                (jlsl/statement (set fov camera.fov))
                (jlsl/statement (set ortho-scale camera.scale)))
              (break))
            (jlsl/statement
              # just fall through
              ))
          1:s (do
            (var camera-rotation-matrix
              (* (rotation-y (* tau free-camera-orbit.x))
                 (rotation-x (* tau free-camera-orbit.y))))
            (set ray* (Ray
              (camera-rotation-matrix * [0 0 (base-zoom-distance * free-camera-zoom)] + free-camera-target-3d)
              (camera-rotation-matrix * (perspective-vector free-camera-fov * [1 1 -1]))))
            (set fov free-camera-fov)
            (break))
          2:s (do # XZ
            (set ray* (Ray (ortho-quad.yxz + free-camera-target-3d) [0 -1 0]))
            (break))
          3:s (do # XY
            (set ray* (Ray (ortho-quad.yzx + free-camera-target-3d) [0 0 -1]))
            (break))
          4:s (do # ZY
            (set ray* (Ray (ortho-quad.xzy + free-camera-target-3d) [-1 0 0]))
            (break))))
      (if <camera>
        (jlsl/statement (set ray* (camera/ray <camera>)))
        (jlsl/statement
          (var camera-rotation-matrix
            (* (rotation-y (* tau free-camera-orbit.x))
               (rotation-x (* tau free-camera-orbit.y))))
          (set ray* (Ray
            (camera-rotation-matrix * [0 0 (base-zoom-distance * free-camera-zoom)] + free-camera-target-3d)
            (camera-rotation-matrix * (perspective-vector free-camera-fov * [1 1 -1])))))))

    (var steps 0:u)

    (with [ray ray*
           depth (march steps)
           P (+ ray.origin (* ray.direction depth))
           p P
           dist (nearest-distance)
           normal (calculate-normal (nearest-distance))]
      (var color (vec4 0))
      ,(case render-type
        0 (jlsl/statement
          (set color (if (>= dist MAXIMUM_HIT_DISTANCE)
            <background-color>
            [,(@or <color-field> <default-color>) 1])))
        # ignore color field
        1 (jlsl/statement
          (set color (if (>= dist MAXIMUM_HIT_DISTANCE)
            <background-color>
            [<default-color> 1])))
        # convergence debug view
        2 (jlsl/statement
          (set color (if (= steps MAX_STEPS)
            [1 0 1 1]
            [(float steps / float MAX_STEPS | vec3) 1])))
        # overshoot debug view
        3 (jlsl/statement
          (var overshoot (max (- dist) 0 / MINIMUM_HIT_DISTANCE))
          (var undershoot (max dist 0 / MINIMUM_HIT_DISTANCE))
          (set color [overshoot (- 1 undershoot overshoot) (1 - (step 1 undershoot)) 1]))
        (errorf "unknown render type %d" render-type))

      ,(if crosshairs (jlsl/statement
        (if (> crosshairs-3d.w 0) (do
          (var x (intersect-axis crosshairs-3d.xyz [1 0 0] ray))
          (var y (intersect-axis crosshairs-3d.xyz [0 1 0] ray))
          (var z (intersect-axis crosshairs-3d.xyz [0 0 1] ray))

          (var xd (distance x.xyz ray.origin))
          (var yd (distance y.xyz ray.origin))
          (var zd (distance z.xyz ray.origin))
          (var thickness [1 1 1])
          (if (> fov 0)
            (do
              (var angular-resolution (2 * (tan (radians (0.5 * fov)))))
              (*= thickness [xd yd zd * angular-resolution / base-zoom-distance]))
            (*= thickness (ortho-scale / ortho-base-zoom-distance)))
          (if (< x.w thickness.x) (set color (mix color [1 0.1 0.1 1] (if (or (>= xd MAXIMUM_TRACE_DISTANCE) (< xd depth)) 1 0.5))))
          (if (< y.w thickness.y) (set color (mix color [0 1 0.25 1]  (if (or (>= yd MAXIMUM_TRACE_DISTANCE) (< yd depth)) 1 0.5))))
          (if (< z.w thickness.z) (set color (mix color [0 0.5 1 1]   (if (or (>= zd MAXIMUM_TRACE_DISTANCE) (< zd depth)) 1 0.5))))
          )))
      (jlsl/statement))

      (return color))))
