(import jaylib)
(use module)
(import ../src :as bauble)
(import ./ray)
(import ../src/glsl)

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
  (defn new [glsl-program]
    (def shader-source (glsl/render-program glsl-program "330"))
    (when (dyn *verbose*)
      (eprint "```glsl")
      (eprint shader-source)
      (eprint "```"))

    (def shader (jaylib/load-shader-from-memory nil shader-source))
    (def uniforms (glsl/uniforms glsl-program))
    (def uniform-locations (tabseq [name :keys uniforms]
      name (jaylib/get-shader-location shader name)))
    (def unset-uniforms (tabseq [name :keys uniforms] name true))
    (var last-resolution nil)
    (var fbo nil)

    (defn set-uniform-values [& new-values]
      (each [name value] (partition 2 new-values)
        (def name (glsl/identifier name))
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
