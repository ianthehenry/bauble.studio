(use judge)
(use ./util)
(use ./types)

(defn- typecheck* [expr]
  (expr/match expr
    (call function _)
      [;(map (>> param-sig/type type/to-glsl) (function/param-sigs function))
       '->
       (type/to-glsl (function/return-type function))]
    (error "not a call")))

(defmacro* typecheck [expr & args]
  ~(test (,typecheck* ,expr) ,;args))
