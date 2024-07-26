(use judge)
(use module)
(import pat)

(defn unprefix [sym]
  (symbol/slice (drop-until |(= $ (chr "/")) sym) 1))

(test (unprefix 'foo/bar) bar)

(defmacro export [sym]
  ~(put (curenv) ',(unprefix sym) (table/setproto @{:private false} (dyn ',sym))))

(test-macro (export foo/bar)
  (put (curenv) (quote bar) (table/setproto @{:private false} (dyn (quote foo/bar)))))

(defmacro assertf [x & args]
  ~(assert ,x (string/format ,;args)))

(defn stuple? [t]
  (and (tuple? t) (= (tuple/type t) :brackets)))

(test (stuple? [1 2 3]) false)
(test (stuple? '[1 2 3]) true)

(defn get-unique [f ind]
  (pat/match (distinct (map f ind))
    [unique] unique
    values (errorf "no unique value %q" values)))

(defn >> [f g] |(g (f ;$&)))
(defn << [f g] |(f (g ;$&)))

(defn ptuple? [x] (and (tuple? x) (= (tuple/type x) :parens)))
(defn btuple? [x] (and (tuple? x) (= (tuple/type x) :brackets)))

(defn contents= [a b]
  (and
    (= (length a) (length b))
    (all = a b)))

(defn find-last [pred ind &opt dflt]
  (var result dflt)
  (loop [i :down-to [(dec (length ind)) 0]]
    (def x (in ind i))
    (when (pred x)
      (set result x)
      (break)))
  result)

(test (find-last odd? [1 2 3 4]) 3)
(test (find-last odd? [2 4 6]) nil)
(test (find-last odd? [2 4 6] :none) :none)

# This is a bit like postwalk, except that it works
# with cyclic data structures.
#
# The callback is invoked with three arguments: the value
# being visited, a boolean indicated whether the node is
# currently being visited, and a stack of all values currently
# being visited (not including the current value).
#
# (The visited? boolean in the callback is redundant with the
# values on the stack.)
#
# A node will never be visited more than once, which has implications
# for nodes which are value-equal to other nodes. Duplicate nodes will
# never be visited again.
(def- core/walk walk)
(defn visit [structure f &opt walk]
  (default walk core/walk)
  (def visiting @{})
  (def visited @{})
  (def stack @[])

  (defn recur [node]
    (when (in visited node) (break nil))
    (def visiting? (truthy? (in visiting node)))
    (put visiting node true)
    (unless visiting?
      (array/push stack node)
      (walk recur node)
      (array/pop stack))
    (f node visiting? stack)
    (put visited node true)
    (put visiting node nil))
  (recur structure)
  nil)

(test-stdout (do (postwalk (fn [x] (pp x) x) [1 1 1]) nil) `
  1
  1
  1
  (1 1 1)
`)

(test-stdout (visit [1 1 1] (fn [x _ _] (pp x))) `
  1
  (1 1 1)
`)

# useful for invoking macros as regular functions
(defn call [f & args]
  (f ;args))

(defmacro get-or-put [t k v]
  (with-syms [$t $k $v]
    ~(let [,$t ,t ,$k ,k]
      (if (,has-key? ,$t ,$k)
        (,in ,$t ,$k)
        (let [,$v ,v]
          (,put ,$t ,$k ,$v)
          ,$v)))))

(test-macro (get-or-put @{} 1 2)
  (let [<1> @{} <2> 1]
    (if (@has-key? <1> <2>)
      (@in <1> <2>)
      (let [<3> 2]
        (@put <1> <2> <3>)
        <3>))))

(defmodule ref
  (defn new [] @[])
  (defn get [t &opt dflt] (if (empty? t) dflt (in t 0)))
  (defn set [t x] (put t 0 x))
  (defmacro get-or-put [t v]
    (with-syms [$t $v]
      ~(let [,$t ,t]
        (if (,empty? ,$t)
          (let [,$v ,v]
            (,put ,$t 0 ,$v)
            ,$v)
          (,in ,$t 0))))))

(test-macro (ref/get-or-put x y)
  (let [<1> x]
    (if (@empty? <1>)
      (let [<2> y]
        (@put <1> 0 <2>)
        <2>)
      (@in <1> 0))))
