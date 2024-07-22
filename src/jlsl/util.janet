(use judge)
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
