(ns vnctst.transpack.core
  )

;;; NB: This module cannot treat infinity lazy-seq!
;;;     It is include "repeat" fn, because below expr is false.
;;;     (let [r (repeat :a)] (identical? r (rest r)))

;;; ----------------------------------------------------------------

(def current-protocol "TP/0.1")

;;; (stack 0) => {:protocol "TP/0.1", ...} ; meta-info
;;; (stack 1) => [mapped-obj obj-type-key] ; 1 is root
;;; (stack 2) => [mapped-obj obj-type-key] ; 2 and others are children
;;; (stack 3) => ...
(declare ^:private ^:dynamic stack)
(declare ^:private ^:dynamic cache) ; {obj idx, ...} or {idx obj, ...}
(declare ^:private ^:dynamic ext) ; {}
  
(declare ^:private obj->idx ^:private idx->obj)

(definline ^:private atom? [obj] `(= clojure.lang.Atom (class ~obj)))
(definline ^:private ref? [obj] `(= clojure.lang.Ref (class ~obj)))
(definline ^:private object-array? [obj]
  `(= "[Ljava.lang.Object;" (pr-str (class ~obj))))
(definline ^:private array? [class-str obj]
  `(= ~class-str (pr-str (class ~obj))))
  
(defn- mapping [obj]
  ;; if obj may contains other objs, must mapping recursively
  (cond
    (map? obj) [(reduce (fn [prev [k v]]
                          (assoc prev (obj->idx k) (obj->idx v)))
                        {}
                        obj) :map]
    (set? obj) [(reduce (fn [prev one]
                          (conj prev (obj->idx one)))
                        #{}
                        obj) :set]
    (seq? obj) [(doall (map obj->idx obj)) :seq]
    (coll? obj) [(doall (map obj->idx obj)) :coll]
    ;; TODO: ここから下はclassを見てディスパッチするようにする
    (atom? obj) [(obj->idx @obj) :atom]
    (ref? obj) [(obj->idx @obj) :ref]
    (object-array? obj) [(doall (map obj->idx (seq obj))) :object-array]
    (array? "[Z" obj) [(seq obj) :boolean-array]
    (array? "[B" obj) [(seq obj) :byte-array]
    (array? "[S" obj) [(seq obj) :short-array]
    (array? "[C" obj) [(seq obj) :char-array]
    (array? "[I" obj) [(seq obj) :int-array]
    (array? "[J" obj) [(seq obj) :long-array]
    (array? "[F" obj) [(seq obj) :float-array]
    (array? "[D" obj) [(seq obj) :double-array]
    :else [obj nil]))

(defn- unmapping-1 [mapped-obj mtype]
  (case mtype
    nil mapped-obj
    :map (reduce (fn [prev [k v]]
                   (assoc prev (idx->obj k) (idx->obj v)))
                 {}
                 mapped-obj)
    :set (reduce (fn [prev one]
                   (conj prev (idx->obj one)))
                 #{}
                 mapped-obj)
    :seq (doall (map idx->obj mapped-obj))
    :coll (vec (doall (map idx->obj mapped-obj)))
    :boolean-array (boolean-array mapped-obj)
    :byte-array (byte-array mapped-obj)
    :short-array (short-array mapped-obj)
    :char-array (char-array mapped-obj)
    :int-array (int-array mapped-obj)
    :long-array (long-array mapped-obj)
    :float-array (float-array mapped-obj)
    :double-array (double-array mapped-obj)
    ;:atom (atom (idx->obj mapped-obj))
    ;:ref (ref (idx->obj mapped-obj))
    ;:object-array (object-array (doall (map idx->obj mapped-obj)))
    :atom (atom mapped-obj)
    :ref (ref mapped-obj)
    :object-array (object-array mapped-obj)
    ))

(defn- unmapping-2! [obj mtype]
  (case mtype
    :atom (swap! obj idx->obj)
    :ref (dosync (alter obj idx->obj))
    :object-array (let [^"[Ljava.lang.Object;" o obj]
                    (dotimes [i (alength o)]
                      (aset o i (idx->obj (aget o i)))))
    nil))



(defn- obj->idx [src-obj]
  ;; TODO: must be safe from stack overflow
  (if-let [idx (cache src-obj)]
    idx
    (let [idx (count stack)]
      (set! stack (conj stack (delay (mapping src-obj)))) ; reserve to entry
      (set! cache (assoc cache src-obj idx))
      (force (stack idx))
      idx)))

(defn- idx->obj [idx]
  ;; TODO: must be safe from stack overflow
  (if-let [cached (cache idx)]
    cached
    (let [[mapped-obj mtype] (stack idx)
          obj (unmapping-1 mapped-obj mtype)]
      (set! cache (assoc cache idx obj))
      obj)))


(defn- make-meta-info []
  ;; TODO: add more meta-info
  {:protocol current-protocol
   })

;;; TODO: 将来はextにmapを指定する事で、シリアライズ/デシリアライズ可能な
;;;       クラスを追加できるようにする(今は未実装)

(defn pack [root-obj & [user-ext]]
  ;; TODO: must be thread safe
  (binding [stack [0]
            cache {}
            ext user-ext]
    (obj->idx root-obj)
    (into [(make-meta-info)]
          (map deref (rest stack)))))

;;; NB: 環状参照構造を再現するには、まずatom/ref/object-arrayといった、
;;;     参照元の本体だけを作成してから、後から中身をset!する必要がある。
;;;     (srfi-38等のないclojureでは、これ以外に環状参照を作る方法はない、多分)
;;;     よって、最初は中身のunmappingはしないで生成し、後で再設定を行う。
;;;     これは将来にRecord型等をユーザサイドで追加する際に問題となりそうだが、
;;;     とりあえず今はこの方針で行く事にした。
(defn unpack [packed & [user-ext]]
  ;; TODO: must be thread safe
  (let [meta-info (packed 0)]
    ;; TODO: check protocol
    (binding [stack packed
              cache {}
              ext user-ext]
      ;; extract all stacks
      (dotimes [i (count stack)]
        (when-not (zero? i)
          (idx->obj i)))
      ;; fix mutable objects
      (dotimes [i (count stack)]
        (when-not (zero? i)
          (let [[mapped-obj mtype] (stack i)]
            (unmapping-2! (cache i) mtype))))
      ;; return root-obj
      (idx->obj 1))))

