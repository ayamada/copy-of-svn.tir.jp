(ns vnctst.transpack.core
  )

;;; TODO: must be support cyclic structures
;;;       (現在は枝の末端から変換を行おうとする為、循環構造があると止まらない)

;;; ----------------------------------------------------------------

(def current-protocol "TP/0.0")

;;; (stack 0) => {:root 123, :protocol "TP/0.0"} ; :root is index number
;;; (stack 1) => [mapped-obj obj-type-key]
;;; (stack 2) => ...
(declare ^:private ^:dynamic stack)
(declare ^:private ^:dynamic cache) ; {obj idx, ...} or {idx obj, ...}
  
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
    ;; TODO: list? 等の個別対応(coll?だけだと()と[]がいっしょくたになる為)
    (coll? obj) [(doall (map obj->idx obj)) :coll] ; NB: this is fallback
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

(defn- unmapping [mapped-obj mtype]
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
    :coll (doall (map idx->obj mapped-obj))
    :atom (atom (idx->obj mapped-obj))
    :ref (ref (idx->obj mapped-obj))
    :object-array (object-array (doall (map idx->obj mapped-obj)))
    :boolean-array (boolean-array mapped-obj)
    :byte-array (byte-array mapped-obj)
    :short-array (short-array mapped-obj)
    :char-array (char-array mapped-obj)
    :int-array (int-array mapped-obj)
    :long-array (long-array mapped-obj)
    :float-array (float-array mapped-obj)
    :double-array (double-array mapped-obj)
    ))



(defn- obj->idx [src-obj]
  ;; TODO: must be safe recur
  (if-let [idx (cache src-obj)]
    idx
    (let [mapped-obj (mapping src-obj)]
      (set! stack (conj stack mapped-obj))
      (let [idx (dec (count stack))]
        (set! cache (assoc cache src-obj idx))
        idx))))

(defn- idx->obj [idx]
  ;; TODO: must be safe recur
  (if-let [cached (cache idx)]
    cached
    (let [[mapped-obj mtype] (stack idx)
          obj (unmapping mapped-obj mtype)]
      (set! cache (assoc cache idx obj))
      obj)))


(defn- make-meta-info [root-idx]
  {:root root-idx
   :protocol current-protocol
   })

;;; TODO: 将来はextにmapを指定する事で、シリアライズ/デシリアライズ可能な
;;;       クラスを追加できるようにする

(defn pack [root-obj & [ext]]
  ;; TODO: must be thread safe
  (binding [stack [0]
            cache {}]
    (into [(make-meta-info (obj->idx root-obj))]
          (rest stack))))

(defn unpack [packed & [ext]]
  ;; TODO: must be thread safe
  (binding [stack packed
            cache {}]
    (let [meta-info (packed 0)]
      ;; TODO: check protocol
      (idx->obj (:root meta-info)))))

