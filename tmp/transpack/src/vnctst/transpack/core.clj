(ns vnctst.transpack.core
  (:import (java.lang.reflect Method))
  )

;;; NB: This module cannot treat infinity lazy-seq!
;;;     It is include "repeat" fn, because below expr is false.
;;;     (let [r (repeat :a)] (identical? r (rest r)))

;;; TODO: シリアライズ不可のオブジェクトが含まれていた時の挙動が微妙。
;;;       (pack後にそのまま含まれてしまう)
;;;       シリアライズ時に、不可オブジェクトを検出したら、例外を投げた方がよい
;;;       (もしくは、もっと良い他の仕様を考える)

;;; TODO: 将来にユーザ拡張可能にした時、record判定の優先順位を下げる事
;;;       (ユーザ作成の個別のrecord判定の方が優先順位が高くないといけない)
;;;       しかしrecordはmapでもある為、判定順の決定が難しい…

;;; ----------------------------------------------------------------

(defn record? [obj]
  (.isInstance clojure.lang.IRecord obj))
(defn record->map [record]
  (into {} record))
;(defn map->record [class-symbol m]
;  (let [s (symbol (str class-symbol "/create"))]
;    (eval `(~s ~m))))
(defn map->record* [^Class a-class m]
  (let [^Method method (.getMethod a-class "create"
                                   (into-array [clojure.lang.IPersistentMap]))]
    (.invoke method nil (into-array [m]))))
(defn map->record [class-symbol m]
  (map->record* (resolve class-symbol) m))


;;; ----------------------------------------------------------------

(def current-protocol "TP/0.3")

;;; (stack 0) => {:protocol "TP/0.1", ...} ; meta-info
;;; (stack 1) => [mapped-obj "obj-type"] ; 1 is root
;;; (stack 2) => [mapped-obj "obj-type"] ; 2 and others are children
;;; (stack 3) => ...
(declare ^:private ^:dynamic stack)
(declare ^:private ^:dynamic cache) ; {obj idx, ...} or {idx obj, ...}
(declare ^:private ^:dynamic ext) ; convert table for user
  
(declare obj->idx idx->obj)

;;; ----------------------------------------------------------------

;;; シリアライズすべきオブジェクトの定義
;;; TODO: この辺りの関数名はもうちょっと考える必要がある(将来的にはモジュール外に提供する為)

(defn assoc-objpack
  [table ^java.lang.Class class1 mapper unmapper & [replacer]]
  ;; keyがクラスシンボルでなくクラス文字列なのは、pack後のedn安全を保証する為。
  ;; (int-array等のクラスをそのままpr-strするとedn安全でなくなる)
  ;; 高速化する為に色々といじる余地はあるが、文字列で扱う事にする。
  ;; (ただし何にせよ、pack後のクラス識別子は安全性の為に文字列にすべき)
  (let [k (.getName class1)
        v {:mapper mapper, :unmapper unmapper, :replacer replacer}]
    (assoc (or table {}) k v)))

;;; 組み込みの変換テーブルを作成
(def ^:private objpack-table
  (-> {}
    (assoc-objpack (class (boolean-array 0)) seq boolean-array)
    (assoc-objpack (class (byte-array 0)) seq byte-array)
    (assoc-objpack (class (short-array 0)) seq short-array)
    (assoc-objpack (class (char-array 0)) seq char-array)
    (assoc-objpack (class (int-array 0)) seq int-array)
    (assoc-objpack (class (long-array 0)) seq long-array)
    (assoc-objpack (class (float-array 0)) seq float-array)
    (assoc-objpack (class (double-array 0)) seq double-array)
    (assoc-objpack (class (object-array 0))
                   #(doall (map obj->idx %))
                   object-array
                   (fn [^"[Ljava.lang.Object;" os]
                     (dotimes [i (alength os)]
                       (aset os i (idx->obj (aget os i))))))
    (assoc-objpack clojure.lang.Atom
                   #(obj->idx @%)
                   atom
                   #(swap! % idx->obj))
    (assoc-objpack clojure.lang.Ref
                   #(obj->idx @%)
                   ref
                   #(dosync (alter % idx->obj)))
    ))


;;; ----------------------------------------------------------------

(defn- mapping [obj]
  ;; if obj may contains other objs, must mapping recursively
  (cond
    (nil? obj) [nil nil]
    ;; NB: record is instance of map!
    (record? obj) [(reduce (fn [prev [k v]]
                             (assoc prev (obj->idx k) (obj->idx v)))
                           {}
                           obj) (.getName (class obj))]
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
    :else (let [class-name (.getName (class obj))]
            (if-let [objpack-entry (objpack-table class-name)]
              [((:mapper objpack-entry) obj) class-name]
              [obj nil]))))

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
    (if-let [objpack-entry (objpack-table mtype)]
      ((:unmapper objpack-entry) mapped-obj)
      (try
        ;; try unmap defrecord
        (let [m (reduce (fn [prev [k v]]
                          (assoc prev (idx->obj k) (idx->obj v)))
                        {}
                        mapped-obj)]
          (map->record (symbol mtype) m))
        (catch Throwable e
          (throw (RuntimeException. (str "cannot unmap: " mtype))))))))

(defn- unmapping-2! [obj mtype]
  (when-let [replacer (:replacer (objpack-table mtype))]
    (replacer obj)))



(defn obj->idx [src-obj]
  ;; TODO: must be safe from stack overflow
  (if-let [idx (cache src-obj)]
    idx
    (let [idx (count stack)]
      (set! stack (conj stack (delay (mapping src-obj)))) ; reserve to entry
      (set! cache (assoc cache src-obj idx))
      (force (stack idx))
      idx)))

(defn idx->obj [idx]
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

