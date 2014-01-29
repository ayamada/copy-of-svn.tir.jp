(ns vnctst.transpack.core-test
  (:require [clojure.test :refer :all]
            [vnctst.transpack.core :refer :all]
            [taoensso.nippy :as nippy]
            [clojure.edn :as edn]
            ;[expectations :as test :refer :all]
            ))

(def stress-data
  ;; remove unsupported objects and uncomparable objects
  (dissoc nippy/stress-data
          :throwable :ex-info :exception :stress-record :bytes))

(def mutable-data
  {:atom (atom 1)
   :ref (ref :a)
   :boolean-array (boolean-array [false true false])
   :byte-array (byte-array (map byte [1 2 3]))
   :short-array (short-array (map short [1 2 3]))
   :char-array (char-array (map char [65 66 67]))
   :int-array (int-array (map int [1 2 3]))
   :long-array (long-array (map long [1 2 3]))
   :float-array (float-array (map float [1 2 3]))
   :double-array (double-array (map double [1 2 3]))
   :object-array (object-array [4 'a :b "ccc" stress-data])
   :nested (atom (atom 3))
   })

(def isomorphic-data (list mutable-data mutable-data))

  
(deftest stress-test
  (testing "pack->unpack stress-data"
    (is (doall
          (map
            (fn [[k v]]
              (let [v2 (unpack (pack v))]
                (when-not (= v v2)
                  (prn "cannot match" k v v2)
                  (throw (Exception. "cannot match")))))
            stress-data)))
    (is stress-data (unpack (pack stress-data))))
  (testing "pack->unpack mutable-data"
    (let [original mutable-data
          target (unpack (pack mutable-data))]
      (is (= @(:atom original) @(:atom target)))
      (is (= @(:ref original) @(:ref target)))
      (is (= (seq (:boolean-array original)) (seq (:boolean-array target))))
      (is (= (seq (:byte-array original)) (seq (:byte-array target))))
      (is (= (seq (:short-array original)) (seq (:short-array target))))
      (is (= (seq (:char-array original)) (seq (:char-array target))))
      (is (= (seq (:int-array original)) (seq (:int-array target))))
      (is (= (seq (:long-array original)) (seq (:long-array target))))
      (is (= (seq (:float-array original)) (seq (:float-array target))))
      (is (= (seq (:double-array original)) (seq (:double-array target))))
      (is (= (seq (:object-array original)) (seq (:object-array target))))
      (is (= @@(:nested original) @@(:nested target)))
      ))
  (testing "edn-safe?"
    (are [m] (doall
               (map
                 (fn [[k v]]
                   (try
                     (edn/read-string (pr-str (pack v)))
                     (catch Exception e
                       (prn "error at" k v)
                       (throw e))))
                 m))
      stress-data
      mutable-data)
    (are [m] (edn/read-string (pr-str (pack m)))
      stress-data
      mutable-data)
    )
  (testing "isomorphic?"
    (let [target (unpack (pack isomorphic-data))
          target-a (:int-array (first target))
          target-b (:int-array (fnext target))
          ]
      (is (= target-a target-b))
      (aset-int target-a 0 999)
      (is (= (seq target-a) (seq target-b)))
      ))
  )

