(ns com.phronemophobic.automerge
  (:refer-clojure :exclude [load])
  (:require [com.phronemophobic.automerge.impl.raw :as raw]
            [com.phronemophobic.automerge.protocols :as protocols]
            [com.phronemophobic.automerge.impl.protocols :as impl.protocols
             :refer [get-doc]]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype :as dt]
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype.native-buffer :as native-buffer]
            tech.v3.resource)
  (:import
   [tech.v3.datatype.ffi Pointer]
   [java.util Map List]
   [clojure.lang MapEntry IObj IFn ILookup]))

(set! *warn-on-reflection* true)

(defn pop! 
  "For lists, removes the last item."
  [doc]
  (protocols/delete! doc -1))

(defn delete!
  "For maps, removes for the given key, `k`. Has no effect if `k` does not already exist.
  
  For lists, removes the item at index `k`. Throws exception if `k`>= (count doc). Passing a negative `k` is undefined and behavior may change in the future."
  [doc k]
  (protocols/delete! doc k))

(defn insert! 
  "For lists, inserts `v` at index `idx`."
  [doc idx v]
  (protocols/insert! doc idx v))
(defn append! 
  "For lists, adds `v` to the end of the list."
  [doc v]
  (protocols/insert! doc -1 v))

(defn put!
  "For maps, sets value to `v` for the given key, `k`.
  
  For lists, sets the value at index `k` to `v`."
  [doc k v]
  (protocols/put! doc k v))
(defn ->clj
  "Converts a document to an immutable clojure data structure."
  [doc]
  (protocols/->clj doc))

(defn root-item
  "Returns the root of a document or document item."
  [doc]
  (protocols/root-item doc))

(defn merge!
  "Applies all of the changes in `dest` which are not in `src` to `src`."
  [dest src]
  (protocols/merge! dest src))

(defn clone
  "Duplicates `doc` and returns the copy."
  [doc]
  (protocols/clone doc))

(defn commit!
  "Commits any pending operations on a document with an optional message or instant.
  
  `message`: may be nil or a string if provided.
  `t`: may be nil or java.time.Instant"
  ([doc]
   (protocols/commit! doc))
  ([doc message t]
   (protocols/commit! doc message t)))

(defn empty-change
  "Creates an empty change with `message` and `t`.
  
  `message`: may be nil or a string if provided.
  `t`: may be nil or java.time.Instant"
  [doc message t]
  (protocols/empty-change doc message t))

(defn fork
  "Forks this document for use by a difference actor."
  ([doc]
   (protocols/fork doc))
  ([doc heads]
   (protocols/fork doc heads)))

(defn get-actor-id
  "Returns the actor id for a document.
  
  returns a dtype native-buffer with the bytes that represent the actor id."
  [doc]
  (protocols/get-actor-id doc))

(defn save
  "Saves the entirety of the document in a compact form."
  [doc]
  (protocols/save doc))

(defn set-actor-id
  "Sets the actor id for the document.
  
  `actor-id`: a native-buffer representing the actor id."
  [doc actor-id]
  (protocols/set-actor-id doc actor-id))


(declare ->MapItem ->ListItem -put list-put map-put ->clj*)

(defn ^:private bytespan->str [bytespan]
  (let [inbuf (native-buffer/wrap-address (:src bytespan)
                                          (:count bytespan))
        outbuf (byte-array (:count bytespan))]
    (dt/copy! inbuf outbuf)
    (String. outbuf "utf-8")))

(defn ^:private handle-error-result [result]
  (let [bs (raw/AMresultError result)
        s (bytespan->str bs)]
    (raw/AMresultFree result)
    (throw (ex-info s {}))))

(defn ^:private check-result [result]
  (when (not= (raw/AMresultStatus result)
              raw/AM_STATUS_OK)
    (handle-error-result result))
  result)

(defn ^:private check-bool [o]
  (when (zero? o)
    (throw (ex-info "Expected true"
                    {}))))

(defn ^:private check-and-free-result [result]
  (when (not= (raw/AMresultStatus result)
              raw/AM_STATUS_OK)
    (handle-error-result result))
  (raw/AMresultFree result)
  nil)

(defn ^:private result->item
  "Returns the item for a result. Item will hang onto result until gc'd."
  [^Pointer result]
  (if (= (raw/AMresultStatus result)
         raw/AM_STATUS_OK)
    (let [item (raw/AMresultItem result)
          result-address (.address result)]
      (assert (not (zero? result-address)))
      (tech.v3.resource/track item
                              {:dispose-fn
                               (fn []
                                 (raw/AMresultFree (Pointer. result-address)))})
      item)
    ;; else
    (handle-error-result result)))

(defmacro ^:private with-item
  "Extract `item` from result. Frees contents of result and item. at end of scope."
  [[item result] & body]
  `(let [^Pointer
         result# ~result]
     (if (= (raw/AMresultStatus result#)
            raw/AM_STATUS_OK)
       (let [~item (raw/AMresultItem result#)]
         (try
           ~@body
           (finally
             (raw/AMresultFree result#))))
       ;; else
       (handle-error-result result#))))



(defn ^:private create-doc [actor-id]
  (let [actor-id* (when actor-id
                    (let [result (check-result
                                  (raw/AMactorIdFromBytes actor-id
                                                          (native-buffer/native-buffer-byte-len actor-id)))
                          item (raw/AMresultItem result)
                          actor-id* (dt-ffi/make-ptr :pointer 0)
                          _ (check-bool (raw/AMitemToActorId item actor-id*))

                          actor-id (nth actor-id* 0)]
                      (tech.v3.resource/track
                       actor-id
                       {:dispose-fn
                        (fn []
                          (raw/AMresultFree result))})
                      actor-id))

        doc-result (raw/AMcreate actor-id*)]
    (if (= (raw/AMresultStatus doc-result)
           raw/AM_STATUS_OK)
      (let [item (raw/AMresultItem doc-result)
            doc* (dt-ffi/make-ptr :pointer 0)]
        (check-bool (raw/AMitemToDoc item doc*))
        (let [doc (dt-ffi/->pointer (first doc*))]
          (tech.v3.resource/track doc
                                  {:dispose-fn
                                   (fn []
                                     (raw/AMresultFree doc-result))})
          doc))
      ;; else
      (handle-error-result doc-result))))

;; We can probably make this faster
(defn ^:private ->AMstr [s]

  (let [bs (dt-ffi/string->c s)
        amstr (dt-struct/map->struct
               :AMbyteSpan
               {:src (.address (dt-ffi/->pointer bs))
                ;; don't include null at end of string
                :count (dec (native-buffer/native-buffer-byte-len bs))})]
    ;; make sure bytes aren't garbage collected prematurely
    (tech.v3.resource/track
     amstr
     {:dispose-fn
      (fn []
        (identity bs))})
    amstr))

(def ^:private AM_ROOT nil)

(comment
  get
  put
  delete
  increment
  splice
  spliceText
  ;;mark
  )


(defmulti ^:private -list-put (fn -list-put-dispatch [doc obj-id idx insert? v vtype]
                                vtype))

(defmethod -list-put Boolean [doc obj-id k insert? v  _]
  (check-and-free-result
   (raw/AMlistPutBool doc obj-id k (if insert? 1 0) (if v 1 0))))

(defmethod -list-put byte/1 [doc obj-id k insert? v  _]
  (let [buf  (native-buffer/malloc (alength ^bytes v))
        _ (dt/copy! v buf)

        bytespan (dt-struct/map->struct :AMbyteSpan {:src (.address (dt-ffi/->pointer buf)) :count (alength ^bytes v)})]
    (check-and-free-result
     (raw/AMlistPutBytes doc obj-id k (if insert? 1 0) bytespan))))

(defmethod -list-put :counter [doc obj-id k insert? v  _]
  (check-and-free-result
   (raw/AMlistPutCounter doc obj-id k (if insert? 1 0) v)))

(defmethod -list-put Double [doc obj-id k insert? v  _]
  (check-and-free-result
   (raw/AMlistPutF64 doc obj-id k (if insert? 1 0) v)))

(defmethod -list-put Long [doc obj-id k insert? v  _]
  (check-and-free-result
   (raw/AMlistPutInt doc obj-id k (if insert? 1 0) v)))

(defmethod -list-put nil [doc obj-id k insert? v  _]
  (check-and-free-result
   (raw/AMlistPutNull doc obj-id k (if insert? 1 0))))

(defmethod -list-put Map [doc obj-id k insert? v  _]
  (with-item
    [item (raw/AMlistPutObject doc obj-id k (if insert? 1 0)
                               (raw/kw->obj-type :obj-type/map))]
    (doseq [[k v] v]
      (-put doc item k v))))

(defmethod -list-put List [doc obj-id k insert? v  _]
  (with-item
    [item (raw/AMlistPutObject doc obj-id k (if insert? 1 0)
                               (raw/kw->obj-type :obj-type/list))]
    (let [list-obj-id (raw/AMitemObjId item)]
      (doseq [[i x] (map-indexed vector v)]
        (list-put doc list-obj-id -1 true x)))))

(defmethod -list-put String [doc obj-id k insert? v  _]
  (let [sbuf (.getBytes ^String v "utf-8")

        buf  (native-buffer/malloc (alength sbuf))
        _ (dt/copy! sbuf buf)

        bytespan (dt-struct/map->struct :AMbyteSpan {:src (.address (dt-ffi/->pointer buf)) :count (alength sbuf)})]
    (check-and-free-result
     (raw/AMlistPutStr doc obj-id k (if insert? 1 0) bytespan))))

(defmethod -list-put java.time.Instant [doc obj-id k insert? v  _]
  (let [ms (java.time.Instant/.toEpochMilli v)]
    (check-and-free-result
     (raw/AMlistPutTimestamp doc obj-id k (if insert? 1 0) ms))))

(defmulti ^:private -map-put (fn [doc obj-id k v vtype]
                               vtype))

(defmethod -map-put Boolean [doc obj-id k v _]
  (check-and-free-result
   (raw/AMmapPutBool doc obj-id (->AMstr k) (if v 1 0))))

(defmethod -map-put byte/1 [doc obj-id k v _]
  (let [buf  (native-buffer/malloc (alength ^bytes v))
        _ (dt/copy! v buf)

        bytespan (dt-struct/map->struct :AMbyteSpan {:src (.address (dt-ffi/->pointer buf)) :count (alength ^bytes v)})]
    (check-and-free-result
     (raw/AMmapPutBytes doc obj-id (->AMstr k) bytespan))))

(defmethod -map-put :counter [doc obj-id k v _]
  (check-and-free-result
   (raw/AMmapPutCounter doc obj-id (->AMstr k) v)))

(defmethod -map-put Double [doc obj-id k v _]
  (check-and-free-result
   (raw/AMmapPutF64 doc obj-id (->AMstr k) v)))

(defmethod -map-put Long [doc obj-id k v _]
  (check-and-free-result
   (raw/AMmapPutInt doc obj-id (->AMstr k) v)))

(defmethod -map-put nil [doc obj-id k v _]
  (check-and-free-result
   (raw/AMmapPutNull doc obj-id (->AMstr k))))

(defmethod -map-put Map  [doc obj-id k v _]
  (with-item
    [item (raw/AMmapPutObject doc obj-id (->AMstr k)
                              (raw/kw->obj-type :obj-type/map))]
    (doseq [[k v] v]
      (-put doc item k v))))

(defmethod -map-put List  [doc obj-id k v _]
  (with-item
    [item (raw/AMmapPutObject doc obj-id (->AMstr k) (raw/kw->obj-type :obj-type/list))]
    (let [list-obj-id (raw/AMitemObjId item)]
      (doseq [[i x] (map-indexed vector v)]
        (list-put doc list-obj-id -1 true x)))))

(defmethod -map-put String [doc obj-id k v _]
  (let [sbuf (.getBytes ^String v "utf-8")

        buf  (native-buffer/malloc (alength sbuf))
        _ (dt/copy! sbuf buf)

        bytespan (dt-struct/map->struct :AMbyteSpan {:src (.address (dt-ffi/->pointer buf)) :count (alength sbuf)})]
    (check-and-free-result
     (raw/AMmapPutStr doc obj-id (->AMstr k) bytespan))))

(defmethod -map-put java.time.Instant [doc obj-id k v _]
  (let [ms (java.time.Instant/.toEpochMilli v)]
    (check-and-free-result
     (raw/AMmapPutTimestamp doc obj-id (->AMstr k) ms))))

(defn ^:private -put [doc item k v]
  (if (nil? item)
    ;; document root
    (map-put doc AM_ROOT k v)
    (if-let [obj-id (raw/AMitemObjId item)]
      (let [obj-type (raw/obj-type->kw (raw/AMobjObjType doc obj-id))]
        (case obj-type
          :obj-type/list
          (list-put doc obj-id k false v)

          :obj-type/map
          (map-put doc obj-id k v)

          ;; else
          (throw (ex-info "Invalid item type for put"
                          {:item item
                           :obj-type obj-type
                           :doc doc
                           :k k
                           :v v}))))
      ;; else
      (throw (ex-info "Invalid item type for put"
                      {:item item
                       :doc doc
                       :k k
                       :v v})))))

(defn ^:private get-item-result [doc item]
  (if (= :val-type/obj-type
         (raw/val-type->kw (raw/AMitemValType item)))
    (let [obj-id (raw/AMitemObjId item)
          obj-type (raw/obj-type->kw (raw/AMobjObjType doc obj-id))]
      (case obj-type
        :obj-type/list
        (->ListItem doc item)

        :obj-type/map
        (->MapItem doc item)

          ;; else
        (throw (ex-info "Unexpected get item result"
                        {}))))
    (->clj* doc item)))

(defn ^:private map-get [doc obj-id k]
  (let [result (raw/AMmapGet doc obj-id (->AMstr k) nil)
        item (result->item result)]
    (get-item-result doc item)))

(defn ^:private list-get [doc obj-id k]
  (let [result (raw/AMlistGet doc obj-id k nil)
        item (result->item result)]
    (get-item-result doc item)))

(defn ^:private -get [doc item k]
  (if (nil? item)
    ;; document root
    (map-get doc AM_ROOT k)
    (if-let [obj-id (raw/AMitemObjId item)]
      (let [obj-type (raw/obj-type->kw (raw/AMobjObjType doc obj-id))]
        (case obj-type
          :obj-type/list
          (list-get doc obj-id k)

          :obj-type/map
          (map-get doc obj-id k)

          ;; else
          (throw (ex-info "Invalid item type for get"
                          {:item item
                           :obj-type obj-type
                           :k k
                           :doc doc}))))
      ;; else
      (throw (ex-info "Invalid item type for get"
                      {:item item
                       :doc doc
                       :k k})))))


(defn ^:private ->clj-map [doc obj-id]
  (let [result (raw/AMkeys doc obj-id nil)]
    (try
      (let [items (raw/AMresultItems result)
            key-byte-span (dt-struct/new-struct
                           :AMbyteSpan
                           {:container-type :native-heap})]
        (loop [m {}]
          (if-let [item (raw/AMitemsNext items 1)]
            (do
              (check-bool (raw/AMitemToStr item key-byte-span))
              (let [k (bytespan->str key-byte-span)

                    v (with-item
                        [item (raw/AMmapGet doc obj-id key-byte-span nil)]

                        (->clj* doc item))]

                (recur (assoc m k v))))
            m)))
      (finally
        (raw/AMresultFree result)))))

(defn ^:private ->clj-vec [doc obj-id]
  (let [result (raw/AMobjItems doc obj-id nil)]
    (try
      (let [items (raw/AMresultItems result)]
        (loop [xs []]
          (if-let [item (raw/AMitemsNext items 1)]
            (recur (conj xs (->clj* doc item)))
            xs)))
      (finally
        (raw/AMresultFree result)))))


(defn ^:private ->clj* [doc item]
  (let [val-type (if item
                   (raw/val-type->kw (raw/AMitemValType item))
                   :val-type/obj-type)]

    (case val-type
      ;; :val-type/default
      ;; :val-type/cursor
      ;; :val-type/unknown
      ;; :val-type/sync-have
      ;; :val-type/change-hash
      ;; :val-type/actor-id 
      ;; :val-type/sync-state
      ;; :val-type/change
      ;; :val-type/mark
      ;; :val-type/sync-message

      (:val-type/null :val-type/void) nil

      :val-type/uint
      (let [ptr (dt-ffi/make-ptr :int64 0)]
        (check-bool (raw/AMitemToUint item ptr))
        (nth ptr 0))

      :val-type/int
      (let [ptr (dt-ffi/make-ptr :int64 0)]
        (check-bool (raw/AMitemToInt item ptr))
        (nth ptr 0))

      :val-type/bool (let [ptr (dt-ffi/make-ptr :int8 0)]
                       (check-bool (raw/AMitemToBool item ptr))
                       (not (zero? (nth ptr 0))))
      :val-type/f64 (let [ptr (dt-ffi/make-ptr :float64 0)]
                      (check-bool (raw/AMitemToF64 item ptr))
                      (nth ptr 0))

      :val-type/counter (let [ptr (dt-ffi/make-ptr :int64 0)]
                          (check-bool (raw/AMitemToCounter item ptr))
                          (nth ptr 0))
      :val-type/timestamp (let [ptr (dt-ffi/make-ptr :int64 0)
                                _ (check-bool (raw/AMitemToTimestamp item ptr))
                                ms (nth ptr 0)]
                            (java.time.Instant/ofEpochMilli ms))

      :val-type/bytes (let [byte-span (dt-struct/new-struct
                                       :AMbyteSpan
                                       {:container-type :native-heap})]
                        (check-bool (raw/AMitemToBytes item byte-span))
                        (native-buffer/clone-native
                         (native-buffer/wrap-address (:src byte-span)
                                                     (:count byte-span))))

      :val-type/str (let [byte-span (dt-struct/new-struct
                                     :AMbyteSpan
                                     {:container-type :native-heap})]
                      (check-bool (raw/AMitemToStr item byte-span))
                      (bytespan->str byte-span))

      (:val-type/doc :val-type/obj-type)
      (let [[obj-id obj-type] (if item
                                (let [obj-id (raw/AMitemObjId item)
                                      obj-type (raw/obj-type->kw (raw/AMobjObjType doc obj-id))]
                                  [obj-id obj-type])
                                ;; doc root
                                [AM_ROOT :obj-type/map])]
        (case obj-type
          :obj-type/list (->clj-vec doc obj-id)
          :obj-type/map (->clj-map doc obj-id)

          ;; else
          (throw (ex-info "Unsupported object type."
                          {:item item
                           :obj-type obj-type
                           :doc doc})))))))

#_(defprotocol IItem
    (put! [item k v])
    (->clj [item])
    (root-item [item])
    (get-doc [item]))

#_(defprotocol IDocument
    (merge! [dest src])
    (clone [doc])
    (commit!
      [doc]
      [doc message t]
      "Commits the current operations on a document.
    
    `message`: optional, may be nil.
    `t`: optional, may be nil or a java.time.Instant.")
    (empty-change [doc message t])
    (fork
      [doc]
      [doc heads])
    (get-actor-id [doc])
  ;; (get-heads [doc])
  ;; (load-incremental [doc src count])
    (save [doc])
    (set-actor-id [doc]))

;; equals




;; This could be improved so that
;; key/value map entries are lazily produced
(defn ^:private map-seq [doc item]
  (let [obj-id (raw/AMitemObjId item)
        result (raw/AMkeys doc obj-id nil)]
    (try
      (let [items (raw/AMresultItems result)
            key-byte-span (dt-struct/new-struct
                           :AMbyteSpan
                           {:container-type :native-heap})]
        (loop [m {}]
          (if-let [item (raw/AMitemsNext items 1)]
            (do
              (check-bool (raw/AMitemToStr item key-byte-span))
              (let [v (let [result (raw/AMmapGet doc obj-id key-byte-span nil)
                            item (result->item result)]
                        (get-item-result doc item))

                    k (bytespan->str key-byte-span)]
                (recur (assoc m k v))))
            (seq m))))
      (finally
        (raw/AMresultFree result)))))

(defn ^:private map-contains-key [doc item k]
  (let [obj-id (raw/AMitemObjId item)
        result (raw/AMmapGet doc obj-id (->AMstr k) nil)]
    (with-item
      [item result]
      (not= :val-type/void
            (raw/val-type->kw
             (raw/AMitemValType item))))))


(defn ^:private map-delete! [doc item k]
  (let [obj-id (raw/AMitemObjId item)]
    (check-and-free-result
                (raw/AMmapDelete doc obj-id (->AMstr k)))
    nil))

(deftype ^{:doc ""}
 MapItem [doc item]

  ILookup
  (valAt [this k] (.get this k))
  (valAt [this k not-found] (.getOrDefault this k not-found))

  IFn
  (invoke [this k] (.get this k))
  (applyTo [this args]
    (when (not= 1 args)
      (throw (ex-info "must be called with one arg"
                      {:args args})))
    (.get this (first args)))

  impl.protocols/IItem
  (get-doc [this]
    doc)
  protocols/IItem
  (root-item [this]
    (MapItem. doc nil))
  (put! [this k v]
    (-put doc item k v)
    nil)
  (->clj [this]
    (->clj* doc item))
  (delete! [this k]
    (map-delete! doc item k)
    nil)

  ;; ItemType
  ;; (item-type [this]
  ;;   (raw/val-type->kw (raw/AMitemValType item)))

  clojure.lang.Associative
  clojure.lang.Seqable
  (seq [this]
    (map-seq doc item))

  clojure.lang.Counted
  (count [this]
    (raw/AMobjSize doc (raw/AMitemObjId item) nil))

  Map
  (size [_m]
    (raw/AMobjSize doc (when item (raw/AMitemObjId item)) nil))
  (containsKey [_m k]
    (map-contains-key doc item k))
  #_(entrySet [m]
              (let [map-entry-data (map (comp #(MapEntry. % (.get m %)) :name)
                                        (:data-layout struct-def))]
                (LinkedHashSet. ^Collection map-entry-data)))
  #_(keySet [_m] #_(.keySet ^Map (:layout-map struct-def)))
  (get [this k]
    (-get doc item k))
  (getOrDefault [this k d]
    (if (.containsKey this k)
      (-get doc item k)
      d))
  (put [this k v]
    (let [prev (get this k)]
      (-put doc item k v)
      prev))

  Object
  (toString [this]
    (str "#" `MapItem " " (pr-str (->clj* doc item)))))

(defn ^:private list-seq [doc item]
  (let [obj-id (raw/AMitemObjId item)
        n (raw/AMobjSize doc obj-id nil)]
    (seq
     (sequence
      (map (fn [i]
             (let [result (raw/AMlistGet doc obj-id i nil)
                   item (result->item result)]
               (get-item-result doc item))))
      (range n)))))

(defn ^:private list-delete! [doc item idx]
  (let [obj-id (raw/AMitemObjId item)]
    (check-and-free-result
     (raw/AMlistDelete doc obj-id idx))
    nil))


(deftype ^{:doc ""}
 ListItem [doc item]

  ILookup
  (valAt [this k] (.get this k))
  (valAt [this k not-found] (.getOrDefault this k not-found))

  IFn
  (invoke [this k] (.get this k))
  (applyTo [this args]
    (when (not= 1 args)
      (throw (ex-info "must be called with one arg"
                      {:args args})))
    (.get this (first args)))

  clojure.lang.Indexed
  (nth [this i] (.get this i))
  (nth [this i not-found] (.getOrDefault this i not-found))
  (count [this]
    (raw/AMobjSize doc (raw/AMitemObjId item) nil))
  clojure.lang.Seqable
  (seq [this]
    (list-seq doc item))

  impl.protocols/IItem
  (get-doc [this]
    doc)
  protocols/IItem
  (root-item [this]
    (MapItem. doc nil))
  (put! [this k v]
    (-put doc item k v)
    nil)
  (->clj [this]
    (->clj* doc item))
  (delete! [this k]
    (list-delete! doc item k)
    nil)

  protocols/IListItem
  (insert! [this idx v]
    (list-put doc (raw/AMitemObjId item) idx true v))

  ;; ItemType
  ;; (item-type [this]
  ;;   (raw/val-type->kw (raw/AMitemValType item)))

  Map
  (size [_m]
    (raw/AMobjSize doc (raw/AMitemObjId item) nil))
  (containsKey [_m k] #_(.containsKey ^Map (:layout-map struct-def) k))
  (keySet [_m] #_(.keySet ^Map (:layout-map struct-def)))
  (get [this k]
    (-get doc item k))
  (getOrDefault [m k d]
    (let [n (raw/AMobjSize doc (raw/AMitemObjId item) nil)]
      (if (< k n)
        (-get doc item k)
        d)))
  (put [this k v]
    (let [prev (get this k)]
      (-put doc item k v)
      prev))
  Object
  (toString [this]
    (str "#" `ListItem " " (pr-str (->clj* doc item)))))

(deftype ^{:doc ""}
 Document [doc]

  dt-ffi/PToPointer
  (convertible-to-pointer? [this]
    true)
  (->pointer [this]
    doc)

  ILookup
  (valAt [this k] (.get this k))
  (valAt [this k not-found] (.getOrDefault this k not-found))

  IFn
  (invoke [this k] (.get this k))
  (applyTo [this args]
    (when (not= 1 args)
      (throw (ex-info "must be called with one arg"
                      {:args args})))
    (.get this (first args)))

  impl.protocols/IItem
  (get-doc [this]
    doc)
  protocols/IItem
  (root-item [this]
    this)
  (put! [this k v]
    (-put doc nil k v)
    nil)
  (->clj [this]
    (->clj* doc nil))
  (delete! [this k]
    (map-delete! doc nil k)
    nil)

  ;; ItemType
  ;; (item-type [this]
  ;;   :val-type/obj-type)

  clojure.lang.Associative
  (equiv [this other]
    (.equals this other))
  clojure.lang.Seqable
  (seq [this]
    (map-seq doc nil))

  clojure.lang.Counted
  (count [this]
    (raw/AMobjSize doc nil nil))

  Map
  (size [_m]
    (raw/AMobjSize doc nil nil))
  (containsKey [_m k]
    (map-contains-key doc nil k))
  #_(entrySet [m]
              (let [map-entry-data (map (comp #(MapEntry. % (.get m %)) :name)
                                        (:data-layout struct-def))]
                (LinkedHashSet. ^Collection map-entry-data)))
  #_(keySet [_m] #_(.keySet ^Map (:layout-map struct-def)))
  (get [this k]
    (-get doc nil k))
  (getOrDefault [this k d]
    (if (.containsKey this k)
      (-get doc nil k)
      d))
  (put [this k v]
    (let [prev (get this k)]
      (-put doc nil k v)
      prev))

  protocols/IDocument
  (merge! [this src]
    (raw/AMmerge doc src)
    this)
  (clone [this]
    (let [result (check-result (raw/AMclone doc))
          item (raw/AMresultItem result)
          doc* (dt-ffi/make-ptr :pointer 0)
          _ (check-bool (raw/AMitemToDoc item doc*))
          newdoc (nth doc* 0)]
      (tech.v3.resource/track newdoc
                              {:dispose-fn
                               (fn []
                                 (raw/AMresultFree result))})
      (Document. newdoc)))
  (commit! [this]
    (.commit! this nil nil))
  (commit! [this message t]
    (let [message* (if message
                     (->AMstr message)
                     (dt-struct/map->struct :AMbyteSpan {:src 0 :count 0}))
          t* (when t
               (dt-ffi/make-ptr :int64 (java.time.Instant/ofEpochMilli t)))]
      (check-and-free-result
       (raw/AMcommit doc message* t*)))
    nil)
  (empty-change [this message t]
    (let [message* (->AMstr message)
          t* (when t
               (dt-ffi/make-ptr :int64 (java.time.Instant/.toEpochMilli t)))
          result (check-and-free-result
                  (raw/AMemptyChange doc message* t*))]
      nil))
  (fork [this]
    (.fork this nil))
  (fork [this heads]
    (check-and-free-result
     (raw/AMfork doc))
    nil)
  (get-actor-id [this]
    (with-item
      [item (raw/AMgetActorId doc)]
      (let [actor-id* (dt-ffi/make-ptr :pointer 0)
            _ (check-bool (raw/AMitemToActorId item actor-id*))

            bytespan (raw/AMactorIdBytes (nth actor-id* 0))

            buf (native-buffer/wrap-address (:src bytespan)
                                            (:count bytespan))]
        (native-buffer/clone-native buf))))
  ;; (get-heads [doc])
  ;; (load-incremental [doc src count])

  (save [this]
    (with-item
      [item (raw/AMsave doc)]
      (let [bytespan (dt-struct/new-struct
                      :AMbyteSpan
                      {:container-type :native-heap})
            _ (check-bool (raw/AMitemToBytes item bytespan))

            buf (native-buffer/wrap-address (:src bytespan)
                                            (:count bytespan))]
        (native-buffer/clone-native buf))))
  (set-actor-id [this actor-id]
    (let [result (check-result
                  (raw/AMactorIdFromBytes actor-id
                                          (native-buffer/native-buffer-byte-len actor-id)))
          item (raw/AMresultItem result)
          actor-id* (dt-ffi/make-ptr :pointer 0)
          _ (check-bool (raw/AMitemToActorId item actor-id*))

          actor-id (nth actor-id* 0)]
      (check-and-free-result
       (raw/AMsetActorId doc actor-id))
      (raw/AMresultFree result)
      nil))

  Object
  (equals [this other]
    (and
     (instance? Document other)
     (not=
      (zero?
       (raw/AMequal doc
                    (get-doc other))))))
  (toString [this]
    (str "#" `Document " " (pr-str (->clj* doc nil)))))


(defn ^:private map-put
  ([doc obj-id k v]
   (map-put doc obj-id k v (type v)))
  ([doc obj-id k v vtype]
   (-map-put doc obj-id k v vtype)))


(defn ^:private list-put
  ([doc obj-id k insert? v]
   (list-put doc obj-id k insert? v (type v)))
  ([doc obj-id k insert? v vtype]
   (-list-put doc obj-id k insert? v vtype)))


(defn doc
  ([]
   (->Document (create-doc nil)))
  ([actor-id]
   (->Document (create-doc actor-id))))

(defn load
  "Returns a new document loaded from `save`.
  
  `save` should be a dtype native-buffer. see `automerge/save`."
  [save]
  (let [result (check-result
                (raw/AMload save
                            (native-buffer/native-buffer-byte-len save)))
        item (raw/AMresultItem result)
        doc* (dt-ffi/make-ptr :pointer 0)
        _ (check-bool (raw/AMitemToDoc item doc*))
        newdoc (->Document (Pointer. (nth doc* 0)))]
    (tech.v3.resource/track newdoc
                            {:dispose-fn
                             (fn []
                               (raw/AMresultFree result))})
    newdoc))


(deftype SyncState [ptr]
  dt-ffi/PToPointer
  (convertible-to-pointer? [this]
    true)
  (->pointer [this]
    ptr))

(defn sync-state-init
  "Returns a new Synchronization State."
  []
  (let [result (check-result (raw/AMsyncStateInit))
        item (raw/AMresultItem result)

        sync-state* (dt-ffi/make-ptr :pointer 0)
        _ (check-bool (raw/AMitemToSyncState item sync-state*))
        sync-state (->SyncState (Pointer.
                                 (nth sync-state* 0)))]
    (tech.v3.resource/track
     sync-state
     {:dispose-fn
      (fn []
        (raw/AMresultFree result))})
    sync-state))

(defn sync-state-encode
  "Encodes a synchronization state as a dtype native buffer."
  [sync-state]
  (with-item
   [item (raw/AMsyncStateEncode sync-state)]
   (let [byte-span (dt-struct/new-struct
                    :AMbyteSpan
                    {:container-type :native-heap})
         _ (check-bool (raw/AMitemToBytes item byte-span))]
     (native-buffer/clone-native
      (native-buffer/wrap-address
       (:src byte-span)
       (:count byte-span))))))

(defn sync-state-decode
  "Decodes a dtype native buffer into a synchronization state."
  [buf]
  (let [result (check-result
                (raw/AMsyncStateDecode buf
                                       (native-buffer/native-buffer-byte-len buf)))
        item (raw/AMresultItem result)
         sync-state* (dt-ffi/make-ptr :pointer 0)
         _ (check-bool (raw/AMitemToSyncState item sync-state*))
         sync-state (->SyncState (Pointer. (nth sync-state* 0)))]
    (tech.v3.resource/track
     sync-state
     {:dispose-fn
      (fn []
        (raw/AMresultFree result))})
    sync-state))

(defn generate-sync-message
  "Generates a syncronization message (as dtype native buffer) for a peer
  based upon the given syncronization state.
  
  The synchronization is mutated in place.
  
  Returns a dtype native buffer with the message or nil no message needs to be sent."
  [doc sync-state]
  (with-item
    [item (raw/AMgenerateSyncMessage doc sync-state)]
    (let [val-type (raw/val-type->kw (raw/AMitemValType item))]
      (when (= val-type :val-type/sync-message)
        (let [sync-message* (dt-ffi/make-ptr :pointer 0)
              _ (check-bool (raw/AMitemToSyncMessage item sync-message*))
              sync-message (Pointer. (nth sync-message* 0))]
          (with-item
            [item (raw/AMsyncMessageEncode sync-message)]
            (let [byte-span (dt-struct/new-struct
                             :AMbyteSpan
                             {:container-type :native-heap})
                  _ (check-bool (raw/AMitemToBytes item byte-span))]
              (native-buffer/clone-native
               (native-buffer/wrap-address
                (:src byte-span)
                (:count byte-span))))))))))

(defn receive-sync-message
  "Applies the updates from the message to `doc` and updates
  the syncronization state."
  [doc sync-state sync-message]
  (with-item
    [sync-message-item (raw/AMsyncMessageDecode
                        sync-message
                        (native-buffer/native-buffer-byte-len sync-message))]
    (let [sync-message* (dt-ffi/make-ptr :pointer 0)
          _ (check-bool (raw/AMitemToSyncMessage sync-message-item sync-message*))
          sync-message (nth sync-message* 0)]
      (check-and-free-result
       (raw/AMreceiveSyncMessage doc sync-state sync-message))))
  nil)

(defn map-item? [o]
  (instance? MapItem o))
(defn list-item? [o]
  (instance? ListItem o))

(defn merge-doc-with-clj! 
  "Merges a clj datastructure with an automerge doc.
  Will mutate the doc in place. Tries to reduce the
  number of edits to the doc. Changes will be purely
  additive (ie. data in `doc`, but not in `m` will
  not be removed).
  
  `doc`: an automerge document
  `m`: a clojure map.
  "
  [doc m]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY
                 [doc m])]
    (when-let [[doc o] (peek q)]
      (let [q (pop q)]
        (cond
          (map? o)
          (recur
           (reduce
            (fn [q [k v]]
              (let [other (get doc k)]
                (cond
                  (and (map? v)
                       (map-item? other))
                  (conj q [other v])
                  
                  (and (instance? List v)
                       (list-item? other))
                  (conj q [other v])
                  
                  (= other v) q
                  
                  :else
                  (do
                    (put! doc k v)
                    q))))
            q
            o))
          
          (seqable? o)
          (let [size (count doc)]
            (recur
             (transduce
              (map-indexed vector)
              (completing
               (fn [q [i x]]
                 (if (>= i size)
                   (do (append! doc x)
                       q)
                   (let [other (nth doc i)]
                     (cond
                       (and (map? x)
                            (map-item? other))
                       (conj q [other x])
                       
                       (and (instance? List x)
                            (list-item? other))
                       (conj q [other x])
                       
                       (= other x) q
                       
                       :else (do (put! doc i x)
                                 q))))))
              q
              o)))
          
          :else (throw (ex-info "unexpected type"
                                {:o o}))))))
  nil)


(defn sync-doc-with-clj! 
  "Merges a clj datastructure with an automerge doc.
  Will mutate the doc in place. Tries to reduce the
  number of edits to the doc. Unlike `merge-doc-with-clj!`,
  excess keys and values with be removed from maps and lists.        
  
  `doc`: an automerge document
  `m`: a clojure map."
  [doc m]
  (loop [q (conj clojure.lang.PersistentQueue/EMPTY
                 [doc m])]
    (when-let [[doc o] (peek q)]
      (let [q (pop q)]
        (cond
          (map? o)
          (recur
           (do
             ;; remove all keys in doc that are not in o
             (doseq [[k v] doc]
               (when (not (contains? o k))
                 (delete! doc k)))
             
             (reduce
              (fn [q [k v]]
                (let [other (get doc k)]
                  (cond
                    (and (map? v)
                         (map-item? other))
                    (conj q [other v])
                    
                    (and (instance? List v)
                         (list-item? other))
                    (conj q [other v])
                    
                    (= other v) q
                    
                    :else
                    (do
                      (put! doc k v)
                      q))))
              q
              o)))
          
          (instance? List o)
          (let [size (count doc)]
            ;; remove all excess items at end of doc
            (let [to-remove (- size (count o))]
              (when (pos? to-remove)
                (dotimes [i to-remove]
                  (pop! doc))))

            (recur
             (transduce
              (map-indexed vector)
              (completing
               (fn [q [i x]]
                 (if (>= i size)
                   (do (append! doc x)
                       q)
                   (let [other (nth doc i)]
                     (cond
                       (and (map? x)
                            (map-item? other))
                       (conj q [other x])
                       
                       (and (instance? List x)
                            (list-item? other))
                       (conj q [other x])
                       
                       (= other x) q
                       
                       :else (do (put! doc i x)
                                 q))))))
              q
              o)))
          
          :else (throw (ex-info "unexpected type"
                                {:o o}))))))
  nil)




