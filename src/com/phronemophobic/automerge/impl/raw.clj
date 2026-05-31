(ns com.phronemophobic.automerge.impl.raw
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.native-buffer :as native-buffer]
            [tech.v3.datatype.graal-native :as graal-native]
            [com.phronemophobic.clong.gen.dtype-next :as gen]
            [tech.v3.datatype.graal-native :as graal-native]
            ;;[com.rpl.specter :as specter]
            )
  (:import
   java.io.File
   java.io.PushbackReader)
  (:gen-class))

(set! *warn-on-reflection* true)

(defn ^:private write-edn [w obj]
  (binding [*print-length* nil
            *print-level* nil
            *print-dup* false
            *print-meta* false
            *print-readably* true

            ;; namespaced maps not part of edn spec
            *print-namespace-maps* false

            *out* w]
    (pr obj)))

(def ^:private default-arguments
  ["-resource-dir"
   "/opt/local/libexec/llvm-22/lib/clang/22"
   "-isysroot"
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk"
   "-I/usr/local/include"
   "-internal-isystem"
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/local/include"
   "-internal-isystem"
   "/opt/local/libexec/llvm-22/lib/clang/22/include"
   "-internal-externc-isystem"
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include"
   "-internal-iframework"
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks"
   "-internal-iframework"
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/SubFrameworks"
   "-internal-iframework"
   "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/Library/Frameworks"])

(defn dump-api []
  (let [outf (io/file
              "resources"
              "com"
              "phronemophobic"
              "automerge"
              "api.edn")

        include-dir (io/file "/Users/adrian/workspace/automerge/rust/automerge-c/out/include/")
        ->path (fn [s]
                 (-> (io/file include-dir s)
                     File/.getCanonicalPath))

        clang-args (into (conj default-arguments
                               (str "-I" (File/.getCanonicalPath include-dir))
                               (->path "automerge-c/automerge.h"))
                         (mapcat (fn [header]
                                   ["-include" (->path header)]))
                         ["automerge-c/config.h"
                          "automerge-c/utils/stack_callback_data.h"
                          "automerge-c/utils/enum_string.h"
                          "automerge-c/utils/result.h"
                          "automerge-c/utils/stack.h"
                          "automerge-c/utils/string.h"])]
    (.mkdirs (.getParentFile outf))
    (with-open [w (io/writer outf)]
      (write-edn w
                 ((requiring-resolve 'com.phronemophobic.clong.clang/easy-api)
                  nil
                  clang-args)))))

(defn load-api []
  (with-open [rdr (io/reader
                   (io/resource
                    "com/phronemophobic/automerge/api.edn"))
              rdr (java.io.PushbackReader. rdr)]
    (edn/read rdr)))

;; dtype-next does not allow struct by value arguments
;; with members that are arrays
(defn adjust-amitems [api]
  (update api :structs
          (fn [structs]
            (into []
                  (map (fn [struct]
                         (if (not= :clong/AMitems (:id struct))
                           struct
                           (update struct
                                   :fields
                                   (fn [fields]
                                     (assert (=
                                              (:datatype (first fields))
                                              [:coffi.mem/array :coffi.mem/char 24]))
                                     (into []
                                           (map (fn [i]
                                                  {:name (str "u" i)
                                                   :datatype :coffi.mem/char}))
                                           (range 24)))))))
                  structs))))


(def ^:private fn-names
  #{"AMactorIdBytes"
    "AMactorIdFromBytes"
    "AMclone"
    "AMcommit"
    "AMcreate"
    "AMemptyChange"
    "AMequal"
    "AMfork"
    "AMgenerateSyncMessage"
    "AMgetActorId"
    "AMitemObjId"
    "AMitemToActorId"
    "AMitemToBool"
    "AMitemToBytes"
    "AMitemToCounter"
    "AMitemToDoc"
    "AMitemToF64"
    "AMitemToInt"
    "AMitemToStr"
    "AMitemToSyncMessage"
    "AMitemToSyncState"
    "AMitemToTimestamp"
    "AMitemToUint"
    "AMitemValType"
    "AMitemsNext"
    "AMkeys"
    "AMlistGet"
    "AMlistPutBool"
    "AMlistPutBytes"
    "AMlistPutCounter"
    "AMlistPutF64"
    "AMlistPutInt"
    "AMlistPutNull"
    "AMlistPutObject"
    "AMlistPutStr"
    "AMlistPutTimestamp"
    "AMload"
    "AMmapGet"
    "AMmapPutBool"
    "AMmapPutBytes"
    "AMmapPutCounter"
    "AMmapPutF64"
    "AMmapPutInt"
    "AMmapPutNull"
    "AMmapPutObject"
    "AMmapPutStr"
    "AMmapPutTimestamp"
    "AMmerge"
    "AMobjItems"
    "AMobjObjType"
    "AMobjSize"
    "AMreceiveSyncMessage"
    "AMresultError"
    "AMresultFree"
    "AMresultItem"
    "AMresultItems"
    "AMresultStatus"
    "AMsave"
    "AMsetActorId"
    "AMstr"
    "AMsyncMessageDecode"
    "AMsyncMessageEncode"
    "AMsyncStateInit"
    "AMsyncStateDecode"
    "AMsyncStateEncode"})


(def api (-> (load-api)
             (update :functions
                     (fn [fns]
                       (into []
                             (filter (fn [f]
                                       (contains? fn-names (:symbol f))))
                             fns)))
             (adjust-amitems)))

(def dtype-api
  (gen/api->library-interface
   ;; graal native image doesn't allow struct by value arguments or return values
   ;; we'll use libffi to call these functions
   (graal-native/if-defined-graal-native
    (update api :functions (fn [fns]
                             (into []
                                   (remove (fn [f]
                                             (or(#{:clong/AMitems
                                                   :clong/AMbyteSpan}
                                                 (:function/ret f))
                                                (some (fn [arg]
                                                        (#{:clong/AMitems
                                                           :clong/AMbyteSpan}
                                                         arg))
                                                      (:function/args f)))))
                                   fns)))
    api)))

(def dtype-structs (gen/api->structs api))
(doseq [[id fields] dtype-structs]
  (dt-struct/define-datatype! id fields))

(dt-ffi/define-library-interface 
 dtype-api
 :libraries ["automerge"])

(gen/def-enums api)

(defn ^:private normalize-str [s]
  (let [s (-> s
              str/lower-case
              (str/replace #"_" "-"))
        s (if (re-find #"^[0-9]" s)
            (str "_" s)
            s)]
    s))


(def val-type->kw
  (->> (:enums api)

       (filter (fn [enum]
                 (= "AMvalType" (:enum enum))))
       (map (juxt :value
                  (fn [enum]
                    (keyword "val-type"
                             (-> (subs (:name enum)
                                       (count "AM_VAL_TYPE_"))
                                 normalize-str)))))
       (into {})))

(def kw->val-type
  (into
   {}
   (map (fn [[k v]]
          [v (int k)]))
   val-type->kw))

(def obj-type->kw
  (->> (:enums api)

       (filter (fn [enum]
                 (= "AMobjType" (:enum enum))))
       (map (juxt :value
                  (fn [enum]
                    (keyword "obj-type"
                             (-> (subs (:name enum)
                                       (count "AM_OBJ_TYPE_"))
                                 normalize-str)))))
       (into {})))

(def kw->obj-type
  (into
   {}
   (map (fn [[k v]]
          [v (int k)]))
   obj-type->kw))



;; native image ffi does not support struct by value return types
;; but libffi does!
(graal-native/if-defined-graal-native
 (require '[com.phronemophobic.clj-libffi :as ffi])
 nil)

(defn dtype-ffi-fn->cljlibffi-code [fn-name {:keys [rettype argtypes doc]}]
  (let [argnames (into []
                       (map first)
                       argtypes)
        
        ->ffi-type (fn [arg-type]
                     (if (and (list? arg-type)
                              (= 'by-value (first arg-type)))
                       (second arg-type)
                       arg-type))
        ->ffi-arg (fn [arg-type arg-sym]
                    (if (= arg-type :pointer?)
                      `(if-let [x# ~arg-sym]
                         x#
                         (tech.v3.datatype.ffi.Pointer. 0))
                      arg-sym))]
    `(defn ~(symbol (name fn-name)) 
       ~@(when (seq doc)
           [doc])
       [~@argnames]
       (ffi/call ~(name fn-name) ~(->ffi-type rettype)
                 ~@(eduction
                    (mapcat
                     (fn [[arg-sym arg-type]]
                       [(->ffi-type arg-type) 
                        (->ffi-arg arg-type arg-sym)]))
                    argtypes)))))


(def libffi-api-functions 
  (graal-native/if-defined-graal-native
   (gen/api->library-interface 
    {:functions
     (into []
           (filter (fn [f]
                     (or (#{:clong/AMitems
                            :clong/AMbyteSpan}
                          (:function/ret f))
                         (some (fn [arg]
                                 (#{:clong/AMitems
                                    :clong/AMbyteSpan}
                                  arg))
                               (:function/args f)))))
           (:functions api))})
   nil))

(doseq [[fn-name fn-def] libffi-api-functions]
  (let [code (dtype-ffi-fn->cljlibffi-code fn-name fn-def)]
    (eval code)))


