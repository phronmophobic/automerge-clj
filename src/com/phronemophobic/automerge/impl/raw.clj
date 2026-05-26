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

(def default-arguments
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

(def api (-> (load-api)
             (adjust-amitems)))

(def dtype-api (gen/api->library-interface api))
(def dtype-structs (gen/api->structs api))
(doseq [[id fields] dtype-structs]
  (dt-struct/define-datatype! id fields))

(dt-ffi/define-library-interface dtype-api
                                 ;; :symbols
                                 ;; '#{AMsplice}
  :libraries
  (graal-native/if-defined-graal-native
   []
   ["automerge"]))



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