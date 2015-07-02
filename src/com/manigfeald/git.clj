(ns com.manigfeald.git
  (:require [clojure.java.shell :as shell]
            [clojure.string :as string]
            [clojure.java.io :as io])
  (:import (java.io ByteArrayOutputStream)
           (java.nio ByteBuffer)))

(defn sh [& args]
  (let [result (apply shell/sh args)]
    (assert (zero? (:exit result)))
    result))

(defn get-blob
  "given the sha of a git blob object, get the contents as a
  ByteBuffer"
  [sha]
  (with-open [i (-> (ProcessBuilder.
                     ^"[Ljava.lang.String;"
                     (into-array String ["git" "cat-file" "-p" sha]))
                    (.start)
                    (.getInputStream))
              o (ByteArrayOutputStream.)]
    (io/copy i o)
    (ByteBuffer/wrap (.toByteArray o))))

(defn put-blob
  "given bytes or an inputstream, store the data as a blob in git and
  return the sha"
  [data]
  (-> (sh "git" "hash-object" "--stdin"  "-w" :in data)
      (:out)
      (string/split #"\n")
      (first)))

(defn put-tree [m]
  (-> (sh "git" "mktree"
          :in
          (with-out-str
            (doseq [[k [type sha]] m
                    :let [type (name type)]]
              (printf "%s %s %s\t%s\n"
                      (case type
                        "tree" "040000"
                        "blob" "100644")
                      type sha (name k)))))
      (:out)
      (string/split #"\n")
      (first)))

(defn get-tree [sha]
  (into {}
        (for [^String line (.split
                            ^String (:out
                                     (sh "git"
                                         "ls-tree"
                                         sha))
                            "\n")
              :when (not (empty? line))
              :let [idx (inc (.indexOf line " "))
                    line (subs line idx)
                    idx (inc (.indexOf line " "))
                    type (subs line 0 (dec idx))
                    line (subs line idx)
                    idx (inc (.indexOf line "\t"))
                    sha (subs line 0 (dec idx))
                    name (subs line idx)]]
          [name [(keyword type) sha]])))

(defn put-commit [sha {:keys [message parent]}]
  (-> (apply sh
             (concat
              ["git" "commit-tree" sha]
              (when parent
                ["-p" parent])
              [:in message]))
      (:out)
      (string/split #"\n")
      (first)))

(defn get-commit [sha]
  (loop [[c & cs] (->> (sh "git" "cat-file" "-p" sha)
                       (:out))
         headers []
         buffer []]
    (if (= c \newline)
      (let [headers (conj headers (apply str buffer))
            buffer []]
        (if (= (first cs) \newline)
          (into {:message (.trim ^String (apply str (rest cs)))}
                (for [^String header headers
                      :let [idx (.indexOf header " ")
                            type (subs header 0 idx)
                            value (subs header (inc idx))]]
                  [(keyword type) value]))
          (recur cs headers buffer)))
      (recur cs headers (conj buffer c)))))

(defn put-branch [sha name]
  (sh "git" "update-ref" (str "refs/heads/" name)
      sha)
  nil)

(defn get-branch [name]
  (-> (sh "git" "show-ref" name)
      (:out)
      (string/split #"\s")
      (first)))
