(ns com.manigfeald.git-test
  (:require [clojure.test :refer :all]
            [com.manigfeald.git :refer :all]))

(deftest test-blob
  (let [sha (put-blob (.getBytes "Foo Bar"))]
    (is sha)
    (is (= "Foo Bar" (String. (.array (get-blob sha)))))))

(deftest test-tree
  (let [sha (put-blob (.getBytes "Foo Bar"))]
    (is (= {"foo" [:blob sha]}
           (get-tree (put-tree {"foo" [:blob sha]}))))))

(deftest test-commit
  (let [sha (put-tree {"foo" [:blob (put-blob (.getBytes "Foo Bar"))]})
        commit-sha (put-commit sha {:message "this is a commit1"})
        commit-sha2 (put-commit sha {:message "this is a commit2"
                                     :parent commit-sha})]
    (is commit-sha)
    (let [{:keys [message tree]} (get-commit commit-sha)]
      (is (= message "this is a commit1"))
      (is (= tree sha)))
    (let [{:keys [parent message tree]} (get-commit commit-sha2)]
      (is (= message "this is a commit2"))
      (is (= tree sha))
      (is (= parent commit-sha)))))

(deftest test-branch
  (let [sha (put-tree {"foo" [:blob (put-blob (.getBytes "Foo Bar"))]})
        commit-sha (put-commit sha {:message "this is a commit1"})]
    (put-branch commit-sha "some-branch")
    (is (= "this is a commit1"
           (:message (get-commit (get-branch "some-branch")))))))
