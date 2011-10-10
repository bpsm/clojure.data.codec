(ns clojure.data.codec.test-base64
  (:import org.apache.commons.codec.binary.Base64)
  (:use clojure.test
        clojure.data.codec.base64))

(defn rand-bytes [n]
  (->> #(byte (- (rand-int 256) 128))
    repeatedly
    (take n)
    (byte-array)))

(deftest correctness
  (doseq [n (range 1 100)]
    (is (let [input (rand-bytes n)
              a1 (encode input)
              a2 (Base64/encodeBase64 input)]
          (= (seq a1) (seq a2))))))

(deftest offset-correctness
  (doseq [n (range 1 100)]
    (doseq [off (range 1 n)]
      (is (let [input (rand-bytes n)
                len (- n off)
                a1 (encode input off len)
                input2 (byte-array len)
                _ (System/arraycopy input off input2 0 len)
                a2 (Base64/encodeBase64 input2)]
            (= (seq a1) (seq a2)))))))

(deftest test-dec-length
  (doseq [n (range 5)]
    (is (= n (dec-length (enc-length n)
                         (unchecked-remainder-int n 3))))))


(deftest round-tripability
  (let [round-trip (comp decode encode)]
    (doseq [n (range 1 100)]
      (is (let [input (rand-bytes n)]
            (= (seq input) (seq (round-trip input))))))))


