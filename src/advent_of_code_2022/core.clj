(ns advent-of-code-2022.core
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [advent-of-code-2022.day7 :as day7]
            [advent-of-code-2022.day8 :as day8]
            [advent-of-code-2022.day11 :as day11]))



(defn sum-it
  [x]
  (->>
    x
    (map #(Integer/parseInt %))
    (apply +)))



(defn day1
  []
  (let [
        input (->>
                "resources/day1.data"
                slurp
                str/split-lines
                (partition-by #(= "" %))
                (filter #(not= '("") %))
                (map sum-it))
        question1 (apply max input)
        question2 (->> input
                       (sort-by -)
                       (take 3)
                       (apply +))]

    [question1 question2]))


(defn- calculate-score
  [line]
  (let [
        [opponent me] (str/split line #" ")
        score (case me
                "X" 1
                "Y" 2
                "Z" 3)]
    (if
      (or
        (and (= opponent "A") (= me "X"))
        (and (= opponent "B") (= me "Y"))
        (and (= opponent "C") (= me "Z")))
      (+ score 3)
      (if
        (or
          (and (= opponent "A") (= me "Y"))
          (and (= opponent "B") (= me "Z"))
          (and (= opponent "C") (= me "X")))
        (+ score 6)
        score))))


(defn calculate-score-2
  [s]
  (let [rpc {:rock     1
             :paper    2
             :scissors 3}
        xyz {"X" 0
             "Y" 3
             "Z" 6}
        [opponent me] (str/split s #" ")

        score (xyz me)

        mc (case opponent
             "A" (case me
                   "X" :scissors
                   "Y" :rock
                   "Z" :paper)

             "B" (case me
                   "X" :rock
                   "Y" :paper
                   "Z" :scissors)
             "C" (case me
                   "X" :paper
                   "Y" :scissors
                   "Z" :rock))]
    (+ score (rpc mc))))

(defn day2
  []
  (let [question1 (->>
                    "resources/day2.data"
                    slurp
                    str/split-lines
                    (map calculate-score)
                    (apply +))

        question2 (->>
                    "resources/day2.data"
                    slurp
                    str/split-lines
                    (map calculate-score-2)
                    (reduce +))]

    [question1 question2]))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(defn- split-in-half
  [sack]
  (let [halfed (/ (count sack) 2)
        [comp-1 comp-2] (split-at halfed sack)]
    (set/intersection (set comp-1) (set comp-2))))

(defn- get-intersection
  [[fst sec third]]
  (set/intersection (set fst) (set sec) (set third)))

(defn day3
  [file-name]
  (let [day3-scores (zipmap
                      (concat (char-range \a \z) (char-range \A \Z))
                      (range 1 53))
        question1 (->>
                    file-name
                    slurp
                    str/split-lines
                    (map split-in-half)
                    (map #(get-in day3-scores %))
                    (reduce +))
        question2 (->>
                    file-name
                    slurp
                    str/split-lines
                    (partition 3)
                    (map get-intersection)
                    (map #(get-in day3-scores %))
                    (reduce +))]
    [question1 question2]))


(defn day4
  [file-name]
  (let [
        find-subsets (fn [ranges]
                       (let [range-1 (range (-> ranges
                                                first
                                                Integer/parseInt)
                                            (->> ranges
                                                 second
                                                 Integer/parseInt
                                                 inc))
                             range-2 (range (->
                                              ranges
                                              (nth 2)
                                              Integer/parseInt)
                                            (-> ranges
                                                last
                                                Integer/parseInt
                                                inc))

                             set-1 (set range-1)
                             set-2 (set range-2)]

                         (or (set/subset? set-1 set-2)
                             (set/subset? set-2 set-1))))

        find-overlaps (fn [ranges]
                        (let [range-1 (range (-> ranges
                                                 first
                                                 Integer/parseInt)
                                             (->> ranges
                                                  second
                                                  Integer/parseInt
                                                  inc))
                              range-2 (range (->
                                               ranges
                                               (nth 2)
                                               Integer/parseInt)
                                             (-> ranges
                                                 last
                                                 Integer/parseInt
                                                 inc))

                              set-1 (set range-1)
                              set-2 (set range-2)]
                          (set/intersection set-1 set-2)))
        question1 (->>
                    file-name
                    slurp
                    str/split-lines
                    (map #(str/split % #"[-,]"))
                    (filter find-subsets)
                    count)

        question2 (->>
                    file-name
                    slurp
                    str/split-lines
                    (map #(str/split % #"[-,]"))
                    (map find-overlaps)
                    (filter #(not= 0 (count %)))
                    count)]
    [question1 question2]))


(defn day6
  [test-string]
  (let [
        lenght-of-block 14
        parts (partition lenght-of-block 1 test-string)
        lenghts (map (fn [block]
                       (-> block
                           set
                           count))


                     parts)

        index (.indexOf lenghts lenght-of-block)
        question1 (+ lenght-of-block index)]

    question1))

(defn main
  []
  (day6 "lrgrvgvttzmtmtgglmgmccpclppvdvtvvllvggvrggbwwlzlmzzbppnvpnvppcjjzhjhthnhjnhhhndhnnnsbnnhzzvhhplplzlrzzgpzpwzpwwsvsjvjfvvphpspwswrswscwscwsscffspsbbjjcjwjrwwtgwwgswswwzbzddqnnpqnpnqppwzwszsnsjjpddhvvcbbhhpzzlpzlzppfpvvmcmvvflfttrltrlldlglbgblltqtffrtrwrzwwzmzwmwwlzzhttwzzwnnmrrcdrdjrjqjvqvvjzzgccrllhmhzzfnfwwtzwzwpwhhdjhhmzzbbvggzdzccbzbbpcpqccjbcbppsttdjdnjnppjjnmmszmzgzddtctvctcvttgtbbzqqggnmmdllvdvmvzzhfffzvfvtfvtvwwcnwnvwwbccggjcjqcqcbcrrppdqppdzpzqppttjhjdjqjppzgzjjpllwrrbttrvvzzbhzzqppndppwqppnrpnnttfwttsrrgprggmtmhmzhzczwzmwzwrwqwrrrdqrrvssnlngnppfqqgbgjjcttbgtbtmtctmcmcmgmsgsffhghqhbbvtbbtltmltlnlpnngcnggbngbnnzgzccgcpgcpcjppnnzjzdjdggzjzljjhnncgcjcscfctcvttvqtqmqjjsqjqpqfqhqmmlvvmppfrfjjngnnfllrlhhppcjcbjcctgcgtcgcvgvffqfcfpcpdpffrbrvbvnnphpqpfqqtnttmtgtlgtgzttnvvpwvwvcwcfwcwmccwlclqlflpflplwpllndlltlqtlqqmqnqmnqnvqvrrtddqndnrdnnpzprrqnnggvqvhvpvptvvvzwzrwwscsqqmcmttbgtgpptzptzzvszvzdvvtsscbbrpptssltssztszttlvlqljlgljlhhwvhwvvqhvqhqrhqqcnqccnbcbppbffzqfqsfspsqsjjrhjjchcmhmnhmmzjmjmfjmmsbsvvgcggtdgghchrrpnnrttnthtdtmmhmdmppmgpgllrwlrwlwvvlmlglppzttsvsbsnbncnjnffddzcddbzzbzgbghhhtltwtggljjggsdswwpmmfhfsfvfrrgmrgrfggvzzbnbttwqqdcdppqcqpcpqpjqpjpbbgjbgjjfwfwpfpgpzgzmzgzdzzpwzwqqjqfqllgrgjjfvvqnvncntngnhgnhgnnzvvbsbmbqmqwmqwwhbwhhsccvhcclncnqccnvnzvvdgvgnvnttmbbhccwgwttlwtwqttqcqmcqcdcmmjpmmjsjhhprrnnqddjwdjjvvhvgvssthhnfhnnntfthhtggthhbrbrjbbjfbjjrgrsrjrqqqfwflfclflnnnnvggfqgqzzbbvttfcfvcvsswvssnzndndvnvqqznnrjnnsmmptmppncpchcctwtbbgbqqjqtqsqfsfvfvnvmvzzpgzppdzdvdqdjdnjnttvvjbbzrzqrqwrqrbqrqsqpspjssnqnpqqnjndjjzmmvbbrqrccrffhwhggbttpnpphwhhmrrndrnddzqzzfbfwbwnwtwjjwjmjsjcjgcjjfcftcffvpvwwbffgzgnnlfffnddtdbdlbbcjbjmmfpfzfbbwbdwwfmfpmmfjfffvzvdvvhrvrcvcscjjpfjjnfnzzrtrpphtppzrppwhhphthltlllttghgwwvlwlflhldlzzmbzzjppnwppvlplqqbtbwwccswccqzzjhjbbhbnhnshnsslmmlqqjfjrjjmvvhpjqhzqffhsdsbwpjvgpvmbfqltrmpnwfcptpfmtjcpbzfldbhcmzchshrlbjgggrfjcqhzqqvbzsczmbgqmzqmltlrtlbnsfvmlhbbcqbbltjpdrpznrglshvgdnqwlhthghvtbffddcjwgdzfswzbppjtdhstcqqmvzmjrvfjbhmrznwqczdjjclnhbmtdvvzwttwnrlfqwpglpcppdwdcvfqpqfnmbvzvmqlmnlgnrsqdjvtsftgnlrtzsrcqhltmhzhpmzqqfqrjwhqfnqdtnshwgfhcpjrlplnqczdlntnhsczrgfhflsfbmftsbptflqbpwblrfnfzvqtpblftmscpzgdhhsbdbjhqclnptwtmhbbfglmvwnbqgvqhmmswwjpfwqjbvznmcpdzcvbzjmfqnwstvvtdnlvnpznnblfqzjjrjgnsbtmmbjzsvmgwddtnzcvhvtdrmjgtcrjzznrssscrzcfbfpgpnpppsqcqpccnbdjnwrbvhrcwgqncjrzbdhzqpfhqbnvbfrzmlfbfvtpggrtdswnvlsvpjsmfchhpbbszbnqqfrmhpqzdjhmhmnnmplbtrpgphvvqdfbcfnrfrbfbtshlmlfltjnbmggqntvhdnlvtcvlhmlrlfzfrqmlwqzrdghvdvtsqvmpdjrjclmlmgjqwzzldnzvfmwmrrnfghsvpcwjdtlnrhpjczwpgfbhpnmcbpthsndfflbjhnlwdbbmlttfqcmswvppslptgzbvfgppvpnhjccrpgrpwtngmmccjghhcwddmnglschnpjwqtrtsvggnpzvsqshfvcnhptphtlmqmpznfzwvbnhwpsfwvpflsdjcjgfzjprbbfzgdbmrjgwrgfdphghrhnpvfncrdzcwtthmqtdwlhjsdthqpzhbjpgggndtrmwvcsqhzrzwbhtqsqthvqncprvnpsrlpvlvcjrcflhbdhrfthlfnqbzbmvlvhmbjnbbjhpjwlfflfhpfwcwnnsljthvzwprqjmgpldlzjnjtjfjrgnrpzpvzfcsrprbjhwnmccwhppjrlnndjdjzqwpcwnvqwgmnwbrjqqvbplvsncnmdfrbhrrhghfllhrghzmlnltgdsqlgbvnlchgcbqlpqptdwmsjpqrprlhqmstzjfnzgbgvlfshwpcrgzcqmmfwvhwlsdvplmdgrtfrjwpfvhnjqdbwsfcqhchstlzfpdljgvcqsfcnqccnpmvsqbmwjtzwhpglhbjwzmvgqwjhvwfhnlbtsgljzmlldcpjwdcfppmnmphdmhpmdqwwtjtrdhlrjlvzgpbcgvwcmtclgpqwhtpbdtdbdscfzbrzmgjlbppcnvphphfnvzdzzlvfsvsgbgqcnlqwmtcrpwzcvnmnvtmcdsstvqpqzdpvtdsbvtwhdvgzqmzvwlspgbwmlnsrqdqnjwrllncflqsrzdqtjqvpnpjlqfwqtlqfqwlltszcwtpmjtldjgvmvptpmzqhwmlvjgnntpvcslmhlhdbjtjjnvsbnzwtdclwbzrvlqzjljtbdjvwgbwcltvnbhfvtgqrbmzbbfvldhmdvfvtlqglnblfmmpjqmzlnfjltsqdrgmlhbhngrrmhnjndggsdcfmtssmmtmzvhzrmwjsqjcvbsgqgtvdmvqlvlrvglrtlshfdmfrmljjggwjbcsztsjmjftcbbjwrmgqvssrvtgzcgthtlgsjspfmdgwptjdrbswqlpfsbtjlnhllmjpbfhgpfcprpdnqqvqdmcbqhbcqtstvnjdzwzwvhhwmcvcfbdwczpwpdhvnstjnbblbprzsccmwrzgfhmrpvzfztvsrtncdhzhptpfqtnqwvqtwdpvcqztgjgrcbdnvqftphtfbtqdhrffdrdmwsbpvhshzvjbvsrljnzddmmfgcnfdssvzdbsfwmfjsdnslbrqsqfwfqbqszjwvgcjbhrfjcnlfhzvhcbbbpmhhvjdtgrqlcchqtvnhlrgtssllvgcdjrlzlzfbrrrvwvvcgfjdlpscsqljmmwmvwnvrgdmgcbvmwmgprbfrbgptlfjbhrmczwrzwbdhdvtgvldnzfgcngdfhbgqsfzlrbwbvdflrrsrcwthjzvgmdtndgtsjtswfbdqvcjtsdvrvqpmmdlghsdbzplgpfnstplpjdvttgzmnhssftqcqjvdvvdrmltbrpsjvqwbljrqrtqldzbwzznsdstvmdzbrvvtgrrphmbrzwnjbmqvfhljcdlbzqtcbjsfqdqc"))


(* 34 12)
