(ns fancy-split.core
  (use [clojure.core.match :only [match]]))

(declare compile-rule)

(defn- compile-fields [fields value rule]
  (let [rule-c (compile-rule rule)
        predicate (if (instance? java.util.regex.Pattern value)
                    #(not (empty? (re-find value %)))
                    #{value})]
    (fn [m] (if (some predicate (map m (or fields (keys m)))) (rule-c m) []))))

(defn compile-juxt [rules]
  (let [rules-c (map compile-rule rules)]
    (fn [m] (remove empty? ((apply juxt rules-c) m)))))

(defn- string-or-regex? [value]
  (or (string? value) (instance? java.util.regex.Pattern value)))

(let [or-symbol (symbol "|")
      and-symbol (symbol "&")
      thread-symbol (symbol "->")
      any-symbol (symbol "any")]
  (defn compile-rule [rule]
    ;; return the result as a set of strings, which does not contain nil
    (comp #(disj % nil) (partial into #{})
          (match rule

                 ;; nil-rules are ignored
                 nil
                 (fn [msg] [])

                 ;; function calls
                 (f :when ifn?)
                 f
                 
                 ;; example: "mail.private"
                 (s :when string?)
                 (fn [msg] [s])

                 ;; example: ([:to :from] "foo@bar.com" "mail.private")
                 ([(fields :when coll?) (value :when string-or-regex?) r] :seq)
                 (compile-fields fields value r)

                 ;; example: (any "foo@bar.com" "mail.private")
                 ([any-symbol (value :when string-or-regex?) r] :seq)
                 (compile-fields nil value r)

                 ;; example: (:to "foo@bar.com" "mail.private")
                 ([(field :when keyword?) (value :when string-or-regex?) r] :seq)
                 (compile-fields [field] value r)
                 
                 ;; example: (| rule1 rule2 rule3)
                 ;; (take first matching rule)
                 ([or-symbol & r] :seq)
                 (comp first (compile-juxt r))

                 ;; example: (& rule1 rule2 rule3)
                 ;; (take all matching rules)
                 ([and-symbol & r] :seq)
                 (comp (partial reduce into #{}) (compile-juxt r))

                 ;; threading
                 ;; TODO

                 ))))