(ns sazhet.ui)

;; ## Events

(defmacro evt>
  "Wraps a seq of expressions (`body`) in an event handler. If the first
  item in `body` is a symbol, binds the event to it, otherwise the
  event is unavailable within `body`. Marks the event to request no
  further handling unless `body` evals to the namespaced keyword
  `:evt>/unhandled`."
  [& [f & r :as body]]
  (let [[event body] (if (symbol? f)
                       [f r]
                       [(gensym) body])]
    `(fn [~event]
       (when-not (default-prevented? ~event)
         (let [value# (do ~@body)]
           (when-not (= :evt>/unhandled value#)
             (prevent-default ~event))
           value#)))))
