(ns sazhet.ui
  "# UI Convenience functions

  Adapted from and inspired by `keechma.toolbox.ui`."
  (:require [clojure.string :as str]
            [keechma.ui-component :as ui])
  (:require-macros [cljs.core :refer [exists?]]
                   [sazhet.ui :refer [evt>]]))

;; ## Routes

(defn route>
  "Reads current route data. Optional `ks` specify a key-path within the
  route's data."
  [ctx & ks]
  (get-in (:data @(ui/current-route ctx)) ks))

(defn <route
  "Redirects to a route based on `route-data`. `route-data` can be
  specified as a map or as inline keys and values."
  ([ctx route-data]
   (ui/redirect ctx route-data))
  ([ctx k v & {:as route-data}]
   (<route ctx (assoc route-data k v))))

(defn <url
  "Constructs a `url` string from `route-data`. `route-data` can be
  specified as a map or as inline keys and values."
  ([ctx route-data]
   (ui/url ctx route-data))
  ([ctx k v & {:as route-data}]
   (<url ctx (assoc route-data k v))))

;; ## Subscriptions

(defn sub>
  "Returns a dereferenced reaction specified by `subscription` (a
  keyword) and optional inline `args`."
  [ctx subscription & args]
  @(ui/subscription ctx subscription args))

;; ## Components

;; ### Keechma

(defn <comp
  "Creates a Keechma component template from `data`. Valid keys for
  `data` are: :renderer, :component-deps, :subscription-deps. `data`
  can be specified as a map or as inline keys and values."
  ([data]
   (ui/constructor data))
  ([k v & {:as data}]
   (<comp (assoc data k v))))

;; ### Reagent

(defn comp>
  "Returns a Reagent component vector based on a Keechma component
  template specified by `key` and optional inline `args`."
  [ctx key & args]
  (apply vector (ui/component ctx key) args))

(defn map>
  "Returns a realized seq of Reagent components. For each item in `coll`,
  creates a Reagent component by calling `f` with the item as the
  first argument. Any additional arguments are the result of calling
  each `arg-fn` on the item. Each component is marked with a unique
  key for React via metadata. The unique key is produced by applying a
  `key-fn` to the item. The default `key-fn` is `:id`. To specify a
  different `key-fn`, provide it as `:map>/key-fn` metadata on
  `coll`."
  [f coll & arg-fns]
  (let [key-fn (:map>/key-fn (meta coll) :id)
        args-fn (if arg-fns (apply juxt arg-fns) (constantly nil))
        comp-fn (fn [item]
                  (with-meta
                    (apply f item (args-fn item))
                    {:key (key-fn item)}))]
    (seq (into [] (map comp-fn) coll))))

(defn comps>
  "Returns a realized seq of Reagent components. For each item in `coll`,
  creates a Reagent component by calling `comp>` with `ctx`, `key`,
  and the item as the first 3 arguments. Any additional arguments are
  the results of calling each `arg-fn` on the item. Each component is
  marked with a unique key for React via metadata. The unique key is
  produced by applying a `key-fn` to the item. The default `key-fn` is
  `:id`. To specify a different `key-fn`, provide it as `:map>/key-fn`
  metadata on `coll`."
  [ctx key coll & arg-fns]
  (apply map> (partial comp> ctx key) coll arg-fns))

;; ## Commands

(defn <cmd
  "Sends an asynchronous `command`. Gathers any additional `args` into a
  seq for transport."
  [ctx command & args]
  (ui/send-command ctx command args))

;; ## Events

(defn default-prevented?
  "Safe predicate that only checks defaultPrevented if it exists."
  [event]
  (when (exists? (.-defaultPrevented event))
    (.-defaultPrevented event)))

(defn prevent-default
  "Safe setter that only calls preventDefault if it exists."
  [event]
  (when (exists? (.-preventDefault event))
    (.-preventDefault event)))

(defn key>
  "Returns an event-handler that uses a KeyboardEvent's `key` value to
  dispatch it to another event-handler. The `handlers` mapping can be
  passed in as a map or as inline keys and values.

  Each entry in `handlers` associates a keyword with a handler. A
  keyword matches a KeyboardEvent `key` value if `(name keyword)` is
  either `key` or `(lower-case key)`. For example, the keywords
  `:Enter` and `:enter` both match the KeyboardEvent `key` value
  \"Enter\".

  On a match, returns the result of calling the matched handler on the
  event, else returns :evt>unhandled."
  ([handlers]
   (evt> event (let [key-name (.-key event)
                     exact-key (keyword key-name)
                     lower-key (keyword (str/lower-case key-name))]
                 (if-let [handler (or (handlers exact-key)
                                      (handlers lower-key))]
                   (handler event)
                   :evt>/unhandled))))
  ([k v & {:as handlers}]
   (key> (assoc handlers k v))))

(defn save>
  "Returns an event handler that calls `handler` on the event and saves
  the result in `atom`. If a `key-path` is provided, stores the value
  at the specified location within `atom`."
  ([handler atom]
   (evt> event (reset! atom (handler event))))
  ([handler atom & key-path]
   (evt> event (reset! atom (assoc-in @atom key-path (handler event))))))

(defn <value
  "Event handler that returns the event target's value."
  [event]
  (.. event -target -value))

(defn <checked
  "Event handler that returns the event target's checked status."
  [event]
  (.. event -target -checked))
