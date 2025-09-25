(var signal-id 0)
(defn next-signal-id []
  (set signal-id (+ signal-id 1))
  signal-id)

(var effect-id 0)
(defn next-effect-id []
  (set effect-id (+ effect-id 1))
  effect-id)

(defn source [value]
  (let [s @{:tag :solid/source
            :id (next-signal-id)
            :value value
            :dependents @{}}]
    s))

(defmacro derived [expr]
  ~(fn [] ,expr))

(defn @ [signal]
  (case (type signal)
    :table (do 
      (when-let [effect (dyn :effect)]
      # (print "signal read " effect)
      (put-in effect [:depends-on (signal :id)] signal)
      (put (signal :dependents) (effect :id) effect))
    
      (signal :value))
    :function (signal)))


(defn consume-batch [batch]
  (each effect (values batch)
    ((effect :f))))

(defn update [signal f & args]
  (let [dependents (signal :dependents)]
    (put signal :value (f (signal :value) ;args))
    (if-let [batch (dyn :batch)]
      (merge-into batch dependents)
      (consume-batch dependents))))

(defmacro batch [& exprs]
  (with-syms [$batch]
    ~(let [$batch @{}]
      (defer (do
              (if-let [parent-batch (dyn :batch)]
                (merge-into parent-batch $batch)
                (consume-batch $batch)))
        (with-dyns [:batch $batch]
          ,;exprs)))))
  

(defn- reset-tracked [effect]
  (array/clear (effect :on-dispose))
  (each signal (effect :depends-on)
    (put (signal :dependents) (effect :id) nil)))

(defn listen [f &opt options]
  (let [effect @{:tag :effect
                 :id (next-effect-id)
                 :depends-on @{}
                 :on-dispose @[]}
        scopedf (fn []
                  (reset-tracked effect)
                  (with-dyns [:effect effect]
                    (f)))]
    (put effect :f scopedf)
    (merge effect {:first-value (scopedf)})))

(defn dispose [effect]
  (each dispose-f (effect :on-dispose)
    (dispose-f))
  (reset-tracked effect))

(defn on-dispose [f]
  (let [effect (dyn :effect)]
    (array/push (effect :on-dispose) f)))


(defn- effect-wrapper [expr]
  (cond 
    (and (tuple? expr) (= (tuple/type expr) :parens)) ~(fn [] ,expr)
    (indexed? expr) (walk effect-wrapper expr)
    (dictionary? expr) (table ;(mapcat (fn [[k v]] [k v]) (pairs expr)))
    :default expr))

(effect-wrapper ~[:div
                    [:h1 "Title1"]
                    [:h2 (@ title)]
                    [:div 
                      {:class "nice"}
                      (if true 2 3)]])

# (effect-wrapper ~[:div {:class "ok"}])


(defn render-view-full [ui]
  (pp (type ui))
  (cond 
    (= (type ui) :function) (ui)
    :default (walk render-view-full ui)))

# (render-view-full [:h1 (fn [] (+ 2 2))])


(defmacro live-view [uiexpr]
  "Takes a tree of hiccup elements, wraps expressions having (@ x) to (fn [] expr).
  Solid.js has a compiler doing a similar transformation, every time a function call
  appears in JSX"
   (effect-wrapper uiexpr))

