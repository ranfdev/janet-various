(import ./solid/solid :prefix "")

(var todos (source @[]))
(var v (live-view
    [:div
      [:h1 "test todo"]
      [:ul
        (map (fn [todo] [:li (todo :title)]) (@ todos))]]))

(let [s (source 0)]
  (var effect (listen (fn [] 
    (pp (@ s))
    (on-dispose (fn []
                  (print "disposing"))))))
  (update s + 3)
  (update s inc)
  (update s
    (fn [val] 
      (pp ["update nested (@ s)"])
      val))
  (dispose effect)
  (print "disposed")
  (update s inc)
  (update s inc))

(let [s (source 5)
      d (derived (* (@ s) (@ s)))]
  (listen (fn []
    (print "the derived signal is " (@ d))))
  (update s inc))


(let [s (source 2)]
  (listen (fn []
    (pp ["changed" (@ s)])))
  (batch
    (update s inc)
    (update s inc)
    (update s inc)
    (batch
      (update s inc)
      (update s inc)
      (update s inc))))