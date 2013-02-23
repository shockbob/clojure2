(def ranks [:ace 2 3 4 5 6 7 8 9 10 :jack :queen :king])
(def rankmap (zipmap ranks (range)))

(def suits [:spade :clubs :hearts :diamonds])
   
(def allcards (for [rank ranks suit suits] 
                {:rank rank :suit suit}))

(def names {:spade ["Weird Al","Dolly Parton","Adam Sandler","Tina Fey","Cowardy Lion", "Evil Witch",
                    "Chris Farley","Roseanne Barr","Chris Christie","Sarah Palin",
                    "Father Guido Sarduchi","Whoopi","McCoy"]
            :clubs ["Bruce Springsteen","Lady Gaga","Bruce Willis","Signourney Weaver","Scare Crow","Dorothy",
                    "Jet Li","Michelle Rodruiguez","Winston Churchill","Margaret Thatcher",
                    "Father Flanagan","Tasha Yar","Kirk"]
            :hearts ["Geddy Lee","Pink","Keannu Reeves","Sandara Bullock","Tin Man","Good Witch",
                     "Alfred Hitchcok","Elle Machherson","Barack Obama","Hillary Clinton",
                     "Dali Lama","Uhuru","Spock"]
            :diamonds ["Paul McCartney","Madonna","Michael Douglas","Kathleen Turner","Wizard","Auntie Em",
                       "King Henry VIII","Marie Antionette","Mitt Romney","Ann Romney",
                       "John Paul II","Deanna Troi","Picard"]})

(def allcards (map (fn [m]
                     (assoc m :name ((names (m :suit)) (rankmap (m :rank)))))
                allcards))
(def allcards (shuffle allcards))
(def names (vec (map (fn [m] (:name m)) allcards )) )
(def cards (map (fn [m] (str (m :rank) " " (m :suit))) allcards))
(def allstrs (vec (interleave cards names)))

(def index (atom -1))
(defn nextindex []  (reset! index (mod (inc @index) (count allstrs))))

(import '(javax.swing JLabel JFrame JButton JPanel))
(import '(java.awt BorderLayout))
(import '(java.awt.event ActionListener))

(defn nextname [] (allstrs (nextindex)))
(def label (new JLabel (nextname)))
(def button (new JButton "Next"))
(def act (proxy [ActionListener] []
           (actionPerformed [event]
             (doto label (.setText (nextname))))))
(doto button (.addActionListener act))
(def frame (new JFrame))
(def pane
   (doto
    (new JPanel)
    (.setLayout (new BorderLayout))
    (.add  label BorderLayout/NORTH)
     (.add button BorderLayout/SOUTH)))
  (def frame
  (doto
    (new JFrame)
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.add pane)
    (.pack)
    (.setVisible true)))