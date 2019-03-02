;; -----------------------------------------------------------------------------
;; Lisp Deutsch ...
;; (c) 2018 Jens Kallup - non-profit Software
;; Alle Rechte vorbehalten !!!
;;
;; nur fuer Testzwecke - nicht fuer den kommerziellen Gebrauch !!!
;; -----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; ein paar kürzel, zur Speicherreduktion ...
;; -----------------------------------------------------------------------------
(defvar m 1)    ;; männlich
(defvar w 2)    ;; weiblich
(defvar s 4)    ;; m + w
(defvar d 8)    ;; sächlich
;;
(defvar männlich m)
(defvar weiblich w)
(defvar sächlich s)

;; ----------------------------------
;; meist deutsche programmierung ...
;; ----------------------------------
(deftype deutsch-fall       () '(member :nominativ :genitiv  :dativ :akkusativ))
(deftype deutsch-geschlecht () '(member :männlich  :weiblich :sächlich))
(deftype deutsch-wortart    () '(member :singular  :plural   :neuter  ))      ;; singular / plural
(deftype deutsch-schrift    () '(member :deutsch   :trennung))
(deftype deutsch-sprache    () '(member :deutsch            ))
;;

;; --------------------------------------------
(defun printem (&rest args)
    (dolist (el args) 
        (princ el) 
        (princ #\SPACE)))

(defstruct erde-deutschland name hauptstadt einwohner)
(defparameter *erde-deutschland*
    (make-erde-deutschland
    :name "Deutschland"
    :hauptstadt "Berlin"
    :einwohner 1000))
;;
(defstruct planet name)
(defstruct land name hauptstadt planet)
(defstruct bio land geboren b gestorben leben s)

(defstruct pers-adamkraft name text bio)
(defparameter *pers-adamkraft*
    (make-pers-adamkraft
    :name "Adam Kraft"
    :text "deutscher Bildhauer und Baumeister (15. Jh)"
    :bio (make-bio
        :land *erde-deutschland*
        :geboren "zwischen 1455 und 1460"
        :b "Schwabach"
        :gestorben "21.01.1509"
        :s "Nürnberg"
        :leben (list
            "Adam Kraft wurde in Nürnberg als Sohn eines Schreiners geboren. "
            "Bis auf die damals üblichen Wanderjahre, die er in Ulm und "
            "Straßburg verbrachte, wirkte er nur in Nürnberg und Umgebung. "
            "Über sein Leben ist nicht viel bekannt. "
            ""
            "Abgesehen von seinen Werken gibt es in den 18 Jahren seiner "
            "Nürnberger Tätigkeit nur sehr wenige Zeugnisse über ihn. "
            "Er erfuhr keine besonderen öffentlichen Ehrungen, erhielt keine Aufträge "
            "von Fürsten, wurde auch nicht in das Kollegium der Genannten des Größeren "
            "Rates berufen. "
            ""
            "Trotz seiner zwei Ehen blieb er wohl kinderlos. "
            "Er hatte nur einen kleinen Betrieb mit zwei oder drei Arbeitern. "
            "Trotz zahlreicher Aufträge befand er sich zeitlebens in finanziellen Nöten."
            ""
            "Adam Kraft wurde in Schwabach bei Nürnberg am 21. Januar 1509 beigesetzt. "
            "Dort ist das Adam-Kraft-Gymnasium nach ihm benannt. "
            "Zu Ehren Adam Krafts wurde seine Büste in der Ruhmeshalle in München aufgestellt. "
            "Auch die Adam-Kraft-Realschule in der Südstadt Nürnbergs ist nach ihm benannt. "
            "Mit dem Bau der Düsseldorfer Kunstakademie wurde sein Name unter den bedeutenden "
            "Bildhauern im Fries der Fassade an der Westseite (Rheinseite) eingemeisselt. " ) 
    )))
;;
(defstruct pers-adamopel name text)
(defparameter *pers-adamopel*
    (make-pers-adamopel
    :name "Adam Opel"
    :text "deutscher Industrieller (19. Jh)"))
    
(defstruct ref-adam kurzform ursprung bedeutung refer namensträger sonstiges)
(defparameter *ref-adam*
    (make-ref-adam
    :kurzform ""
    :ursprung (list
        "aus der Bibel übernommen"
        "Nach der Bibel war Adam der erste, von Gott erschaffene Mensch, aus Ackerboden gebildet und mit Lebensodem erfüllt."
        "männlicher Vorname hebräischen Ursprungs"
        "eigentlich Mensch")
    :bedeutung "s. Ursprung"
    :refer ""
    :namensträger (list
        *pers-adamkraft*
        *pers-adamopel*
    )
    :sonstiges ""
))

(defstruct nounlist-mensch männlich weiblich)
(defparameter *nounlist-mensch*
    (make-nounlist-mensch
    :männlich (list
    (list "Adam" *ref-adam*) "Adolf" "Adolph" "Adrian" "Albrecht" "Albert" "Albero" "Alberich"
    (list "Alex") "Alexander" "Alexis" "Alfonso" "Almar" "Alred" "Alfried" "Alois"
    (list "Alwin") "Amadeus" "Alfons" "Ambrosius" "Anton" "Andreas" "Andrew"
    (list "Andy")
    (list "Hans") "Hänschen"
    (list "Ingo")
    (list "Ingolf")
    (list "Karl") "Kevin"
    (list "Lars") "Leon"
    (list "Jan") "Janosh" "Jens" "Joseph"
    (list "Otto") "Ottfried"
    (list "Ralf") "Ralph"
    (list "Werner") "Wilfried" "Wolfgang")
;;
    :weiblich (list
    "Ada" "Adda" "Adele" "Agathe" "Agnes" "Adriane" "Alexandra" "Alida" "Alice"
    "Aloisa" "Altraud" "Adeltraud" "Alma" "Amalia" "Anastasia" "Anna" "Amata"
    "Alix" "Aline" "Angelika" "Angelina" "Angela" "Andrea" "Angelika"
    "Anita" "Anja" "Anka" "Anke"
    "Maria")
    ))

(defparameter *nounlist-männlich-singular-nom* '(
;;    *nounlist-mensch*
    "Anruf" "Anzug" "Apfel" "April" "Arm" "Arzt" "August" "Ausweis"
    "Bahnhof" "Balkon" "Baum" "Berg" "Beruf" "Bildschirm" "Bus"
    "Computer" "Dezember" "Dienstag" "Durst" "Drucker" "Eintrittskarte"
    "Einwohner" "Fahrschein" "Februar" "Fernseher" "Finger" "Flughafen"
    "Flur" "Frühling" "Füller" "Fuß" "Fußboden" "Garten" "Gast" "Geburtstag"
    "Hafen" "Hamburger" "Herbst" "Herr" "Himmel" "Hut" "Hunger" "Januar"
    "Juli" "Juni" "Kaffee" "Kakao" "Keller" "Kellner" "Kleiderhaken" "Koch"
    "Kongak" "Kuchen" "Kugelschreiber" "Kuchen" "Kunde" "Laden" "Lehrer"
    "Locher" "Löffel" "Mai" "März" "Mann" "Markt" "Marktplatz" "Monitor"
    "Name" "November" "Oktober" "Opa" "Park" "Pass" "Passant" "Platz"
    "Projektor" "Pullover" "Radiergummi" "Regen" "Rock" "Schinken" "Schlüssel"
    "Schnaps" "Schnee" "Schrank" "September" "Sessel" "Sommer" "Star"
    "Strumpf" "Stuhl" "Supermarkt" "Tag" "Tee" "Teppich" "Test" "Tisch"
    "Tourist" "Urlaub" "Vater" "Wagen" "Wein" "Wind" "Winter" "Wunsch"
    "Zeiger" "Zucker" "Zug" "Zuschauer"
    "Vogel" "Mensch" "Staat" "Hund" "Osten" "Sturm" "Sand" "Euro"))

(defparameter *nounlist-männlich-plural-nom* (list
    "Anrufe" "Anzüge" "Äpfel" "Ausweise" "Bahnhöfe" "Bäume"
    "Vögel" "Menschen" "Staaten" "Hunde"
))

(defparameter *die-nouns-männlich* '(
    "der" (*nounlist-männlich-singular-nom*)
    "des"
))

(defun split (str token &aux result (len (length str)))
(labels ((rec (i)
  (if (< i len)
      (let ((p (or (position token str :start i)
                   len)))
        (when (/= p i)
          (push (subseq str i p) result))
        (rec (1+ p)))
      (reverse result))))
(rec 0)))


(defparameter *satz0_1* '())
(setq *wrd*     "")
(defun setup (geschlecht wort1 wort2 wortart fall)
    (ecase geschlecht
    (:m (setq *wrd* (search (list wort1)
        *die-nouns-männlich* :test (function equal)))
        (if (not *wrd*)
            (progn (setf *wrd* #P"<leer>") (print (format nil "Sorry, kann Wort >>~A<< nicht finden." wort1)))
            (progn (setf *satz0_1* (append *satz0_1* (cons wort1 nil)))
            (ecase wortart
                (:singular (ecase fall
                    (:nominativ
                        (setq *wrd* (search (list wort2)
                        *nounlist-männlich-singular-nom* :test (function equal)))
                        (if (not *wrd*)
                            (progn (setf *wrd* #P"<leer>") (print (format nil "Sorry, kann Wort >>~A<< nicht finden !" wort2)))
                            (progn (setf *satz0_1* (append *satz0_1* (cons wort2 nil)))))
                )))
                (:plural   (ecase fall
                    (:nominativ)))
            ))))
    ))

(defun search_mnoun(word)
    (block nil
    (setq *wrd* (search (list word)
        *die-nouns-männlich* :test (function equal)))
    (if (equal *wrd* nil)
        (progn
            (print (format nil "Sorry, kann das Wort >> ~A << nicht finden." word))
            (return "<null>"))
        (progn
            (setf *satz0_1* (append *satz0_1* (cons word nil)))
            (return "<true>"))
    )
    (if (>= *wrd* 0)
        (progn
            (print (format nil ">> ~A << gefunden." word))
            (return "<true>"))
        (progn
            (print (format nil "Sorry, kann das Wort >> ~A << nicht finden." word))
            (return "<null>"))
    )
    (return "<null>"))
)


(setup :m "der" "Mann" :singular :nominativ)

;; ----------------------------------
;; little brain, by static rules ...
;; ----------------------------------
(terpri)
;
(setq l2 *satz0_1*)             ;; just backup
(setq l1 (read-line))           ;; user input text
(setq l1 (split l1 #\space))    ;; skip spaces

;; start scan ...
(setq w1 (string (nth 0 l1)))   ;; zuerst: die, der, ...
(setq w2 (search_mnoun w1))     ;; -"-   : männlich

(if (string= w2 "<true>")       ;; is male ?
    (if (>= (length l1) 1)
        (progn
            (print *nounlist-männlich-singular-nom*)
        )
    )
)

;
;;(print (equal l2 l1))
