;; -----------------------------------------------------------------------------
;; Lisp Deutsch ...
;; (c) 2018 Jens Kallup - non-profit Software
;; Alle Rechte vorbehalten !!!
;;
;; nur fuer Testzwecke - nicht fuer den kommerziellen Gebrauch !!!
;; -----------------------------------------------------------------------------

(ql:quickload :split-sequence)

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

(defparameter *nounlist-male-singular-nom* (list
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

(defparameter *nouns* (list
    (list "der" (list *nounlist-male-singular-nom*)) 
    (list "des" (list *nounlist-male-singular-nom*))
))

;; ----------------------------------------------------------------------------


;; ----------------------------------
;; little brain, by static rules ...
;; ----------------------------------
(terpri)
;;

;; -------------------------------------------
;; type ":exit" to exit the bottles loop ...
;; -------------------------------------------
(loop for bottles from 0 to 99
    do (
    progn
        ;; factory default's ...
        (setq satz-error 0)
        (setq satz-ende  0)
        ;;
        (print "notice: type :exit to exit the loop")
        (print "Ihre Eingabe: ")
        ;;
        (setq wordlist (loop for word in (split-sequence:split-sequence #\Space (read-line)
            :remove-empty-subseqs t)
            for index from 0 collect (list index word))
        )
        ;;
        (if (string= (nth 1 (nth 0 wordlist)) ":exit")
        (progn
            (setq satz-error -1)
            (print "ENDE")
            (exit)                  ;; <---- caution !
            (return)
            ))

        (print wordlist)
        (print (list-length wordlist))

        ;; start scan ...
        (loop for words-record from 0 to (list-length wordlist)
        do (
        progn
            (setq words-word (nth 1 (nth words-record wordlist)))
            (if (eql words-word nil)
                (return)
            )
            (loop for nouns-record from 0 to (list-length *nouns*)
            do (
            progn
                (setq noun-word (nth 0 (nth nouns-record *nouns*)))
                (if (string= noun-word words-word)
                (progn
                    (print "word ok")
                    (if (> (list-length wordlist) 1)
                    (progn
                        (setq words-record (+ 1 words-record))
                        (setq male-noun-list (nth 1 (nth nouns-record *nouns*)))
                        (setq satz-error 0)
                        (loop for male-name-record from 0 to (list-length male-noun-list)
                        do (
                        progn
                            (setq male-word1 (nth 1 (nth words-record wordlist)))
                            (setq male-word2 (nth male-name-record))
                            (if (string= male-word1 male-word2)
                            )
                        )))
                    (progn
                        (setq satz-ende  1)
                        (setq satz-error 0)
                        (return)
                    )))
                (return)))
            when (>= (and satz-error satz-ende) 1)
            do (return))
            ;;
            (if (>= satz-ende 1)
            (progn
                (if (= satz-error 0)
                (progn (print "Satz endet bei Eingabe, enthält KEINE Fehler !"))
                (progn (print "Satz endet bei Eingabe, enthält Fehler !")
                       (return))
                )
            )
            (progn
                (print "Satz endet bei Eingabe, enthält Fehler !")
                (return)
            )))
        when (>= (and satz-error satz-ende) 1)
        do (return))
    )
)
(exit)


;; ----------------------------------------------------------------------------
;;
;;                    +------------- := "der" (0. Position
;;                    |      +------ := "der" (1. Eintrag  -> nouns
;;                    |      |
(setq   der-test (nth 0 (nth 0 *nouns*)))
(print  der-test)
(print "---")

;; ----------------------------------------------------------------------------
;;
;;                    +--------------------------- := nounslist-male -> Anruf, Anzug, ...
;;                    |      +-------------------- := list position 1.
;;                    |      |      +------------- := "der" (2. Position
;;                    |      |      |      +------ := nouns ->  (1. Eintrag: "der"
;;                    |      |      |      |
(setq anruf-test (nth 0 (nth 0 (nth 1 (nth 0 *nouns*)))))
(setq anzug-test (nth 1 (nth 0 (nth 1 (nth 0 *nouns*)))))
(setq apfel-test (nth 2 (nth 0 (nth 1 (nth 0 *nouns*)))))

(terpri)
;;

(print anruf-test)
(print anzug-test)
(print apfel-test)

(setq  result
    (if (string= "der" der-test)
        (print "der")))
(print result)

;(print wA02)

;(if (>= wA02 1)
;    (progn
;        (print "ist männlich")
;        (setq  wB01 (string (nth 1 (nth 1 wordlist))))
;        (print wB01)
;    )
;    (progn (print "Fehler")))


