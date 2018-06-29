;; -----------------------------------------------------
;; Astronomical Object

(class-def
 astro-obj ()
 ((mass :initarg :mass
        :initform 1.0
        :accessor astro-obj-mass
        :type 'float)
  (radius :initarg :radius
          :initform 1.0
          :accessor astro-obj-radius
          :type 'float)
  ;; orbiting around other astro obj?
  (orbiting :initarg :orbiting
            :initform nil
            :accessor astro-obj-orbiting)
  (orbit-angle :initarg :orbit-angle
               :initform 0
               :accessor astro-obj-orbit-angle
               :type 'integer)
  (orbit-radius :initarg :orbit-radius
                :initform 1.0
                :accessor astro-obj-orbit-radius
                :type 'float)
  ;; if orbiting is NIL, use the abusolute position
  (pos-x :initarg :pos-x
         :initform 0.0
         :accessor astro-obj-pos-x)
  (pos-y :initarg :pos-y
         :initform 0.0
         :accessor astro-obj-pos-y)
  (population :initarg :population
              :initform nil
              :accessor astro-obj-population)
  (facilities :initarg :facilities
              :initform nil
              :accessor astro-obj-facilities))
 (:documentation "The basic astronomical object, including stars, planets."))

(class-def
 star (astro-obj)
 ((brightness :initarg :brightness
              :initform 1.0
              :accessor star-brightness
              :type 'float)
  (radiation :initarg :radiation
             :initform 1.0
             :accessor star-radiation
             :type 'float)
  (age :initarg :age
       :initform 0.0
       :accessor star-age
       :type 'float)
  (type :initarg :type
        :initform :star
        :accessor star-type
        :type 'symbol
        :documentation "Valid values: :star :supernova :black-hole :molecular-cloud")))

(class-def
 planet (astro-obj)
 ((env :initarg :env
       :initform 0.0
       :accessor planet-env
       :type 'float
       :documentation "Basic environment value, `")
  (ecosys :initarg :ecosys
          :initform 0.0
          :accessor planet-ecosys
          :type 'float
          :documentation "Ecosystem value")
  (resources :initarg :resources
             :initform nil
             :accessor planet-resources
             :type 'list)
  (type :initarg :type
        :initform :rocky
        :accessor planet-type
        :type 'symbol
        :documentation "Valid values: :rocky Rocky planet, :gas Gas gaint.")))

;; ---------------------------------------------------------------
;; Facilities

(class-def
 facility ()
 ((basic-strength :initarg :basic-strength
                  :initform 100
                  :accessor facility-basic-strength
                  :type 'integer)
  (actual-strength :initarg :actual-strength
                   :initform 100
                   :accessor facility-actual-strength
                   :type 'integer)
  (cost :initarg :cost
        :initform nil
        :accessor facility-cost)
  (icon :initarg :icon
        :initform nil
        :accessor facility-icon)))
