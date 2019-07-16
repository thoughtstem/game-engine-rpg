#lang racket




(module+ test
  (require rackunit)

  ;Setup for the tests
  (define base-player
    (sprite->entity empty-image
                    #:name ""
                    #:position (posn 0 0)))

  (define take-10-damage (make-damager 10))

  
  (define (half x) (/ x 2))

  
  (define (check-combatant-takes-damage? e d #:final-health f #:final-shield (s 100) msg)

    (define g (initialize-game (list e)))

    (define damaged-e
      (entity+damager->entity e d))

    (check-equal? (get-health damaged-e) f msg)

    (check-equal? (get-shield damaged-e) s msg))



  ;Tests for attaching stats to players
  
  (check-equal?
   ((has-stat? "health") (combatant base-player))
   #t)

  (check-equal?
   ((has-stat? "shield") (combatant base-player))
   #t)
  
  ;Tests for damaging stats

  ;Most basic.  No damage processors.  The player will take damage,
  ;  unadjusted from any damagers.
  (let ()
    (define player (combatant base-player))

    (check-combatant-takes-damage? player
                                   take-10-damage
                                   #:final-health 90
                                   "Taking 10 damage normally should give you 90 health"))


  (let ()
    (define player (combatant #:damage-processor (filter-damage-by-tag #:filter-out 'team-a)
                               base-player))

    (check-combatant-takes-damage? player
                                   (make-damager 10 '(team-a))
                                   #:final-health 100
                                   "Taking 10 damage from something 'on your team' should not remove health"))


  ;Combatants can start with a different level of health
  (let ()
    (define player (combatant base-player
                              #:stats (list (make-stat-config 'health 50  (stat-progress-bar-system 'red))
                                            (make-stat-config 'shield 100 (stat-progress-bar-system 'blue)))))

    (check-combatant-takes-damage? player
                                   take-10-damage
                                   #:final-health 40
                                   "Taking 10 damage and starting with 50 health, should give you 40 health"))
  

  ;Example: Taking half damage
  (let ()

    
    (define player (combatant #:damage-processor (simple-numeric-damage #:adj half)
                              base-player))

    (check-combatant-takes-damage? player
                                   take-10-damage
                                   #:final-health 95
                                   "Taking 10 damage with a 50% modifier should give you 95 health"))

  

  ;Slightly more complex.  This makes a player very weak -- taking any amount
  ;  of damage will result in the player taking 10000 damage (probably instant death).
  (let ()
    (define player (combatant #:damage-processor (always-critical-hit)
                              base-player))

    (check-combatant-takes-damage? player
                                   take-10-damage
                                   #:final-health 0
                                   "Taking 10 damage with critical hit should give you 0 health"))


  ;Take different kinds of damage (damage to shield but not health)
  (let ()
    (define player (combatant #:damage-processor (divert-damage #:first-stat "shield"
                                                                #:first-stat-protection half
                                                                
                                                                #:second-stat "health"
                                                                #:second-stat-protection identity)
                              base-player))

    (check-combatant-takes-damage? player
                                   take-10-damage
                                   #:final-health 100
                                   #:final-shield 95
                                   (~a "Taking 10 damage with a shield that\n"
                                       "reduces damage by half should give you 100 health and 95 shield"))

    )

  ;Don't take friendly fire (from self)
  (let ()
    (define player (combatant #:damage-processor (divert-damage #:filter-out '(my-team fire-damage)
                                                                #:first-stat "shield"
                                                                #:second-stat "health")
                              base-player))

    (check-combatant-takes-damage? player
                                   (make-damager 10 '(my-team bullet))
                                   #:final-health 100
                                   #:final-shield 100
                                   (~a "Should not take friendly fire"))

    )

  ;Do take enemy fire 
  (let ()
    (define player (combatant #:damage-processor (divert-damage #:filter-out '(my-team fire-damage)
                                                                #:first-stat "shield"
                                                                #:second-stat "health")
                              base-player))

    (check-combatant-takes-damage? player
                                   (make-damager 10 '(enemy-team bullet))
                                   #:final-health 100
                                   #:final-shield 90
                                   (~a "Should not take friendly fire")) ))


(provide combatant
         player-combatant
         get-stat
         set-stat
         change-stat
         define-stat
         
         divert-damage
         filter-damage-by-tag
         (rename-out (make-damager damager))
         (except-out (struct-out damager) damager)
         damager-amount
         set-damager-amount
         damager-tags
         damager-has-tag?
         ;damager?
         (struct-out damage-processor)
         conditional-damage-processor
         
         no-progress-bar
         make-stat-config
         stat-progress-bar        ;stat-bar entity used for player-combatant
         stat-progress-bar-system ;stat-bar system used for enemy combatants
         add-damager-tag
         remove-damager-tag

         should-filter-out?

         default-health+shields-stats
         stat-progress-bar

         default-combat-particles
         add-by
         subtract-by
         multiply-by
         divide-by

         change-health-by
         set-health-to
         change-shield-by
         set-shield-to

         remove-all-but-basics
         )

(require game-engine)

(component damager          (amount tags) #:transparent)
(struct damage-processor (f))

(define (make-damager amount (tags '()))
  (new-damager amount tags))

(define (make-damage-processor f)
  (damage-processor f))

(define default-combat-particles (custom-particles #:sprite (new-sprite (square 1 'solid (make-color 255 255 0 255)) #:scale 4)
                                                   #:scale-each-tick 1
                                                   #:particle-time-to-live 2
                                                   #:system-time-to-live 5))

(define (simple-numeric-damage #:do (effect change-health) #:adj (adj identity) #:hit-sound (hit-sound #f))
  (make-damage-processor
   (λ(g an-entity a-damager)
     (define hit-img (square 1 'solid (make-color 255 255 0 255)))
     (precompile! hit-img)
     (define hit-particles (particle-system #:sprite (new-sprite hit-img
                                                                #:scale 4)
                                            #:scale-each-tick 1                 ; 1
                                            #:particle-time-to-live 2           ; 2
                                            #:system-time-to-live 5))           ; 5 
     (define d-amt (- (adj (damager-amount a-damager))))
     (~> an-entity
         ;((spawn-on-current-tile hit-particles) g _)
         ((play-sound hit-sound) g _)
         ;((spawn (toast-entity (~a d-amt) #:color "orangered") #:relative? #t) g _)
         (add-components _ hit-particles)
         (add-components _ (toast-system (~a d-amt) #:color "orangered"))
         (effect _ d-amt)))))

(define (filter-damage-by-tag #:do (effect change-health)
                              #:adj (adj identity)
                              #:filter-out (tag #f)
                              #:show-damage? [sd? #f]
                              #:hit-sound [hit-sound #f])
  (make-damage-processor
   (λ(g an-entity a-damager)
     (define hit-img (square 1 'solid (make-color 255 255 0 255)))
     (precompile! hit-img)
     (define hit-particles (particle-system #:sprite (new-sprite hit-img
                                                                #:scale 4)
                                            #:scale-each-tick 1                 ; 1
                                            #:particle-time-to-live 2           ; 2
                                            #:system-time-to-live 5))           ; 5 
     (define d-amt (- (adj (damager-amount a-damager))))
     (if (should-filter-out? a-damager tag )
         an-entity
         (~> an-entity
             ((play-sound hit-sound) g _)
             ;((if sd?
             ;     (spawn (toast-entity (~a d-amt) #:color "orangered") #:relative? #t)
             ;     (λ (g e) e)) g _)
             
             (add-components _ hit-particles)
             
             ((if sd?
                 (λ(g e)
                   (add-components e (toast-system (~a d-amt) #:color "orangered")))
                 (λ (g e) e)) g _)
             
             ;((spawn-on-current-tile hit-particles) g _)
             (effect _ (- (adj (damager-amount a-damager)))))))))

(define (should-filter-out? d tag)
  (define tags (if (list? tag)
                   tag
                   (list tag)))

  (define ret (ormap (curryr member (damager-tags d)) tags))

  ret)

(define (damager-has-tag? tag)
  (lambda (d)
    (member tag (damager-tags d))))

(define (add-damager-tag d t)
  (struct-copy damager d
               [tags (cons t (damager-tags d))]))

(define (remove-damager-tag d t)
  (struct-copy damager d
               [tags (filter-not (curry eq? t) (damager-tags d))]))

(define (set-damager-amount d amt)
  (struct-copy damager d
               [amount amt]))

(define (always-critical-hit)
  (simple-numeric-damage #:adj (thunk* 100000)))

(define (conditional-damage-processor #:rule rule
                                   #:processor p1
                                   #:default-processor p2)
  (define (inner g an-entity a-damager)
    (define final-dp (if (rule a-damager)
                         p1
                         p2))
    (define final-dp-f (damage-processor-f final-dp))
    (final-dp-f g an-entity a-damager))
  (make-damage-processor
   inner))
  

(define (divert-damage #:first-stat-protection  (adj1 identity)
                       #:first-stat             (stat1 "shield") 
                       #:second-stat-protection (adj2 identity)
                       #:second-stat            (stat2 "health")
                       #:filter-out             (tag #f)
                       #:hit-sound              (hit-sound #f))

  ;Wow, you can have contracts on an inner function...
  (define/contract (inner g an-entity a-damager)
    (-> (or/c game? #f)
        (and/c (combatant-with-stat? stat1)
               (combatant-with-stat? stat2))
        damager?
        (and/c (combatant-with-stat? stat1)
               (combatant-with-stat? stat2)))
    
    (define first-damage (adj1 (damager-amount a-damager)))

    ;(displayln (~a "first-damage " first-damage))
    ;(displayln (~a "FINAL DAMAGE AMOUNT: " first-damage))

    (define first-stat-amount (get-stat stat1 an-entity))

    (define leftover-damage (max 0 (- first-damage first-stat-amount)))

    (define second-damage (adj2 leftover-damage))

    (define hit-img (square 1 'solid (make-color 255 255 0 255)))
    (precompile! hit-img)
    ;(displayln (~a "second-damage " second-damage))
    (define hit-particles (particle-system #:sprite (new-sprite hit-img
                                                                #:scale 4)
                                            #:scale-each-tick 1                 ; 1
                                            #:particle-time-to-live 2           ; 2
                                            #:system-time-to-live 5))           ; 5 
    (if (should-filter-out? a-damager tag )
         an-entity
         (~> an-entity
             ((play-sound hit-sound) g _)
             ;((spawn-on-current-tile hit-particles) g _)
             ;((spawn (toast-entity (~a (- first-damage)) #:color "orangered") #:relative? #t) g _)
             (add-components _ hit-particles)
             (add-components _ (toast-system (~a (- first-damage)) #:color "orangered"))
             (change-stat stat1 _ (- first-damage))
             (change-stat stat2 _ (- second-damage)))))

  (make-damage-processor
   inner))

;Takes a bunch of damage processors and a damager and returns an entity.
;  Even if the only thing done to the entity is to adjust the health,
;  this is fundamental technique that can be used to implement:
;    * Armor
;    * Friendly fire (on or off)
;    * Buffs and debuffs
;    * Etc.
;Used in more complex ways, this technique can be used to implement various
;  side effects of damage
;    * Getting poisoned
;    * Losing mana
;    * Armor breaking
;Anything that needs to happen as a result of taking damage should be possible
;  by adding damage-processor components to entities.
;It gives the entity getting damaged a chance to run a variety of adjustment
;  functions wrapped up in damage-processor components
;  Note: The order matters.  Earlier damage-processors run first.


(define (combatant? e)
  (and (entity? e)
       (get-component e damage-processor?)))

(define (has-stat? n)
  (lambda (e)
    (not (not (get-storage (stat-name n) e)))))

(define (combatant-with-stat? n)
  (and/c combatant?
         (has-stat? n)))

(define/contract (entity+damager->entity e damager)
  (-> combatant? damager? combatant?)
  
  (update-entity-from-damage #f e
                             (get-component e damage-processor?)
                             damager))


(define (update-entity-from-damage g e dp the-damager)
  (match-define (damage-processor f) dp)

  (f g e the-damager))


(define (stat-name s)
  (~a s "-stat"))

(define/contract (get-stat s e)
  (-> (or/c string? symbol?) entity? (or/c number? #f))
  (get-storage-data (stat-name s) e))

(define/contract (set-stat s e v)
  (-> (or/c string? symbol?) entity? number? entity?)
  (define max-v (get-storage-data (~a "max-" (stat-name s)) e))
  (set-storage (stat-name s) e (min max-v (max 0 v))))

(define/contract (change-stat s e v)
  (-> (or/c string? symbol?) entity? number? entity?)
  (define max-v (get-storage-data (~a "max-" (stat-name s)) e))
  (set-stat s e (min max-v (+ v (get-stat s e)))))

(define/contract (init-stat s e v mv)
  (-> (or/c string? symbol?) entity? number? number? entity?)
  (add-components e
                  (storage (stat-name s) (min v mv))
                  (storage (~a "max-" (stat-name s)) mv)))

;Could macroify the creation of these...

(define-syntax-rule (define-stat name get set change init)
  (begin
    (provide get set change init)
    (define/contract (get e)
      (-> entity? (or/c number? #f))
      (get-stat (~a 'name) e))

    (define/contract (set e v)
      (-> (combatant-with-stat? (~a 'name)) number? combatant?)
      (set-stat (~a 'name) e v))

    (define/contract (change e v)
      (-> (combatant-with-stat? (~a 'name)) number? combatant?)
      (change-stat (~a 'name) e v))

    (define/contract (init e v mv)
      (-> (and/c combatant?
                 (not (combatant-with-stat? (~a 'name))))
          number?
          number?
          (combatant-with-stat? (~a 'name)))
      (init-stat (~a 'name) e v mv))))

(define-stat health get-health set-health change-health init-health)
(define-stat shield get-shield set-shield change-shield init-shield)



(define (take-damage g e)
  (define damagers
    (map
     (λ(e) (get-component e damager?))
     (filter (has-component? damager?) (colliding-with e g))))

  (define damaged-e
    (foldl (λ(n a)
             (entity+damager->entity a n))
           e
           damagers))

  damaged-e)



(require game-engine
         "./health-bar.rkt")



(struct stat-config (name starting-value max-value display-entity))

(define (make-stat-config n
                          [starting-value 100]
                          [display-system (stat-progress-bar-system 'red)]
                          #:max-value [max-value 100])
  (stat-config n starting-value max-value (display-system n)))

; ======== NEW PROGRESS BAR SPRITE =====
(define (stat-progress-bar-system color
                                  #:max    [max 100]
                                  #:starting-value [sv max]
                                  #:offset [p (posn 0 -20)]
                                  #:width  [w 26]
                                  #:height [h 5]
                                  #:after  [f identity] ;For modifying the entity, e.g. removing lock-to?
                                  )
  (λ(stat-name)
    (define (displayer sn)
      (abstract-progress-bar-system #:color color
                                    #:max   max
                                    #:starting-value sv
                                    #:width  w
                                    #:height h
                                    #:stat-name sn
                                    #:offset p
                                    ))
      
      #|(define (safe-get-stat n e)
        (if e
            (get-stat n e)
            0))

      (displayln (~a "STAT-NAME: " stat-name))

      (define data-source
        (λ(g)
          (~> g
              (find-combatant _)
              (safe-get-stat stat-name _))))|#
    
      (f (displayer stat-name))
      ))

; ================================

; ========= REFACTOR THIS ========
;This is admittedly a mess, but this is what you would
; change to make a new kind of progress bar display
(define (stat-progress-bar color
                           #:max    (max 100)
                           #:offset (p (posn 0 -20))
                           #:width (w 26)
                           #:height (h 5)
                           #:after (f identity) ;For modifying the entity, e.g. removing lock-to?
                           )
  (λ(stat-name)
    (λ(find-combatant)

      (define (displayer data-source)
        (abstract-progress-bar #:color color
                               #:max   max
                               #:height h
                               #:width  w
                               #:data-from data-source
                               ))
      
      (define (safe-get-stat n e)
        (if e
            (get-stat n e)
            0))

      ;(displayln (~a "STAT-NAME: " stat-name))

      (define data-source
        (λ(g)
          (~> g
              (find-combatant _)
              (safe-get-stat stat-name _))))
    
      (f
       (~>
        (displayer data-source)
        (add-component _ (active-on-bg))
        (add-component _ (lock-to find-combatant
                                  #:offset p))))
      )))
; ==========================


(define (no-progress-bar)
 (λ(stat-name)
   #f ; dummy component
    #|(λ(find-combatant)
      (~>
         (sprite->entity empty-image
                         #:position (posn 0 0)
                         #:name "null"
                         #:components
                         (on-start die))
         (add-component _ (lock-to find-combatant)))
      )|#
   ))


(define (default-health+shields-stats health shields)
  (list (make-stat-config 'health health (stat-progress-bar-system 'green #:max health #:offset (posn 0 -40))
                          #:max-value health)
        (make-stat-config 'shield shields (stat-progress-bar-system 'deepskyblue #:max shields #:offset (posn 0 -48))
                          #:max-value health)))



(define/contract (combatant original-e
                   #:damage-processor  (dp     (simple-numeric-damage))
                   #:stats             (stats  (default-health+shields-stats 100 100)))

  (->* (entity?)
       (#:damage-processor damage-processor?
        #:stats (listof stat-config?))
       combatant?)

  (define combatant-id (random 100000)) ;Do I need this anymore?

  #;(define find-combatant ;(curry entity-with-storage "combatant-id" combatant-id) ;This is too slow
    (λ(g)
      ;(displayln (~a "finding combatant " ))

      ;Old, slow way
      ;(current-version-of original-e g)

      ;New, faster way?  Wait... seems to be broken, not sure why...
      (find-entity-by-id (entity-id original-e) g)
      
      )) ;New, fastest way is to not have to find the entity at all!

  (define bar-systems ; LIST OF COMPONENTS
    (map
     (λ(stat)
       ;((stat-config-display-entity stat) find-combatant)
       (stat-config-display-entity stat)
       )
     stats))

  (define (update-bar-sprite s v mv)
    (define percentage (/ v mv))
    (define max-bar-width (get-x-scale s))
    (define bar-width (* percentage max-bar-width))
    (~> s
        (set-x-scale bar-width _)
        (set-x-offset (/ (- bar-width max-bar-width) 2) _)))

  (define (update-bars g e)
    (define (update-bar stat)
      (lambda (g e)
        (define stat-name (stat-config-name stat))
        ;(displayln (~a "UPDATE-BARS STAT-NAME: " stat-name))
        (define main-sprite (get-storage-data (~a stat-name "-main-sprite") e))
        (if main-sprite
            (let ([stat-value (get-stat stat-name e)]
                  [max-value  (stat-config-max-value stat)])
              (update-entity e (curry component-eq? main-sprite)
                             (update-bar-sprite main-sprite stat-value max-value)))
            e)))
    ((apply do-many (map update-bar stats)) g e))
  
  (define e-without-stats
    (add-components original-e
                    (flatten
                     (list (storage "combatant-id" combatant-id)
                           (on-collide (has-component? damager?) (do-many take-damage
                                                                          update-bars)) 
                           ;on-start-spawn-bars
                           bar-systems
                           dp))))
  (foldl
   (λ(n a)
     (init-stat (stat-config-name n) a (stat-config-starting-value n) (stat-config-max-value n)))
   e-without-stats
   stats))

(define/contract (player-combatant original-e
                                   #:damage-processor  (dp     (simple-numeric-damage))
                                   #:stats             (stats  (default-health+shields-stats 100 100)))

  (->* (entity?)
       (#:damage-processor damage-processor?
        #:stats (listof stat-config?))
       combatant?)

  (define combatant-id (random 100000)) ;Do I need this anymore?

  (define find-combatant ;(curry entity-with-storage "combatant-id" combatant-id) ;This is too slow
    (λ(g)
      ;(displayln (~a "finding combatant " ))

      ;Old, slow way
      ;(current-version-of original-e g)

      ;New, faster way?  Wait... seems to be broken, not sure why...
      (find-entity-by-id (entity-id original-e) g)
      
      ))

  (define bars ; LIST OF ENTITIES
    (map
     (λ(stat)
       ((stat-config-display-entity stat) find-combatant)
       )
     stats))
  
  (define on-start-spawn-bars
    (map
     (λ(b)
       (if (eq? (get-name b) "null")
           #f
           (on-start (spawn-on-current-tile (add-components b
                                                            (on-rule (λ (g e) (not (find-combatant g)))
                                                                 
                                                                     (do-many
                                                                      die)
                                                                     )))
                     )))
     bars))
  
  (define e-without-stats
    (add-components original-e
                    (flatten
                     (list (storage "combatant-id" combatant-id)
                           (on-collide (has-component? damager?) take-damage) 
                           on-start-spawn-bars
                           ;bar-systems
                           dp))))



  (foldl
   (λ(n a)
     (init-stat (stat-config-name n) a (stat-config-starting-value n) (stat-config-max-value n)))
   e-without-stats
   stats))

; === change-damage helpers ====
(define (multiply-by num)
  (curry * num))

(define (divide-by num)
  (curryr / num))

(define (subtract-by num)
  (curryr - num))

(define (add-by num)
  (curryr + num))

; === STAT HANDLERS ===
(define (set-health-to amt)
  (lambda (g e)
    (set-health e (max 0 amt))))

(define (change-health-by amt)
  (lambda (g e)
    (change-health e amt)))

(define (set-shield-to amt)
  (lambda (g e)
    (set-shield e (max 0 amt))))

(define (change-shield-by amt)
  (lambda (g e)
    (change-shield e amt)))

(define (remove-all-but-basics g e)
  (remove-components e (and/c (not/c id?)
                              (not/c bb?)
                              (not/c posn?)
                              (not/c entity-name?)
                              (not/c animated-sprite?)
                              (not/c direction?)
                              (not/c backpack?)
                              (not/c physical-collider?)
                              (not/c key-movement?)
                              (not/c storage?)
                              (not/c damage-processor?)
                              (not/c sound-stream?)
                              (not/c on-collide?)
                              (not/c after-time?))))

