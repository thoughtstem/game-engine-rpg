#lang racket

(provide (rename-out (custom-bullet custom-dart))
         custom-weapon-system
         weapon-slot?
         weapon->turret
         use-weapon
         use-weapon-against-player
         use-weapon-against
         weapon-backpack
         shoot
         process-bullet
         enemy-ai
         weapon-selector
         weapon-is?
         weapon?

         ;custom-particles
         
         (rename-out (spear-sprite spear-bullet-sprite))
         (rename-out (SWORD-ICON sword-sprite))
         (rename-out (swinging-sword-sprite sword-bullet-sprite))
         (rename-out (PAINT-THROWER-ICON paint-thrower-sprite))
         
         spear-sprite
         swinging-sword-sprite
         paint-sprite
         flying-sword-sprite
         flame-sprite

         SPEAR-ICON
         SWORD-ICON
         PAINT-THROWER-ICON
         FLYING-DAGGER-ICON
         RING-OF-FIRE-ICON

         make-icon
         )

(require game-engine
         "./combat.rkt"
         "./health-bar.rkt")

(define SPEAR-ICON         (bitmap "images/spear-sprite.png"))
(define SWORD-ICON         (bitmap "images/sword-sprite.png"))
(define PAINT-THROWER-ICON (bitmap "images/paint-thrower-sprite.png"))
(define FLYING-DAGGER-ICON
  (overlay (text "FD" 20 "black")
         (square 28 "solid" "purple")
         (square 32 "solid" "cyan")))
(define RING-OF-FIRE-ICON
  (overlay (text "RoF" 14 "black")
           (square 28 "solid" "yellow")
           (square 32 "solid" "cyan")))

(define (make-icon t [c1 "yellow"] [c2 "black"])
  (overlay (text t 16 "black")
         (square 28 "solid" c1)
         (square 32 "solid" c2)))
  

(define spear-sprite  (bitmap "images/spear-bullet-sprite.png"))

(define swinging-sword-sprite
  (rotate 90 (beside (rectangle 40 40 "solid" "transparent")
                     (rectangle 8 4 "solid" "black")
                     (rectangle 4 10 "solid" "black")
                     (rectangle 28 4 "solid" "gray"))))

(define flying-sword-sprite
   (beside ;(rectangle 40 10 "solid" "transparent")
    (rectangle 8 4 "solid" "black")
    (rectangle 4 10 "solid" "black")
    (rectangle 28 4 "solid" "gray")))

(define paint-sprite
  (overlay (circle 5 "solid" "blue")
           (circle 6 "solid" "yellow")
           (circle 7 "solid" "magenta")))

(define flame-sprite
  (overlay (circle 5 "solid" "yellow")
           (circle 6 "solid" "orange")
           (circle 7 "solid" "red")))




(define (process-bullet #:filter-out [tag #f])
  (lambda (g an-entity a-damager)
    #;(define hit-particles (custom-particles #:sprite (square 4 'solid (make-color 255 255 0 255))
                                            #:scale-each-tick 1
                                            #:particle-time-to-live 2
                                            #:system-time-to-live 5))
   ; (define tag 'bullet)
    (define damage (damager-amount a-damager))
    (define bullet-hp (get-storage-data "durability-stat" an-entity))
    (define new-bullet-hp (- bullet-hp damage))
    ;(define bullet-spd (get-ai-speed an-entity))
    (define new-bullet-speed (and (get-ai-speed an-entity)
                                  (/ (get-ai-speed an-entity) 2)))
    (displayln (~a "NEW BULLET HP: " new-bullet-hp))
    (if (should-filter-out? a-damager tag ) ;(member tag (damager-tags a-damager))
        an-entity
        (~> an-entity
            ;((spawn-on-current-tile hit-particles) g _) ; looks odd at center of large bullets (ie spear)
            (update-entity _ speed? (speed new-bullet-speed))
            (set-storage "durability-stat" _ new-bullet-hp)))))

(define (die-and-spawn e)
  (do-many
   (spawn-on-current-tile e)
   (λ(g e)
     (add-component e (after-time 2 die)))
   ))

(define (custom-bullet #:position   [p (posn 20 0)]
                       #:sprite     [s (rectangle 10 2 "solid" "green")]
                       #:speed      [spd 10]
                       #:damage     [dmg 10]
                       #:range      [rng 1000]
                       #:durability [dur 10]
                       #:hit-particles  [hit-particles (custom-particles #:sprite (square 2 'solid (make-color 0 255 0 255))
                                                                         #:scale-each-tick 1
                                                                         #:particle-time-to-live 2
                                                                         #:system-time-to-live 5)]
                       #:rotation-style [rs 'face-direction]
                       #:components [c #f]
                                    . custom-components)


  
  (combatant #:damage-processor (damage-processor (process-bullet #:filter-out '(bullet friendly-team)))
             #:stats (list (make-stat-config 'durability dur
                                             (no-progress-bar)
                                             ))
             (sprite->entity s
                  #:name       "Bullet"
                  #:position   p
                  #:components (physical-collider)
                               (direction 0)
                               (active-on-bg 0)
                               (damager dmg (list 'bullet 'friendly-team))
                               (on-rule (λ(g e)
                                          (<= (get-storage-data "durability-stat" e) 0))
                                        ;(die-and-spawn hit-particles)
                                        die
                                        )
                               (rotation-style rs)
                               (hidden)
                               (on-start show)
                               (speed spd)
                               (every-tick (move))
                               (on-edge 'top die)
                               (on-edge 'bottom die)
                               (on-edge 'left die)
                               (on-edge 'right die)
                               (after-time rng die)
                               (cons c custom-components))))

(define (weapon-slot? s)
  (lambda (g e)
    (define current-slot (get-storage-data "Weapon Slot" e))
    (eq? current-slot s)))

(define (weapon-backpack #:slots [slots 1])
  (define (slot->on-key num)
    (on-key num (set-storage-named "Weapon Slot" num)))
  (list (storage "Weapon Slot" 1)
        (map slot->on-key (range 1 (add1 slots)))))

(define (select-backpack-item num)
  (lambda (g e)
    (define backpack-list (get-backpack-entities e))
    (define item-name (if (> (length backpack-list) num)
                          (get-name (list-ref backpack-list num))
                          #f))
    (if item-name
        (begin (displayln (~a "WEAPON SELECTED: " item-name))
               ((set-storage-named "Selected Weapon" item-name) g e))
        e)))

(define (weapon-selector #:slots [slots 1])
  (define (slot->on-key num)
    (on-key num (select-backpack-item (sub1 num))))
  (list (storage "Selected Weapon" "None")
        (map slot->on-key (range 1 (add1 slots)))))

(define (weapon-is? name)
  (lambda (g e)
    (define current-weapon (get-storage-data "Selected Weapon" e))
    (eq? current-weapon name)))

(define (point-to-mouse-component? c)
    (and (on-start? c)
         (eq? (on-start-rule c) mouse-in-game?)
         (eq? (on-start-func c) point-to-mouse)))


;This is gross.  We really need to make a weapon component struct.
;  But this hack will serve for now
(define known-weapons '())
(define (weapon? c)
  (member (component-id c) known-weapons))

(define (custom-weapon-system #:slot              [slot #f]
                              #:dart              [b (custom-bullet)]
                              #:fire-mode         [fm 'normal]
                              #:fire-rate         [fr 3]
                              #:fire-key          [key 'f]
                              #:mouse-fire-button [button #f]
                              #:point-to-mouse?   [ptm? #t]
                              #:rapid-fire?       [rf?     #t]
                              #:rule              [rule (λ (g e) #t)])
  (define fire-interval (max 1 (/ 30 fr)))
  (define fire-rule (if button
                        (and/r (mouse-button-is-down? button)
                               (or/r (λ (g e) (eq? slot #f))
                                     (weapon-slot? slot))
                               rule)
                        (and/r (key-is-down? key)
                               (or/r (λ (g e) (eq? slot #f))
                                     (weapon-slot? slot))
                               rule)))
  (precompile! b)
  (define ret
    (cond
      [(eq? rf?    #t) (if button
                           (do-every fire-interval #:rule fire-rule (shoot #:bullet (if ptm?
                                                                                        (add-components b (on-start #:rule mouse-in-game? point-to-mouse))
                                                                                        b)
                                                                           #:fire-mode fm))
                           (do-every fire-interval #:rule fire-rule (shoot #:bullet b
                                                                           #:fire-mode fm)))]
      [(not button) (on-key   key    #:rule fire-rule (shoot #:bullet b
                                                             #:fire-mode fm))]
      [else         (on-mouse button #:rule fire-rule (shoot #:bullet (if ptm?
                                                                          (add-components b (on-start #:rule mouse-in-game? point-to-mouse))
                                                                          b)
                                                             #:fire-mode fm))]))

  (set! known-weapons (cons (component-id ret) known-weapons))

  ret)

(define (weapon->turret c)
  (cond [(do-every? c) (struct-copy struct-do-every c [rule (near? "Enemy" 120)])]
        [(on-key?   c) (struct-copy struct-on-key   c [rule (near? "Enemy" 120)])]
        [(on-mouse? c) (struct-copy struct-on-mouse c [rule (near? "Enemy" 120)])]
        ))

(define (constant-fire g e)
  #t)

(define (add-bullet-damage-tag b t)
  (if (not (get-component b damager?))
      b
      (update-entity b damager?
                         (~> (get-component b damager?)
                             (remove-damager-tag _ 'friendly-team)
                             (add-damager-tag _ t))))
  )




(define (enemy-ai (machine #f))
  (if machine
      (on-start (λ(g e) (~> e
                            (remove-component _ speed?)
                            (entity-add-machine  machine))))
      #f))

(provide add-weapon-filter)
(define (add-weapon-filter c f)


  (define original-fire-function 
    (cond [(do-every? c) (struct-do-every-func c)]
          [(on-key?   c) (struct-on-key-f c)]
          [(on-mouse? c) (struct-on-mouse-f c)]))

  (define new-fire-function
    (do-many
                                           
     original-fire-function
                                           
     (λ(g e)

       (define new-s (update-what-will-spawn (get-component e spawn-once?)
                                             f))

       (update-entity e spawn-once? new-s))))


  (cond [(do-every? c)
         (struct-copy struct-do-every c
                      [func new-fire-function])]
        [(on-key?   c)
         (struct-copy struct-on-key c
                      [f new-fire-function])]
        [(on-mouse? c)
         (struct-copy struct-on-mouse c
                      [f new-fire-function])])
  )


(define/contract (use-weapon c #:before-spawn [f identity])

  (->* ((or/c do-every? on-key? on-mouse?))
       (#:before-spawn procedure?)
       do-every?)
  
  (define original-fire-function 
    (cond [(do-every? c) (struct-do-every-func c)]
          [(on-key?   c) (struct-on-key-f c)]
          [(on-mouse? c) (struct-on-mouse-f c)]))

  (define ticks-between-shots 
    (cond [(do-every? c) (struct-do-every-speed c)]
          [(on-key?   c) 50]
          [(on-mouse? c) 50]))

  ;But we need to fix it a bit, so we can hack the bullet that gets spawned,
  ;  point the bullet at the player.  Make it hostile to the player.  Etc.
  (define new-fire-function
    (do-many
                                           
     original-fire-function
                                           
     (λ(g e)

       (define new-s (update-what-will-spawn (get-component e spawn-once?)
                                             f))

       (update-entity e spawn-once? new-s))))

  
  (do-every ticks-between-shots new-fire-function)
  )


;When enemies shoot bullets, this hacks the bullets immediately after they spawn
;  and puts tags on them so they don't hurt the shooter.  (Don't want enemies shooting themselves)
;Ultimate, this just returns a component that goes on an enemy and makes it fire at a
; constant rate.
;(More complex AI should use this component in a state machine.  Don't add AI in this function.  It's already gross enough.)
(define/contract (use-weapon-against-player c #:ticks-between-shots (ticks-between-shots 1))
  (->* ((or/c do-every? on-key? on-mouse?))
       (#:ticks-between-shots number?)
       do-every?)

  ;We need to do some hacking to make a weapon suitable for an enemy to use.
  ;  For example, certain things (like keyboard and mouse control) aren't necessary.
  ;  We just need the fireing function
  (define original-fire-function 
    (cond [(do-every? c) (struct-do-every-func c)]
          [(on-key?   c) (struct-on-key-f c)]
          [(on-mouse? c) (struct-on-mouse-f c)]))

  ;But we need to fix it a bit, so we can hack the bullet that gets spawned,
  ;  point the bullet at the player.  Make it hostile to the player.  Etc.
  (define new-fire-function
    (do-many
                                           
     original-fire-function
                                           
     (λ(g e)

       (define new-s (update-what-will-spawn (get-component e spawn-once?)
                                             (compose
                                              (curryr add-bullet-damage-tag 'enemy-team)
                                              (λ(e) (update-entity e posn? (posn 0 0)))
                                              (λ(e) (add-component e
                                                                   (on-start (point-to "player"))))
                                              (λ(e)
                                                (update-entity e damage-processor?
                                                               (damage-processor (process-bullet #:filter-out '(bullet enemy-team)))))
                                              )
                                             ))

       (update-entity e spawn-once? new-s))))

  

  ;Just return a component that fires the weapon at
  ;the appropriate rate.
  ;(More complex AI should use this component in a state machine.  Don't add ai in this function)
  (do-every ticks-between-shots new-fire-function))



;Ripped from above function.  Should combine into something more seamless....
(define/contract (use-weapon-against name c)

  (->* (string? (or/c do-every? on-key? on-mouse?))
       ()
       do-every?)
  
  (define original-fire-function 
    (cond [(do-every? c) (struct-do-every-func c)]
          [(on-key?   c) (struct-on-key-f c)]
          [(on-mouse? c) (struct-on-mouse-f c)]))

  (define ticks-between-shots 
    (cond [(do-every? c) (struct-do-every-speed c)]
          [(on-key?   c) 50]
          [(on-mouse? c) 50]))

  ;But we need to fix it a bit, so we can hack the bullet that gets spawned,
  ;  point the bullet at the player.  Make it hostile to the player.  Etc.
  (define new-fire-function
    (do-many
                                           
     original-fire-function
                                           
     (λ(g e)

       (define new-s (update-what-will-spawn (get-component e spawn-once?)
                                             (compose
                                              ;(curryr add-bullet-damage-tag 'enemy-team)
                                              ;(λ(e) (update-entity e posn? (posn 0 0)))
                                              (λ(e) (add-component e
                                                                   (on-start (point-to name))))
                                              #;(λ(e)
                                                  (update-entity e damage-processor?
                                                                 (damage-processor
                                                                  (process-bullet #:filter-out '(bullet enemy-team)))))
                                              )
                                             ))

       (update-entity e spawn-once? new-s))))

  
  (do-every ticks-between-shots new-fire-function)
  )


(define fire-mode? (or/c 'normal 'homing 'random 'spread))

(define/contract (shoot #:bullet [b (custom-bullet)] #:fire-mode [fm 'normal])
  (->* () (#:bullet entity? #:fire-mode fire-mode?) procedure?)
  
  (lambda (g e)
    ((cond [(eq? fm 'normal) (spawn-on-current-tile b )]
           [(eq? fm 'homing) (let ([homing-bullet (~> b
                                                      ;(update-entity  _ speed? (speed 5))
                                                      (add-components _ (follow "Enemy")
                                                                        #;(after-time 1000 die)))])
                               (spawn-on-current-tile homing-bullet))]
           [(eq? fm 'random) (let ([random-bullet (add-components b (after-time 1 (change-direction-by-random -15 15)))])
                               (spawn-on-current-tile random-bullet))]
           [(eq? fm 'spread) (let ([top-bullet    (~> b
                                                      (update-entity _ posn? (posn 0 0))
                                                      (remove-component _ point-to-mouse-component?)
                                                      (add-components _ (on-start (change-direction-by -10))))]
                                   [middle-bullet b]
                                   [bottom-bullet (~> b
                                                      (update-entity _ posn? (posn 0 0))
                                                      (remove-component _ point-to-mouse-component?)
                                                      (add-components _ (on-start (change-direction-by 10))))])

                               (spawn-on-current-tile (add-components middle-bullet
                                                                      (on-start (spawn-on-current-tile top-bullet))
                                                                      (on-start (spawn-on-current-tile bottom-bullet)))))]) g e)))

