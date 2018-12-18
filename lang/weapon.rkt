#lang racket

(provide custom-bullet
         custom-weapon
         weapon-slot?
         weapon->turret
         weapon-backpack
         shoot
         process-bullet)

(require game-engine
         "./combat.rkt"
         "./health-bar.rkt")

(define (process-bullet g an-entity a-damager)
  (define damage (damager-amount a-damager))
  (define bullet-hp (get-storage-data "durability-stat" an-entity))
  (define new-bullet-hp (- bullet-hp damage))
  ;(define bullet-spd (get-ai-speed an-entity))
  (define new-bullet-speed (and (get-ai-speed an-entity)
                                (/ (get-ai-speed an-entity) 2)))
  (displayln (~a "NEW BULLET HP: " new-bullet-hp))
  (~> an-entity
      (update-entity _ speed? (speed new-bullet-speed))
      (set-storage "durability-stat" _ new-bullet-hp)))

(define (custom-bullet #:position   [p (posn 20 0)]
                       #:sprite     [s (rectangle 10 2 "solid" "green")]
                       #:speed      [spd 10]
                       #:damage     [dmg 10]
                       #:durability [dur 10])
  (combatant #:damage-processor (damage-processor process-bullet)
             #:stats (list (make-stat-config 'durability dur
                                             (no-progress-bar)
                                             ))
             (sprite->entity s
                  #:name       "Bullet"
                  #:position   p
                  #:components (physical-collider)
                               (direction 0)
                               (damager dmg)
                               (on-rule (λ(g e)
                                          (<= (get-storage-data "durability-stat" e) 0)) die)
                               (rotation-style 'face-direction)
                               (hidden)
                               (on-start show)
                               (speed spd)
                               (every-tick (move))
                               (on-edge 'top die)
                               (on-edge 'bottom die)
                               (on-edge 'left die)
                               (on-edge 'right die))))

(define (weapon-slot? s)
  (lambda (g e)
    (define current-slot (get-storage-data "Weapon Slot" e))
    (eq? current-slot s)))

(define (weapon-backpack #:slots [slots 1])
  (define (slot->on-key num)
    (on-key num (set-storage-named "Weapon Slot" num)))
  (list (storage "Weapon Slot" 1)
        (map slot->on-key (range 1 (add1 slots)))))


(define (custom-weapon #:slot              [slot #f]
                       #:bullet            [b (custom-bullet)]
                       #:fire-mode         [fm 'normal]
                       #:fire-rate         [fr 3]
                       #:fire-key          [key 'f]
                       #:mouse-fire-button [button #f]
                       #:rapid-fire? [rf?     #t])
  (define fire-interval (max 1 (/ 30 fr)))
  (define fire-rule (if button
                        (and/r (mouse-button-is-down? button)
                               (not/r health-is-zero?)
                               (or/r (λ (g e) (eq? slot #f))
                                     (weapon-slot? slot)))
                        (and/r (key-is-down? key)
                               (not/r health-is-zero?)
                               (or/r (λ (g e) (eq? slot #f))
                                     (weapon-slot? slot)))))
  (cond
    [(eq? rf?    #t) (if button
                         (do-every fire-interval #:rule fire-rule (shoot #:bullet (add-components b (on-start #:rule mouse-in-game? point-to-mouse))
                                                                         #:fire-mode fm))
                         (do-every fire-interval #:rule fire-rule (shoot #:bullet b
                                                                         #:fire-mode fm)))]
    [(not button) (on-key   key    #:rule fire-rule (shoot #:bullet b
                                                           #:fire-mode fm))]
    [else         (on-mouse button #:rule fire-rule (shoot #:bullet (add-components b (on-start #:rule mouse-in-game? point-to-mouse))
                                                           #:fire-mode fm))]))

(define (weapon->turret c)
  (cond [(do-every? c) (struct-copy struct-do-every c [rule (near? "Enemy" 120)])]
        [(on-key?   c) (struct-copy struct-on-key   c [rule (near? "Enemy" 120)])]
        [(on-mouse? c) (struct-copy struct-on-mouse c [rule (near? "Enemy" 120)])]
        ))

(define (shoot #:bullet [b (custom-bullet)] #:fire-mode [fm 'normal])
  (lambda (g e)
    ((cond [(eq? fm 'normal) (spawn (add-components b (after-time 1000 die)))]
           [(eq? fm 'homing) (let ([homing-bullet (~> b
                                                      (update-entity  _ speed? (speed 5))
                                                      (add-components _ (follow "Enemy")
                                                                        (after-time 1000 die)))])
                               (spawn homing-bullet))]
           [(eq? fm 'random) (let ([random-bullet (add-components b (on-start (change-direction-by-random -15 15)))])
                               (spawn random-bullet))]
           [(eq? fm 'spread) (let ([top-bullet    (~> b
                                                      (update-entity  _ speed? (speed 20))
                                                      (add-components _ (on-start (change-direction-by -10))
                                                                        (after-time 10 die)))]
                                   [middle-bullet (~> b
                                                      (update-entity  _ speed? (speed 20))
                                                      (add-components b (after-time 10 die)))]
                                   [bottom-bullet (~> b
                                                      (update-entity  _ speed? (speed 20))
                                                      (add-components b (on-start (change-direction-by 10))
                                                                    (after-time 10 die)))])
                               (do-many (spawn top-bullet)
                                        (spawn middle-bullet)
                                        (spawn bottom-bullet)
                                        (spawn top-bullet)
                                        (spawn middle-bullet)
                                        (spawn bottom-bullet)))]) g e)))