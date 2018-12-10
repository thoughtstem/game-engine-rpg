#lang racket

(module+ test
  (require rackunit)

  ;Slow way (Does not take advantage of optimizations using animated sprite)
  (let ()
    (define health-entity
      (sprite->entity (draw-health-bar 100 #:max 100)
                      #:name "health"
                      #:position (posn 100 20)
                      #:components (counter 100) (every-tick (maybe-change-health-by -1 #:max 100))))

    (define g (initialize-game (list health-entity)))

    ;Simulate three ticks for this entity
    (define new-health-entity
      (~> health-entity
          (tick-entity g _)
          (tick-entity g _)
          (tick-entity g _)))

    ;Make sure it looks like it lost three points of health
    (check-equal? (draw-entity new-health-entity)
                  (draw-health-bar 97 #:max 100)))


  ;Fast way (avoids producing new images at runtime).
  ;  No need to precompile hundreds of images either.
  (let ()
    (define health-entity
      (health-bar-entity))

    (define g (initialize-game (list health-entity)))

    ;Simulate three ticks for this entity
    (define new-health-entity
      (~> health-entity
          (tick-entity g _ #:ticks 10)))

    #;(displayln (list
                (draw-entity new-health-entity)
                (draw-health-bar 90 #:max 100)))

    ;Make sure it looks like it lost three points of health
    (check-equal? (draw-entity new-health-entity)
                  (draw-health-bar 90 #:max 100)))
  
  )

(provide (all-defined-out))

(require 2htdp/image
         game-engine)

(define (health-bar-entity #:max (max 100)
                           #:starvation-period (starvation-period 0)
                           #:change-by  (change-by -1))
  (define health-bar-slice (rectangle 1 10 'solid 'red))

  (define main-sprite (set-x-scale max (new-sprite health-bar-slice)))

  (define bg-sprite (new-sprite (health-bar-bg max 10)))
  
  (define e
    (sprite->entity main-sprite
                  #:name "health"
                  #:position (posn 100 20)
                  #:components
                  (counter 100)
                  (layer "ui")
                  (storage "efficient-health-bar" #t)
                  (do-every starvation-period (maybe-change-health-by change-by #:max max))
                  (precompiler main-sprite
                               bg-sprite)
                  ))

  (add-component-at-end e bg-sprite)
  )

(define/contract (draw-health-bar amount #:max [max-val 100])
  (-> exact-nonnegative-integer? #:max exact-nonnegative-integer? image?)
  (define health-bar     (pad (rectangle amount 10 "solid" "red")4 4))
  (define max-health-bar (pad (rectangle max-val 10 "solid" "transparent") 0 0))
  (overlay/align "left" "middle"
                 health-bar
                 (health-bar-bg (image-width max-health-bar) (image-height max-health-bar))))

(define/contract (health-bar-bg w h)
  (-> number? number? image?)
  (overlay
   (rectangle (+ 4 w) (+ 4 h) "outline" (pen "white" 2 "solid" "butt" "bevel"))
   (rectangle (+ 8 w) (+ 8 h) "solid"  (make-color 20 20 20 150))))

(define (update-health-bar #:max [max-val 100])
  (lambda (g e)
    (define count (get-counter e))

    (if (efficient-health-bar? e)
        (efficient-update-health-bar g e count #:max max-val)
        (slow-update-health-bar g e count #:max max-val))))

(define (efficient-health-bar? e)
  (get-storage-data "efficient-health-bar" e))

(define (efficient-update-health-bar g e count #:max max-val)
  (define current-health-sprite
    (get-component e animated-sprite?))

  (define percentage (* 100
                        (/ count max-val)))

  (define new-health-sprite
    (~> current-health-sprite
        (set-x-scale  percentage _)
        (set-x-offset (- (/ (- 100 percentage) 2)) _)))
  
  ((change-sprite new-health-sprite) g e)

  )

(define (slow-update-health-bar g e count #:max max-val)
  (define health-bar (draw-health-bar count #:max max-val))

  ((change-sprite (new-sprite health-bar)) g e))

(define (maybe-change-health-by amount #:min [min-val 0] #:max [max-val 100])
  (lambda (g e)
    (define health (+ (get-counter e) amount))
    (define new-health (cond [(> health max-val) max-val]
                             [(< health min-val) min-val]
                             [else health]))
    ((do-many (set-counter new-health)
              (update-health-bar #:max max-val)) g e)))

(define (health-is-zero? g e)
  (define player-health (get-counter (get-entity "health" g)))
  ;(displayln (~a "Player health: " player-health))
  (<= player-health 0)
  )

(define (draw-counter-rpg #:prefix [prefix ""])
  (lambda (g e)
    (define count (get-counter e))

    (define current-sprite (get-component e string-animated-sprite?))
    
    ((change-sprite (set-text (~a prefix count) current-sprite)) g e)))

(define (remove-on-key g e)
  (remove-component e on-key?))

(define (kill-player)
  (lambda (g e1 e2)
    (define dead-player-image (rotate -90 (pick-frame-original (get-component e2 animated-sprite?) 0)))
    (if (health-is-zero? g e2)
        ((do-many remove-on-key
                  (stop-animation)
                  (change-sprite (new-sprite dead-player-image))) g e2)
        e2)))