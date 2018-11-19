#lang racket

(provide (all-defined-out))

(require 2htdp/image
         game-engine)

(define/contract (draw-health-bar amount #:max [max-val 100])
  (-> exact-nonnegative-integer? #:max exact-nonnegative-integer? image?)
  (define health-bar     (pad (rectangle amount 10 "solid" "red")4 4))
  (define max-health-bar (pad (rectangle max-val 10 "solid" "transparent") 0 0))
  (overlay/align "left" "middle"
                 health-bar
                 (overlay
                  (rectangle (+ 4 (image-width max-health-bar)) (+ 4 (image-height max-health-bar)) "outline" (pen "white" 2 "solid" "butt" "bevel"))
                  (rectangle (+ 8 (image-width max-health-bar)) (+ 8 (image-height max-health-bar)) "solid"  (make-color 20 20 20 150)))))

(define (update-health-bar #:max [max-val 100])
  (lambda (g e)
    (define count (get-counter e))
    (define health-bar (draw-health-bar count #:max max-val))
    ((change-sprite (new-sprite health-bar)) g e)))

(define (maybe-change-health-by amount #:min [min-val 0] #:max [max-val 100])
  (lambda (g e)
    (define health (+ (get-counter e) amount))
    (define new-health (cond [(> health max-val) max-val]
                             [(< health min-val) min-val]
                             [else health]))
    ((do-many (set-counter new-health)
              (update-health-bar #:max max-val)
              #;(draw-counter-rpg #:prefix "Health: ")) g e)))

(define (health-is-zero? g e)
  (define player-health (get-counter (get-entity "health" g)))
  ;(displayln (~a "Player health: " player-health))
  (<= player-health 0)
  )

(define (draw-counter-rpg #:prefix [prefix ""])
  (lambda (g e)
    (define count (get-counter e))
    (define count-image (draw-dialog (~a prefix count)))
    ((change-sprite (new-sprite count-image)) g e)))

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