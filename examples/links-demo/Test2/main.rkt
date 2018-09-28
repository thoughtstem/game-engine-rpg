#lang racket


(provide little-room-game
         enter)


(require 2htdp/image
         game-engine
         game-engine-rpg)


(define exit-door
  (sprite->entity (square 30 'solid 'brown)
                  #:name "exit-door"
                  #:position (posn 0 0)
                  #:components
                  (static)
                  (physical-collider)
                  (link "./Test")))

(define door    (room 1 1 wood-tile
                       #;(square 32 'solid 'green)))


(define r       (room 4 4 wood-tile))

(define r2      (place-in-room
                 (room 5 5 wood-tile)
                 bed
                 (nudge-right 1 bed)))

(define rr      (beside-room r2 (nudge-down 2 door) r))

(define rr<->rr (above-room rr door rr))

(define house (beside-room rr<->rr door rr))

(define big-house (above-room house door house))


(define r-back  (place-in-room
                 (room 4 2 wall-tile)
                 sword-wall-hanging))

(define (add-back-wall r)
  (above-room (place-in-room
               (room (/ (width r) 32)
                     2
                     wall-tile)
               sword-wall-hanging) r))

(define little-room
  (beside-room (add-back-wall (place-in-room
                               (place-in-room r bed)
                               (nudge-down 2 exit-door)))
               (nudge-down 3 door)
               (add-back-wall (place-in-room r (apple-barrel)))))

(define (little-room-game p)
  #;(room-list->entity-list little-room)
  (cons p
        (room-list->entity-list little-room)))


(define (default-player)
  (add-component
   (basic-hero (posn 10 100))
   (link-follower)))


(define (player g)
  (or (and g (game->link-follower g))
      (default-player)))

(define (enter g)
  (little-room-game
   (set-posn
    (player g)
    (posn 30 150))))

(module+ test
  (start-game (enter #f))
  #;(test-room little-room)


  #;(test-room 
     #;little-room
     #;house
     #;big-house
     #;(beside-room (nudge-down 1 door) r door)))



