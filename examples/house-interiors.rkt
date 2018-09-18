#lang racket

(require 2htdp/image
         game-engine
         game-engine-rpg)




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



(test-room (beside-room (add-back-wall (place-in-room r bed))
                        (nudge-down 3 door)
                        (add-back-wall (place-in-room r apple-barrel)))
           #;big-house
           #;(beside-room (nudge-down 1 door) r door))



