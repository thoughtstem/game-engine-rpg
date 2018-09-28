#lang racket

(require game-engine
         game-engine-demos-common
         game-engine-rpg
         "./hyperlink.rkt"
         "./house-interiors.rkt")


;1 Player Entity?
;2 Games.  Each with "hyperlinks" -- whatever that means.
;  Can go back and forth between games...



;Here's a basic mockup of the experience (moving one way into an interior).
;  What will be the interface for people to set up these links between their games and others??

(define (get-player g)
  (findf (λ(e) (string=? "player" (get-name e)))
         (game-entities g)))

(define g (start-game (outdoors (basic-hero (posn 100 100)))))

(define p
  (set-posn
   (get-player g)
   (posn 100 100)))

(start-game (little-room-game p))

