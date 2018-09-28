#lang racket

(provide outdoors
         enter
         bg-entity)


(require game-engine
         game-engine-rpg)

(require racket/runtime-path)

(define-runtime-path images "images")

(define forest-bg (bitmap/file (build-path images "bg-1.png")))



(define (bg-entity)
  (sprite->entity forest-bg
                  #:name     "bg"
                  #:position   (posn 0 0)))
(define player-sprite
  (sheet->sprite (bitmap/file (build-path images "girl-sprite.png"))
                 #:rows       4
                 #:columns    4
                 #:row-number 3
                 #:speed      3))

(define house
  (sprite->entity (square 30 'solid 'brown)
                  #:name "house"
                  #:position (posn 200 200)
                  #:components
                  (static)
                  (physical-collider)
                  (link "./Test2")))

(define (player-entity p)
  (sprite->entity player-sprite
                  #:name       "player"
                  #:position   p
                  #:components
                  ;Ultimately don't want to HAVE to do this.
                  ;Should transfer player from world to world, keeping manager attached...
                  ;  Putting it here just for testing....
                  
                  (link-follower)
                  (direction 0)
                  (rotation-style 'left-right)
                  (physical-collider)
                               
                  (key-movement 5 #:rule all-dialog-closed?)
                               
                  (counter 0)
                  (on-no-key-movement (stop-animation))
                  (on-key-movement (start-animation))))

(define (outdoors (player (player-entity (posn 0 0))))
  (list player
        (bg-entity)))

(define (enter g)
  (list (or (game->link-follower g)
            (player-entity (posn 200 250)))
        house
        (bg-entity)))

(module+ test 
  (start-game
   (enter #f)))





