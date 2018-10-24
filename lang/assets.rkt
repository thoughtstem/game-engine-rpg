#lang racket

(provide (all-defined-out))

(require 2htdp/image
         game-engine)

;NOTE.  Don't use these assets in production.
;  Copyright stuff.
;  Educational use is fine...

(define outdoor-set
  (bitmap "images/outdoor-set-1.png"))


(define house-set
  (bitmap "images/interior-set-1.png"))

(define barrel-set
  (bitmap "images/barrels.png"))



(define wall-tile
  (freeze
   (crop (* 0 32) (* 14 32)
         (* 1 32) (* 1 32)
         house-set)))

(define wall-top-tile
  (freeze
   (crop (* 0 32) (* 13 32)
         (* 1 32) (* 1 32)
         house-set)))

(define wall-top-bumper-tile
  (freeze
   (crop 0 0
         32 16
         wall-top-tile)))

(define wall-side-tile
  (freeze
   (crop (* 0 32) (* 11 32)
         (* 1 32) (* 1 32)
         house-set)))

(define wall-side-bumper-tile
  (freeze
   (crop 24 0
         32 32
         wall-side-tile)))

(define bed-tile
  (freeze
   (crop (* 15 32) (- (* 2 32) 16)
         (* 1 32) (* 2 32)
         house-set)))

(define pillow-tile
  (freeze
   (crop (* 15 32) (* 7 32)
         (* 1 32) (* 1 32)
         house-set)))

(define mat-tile
  (freeze
   (crop (* 13 32) (* 2 32)
         (* 1 32) (* 1 32)
         house-set)))

(define sword-tile
  (freeze
   (crop (* 13 32) (* 3 32)
         (* 1 32) (* 1 32)
         house-set)))

(define barrel-tile
  (freeze
   (crop (* 14 32) (* 0 32)
         (* 1 32) (* 2 32)
         house-set)))

(define wood-tile
  (freeze
   (crop (* 4 32) (* 7 32)
         (* 1 32) (* 1 32)
         house-set)))

(define grass-tile
  (freeze
   (crop (* 0 32) (* 0 32)
         (* 1 32) (* 1 32)
         outdoor-set)))

(define dark-grass-tile
  (overlay
   (square 32 'solid (make-color 0 0 0 200))
   grass-tile))

(define base-board-tile
  (freeze
   (crop (* 5 32) (* 14 32)
         (* 1 32) (* 1 32)
         house-set)))


(define apple-barrel-tile
  (freeze
   (crop (* 0 31) (* 1 31)
         (* 1 31) (* 1 31)
         barrel-set)))

(define empty-barrel-tile
  (freeze
   (crop (* 7 31) (* 5 31)
         (* 1 31) (* 1 31)
         barrel-set)))





(define (make-decoration i (static? #f))
  (define e (sprite->entity i
                            #:name "decoration"
                            #:position (posn 0 0)))

  (if (not static?) e
      (add-components e 
                      (static)
                      (physical-collider))))


(define empty-barrel
  (make-decoration empty-barrel-tile #t))

(define (empty-apple-barrel)
  (make-decoration empty-barrel-tile #t))

(define (apple-barrel)
  (add-components
   (make-decoration apple-barrel-tile #t)
   
   (on-collide "player"
               (do-many (λ(g e)
                          ((spawn (empty-apple-barrel)) g e))
                        
                        (λ(g e)
                          (add-component e (after-time 1 die)))))
   (on-collide "hero"
               (do-many (λ(g e)
                          ((spawn (empty-apple-barrel)) g e))
                        
                        (λ(g e)
                          (add-component e (after-time 1 die)))))))


(define bed
  (make-decoration bed-tile #t))

(define barrel
  (make-decoration barrel-tile #t))

(define pillow
  (make-decoration pillow-tile))

(define mat
  (make-decoration mat-tile))

(define sword-wall-hanging
  (make-decoration sword-tile))





(provide red-house-style
         green-house-style
         brown-house-style

         closed-door
         open-door

         sword-sign
         shield-sign
         potion-sign

         house-exterior)

(define overworld-set
  (bitmap "images/house-overworld-1.png"))

(define red-house-style
  (freeze
   (crop (* 0 32) (* 3 32)
         (* 3 32) (* 4 32)
         overworld-set)))


(define green-house-style
  (freeze
   (crop (* 0 32) (* 7 32)
         (* 3 32) (* 4 32)
         overworld-set)))


(define brown-house-style
  (freeze
   (crop (* 3 32) (* 3 32)
         (* 3 32) (* 4 32)
         overworld-set)))

(define closed-door
  (freeze
   (crop (* 1 32) (* 0 32)
         (* 1 32) (* 1 32)
         overworld-set)))

(define open-door
  (freeze
   (crop (* 2 32) (* 0 32)
         (* 1 32) (* 1 32)
         overworld-set)))

(define sword-sign
  (freeze
   (crop (* 0 32) (* 1 32)
         (* 1 32) (* 1 32)
         overworld-set)))

(define shield-sign
  (freeze
   (crop (* 1 32) (* 1 32)
         (* 1 32) (* 1 32)
         overworld-set)))

(define potion-sign
  (freeze
   (crop (* 2 32) (* 1 32)
         (* 1 32) (* 1 32)
         overworld-set)))


;Can we make this an entity?
;  How cool can it be?
(define (house-exterior style door (sign empty-image))
  (overlay/offset
   sign
   0 -20
   (overlay/align "middle" "bottom"
                  door
                  style)))






(provide portal
         portal-style-1)

;https://opengameart.org/content/portals
(define portal-rings-1
  (bitmap "images/portalRings1.png"))

(define (portal-rings-1-row n)
  (freeze
   (crop (* 0 32) (* n 32)
         (* 4 32) (* 1 32)
         portal-rings-1)))

(define last-portal-frame
  (freeze
   (crop (* 0 32) (* 4 32)
         (* 1 32) (* 1 32)
         portal-rings-1)))

(define portal-style-1
  (beside (portal-rings-1-row 0)
          (portal-rings-1-row 1)
          (portal-rings-1-row 2)
          (portal-rings-1-row 3)
          last-portal-frame))

(define (portal style)
  (sprite->entity
   (sheet->sprite style
    #:rows       1
    #:columns    17
    #:row-number 1
    #:speed      3)
   #:name "portal"
   #:position (posn 0 0)))


;Maybe for maching squares later?

(provide stone-platform-tile)

(define outdoor-set-2
  (bitmap "images/outdoor-set-2.png"))

(define stone-platform-tile
  (freeze
   (crop (* 12 32) (* 1 32)
         (* 2 32) (* 2 32)
         outdoor-set-2)))



;Other house set

(define (stone-house [p (posn 0 0)] #:tile [tile 0] #:components [c #f] . custom-components )
  (generic-entity (scale 0.75 (bitmap "images/stone-house.png"))
                  p
                  #:tile tile
                  #:components (cons c custom-components))  )

(define (wood-house [p (posn 0 0)] #:tile [tile 0] #:components (c #f) . custom-components )
  (generic-entity (scale 0.75 (bitmap "images/wood-house.png"))
                  p
                  #:tile tile
                  #:components (cons c custom-components))  )

(define (brick-house [p (posn 0 0)] #:tile [tile 0] #:components (c #f) . custom-components )
  (generic-entity (bitmap "images/brick-house.png")
                  p
                  #:tile tile
                  #:components (cons c custom-components))  )

(define (round-tree [p (posn 0 0)] #:tile [tile 0] #:components (c #f) . custom-components )
  (generic-entity (bitmap "images/round-tree.png")
                  p
                  #:tile tile
                  #:components (cons c custom-components))  )

(define (pine-tree [p (posn 0 0)] #:tile [tile 0] #:components (c #f) . custom-components )
  (generic-entity (bitmap "images/pine-tree.png")
                  p
                  #:tile tile
                  #:components (cons c custom-components))  )

(define (chest [p (posn 0 0)] #:tile [tile 0] #:components (c #f) . custom-components )
  (generic-entity (crop 0 0
                        32 32
                        (bitmap "images/chests.png"))
                  p
                  #:tile tile
                  #:components (cons c custom-components)))

(define (generic-entity i p #:tile tile #:components (custom-components '()))
  (define  required-components
    (list
     (physical-collider)
     (static)
     (active-on-bg tile)))
  
  (sprite->entity i
                  #:name  "thing"
                  #:position p
                  #:components (append
                                required-components
                                custom-components)  ))


(define (builder p thing-to-build)
  (chest p
         #:components
         (active-on-bg 0)
         (producer-of thing-to-build)))





