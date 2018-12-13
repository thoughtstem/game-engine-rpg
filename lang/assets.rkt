#lang racket

(provide (all-defined-out))


(require 2htdp/image
         game-engine)

(define (simple-sheet->sprite i)
  (sheet->sprite i
                 #:rows       1
                 #:columns    1
                 #:row-number 1
                 #:speed      0))


;NOTE.  Don't use these assets in production.
;  Copyright stuff.
;  Educational use is fine...

(define FOREST-BG
  (bitmap "images/lpc_forest.png"))

(define SNOW-BG
  (bitmap "images/lpc_snow.png"))

(define barrel-set
  (bitmap "images/barrels.png"))

#|
(define outdoor-set
  (bitmap "images/outdoor-set-1.png"))

(define house-set
  (bitmap "images/interior-set-1.png"))

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

|#

(define apple-barrel-tile
  (freeze
   (crop (* 0 31) (* 1 31)
         (* 1 31) (* 1 31)
         barrel-set)))

;Other house set

(define (player [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components [c #f] . custom-components )
  (generic-entity (simple-sheet->sprite (scale 0.75 (bitmap "images/player.png")))
                  p
                  #:name "Player"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components))  )

(define (npc [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components [c #f] . custom-components )
  (generic-entity (simple-sheet->sprite (scale 0.75 (bitmap "images/npc.png")))
                  p
                  #:name "NPC"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components))  )

(define (stone-house [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components [c #f] . custom-components )
  (generic-entity (simple-sheet->sprite (scale 0.75 (bitmap "images/stone-house.png")))
                  p
                  #:name "Stone House"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components))  )

(define (wood-house [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components )
  (generic-entity (simple-sheet->sprite (scale 0.75 (bitmap "images/wood-house.png")))
                  p
                  #:name "Wood House"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components))  )

(define (brick-house [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components )
  (generic-entity (simple-sheet->sprite (bitmap "images/brick-house.png"))
                  p
                  #:name "Brick House"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components))  )

(define (round-tree [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components )
  (define tree-top-entity
    (sprite->entity  (sprite-map (curry scale size)
                                 (sprite-map (curry change-img-hue hue) (new-sprite (bitmap "images/round-tree-top.png"))))
                    #:name       "Round Tree Top"
                    #:position   (posn 0 -56)
                    #:components (layer "tops")
                                 (active-on-bg tile)
                                 (hue-val hue)
                                 (size-val size)))
  (generic-entity (simple-sheet->sprite (bitmap "images/round-tree-trunk.png"))
                  p
                  #:name "Round Tree Trunk"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (append
                                (list (precompiler tree-top-entity)
                                      (on-start (spawn tree-top-entity)))
                                (cons c custom-components)))  )

(define (pine-tree [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components )
  (define tree-top-entity
    (sprite->entity  (sprite-map (curry scale size)
                                 (sprite-map (curry change-img-hue hue) (new-sprite (bitmap "images/pine-tree-top.png"))))
                    #:name       "Pine Tree Top"
                    #:position   (posn 0 -50)
                    #:components (layer "tops")
                                 (active-on-bg tile)
                                 (hue-val hue)
                                 (size-val size)))
  (generic-entity (simple-sheet->sprite (bitmap "images/pine-tree-trunk.png"))
                  p
                  #:name "Pine Tree Trunk"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (append
                                (list (precompiler tree-top-entity)
                                      (on-start (spawn tree-top-entity)))
                                (cons c custom-components)))  )

(define (chest [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components )
  (generic-entity (simple-sheet->sprite
                   (crop 0 0
                        32 32
                        (bitmap "images/chests.png")))
                  p
                  #:name "Chest"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))



;MISC from LPC

; ==== MISC SHEETS =====
(define CAT-SHEET        (bitmap "images/cat-sprite.png"))
(define BLACK-CAT-SHEET  (bitmap "images/black-cat-sprite.png"))
(define WHITE-CAT-SHEET  (bitmap "images/white-cat-sprite.png"))
(define BAT-SHEET        (bitmap "images/bat-sprite.png"))
(define SLIME-SHEET      (bitmap "images/slime-sprite.png"))
(define SNAKE-SHEET      (bitmap "images/snake-sprite.png"))

; ==== MISC SPRITES =====
(define cat-sprite
  (sheet->sprite CAT-SHEET 
                 #:rows       1
                 #:columns    3
                 #:row-number 1
                 #:speed      3))

(define black-cat-sprite
  (sheet->sprite  BLACK-CAT-SHEET
                  #:rows       4
                  #:columns    3
                  #:row-number 1
                  #:speed      3))

(define white-cat-sprite
  (sheet->sprite WHITE-CAT-SHEET 
                 #:rows       4
                 #:columns    3
                 #:row-number 1
                 #:speed      3))

(define bat-sprite
  (sheet->sprite BAT-SHEET
                 #:rows       4
                 #:columns    3
                 #:row-number 1
                 #:speed      3))

(define slime-sprite
  (sheet->sprite SLIME-SHEET 
                 #:rows       4
                 #:columns    3
                 #:row-number 1
                 #:speed      3))

(define snake-sprite
  (sheet->sprite SNAKE-SHEET 
                 #:rows       4
                 #:columns    3
                 #:row-number 4
                 #:speed      3))


; ==== MISC ENTITIES =====
(define (cat [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components)
  (generic-entity cat-sprite
                  p
                  #:name "Cat"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (black-cat [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components)
  (generic-entity black-cat-sprite
                  p
                  #:name "Black Cat"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (white-cat [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components)
  (generic-entity white-cat-sprite
                  p
                  #:name "White Cat"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (bat [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components)
  (generic-entity bat-sprite
                  p
                  #:name "Bat"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (slime [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components)
  (generic-entity slime-sprite
                  p
                  #:name "Slime"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (snake [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components)
  (generic-entity snake-sprite
                  p
                  #:name "Snake"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

;food LPC entities

;==== FOOD SHEETS ====
(define APPLES-SHEET  apple-barrel-tile)
(define CHERRY-SHEET  (bitmap "images/cherry-sprite.png"))
(define STEAK-SHEET   (bitmap "images/steak-sprite.png"))

;==== FOOD SPRITES ====
(define apples-sprite  (simple-sheet->sprite APPLES-SHEET))

(define cherry-sprite  (simple-sheet->sprite CHERRY-SHEET))

(define steak-sprite   (simple-sheet->sprite STEAK-SHEET))

;==== FOOD ENTITIES ====
(define (apples [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components [c #f] . custom-components)
  (generic-entity apples-sprite
                  p
                  #:name "Apples"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

;no (cherry-entity)


; ==== COIN SHEETS =====
(define COIN-SHEET        (bitmap "images/coin.png"))
(define COPPER-COIN-SHEET (bitmap "images/copper-coin.png"))
(define SILVER-COIN-SHEET (bitmap "images/silver-coin.png"))
(define GOLD-COIN-SHEET   (bitmap "images/gold-coin.png"))

; ==== COIN SPRITES ====
(define coin-sprite
  (sheet->sprite (bitmap "images/coin.png") 
                                 #:columns  8
                                 #:delay    2))

(define copper-coin-sprite
  (sheet->sprite (bitmap "images/copper-coin.png") 
                                 #:columns  8
                                 #:delay    2))

(define silver-coin-sprite
  (sheet->sprite (bitmap "images/silver-coin.png") 
                                 #:columns  8
                                 #:delay    2))

(define gold-coin-sprite
  (sheet->sprite (bitmap "images/gold-coin.png") 
                                 #:columns  8
                                 #:delay    2))


; ==== COIN ENTITIES ====
(define (coin-entity)
  (sprite->entity (sheet->sprite (bitmap "images/coin.png") 
                                 #:columns  8
                                 #:delay    2)
                  #:position   (posn 0 0)
                  #:name       "Coin"
                  #:components (active-on-bg 0)
                               (physical-collider)
                               ))

(define (copper-coin-entity)
  (sprite->entity (sheet->sprite (bitmap "images/copper-coin.png") 
                                 #:columns  8
                                 #:delay    2)
                  #:position   (posn 0 0)
                  #:name       "Copper Coin"
                  #:components (active-on-bg 0)
                               (physical-collider)
                               ))

(define (silver-coin-entity)
  (sprite->entity (sheet->sprite (bitmap "images/silver-coin.png") 
                                 #:columns  8
                                 #:delay    2)
                  #:position   (posn 0 0)
                  #:name       "Silver Coin"
                  #:components (active-on-bg 0)
                               (physical-collider)
                               ))

(define (gold-coin-entity)
  (sprite->entity (sheet->sprite (bitmap "images/gold-coin.png") 
                                 #:columns  8
                                 #:delay    2)
                  #:position   (posn 0 0)
                  #:name       "Gold Coin"
                  #:components (active-on-bg 0)
                               (physical-collider)
                               ))


(define (customizer-system)
  (hue-val 0)
  (size-val 1)
  (on-key "[" #:rule carried? (do-many (scale-sprite 0.75)
                                       (multiply-size-val-by 0.75)))
  (on-key "]" #:rule carried? (do-many (scale-sprite 1.25)
                                       (multiply-size-val-by 1.25)))
  (on-key "p" #:rule carried? (do-many (change-color-by 20)
                                       (change-hue-val-by 20)))
  (on-key 'backspace #:rule carried? die)
  )

(define/contract (generic-entity i p #:name [name "thing"] #:tile tile #:hue hue #:size size #:components (custom-components '()))
  (->* ((or/c animated-sprite? image?) posn?  #:tile number? #:hue number? #:size number?)
       (#:name string? #:components list?)
       entity?)

  (define sprite (if (image? i)
                     (new-sprite i)
                     i))
  (define  required-components
    (list
     (physical-collider)
     (static)
     (active-on-bg tile)
     (hue-val hue)
     (size-val size)
     (on-key "[" #:rule carried? (do-many (scale-sprite 0.75)
                                          (multiply-size-val-by 0.75)))
     (on-key "]" #:rule carried? (do-many (scale-sprite 1.25)
                                          (multiply-size-val-by 1.25)))
     (on-key "p" #:rule carried? (do-many (change-color-by 20)
                                          (change-hue-val-by 20)))
     (on-key 'backspace #:rule carried? die)
     ))
  
  (sprite->entity (sprite-map (curry scale size)
                              (sprite-map (curry change-img-hue hue) sprite))
                  #:name  name
                  #:position p
                  #:components (append
                                required-components
                                custom-components)  ))

(define (builder p thing-to-build #:build-time [build-time 0])
  (chest p
         #:components
         (active-on-bg 0)
         (producer-of thing-to-build #:build-time build-time)))






