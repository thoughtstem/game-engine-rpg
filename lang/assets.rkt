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
  (generic-entity (simple-sheet->sprite (bitmap "images/round-tree.png"))
                  p
                  #:name "Round Tree"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components))  )

(define (pine-tree [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components )
  (generic-entity (simple-sheet->sprite (bitmap "images/pine-tree.png"))
                  p
                  #:name "Pine Tree"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components))  )

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

(define (crafting-chest [p (posn 0 0)]
                        #:icon   [icon empty-image]
                        #:sprite [sprite #f]
                        #:tile   [tile 0]
                        #:hue    [hue 0]
                        #:size   [size 1]
                        #:components (c #f) . custom-components )
  (define chest-image (crop 0 0
                        32 32
                        (bitmap "images/chests.png")))
  (generic-entity (if sprite
                      sprite
                      (simple-sheet->sprite
                       (overlay (if (= (image-width icon) 0)
                                    icon
                                    (crafting-chest-icon icon chest-image))
                                chest-image)))
                  p
                  #:name "Crafting Chest"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (crafting-chest-icon avatar chest)
  (define avatar-w (image-width avatar))
  (define avatar-h (image-height avatar))
  
  (define squared-icon
    (scale .5
           (scale-to-fit
            (overlay
             avatar
             (freeze (if (> avatar-w avatar-h)
                 (rectangle avatar-w avatar-w "solid" 'transparent)
                 (rectangle avatar-h avatar-h "solid" 'transparent))))
            (image-width chest))))

  (define icon-w (image-width squared-icon))
  (define icon-h (image-height squared-icon))
  (overlay squared-icon
           (freeze (overlay (freeze (rectangle (+ icon-w 4) (+ icon-h 4) "outline" (pen "white" 1 "solid" "butt" "bevel")))
                            (rectangle (+ icon-w 6) (+ icon-h 6)  "solid"  (make-color 20 20 20 150)))))

  )

;additional entities from LPC

(define (cat [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components)
  (generic-entity (sheet->sprite (bitmap "images/cat-sprite.png") 
                                 #:rows       1
                                 #:columns    3
                                 #:row-number 1
                                 #:speed      3)
                  p
                  #:name "Cat"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (black-cat [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components)
  (generic-entity (sheet->sprite (bitmap "images/black-cat-sprite.png") 
                                 #:rows       4
                                 #:columns    3
                                 #:row-number 1
                                 #:speed      3)
                  p
                  #:name "Black Cat"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (white-cat [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components)
  (generic-entity (sheet->sprite (bitmap "images/white-cat-sprite.png") 
                                 #:rows       4
                                 #:columns    3
                                 #:row-number 1
                                 #:speed      3)
                  p
                  #:name "White Cat"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (bat [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components)
  (generic-entity (sheet->sprite (bitmap "images/bat-sprite.png") 
                                 #:rows       4
                                 #:columns    3
                                 #:row-number 1
                                 #:speed      3)
                  p
                  #:name "Bat"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (slime [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components)
  (generic-entity (sheet->sprite (bitmap "images/slime-sprite.png") 
                                 #:rows       4
                                 #:columns    3
                                 #:row-number 1
                                 #:speed      3)
                  p
                  #:name "Slime"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (snake [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components (c #f) . custom-components)
  (generic-entity (sheet->sprite (bitmap "images/snake-sprite.png") 
                                 #:rows       4
                                 #:columns    3
                                 #:row-number 4
                                 #:speed      3)
                  p
                  #:name "Snake"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (apples [p (posn 0 0)] #:tile [tile 0] #:hue [hue 0] #:size [size 1] #:components [c #f] . custom-components)
  (generic-entity (simple-sheet->sprite apple-barrel-tile)
                  p
                  #:name "Apples"
                  #:tile tile
                  #:hue hue
                  #:size size
                  #:components (cons c custom-components)))

(define (simple-sheet->sprite i)
  (sheet->sprite i
                 #:rows       1
                 #:columns    1
                 #:row-number 1
                 #:speed      0))

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
  (->* (animated-sprite? posn?  #:tile number? #:hue number? #:size number?)
       (#:name string? #:components list?)
       entity?)
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
                              (sprite-map (curry change-img-hue hue) i))
                  #:name  name
                  #:position p
                  #:components (append
                                required-components
                                custom-components)  ))

(define component? any/c)

(define/contract (crafting-entity #:sprite sprite #:position [p (posn 0 0)] #:name [name "thing"] #:tile [tile 0] #:components [c #f] . custom-components)
  (->* (#:sprite animated-sprite? )
       (#:position posn? #:name string? #:tile number? #:components component?)
       #:rest (listof component?)
       entity?)
  (define  required-components
    (list
     (physical-collider)
     (static)
     (active-on-bg tile)
     ))
  
  (sprite->entity sprite
                  #:name  name
                  #:position p
                  #:components (append
                                required-components
                                (list c)
                                custom-components)))


(define (builder p thing-to-build #:build-time [build-time 0])
  (chest p
         #:components
         (active-on-bg 0)
         (producer-of thing-to-build #:build-time build-time)))

;---- Added icon-list parameter
(define (crafting-list dialog-list icon-list pos #:sound [rsound #f])
  (define selection 0)
  (define font-size 18)
  (define dialog-list-sprite (draw-crafting-list dialog-list icon-list font-size selection))
  (sprite->entity dialog-list-sprite
                  #:name       "crafting list"
                  #:position   pos
                  #:components (static)
                               (hidden)
                               (layer "ui")
                               (on-start (do-many (go-to-pos 'center)
                                                  show
                                                  (spawn (crafting-selection dialog-list
                                                                             (image-width dialog-list-sprite)
                                                                             font-size
                                                                             selection
                                                                             rsound) #:relative? #f)))
                               (on-key 'enter die)
                               ))
; ----

(define (crafting-selection dialog-list max-width font-size selection rsound)
  (define select-box
    (overlay (rectangle (- max-width 14)
                        (+ 4 (image-height (text "Blank" font-size "transparent")))
                        "outline"
                        (pen "white" 2 "solid" "butt" "bevel"))
             (rectangle (- max-width 10)
                        (+ 8 (image-height (text "Blank" font-size "transparent")))
                        "outline"
                        (pen "black" 4 "solid" "butt" "bevel"))))
  (define box-height (image-height select-box))
  (define offset (posn 0 (get-selection-offset (length dialog-list) box-height selection)))
  (sprite->entity select-box
                  #:name       "crafting selection"
                  #:position   (posn 0 0) ;(posn (/ WIDTH 2) (+ (/ HEIGHT 2) (posn-y offset)))
                  #:components (static)
                               (hidden)
                               (layer "ui")
                               (counter selection)
                               (on-start show)
                               (lock-to "crafting list" #:offset offset)
                               ;(on-key 'space die)
                               (on-key 'enter die)
                               (on-key 'up   (if rsound
                                                 (do-many (previous-crafting-option dialog-list box-height)
                                                          (play-sound-from "player" rsound))
                                                 (previous-dialog-option dialog-list box-height)))
                               (on-key 'down (if rsound
                                                 (do-many (next-crafting-option dialog-list box-height)
                                                          (play-sound-from "player" rsound))
                                                 (next-dialog-option dialog-list box-height)))))

(define (next-crafting-option dialog-list box-height)
  (lambda (g e)
    (define new-index (modulo (add1 (get-counter e)) (length dialog-list)))
    (define offset (posn 0 (get-selection-offset (length dialog-list) box-height new-index)))
    (update-entity (update-entity e lock-to? (lock-to "crafting list" #:offset offset))
                   counter?
                   (counter new-index))))

(define (previous-crafting-option dialog-list box-height)
  (lambda (g e)
    (define new-index (modulo (sub1 (get-counter e)) (length dialog-list)))
    (define offset (posn 0 (get-selection-offset (length dialog-list) box-height new-index)))
    (update-entity (update-entity e lock-to? (lock-to "crafting list" #:offset offset))
                   counter?
                   (counter new-index))))

(define (get-crafting-selection)
  (lambda (g e)
    (define selection (get-counter (get-entity "crafting selection" g)))
    (displayln (~a "Crafting Selection: " selection))
    (update-entity e counter? (counter selection))))

(define (crafter p
                 thing-to-build
                 #:sprite     [sprite #f]
                 #:build-time [build-time 0]
                 #:show-info? [show-info? #f]
                 #:rule       [rule (λ (g e) #t)]
                 #:components [c #f] . custom-components)
  (define as (if (procedure? thing-to-build)
                 (get-component (thing-to-build) animated-sprite?)
                 (get-component thing-to-build animated-sprite?)))
  (crafting-chest p
                  #:icon       (render as)
                  #:sprite     sprite
                  #:components (active-on-bg 0)
                               (counter 0)
                               (crafter-of thing-to-build #:build-time build-time #:show-info? #f #:rule rule #:selection 0)
                               (cons c custom-components)))

(define (crafting-menu #:open-key [open-key 'space]
                       #:open-sound [open-sound #f]
                       #:select-sound [select-sound #f]
                       #:recipes r
                                 . recipes)

  (define all-recipes (append (list r) recipes))

  (define (ingredients-list->rule i-list)
    (define rules-list (map in-backpack? i-list))
    (if (empty? i-list)
        (λ (g e) #t)
        (apply and/r  rules-list)))

  (define (recipe-list->icon l)
    (define recipe-item (first l))
    (define as (if (procedure? recipe-item)
                   (get-component (recipe-item) animated-sprite?)
                   (get-component recipe-item animated-sprite?)))
    (render as))

  (define (recipe-list->name l)
    (define recipe-item (first l))
    (define recipe-entity (if (procedure? recipe-item)
                             (recipe-item)
                             recipe-item))
    (get-name recipe-entity))
  
  (define crafting-icons
    (map recipe-list->icon all-recipes))

  (define crafting-options
    (map recipe-list->name all-recipes))
       
  (list
   (on-key open-key #:rule (and/r near-player?
                                  all-dialog-closed?)
          (do-many (set-counter 0)
                   (spawn (crafting-list crafting-options
                                               crafting-icons
                                               (posn 0 0)
                                               ;(posn (/ WIDTH 2) (/ HEIGHT 2))
                                               #:sound select-sound))
                   (play-sound open-sound)))

   
   (for/list ([i (range 0 (length all-recipes))]
              [j all-recipes])
     (crafter-of (first j)
                 #:build-time (second j)
                 #:show-info? #f
                 #:rule (and/r (ingredients-list->rule (third j))
                               (fourth j))
                 #:selection i)
             )))

(define (recipe #:product product #:build-time [build-time 0] #:ingredients [i-list '()] #:rule [rule (λ (g e) #t)])
  (list product
        build-time
        i-list
        rule))





