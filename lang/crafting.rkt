#lang racket

(provide (rename-out (make-recipe recipe))
         (except-out (struct-out recipe) recipe)
         recipe->system
         crafting-entity
         crafting-chest
         crafting-menu
         
         carrot-sprite
         carrot-entity
         carrot-stew-entity
         
         cauldron-sprite
         campfire-sprite
         wood-table-sprite
         fish-sprite
         cooked-fish-sprite
         bowl-sprite
         toasted-marshmallow-sprite
         smores-sprite
         carrot-stew-sprite
         fish-stew-sprite
         
         consumable)
 
(require 2htdp/image
         game-engine
         "./assets.rkt")

(define component? any/c)

(define (crafting-chest [p (posn 0 0)]
                        #:name   [name "Crafting Chest"]
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
                  #:name name
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


(define (get-selection-offset max-options box-height selection)
  (* (- selection (/ (sub1 max-options) 2)) box-height))

;---- Added icon-list parameter
(define (crafting-list dialog-list icon-list pos
                       #:selection    [selection 0]
                       #:select-sound [select-sound #f])
  (define LINE-HEIGHT 24)
  (define main-box-height (* LINE-HEIGHT (length dialog-list)))
  (define dialog-list-sprites (fast-crafting-list dialog-list icon-list selection))

  (define (next-option)
    (lambda (g e)
      (define new-index (modulo (add1 (get-counter e)) (length dialog-list)))
      (define selection-box-sprite
        (get-component e (curry component-eq? (get-storage-data "selection-box-sprite" e))))
      
      (define new-offset (+ (/ LINE-HEIGHT 2)
                            (- (* new-index LINE-HEIGHT)
                               (/ main-box-height 2))))
      
      (define new-selection-box-sprite
        (set-y-offset new-offset selection-box-sprite))
      (~> e
          (update-entity _ (curry component-eq? selection-box-sprite) new-selection-box-sprite)
          (update-entity _ counter? (counter new-index)))))

  (define (previous-option)
    (lambda (g e)
      (define new-index (modulo (sub1 (get-counter e)) (length dialog-list)))
      (define selection-box-sprite
        (get-component e (curry component-eq? (get-storage-data "selection-box-sprite" e))))
      
      (define new-offset (+ (/ LINE-HEIGHT 2)
                            (- (* new-index LINE-HEIGHT)
                               (/ main-box-height 2))))
      
      (define new-selection-box-sprite
        (set-y-offset new-offset selection-box-sprite))
      (~> e
          (update-entity _ (curry component-eq? selection-box-sprite) new-selection-box-sprite)
          (update-entity _ counter? (counter new-index)))))
  
  (sprite->entity dialog-list-sprites
                  #:name       "crafting list"
                  #:position   pos
                  #:components (static)
                               (hidden)
                               (layer "ui")
                               (counter selection)
                               (storage "selection-box-sprite" (list-ref dialog-list-sprites (- (length dialog-list-sprites) 4)))
                               (on-start (do-many (go-to-pos 'center)
                                                  show))
                               (on-key 'enter die)
                               (on-key 'up   (if select-sound
                                                 (do-many (previous-option)
                                                          (play-sound-from "player" select-sound))
                                                 (previous-option)))
                               (on-key 'down (if select-sound
                                                 (do-many (next-option)
                                                          (play-sound-from "player" select-sound))
                                                 (next-option)))
                               ))

(struct recipe (product build-time ingredients cost rule))

(define (crafting-menu #:open-key [open-key 'space]
                       #:open-sound [open-sound #f]
                       #:selection    [selection 0]
                       #:select-sound [select-sound #f]
                       #:recipe-list [r-list '()]
                       ;#:recipes r
                       ;          . recipes
                                 )

  ;(set! recipes (apply append recipes))
  ;(displayln recipes)
  (define all-recipes r-list
    ;(flatten (append (list r) recipes))
    )

  (define (ingredients-list->rule i-list)
    (define rules-list (map in-backpack? i-list))
    (if (empty? i-list)
        (λ (g e) #t)
        (apply and/r  rules-list)))


  ; TODO: Pass along sprites as is instead of rendering.
  (define (recipe->icon r)
    (define recipe-item (recipe-product r))
    (if (procedure? recipe-item)
        (draw-entity (recipe-item))
        (draw-entity recipe-item))
    )

  (define (recipe->name r)
    (define recipe-item (recipe-product r))
    (define recipe-entity (if (procedure? recipe-item)
                             (recipe-item)
                             recipe-item))
    (get-name recipe-entity))
  
  (define crafting-icons
    (map recipe->icon all-recipes))

  (apply precompile! (map (curryr scale-to-fit 24) crafting-icons))

  (define crafting-options
    (map recipe->name all-recipes))

  (define crafting-list-entity
    (crafting-list crafting-options
                   crafting-icons
                   (posn 0 0)
                   #:selection    selection
                   #:select-sound select-sound))
  
  (flatten (list
   (precompiler crafting-list-entity)
   (sound-stream)
   (on-key open-key #:rule (and/r near-player?
                                  all-dialog-closed?)
          (do-many (set-counter 0)
                   (spawn crafting-list-entity #:relative? #f)
                   (play-sound open-sound)))

   
   (for/list ([i (range 0 (length all-recipes))]
              [j all-recipes])
     (crafter-of (recipe-product j)
                 #:build-time (recipe-build-time j)
                 #:show-info? #f
                 #:rule (and/r (ingredients-list->rule (recipe-ingredients j))
                               (recipe-rule j))
                 #:selection i)
             ))))

(define (has-score? amount)
  (lambda (g e)
    (define score (get-counter (get-entity "score" g)))
    (>= score amount)))

(define (make-recipe #:product product
                     #:build-time [build-time 0]
                     #:ingredients [i-list '()]
                     #:cost [cost 0]
                     #:rule [rule (if (> cost 0)
                                      (has-score? cost)
                                      (λ (g e) #t))])

  (recipe product #;(~> product
                        (remove-component _ on-key?)
                        (add-components _ (consumable))) ; this is handled at the custom food level
          build-time
          i-list
          cost
          rule))

(define (recipe->system r)
  (define product-name (get-name (recipe-product r)))
  (define i-list (recipe-ingredients r))
  (define (remove-items g e1 e2)
    (if ((crafting? product-name) g e2)
        (begin ;(displayln (~a "CRAFTING: " product-name))
               ((apply do-many (map remove-item-by-name i-list)) g e2)
                   #;((spawn (backpack-entity #:components (on-rule (crafting? product-name) die))
                           #:relative? #f) g _))
        (begin ;(displayln (~a "NOT CRAFTING: " product-name))
               e2)))
  (observe-change (crafting? product-name) remove-items))


(define (consumable) (on-key 'space #:rule (and/r near-player?
                                                  (nearest-to-player? #:filter (has-component? on-key?))) die))

; ==== CRAFTER AND CRAFTING ENTITY SPRITES ====
(define cauldron-sprite
  (sheet->sprite (scale 0.75 (bitmap "images/cauldron.png"))
                 #:columns 4
                 #:delay 2))

(define campfire-sprite
  (sheet->sprite (bitmap "images/campfire.png")
                 #:columns 4
                 #:delay 4))

(define wood-table-sprite
  (new-sprite (bitmap "images/wood-table.png")))

(define carrot-sprite
  (new-sprite (bitmap "images/carrot.png")))

(define carrot-stew-sprite
  (new-sprite (bitmap "images/carrot-stew.png")))

(define fish-sprite
  (new-sprite (bitmap "images/fish.png")))

(define cooked-fish-sprite
  (new-sprite (bitmap "images/cooked-fish.png")))

(define toasted-marshmallow-sprite
  (new-sprite (bitmap "images/toasted-marshmallow.png")))

(define smores-sprite
  (new-sprite (bitmap "images/smores.png")))

(define bowl-sprite
  (new-sprite (bitmap "images/bowl.png")))

(define fish-stew-sprite
  (new-sprite (bitmap "images/fish-stew.png")))


; === RESPAWNABLE CRAFTING ASSETS ===
; These assets respawn and relocate when collected with spacebar
(define (carrot-entity)
  (sprite->entity (sheet->sprite (bitmap "images/carrot.png")
                                 #:columns  1
                                 #:delay    2)
                  #:position   (posn 0 0)
                  #:name       "Carrot"
                  #:components (active-on-bg 0)
                               ;(hidden)
                               (storable)
                               (physical-collider)
                               (stop-on-edge)
                               #;(on-start (do-many (active-on-random)
                                                  (respawn 'anywhere)
                                                  show))
                               #;(on-key 'space
                                       #:rule (and/r near-player?
                                                     (nearest-to-player? #:filter (has-component? on-key?)))
                                       (do-many (respawn 'anywhere)
                                                (active-on-random)))
                               ))
  
; === CRAFTING PRODUCTS ===
(define (carrot-stew-entity)
  (crafting-entity #:sprite     (new-sprite (bitmap "images/carrot-stew.png"))
                   #:name       "Carrot Stew"
                   #:components (consumable)
                                (storable)))

; === FLATTEN TEST ====


(module+ test
  (require rackunit)
  
  (define smores-recipe
    (make-recipe #:product    (apples)
                 #:build-time 10
                 #:ingredients (list "Toasted Marshmallow")
                 ;#:rule        (has-gold? 10)
                 ))

  (define marshmallows-recipe
    (make-recipe #:product (bat)))

  (define test-menu (flatten (crafting-menu #:open-key 'space
                                            #:recipe-list (list smores-recipe
                                                                marshmallows-recipe)
                                            )))

  
  (define test-menu2 (flatten
                      (crafting-menu #:open-key 'space
                                     #:recipe-list (list smores-recipe
                                                         marshmallows-recipe))))

  
  (define crafting-menu? (listof (or/c on-key? observe-change? precompiler? sound-stream?)))
  (check-equal? (crafting-menu? test-menu) #t)
  (check-equal? (crafting-menu? test-menu2) #t)
  )