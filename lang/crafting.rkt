#lang racket

(provide (rename-out (make-recipe recipe))
         recipe->system
         crafting-entity
         crafting-chest
         crafting-menu)
 
(require 2htdp/image
         game-engine
         "./assets.rkt")

(define component? any/c)

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

(struct recipe (product build-time ingredients rule))

(define (crafting-menu #:open-key [open-key 'space]
                       #:open-sound [open-sound #f]
                       #:select-sound [select-sound #f]
                       #:recipes r
                                 . recipes)

  ;(set! recipes (apply append recipes))
  ;(displayln recipes)
  (define all-recipes (flatten (append (list r) recipes)))

  (define (ingredients-list->rule i-list)
    (define rules-list (map in-backpack? i-list))
    (if (empty? i-list)
        (λ (g e) #t)
        (apply and/r  rules-list)))

  (define (recipe->icon r)
    (define recipe-item (recipe-product r))
    (define as (if (procedure? recipe-item)
                   (get-component (recipe-item) animated-sprite?)
                   (get-component recipe-item animated-sprite?)))
    (render as))

  (define (recipe->name r)
    (define recipe-item (recipe-product r))
    (define recipe-entity (if (procedure? recipe-item)
                             (recipe-item)
                             recipe-item))
    (get-name recipe-entity))
  
  (define crafting-icons
    (map recipe->icon all-recipes))

  (define crafting-options
    (map recipe->name all-recipes))
       
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
     (crafter-of (recipe-product j)
                 #:build-time (recipe-build-time j)
                 #:show-info? #f
                 #:rule (and/r (ingredients-list->rule (recipe-ingredients j))
                               (recipe-rule j))
                 #:selection i)
             )))

(define (make-recipe #:product product
                     #:build-time [build-time 0]
                     #:ingredients [i-list '()]
                     #:rule [rule (λ (g e) #t)])
  (recipe product
          build-time
          i-list
          rule))

(define (recipe->system r)
  (define product-name (get-name (recipe-product r)))
  (define i-list (recipe-ingredients r))
  (define (remove-items g e1 e2)
    (if ((crafting? product-name) g e2)
        (begin (displayln (~a "CRAFTING: " product-name))
               ((apply do-many (map remove-item-by-name i-list)) g e2)
                   #;((spawn (backpack-entity #:components (on-rule (crafting? product-name) die))
                           #:relative? #f) g _))
        (begin (displayln (~a "NOT CRAFTING: " product-name))
               e2)))
  (observe-change (crafting? product-name) remove-items))


; === FLATTEN TEST ====


(module+ test
  (require rackunit)
  
  (define smores-recipe
    (make-recipe #:product    apples
                 #:build-time 10
                 #:ingredients (list "Toasted Marshmallow")
                 ;#:rule        (has-gold? 10)
                 ))

  (define marshmallows-recipe
    (make-recipe #:product bat))

  (define test-menu (flatten (crafting-menu #:open-key 'space
                                            #:recipes smores-recipe
                                             marshmallows-recipe
                                             )))

  
  (define test-menu2 (flatten
                      (crafting-menu #:open-key 'space
                                     #:recipes smores-recipe
                                     (list marshmallows-recipe))))

  
  (define crafting-menu? (listof (or/c on-key? observe-change?)))
  (check-equal? (crafting-menu? test-menu) #t)
  (check-equal? (crafting-menu? test-menu2) #t)
  )