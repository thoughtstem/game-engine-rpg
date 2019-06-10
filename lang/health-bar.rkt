#lang racket

(module+ test
  (require rackunit)

  ;Slow way (Does not take advantage of optimizations using animated sprite)
  (let ()
    (define health-entity
      (sprite->entity (draw-health-bar 100 #:max 100)
                      #:name "health"
                      #:position (posn 100 20)
                      #:components (counter 100)
                                   (every-tick (maybe-change-health-by -1 #:max 100))))

    (define g (initialize-game (list health-entity)))

    ;Simulate three ticks for this entity
    (define new-health-entity
      (~> health-entity
          (tick-entity g _)
          (tick-entity g _)
          (tick-entity g _)))

    ;Make sure it looks like it lost three points of health
    (check-equal? (draw-entity new-health-entity)
                  (draw-health-bar 97 #:max 100)))


  ;Fast way (avoids producing new images at runtime).
  ;  No need to precompile hundreds of images either.
  (let ()
    (define health-entity
      (health-bar-entity))

    (define g (initialize-game (list health-entity)))

    ;Simulate three ticks for this entity
    (define new-health-entity
      (~> health-entity
          (tick-entity g _ #:ticks 50)))


    (check-equal? (image-width (draw-entity new-health-entity))
                  (image-width (draw-entity health-entity))
                  "Health bar gui should not change width")

    (check-equal? (image-width (render (last (get-components new-health-entity animated-sprite?))))
                  50
                  "Health bar red portion should change width")))

(provide (all-defined-out))

(require 2htdp/image
         game-engine)

(define (health-bar-entity #:max (max 100)
                           #:starvation-period (starvation-period 0)
                           #:change-by  (change-by -1))
  (define health-bar-slice (rectangle 1 10 'solid 'red))

  (define main-sprite (set-x-scale max (new-sprite health-bar-slice #:animate #f)))

  (define bg-image (rectangle 1 1 'solid (make-color 0 0 0 100)))
  (precompile! bg-image health-bar-slice)
  
  (define bg-sprite (~> bg-image
                        (new-sprite _ #:animate #f)
                        (set-x-scale 104 _)
                        (set-y-scale 14 _)))
  
  (define e
    (sprite->entity bg-sprite
                    #:name "health"
                    #:position (posn 100 20)
                    #:components
                    (storage "health-bar-sprite" main-sprite) ;storing this to get id later
                    (counter 100)
                    (layer "ui")
                    (storage "efficient-health-bar" #t)
                    (if (eq? starvation-period #f)
                        #f
                        (do-every starvation-period (maybe-change-health-by change-by #:max max)))
                    (precompiler main-sprite
                                 bg-sprite)
                    ))

  (add-component e main-sprite)
  )

(define/contract (draw-health-bar amount #:max [max-val 100])
  (-> exact-nonnegative-integer? #:max exact-nonnegative-integer? image?)
  (define health-bar     (pad (rectangle amount 10 "solid" "red")4 4))
  (define max-health-bar (pad (rectangle max-val 10 "solid" "transparent") 0 0))
  (overlay/align "left" "middle"
                 health-bar
                 (health-bar-bg (image-width max-health-bar) (image-height max-health-bar))))

(define/contract (health-bar-bg w h)
  (-> number? number? image?)
  (overlay
   (rectangle (+ 4 w) (+ 4 h) "outline" (pen "white" 2 "solid" "butt" "bevel"))
   (rectangle (+ 8 w) (+ 8 h) "solid"  (make-color 20 20 20 150))))

(define (update-health-bar #:max [max-val 100])
  (lambda (g e)
    (define count (get-counter e))

    (if (efficient-health-bar? e)
        (efficient-update-health-bar g e count #:max max-val)
        (slow-update-health-bar g e count #:max max-val))))

(define (efficient-health-bar? e)
  (get-storage-data "efficient-health-bar" e))

(define (efficient-update-health-bar g e count #:max max-val)
  (define current-health-sprite
    (get-component e ;(get-storage-data "health-bar-sprite" e)
                   (curry component-eq? (get-storage-data "health-bar-sprite" e))
                   ))

  (displayln (~a "FOUND COMPONENT? " current-health-sprite))

  (define percentage (* 100
                        (/ count max-val)))

  (define new-health-sprite
    (~> current-health-sprite
        (set-x-scale  percentage _)
        (set-x-offset (- (/ (- 100 percentage) 2)) _)))


  (update-entity e
                 (is-component? current-health-sprite)
                 new-health-sprite)
  )

(define (slow-update-health-bar g e count #:max max-val)
  (define health-bar (draw-health-bar count #:max max-val))

  ((change-sprite (new-sprite health-bar)) g e))

(define (maybe-change-health-by amount #:min [min-val 0] #:max [max-val 100])
  (lambda (g e)
    (define health (+ (get-counter e) amount))
    (define new-health (cond [(> health max-val) max-val]
                             [(< health min-val) min-val]
                             [else health]))
    ((do-many (set-counter new-health)
              (update-health-bar #:max max-val)) g e)))

(define/contract (health-is-zero? g e)
  (-> (and/c (game-has-entity-named/c "health"))
      entity?
      boolean?)
  
  (define player-health (get-counter (get-entity "health" g)))
  ;(displayln (~a "Player health: " player-health))
  (<= player-health 0)
  )

(define (draw-counter-rpg #:prefix [prefix ""] #:exact-floor? [ef? #f])
  (lambda (g e)
    (define count (get-counter e))

    (define current-sprite (get-component e string-animated-sprite?))
    
    ((change-sprite (set-text (~a prefix count) current-sprite)) g e)

    (update-entity e
                   (is-component? current-sprite)
                   (set-text (~a prefix (if ef? (exact-floor count)
                                            count)) current-sprite))
    ))

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


;==== Version 2 - More abstract bars ====


(define (abstract-progress-bar #:color (color 'red)
                               #:max   (m 100)
                               #:width (w 100)
                               #:height (h 10)
                               #:data-from data-from)
  
  (define health-bar-slice (rectangle 1 1 'solid color))
  (define initial-bar-width w) ;should get an inital value somehow
  (define main-sprite (new-sprite health-bar-slice
                                  #:animate #f          ; MUST NOT ANIMATE TO SATISFY IS-COMPONENT?
                                  #:x-scale initial-bar-width
                                  #:x-offset (/ (- initial-bar-width w) 2)
                                  #:y-scale h))
  (define bg-image (square 1 'solid 'dimgray))
  
  (define bg-sprite
    (new-sprite bg-image
                #:animate #f
                #:x-scale (+ 2 w)
                #:y-scale (+ 2 h))
    )


  (define (update-from-data g e)
    (define data (data-from g))

    (define percentage (/ data m)) ;This should div by max??

    (define bar-width (* percentage w))
    ;(displayln (~a "BAR WIDTH: " bar-width))

    (define new-bar-sprite (~> main-sprite
                               (set-x-scale bar-width _)
                               (set-x-offset (/ (- bar-width w) 2) _)))
    (if data
        (update-entity e
                       (curry component-eq? main-sprite)  
                       new-bar-sprite ;Need to make this a percentage
                       )
        e))

  (define border-image (square 1 'solid 'white #;(make-color 20 20 20 150)))
  
  (define border-sprite (new-sprite border-image
                                    #:animate #f
                                    #:x-scale (+ 4 w)
                                    #:y-scale (+ 4 h))
    )
  
  (sprite->entity (list main-sprite
                          bg-sprite
                          border-sprite)
                    #:name "progress-bar"
                    #:position (posn 0 0)
                    #:components
                    (layer "ui")
                    (precompiler main-sprite
                                 bg-sprite
                                 border-sprite)
                    (do-every 20 update-from-data)
                    )

  ;(add-components e bg-sprite
  ;                  main-sprite
  ;                  )
 
  )

(define (abstract-progress-bar-system #:color [color 'red]
                                      #:max   [m 100]
                                      #:width [w 100]
                                      #:height [h 10]
                                      ;#:data-from data-from
                                      #:stat-name [stat-name "health"]
                                      #:offset [offset (posn 0 0)]
                                      )
  (displayln (~a "BAR-SYSTEM STAT NAME: " stat-name))
  (define health-bar-slice (rectangle 1 1 'solid color))
  (define initial-bar-width w) ;should get an inital value somehow
  (define main-sprite (new-sprite health-bar-slice
                                  #:animate #f ; MUST NOT ANIMATE TO SATISFY IS-COMPONENT?
                                  #:x-scale initial-bar-width
                                  #:x-offset (+ (posn-x offset) (/ (- initial-bar-width w) 2))
                                  #:y-offset (posn-y offset)
                                  #:y-scale h))
  (define bg-image (square 1 'solid 'dimgray))
  
  (define bg-sprite
    (new-sprite bg-image
                #:animate #f
                #:x-scale (+ 2 w)
                #:y-scale (+ 2 h)
                #:x-offset (posn-x offset)
                #:y-offset (posn-y offset))
    )


  #;(define (update-from-data g e)
    (define data (data-from g))

    (define percentage (/ data m)) ;This should div by max??

    (define bar-width (* percentage w))
    ;(displayln (~a "BAR WIDTH: " bar-width))

    (define new-bar-sprite (~> main-sprite
                               (set-x-scale bar-width _)
                               (set-x-offset (/ (- bar-width w) 2) _)))
    (if data
        (update-entity e
                       (curry component-eq? main-sprite)  
                       new-bar-sprite ;Need to make this a percentage
                       )
        e))

  (define border-image (square 1 'solid 'white #;(make-color 20 20 20 150)))
  
  (define border-sprite (new-sprite border-image
                                    #:animate #f
                                    #:x-scale (+ 4 w)
                                    #:y-scale (+ 4 h)
                                    #:x-offset (posn-x offset)
                                    #:y-offset (posn-y offset))
    )
  
  (precompile! health-bar-slice
               bg-image
               border-image)
  
  (reverse (list (storage (~a stat-name "-main-sprite") main-sprite)
                 main-sprite
                 bg-sprite
                 border-sprite
        ;(do-every 20 update-from-data)
        ))
  )



