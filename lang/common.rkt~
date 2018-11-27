#lang racket

(provide place-in-room
         nudge-down
         nudge-right
         above-room
         beside-room
         room
         room->image
         texture-with
         room-width
         room-height
         room-list->entity-list
         test-room
         (all-from-out "./assets.rkt")
         (all-from-out "./heros.rkt")
         (all-from-out "./links.rkt"))

(require 2htdp/image)
(require (prefix-in h: lang/posn))
(require game-engine)
(require "./heros.rkt")
(require "./assets.rkt")
(require "./links.rkt")

(module+ test
  (require rackunit))

(define (repeat n f x)
  (if (= 0 n)
      x
      (repeat (sub1 n) f (f x))))

(define (explode-tile t)
  (freeze
   (above (beside t t)
          (beside t t))))

(define (texture-with i texture)
  (define larger-texture
    (place-image
     (repeat 6 explode-tile texture)
     0 0
     i))
  
  (freeze
   (mask i larger-texture )))

(define (tile-width t)
  (/ (image-width t) 32))

(define (tile-height t)
  (/ (image-height t) 32))

(define (surround-with i texture)
  (define larger-texture
    (place-image
     (repeat 6 explode-tile texture)
     0 0
     (rectangle (+ 32 (image-width i))
                (+ 32 (image-height i))
                'solid
                'black)))

  (define p1
    (overlay
     i
     larger-texture))

  p1)

(define (add-bg i)
  (overlay i
           (rectangle (+ 32 (image-width i))
                      (+ 32 (image-height i))
                      'solid
                      'black)))

(define (mask-pixel color1 color2)
  (if (eq? (color-alpha color1) 0)
      (make-color 0 0 0 0)
      color2))

(define (mask image1 image2)
  (define image1-list (image->color-list image1))
  (define image2-list (image->color-list image2))
  (color-list->bitmap (map mask-pixel image1-list image2-list) (image-width image1) (image-height image1)))
  


(define (boxify i)
  (overlay (rectangle (sub1 (image-width i))
                      (sub1 (image-height i))
                      'outline
                      'orange)
           i))



;ROOM FUNCTIONS

;Is this gonna work?  Contracts + recursion is blowing my mind...
(define room?
  (or/c entity?
        list?))



(define (room-sprite r)
  #;(room->image r)
  (sheet->sprite (freeze (beside r (boxify r)))
                 #:rows       1
                 #:columns    2
                 #:row-number 1
                 #:speed      -1))



(require threading)




(struct storage (name data))

(define (storage-named? s)
  (and/c storage?
         (λ(c) (string=? s
                         (storage-name c)))))

(define (get-stored-entities e)
  (define c
    (get-component e (storage-named? "to-spawn")))

  (if c
      (storage-data c)
      '()))

(define (set-stored-entities e es)
  (define c
    (get-component e (storage-named? "to-spawn")))

  (define new-c
    (storage "to-spawn" (flatten es)))

  ;Not safe if multiple storages....
  (if c
      (update-entity e storage? new-c)
      (add-component e new-c)))

(define (place-in-room r . es)
  (add-entities-to-room r es))

(define (add-entities-to-room r entities)
  (set-stored-entities r (append entities
                                 (get-stored-entities r))))




(define/contract (image->room r)
  (-> image? room?)

  (define e
    (sprite->entity r
                    #:name       (~a "room" (random 10000))
                    #:position   (posn 0 0)
                    #:components
                    #;(static)
                    #;(physical-collider)
                    (hidden)))

  e)



(define (uniqify-name r)
  (if (entity? r)
      (update-entity r
                     entity-name?
                     (entity-name (~a "room" (random 10000))))
      (map uniqify-name r)))




(define (half x)
  (/ x 2))




(define (room->image_ r)
  (render (get-component r animated-sprite?)))

(define (room->image r)
  (if (entity? r)
      (room->image_ r)
      (if (empty? r)
          empty-image
          (room-list->image (flatten r)))))



(define (highlight-room e n)
  (update-entity e animated-sprite?
                 (struct-copy animated-sprite
                              (get-component e animated-sprite?)
                              [current-frame n])))



(define/contract (room w h texture)
  (-> number? number? image? room?)
  
  (define floor-img
    (texture-with
     (rectangle (* 32 w)
                (* 32 h)
                'solid
                'brown)
     texture))

  (image->room floor-img))





(define/contract (room-x_ r)
  (-> entity? number?)
  (posn-x (get-component r posn?)))

(define/contract (room-x r)
  (-> room? number?)

  (if (entity? r)
      (room-x_ r)
      (apply min (map room-x_ r))))




(define/contract (room-y_ r)
  (-> entity? number?)
  (posn-y (get-component r posn?)))

(define/contract (room-y r)
  (-> room? number?)

  (if (entity? r)
      (room-y_ r)
      (apply min (map room-y_ r))))


(struct my-line (p1 p2) #:transparent)

(define (line-rise l)
  (abs (- (posn-y (my-line-p1 l))
          (posn-y (my-line-p2 l)))))

(define (line-run l)
  (abs (- (posn-x (my-line-p1 l))
          (posn-x (my-line-p2 l)))))

(define (line-length l)
  (define a (abs (line-rise l)))
  (define b (abs (line-run l)))


  (sqrt (+ (sqr a)
           (sqr b))))

(define/contract (room->lines_ r)
  (-> entity? (listof my-line?))
  
  (match-define (list (posn x1 y1)
                      (posn x2 y2))
    (room->box_ r))

  (define top (my-line (posn x1 y1)
                       (posn x2 y1)))

  (define right (my-line (posn x2 y1)
                         (posn x2 y2)))

  (define bottom (my-line (posn x1 y2)
                          (posn x2 y2)))

  (define left (my-line (posn x1 y1)
                        (posn x1 y2)))

  (list top right bottom left))


(define (room->lines r)
  (if (entity? r)
      (room->lines_ r)
      (map room->lines_ (flatten r))))


(define (colinear? l1 l2)

  (match-define (my-line (posn l1-x1 l1-y1)
                         (posn l1-x2 l1-y2))
    l1)

  (match-define (my-line (posn l2-x1 l2-y1)
                         (posn l2-x2 l2-y2))
    l2)

  
  (define shares-x? 
    (= l1-x1
       l1-x2
       l2-x1
       l2-x2))

  (define shares-y? 
    (= l1-y1
       l1-y2
       l2-y1
       l2-y2))

  (or shares-x?
      shares-y?))



(define (point-on-line? p l)
  (match-define (my-line (posn sx sy)
                         (posn ex ey))
    l)

  (match-define (posn px py)
    p)

  (define a?
    (and (> px (min sx ex))
         (< px (max sx ex))))

  (define b?
    (and (> py (min sy ey))
         (< py (max sy ey))))

  (or a? b?))

(define (sharing-points? l1 l2)
  (match-define (my-line l1-start
                         l1-end)
    l1)

  (match-define (my-line l2-start
                         l2-end)
    l2)

  (or
   (equal? l1-start
           l2-start)
   (equal? l1-start
           l2-end)
   (equal? l1-end
           l2-start)
   (equal? l1-end
           l2-end)))

(define (lines-overlap? l1 l2)

  (match-define (my-line l1-start
                         l1-end)
    l1)

  (match-define (my-line l2-start
                         l2-end)
    l2)


  (define points-on-line
    (filter identity
            (list (point-on-line? l1-start l2)
                  (point-on-line? l1-end l2)
                  (point-on-line? l2-start l1)
                  (point-on-line? l2-end l1))))

  (and
   (<= 1 (length points-on-line))
   (colinear? l1 l2)))





(module+ test

  (define p0:0   (posn 0 0))
  (define p0:32  (posn 0 32))
  (define p32:32 (posn 32 32))
  (define p32:0  (posn 32 0))
  (define p0:-32 (posn 0 -32))
  (define p0:64  (posn 0 64))

  (define p-32:0  (posn -32 0))
  (define p64:0   (posn 64 0))
  (define p16:0   (posn 16 0))
  
  (define top    (my-line p0:0  p32:0))
  (define right  (my-line p32:0 p32:32))
  (define bottom (my-line p32:32 p0:32))
  (define left   (my-line p0:32 p0:0))
  
  

  
  (define l5 (my-line p0:-32 p0:64))
  (define l6 (my-line p0:-32 p0:0))
  (define l7 (my-line p-32:0 p64:0))
  (define l8 (my-line p-32:0 p16:0))


  (define l9 (my-line (posn 224 0)
                      (posn 512 0)))
  (define l10 (my-line (posn 224 288)
                       (posn 512 288)))

  (define l11 (my-line (posn 200 0)
                       (posn 200 200)))
  (define l12 (my-line (posn 200 0)
                       (posn 200 100)))
  
  (define (no-overlap! l1 l2)
    (check-equal? #f
                  (lines-overlap? l1 l2)
                  (list "Shouldn't overlap..." l1 l2))
    (check-equal? #f
                  (lines-overlap? l2 l1)
                  (list "Shouldn't overlap..." l2 l1)))


  
  (define (overlap! l1 l2)
    (check-equal? #t
                  (lines-overlap? l1 l2)
                  (list "Should overlap..." l1 l2))
    (check-equal? #t
                  (lines-overlap? l2 l1)
                  (list "Should overlap..." l2 l1)))

  

  ;None of the little square's lines should be overlapping
  (no-overlap! top left)
  (no-overlap! left right)
  (no-overlap! bottom top)
  (no-overlap! left bottom)
  (no-overlap! right bottom)

  (overlap! left l5)
  
  (no-overlap! left l6)
  
  (no-overlap! l5 top)
  (no-overlap! l5 bottom)
  (no-overlap! l5 right)

  (overlap! top l7)

  (no-overlap! l7 left)
  (no-overlap! l7 bottom)
  (no-overlap! l7 right)

  (overlap! top l8)

  (no-overlap! l8 left)
  (no-overlap! l8 bottom)
  (no-overlap! l8 right)

  (no-overlap! l9 l10)

  (overlap! l11 l12)
  
  #;(check-equal? (list (my-line p0:0 p0:-32)
                        (my-line p0:32 p0:64))
                  (subtract-lines l3 l5))

  )




(define (subtract-lines l1 l2)

  (match-define (my-line (posn l1-x1 l1-y1)
                         (posn l1-x2 l1-y2))
    l1)

  (match-define (my-line (posn l2-x1 l2-y1)
                         (posn l2-x2 l2-y2))
    l2)

  (list
   (my-line (posn l1-x1 l1-y1)
            (posn l2-x1 l2-y1))
   (my-line (posn l1-x2 l1-y2)
            (posn l2-x2 l2-y2))))


(define (rooms->bounding-lines rs)
  (define lines (flatten (room->lines rs)))

  (define overlapping-pairs (filter (curry apply lines-overlap?)
                                    (combinations lines 2)))
  
  (define fixed-overlaps (flatten
                          (map (curry apply subtract-lines) overlapping-pairs)))

  (define overlapping-lines (flatten overlapping-pairs))

  (define non-overlapping-lines (filter (not/c (curryr member overlapping-lines)) lines))


  (define ret
    (filter (λ(l) (< 0 (line-length l)))
            (remove-duplicates (append non-overlapping-lines fixed-overlaps)
                               equal?)))

  ret)







(define (line->wall l)
  (match-define (my-line (posn x1 y1)
                         (posn x2 y2))
    l)

  (define horizontal? (= y1 y2))

  (define h
    (if horizontal? ;Horizontal
        16
        (abs (- y1 y2))))

  (define w
    (if horizontal? ;Horizontal
        (+ 8 (abs (- x1 x2)))
        8))


  (define i
    (if horizontal? ;Horizontal
        (rectangle w h 'solid 'green)
        (rectangle w h 'solid 'red)))

  (sprite->entity i
                  #:name       (~a "wall")
                  #:position   (posn (+ (half w) (min x1 x2))
                                     (+ (half h) (min y1 y2)))
                  #:components
                  ;(disabled)
                  (storage "original-line" (list l))
                  (static)
                  (bake)
                  (physical-collider)
                  #;(disabled)
                  #;(hidden)
                  ))



(define/contract (room->box_ r)
  (-> entity? (listof posn?))
  (list
   (posn (room-x_ r)
         (room-y_ r))
   (posn (+ (room-x_ r)
            (room-width_ r))
         (+ (room-y_ r)
            (room-height_ r)))))

(define/contract (room->box r)
  (-> room? (listof posn?))

  (if (entity? r)
      (room->box_ r)
      (let* (( point-cloud (flatten (map room->box_ r)))

             ( min-x (apply min (map posn-x point-cloud)))
             ( min-y (apply min (map posn-y point-cloud)))
             ( max-x (apply max (map posn-x point-cloud)))
             ( max-y (apply max (map posn-y point-cloud))))

        (list (posn min-x min-y)
              (posn max-x max-y)))))





(define/contract (room-width_ r)
  (-> entity? number?)
  (image-width (room->image_ r)))


(define/contract (room-width r)
  (-> room? number?)

  (match-define
    (list
     (posn x1 y1)
     (posn x2 y2))
    (room->box (flatten r)))

  (- x2 x1))


(define/contract (room-height_ r)
  (-> entity? number?)
  (image-height (room->image r)))


(define/contract (room-height r)
  (-> room? number?)
  
  (match-define
    (list
     (posn x1 y1)
     (posn x2 y2))
    (room->box (flatten r)))

  (- y2 y1))

(define/contract (set-room-pos r p)
  (-> room? posn? room?)

  (if (entity? r)
      (update-entity r posn? p)
      (map (curryr set-room-pos p) r)))


(define (add-to-room-pos r p)
  (if (entity? r)
      (update-entity r posn? (posn-add (get-component r posn?) p))
      (map (curryr add-to-room-pos p) r)))


(define (leftmost-room rs)
  (list-ref rs (index-of rs
                         (argmin room-x rs ))))

(define (topmost-room rs)
  (list-ref rs (index-of rs
                         (argmin room-y rs ))))

(define (tallest-room rs)
  (list-ref rs (index-of rs
                         (argmax room-height rs ))))




(define (room-list->image rs) 
  (place-images/align
   (map room->image rs)
   
   (map (λ (r)
          (h:make-posn (+ (room-x r))
                       (+ (room-y r))))
        rs)
   
   "left" "top"
   (rectangle (room-width rs)
              (room-height rs)
              'solid
              'transparent)))


(define (beside-room . rs) ;Add back contract

  (define new-rooms
    (foldl (λ(f res)
             (define h-width (apply + (map room-width res)))
             (cons (add-to-room-pos f
                                    (posn h-width 0))
                   res))
           '()
           rs))

  (map uniqify-name new-rooms))


(define (above-room . rs) ;Add back contract

  (define new-rooms
    (foldl (λ(f res)
             (define v-width (apply + (map room-height res)))
             (cons (add-to-room-pos f (posn 0 v-width))
                   res))
           '()
           rs))

  (map uniqify-name new-rooms))



(define (room-list->entity-list rs)
  (define (adj r)
    (add-to-room-pos r
                     (posn
                      (half (room-width  r))
                      (half (room-height r)))))
  
  (define rooms (map adj (flatten rs)))

  (define lines (rooms->bounding-lines rs))

  (define rooms-with-walls 
    (assign-walls-to-rooms
     (map line->wall
          (flatten (rooms->bounding-lines rs)))
     rooms))


  (define (fix-posn e1 e2)
    (set-posn e2 (posn-add (get-posn e1)
                           (get-posn e2))))

  (define (explode-stored-entities e)
    (define es (get-stored-entities e))

    (cons e (map (curry fix-posn e)
                 es)))

  (define es (sort
              (flatten (map explode-stored-entities rooms-with-walls))
              (λ(a b)
                (> (y a)
                   (y b)))))


  (define house-bg (overlay/align "left" "top"
                                  (room->image rs)
                                  (rectangle (+ 32 (room-width rs))
                                             (+ 32 (room-height rs))
                                             'solid 'transparent)))
  (define bg-entity
    (sprite->entity (overlay/align "left" "top"
                                   house-bg
                                   (texture-with
                                    (square (+ 32 (max (room-width rs)
                                                       (room-height rs))) 'solid 'black)
                                    dark-grass-tile))
                    #:name     "bg"
                    #:position (posn 0 0)
                    #:components (static)))



  (append es (list bg-entity))
  )






(define (subline-of? l1 l2)
  (match-define (my-line (posn l1-start-x l1-start-y)
                         (posn l1-end-x l1-end-y))
    l1)

  (match-define (my-line (posn l2-start-x l2-start-y)
                         (posn l2-end-x l2-end-y))
    l2)
  
  (define (horizontal-subline? l1 l2)
    (and (= l1-start-y l1-end-y l2-start-y l2-end-y)
         (>= (min l1-start-x l1-end-x) (min l2-start-x l2-end-x))
         (<= (max l1-start-x l1-end-x) (max l2-start-x l2-end-x))))

  (define (vertical-subline? l1 l2)
    (and (= l1-start-x l1-end-x l2-start-x l2-end-x)
         (>= (min l1-start-y l1-end-y) (min l2-start-y l2-end-y))
         (<= (max l1-start-y l1-end-y) (max l2-start-y l2-end-y))))
  
  (or (horizontal-subline? l1 l2)
      (vertical-subline? l1 l2)))


(define (wall-for-room? r w)
  
  (define top-left (posn-subtract
                    (get-component r posn?)
                    (posn (half (width r))
                          (half (height r)))))

  (define top-right    (posn-add top-left
                                 (posn (width r) 0)))

  (define bottom-left  (posn-add top-left
                                 (posn 0 (height r))))

  (define bottom-right (posn-add top-left
                                 (posn (width r)
                                       (height r))))

  (define w-line (first (storage-data (get-component w (storage-named? "original-line")))))

  (define ret
    (or
     (subline-of? w-line (my-line top-left top-right))
     (subline-of? w-line (my-line top-right bottom-right))
     (subline-of? w-line (my-line bottom-right bottom-left))
     (subline-of? w-line (my-line bottom-left top-left))))
  
  ret)


(define (assign-walls-to-room walls r)
  (define walls-to-add (filter (curry wall-for-room? r) walls))

  
  (define horizontal-walls (filter (λ(w)
                                     (> (width w) (height w)))
                                   walls-to-add))

  (define vertical-walls (filter (λ(w)
                                   (< (width w) (height w)))
                                 walls-to-add))


  (define topmost-wall-y
    (if (empty? horizontal-walls)
        #f
        (apply min (map y horizontal-walls))))

  (define leftmost-wall-x
    (if (empty? vertical-walls)
        #f
        (apply min (map x vertical-walls))))

  (define top-walls
    (filter (λ(w)
              (= topmost-wall-y
                 (y w)))
            horizontal-walls))

  (define bottom-walls
    (filter (λ(w)
              (< topmost-wall-y
                 (y w)))
            horizontal-walls))

  (define left-walls
    (filter (λ(w)
              (= leftmost-wall-x
                 (x w)))
            vertical-walls))

  (define right-walls
    (filter (λ(w)
              (< leftmost-wall-x
                  (x w)))
            vertical-walls))


  (define (bump-up e)
    (update-entity e animated-sprite?
                   (new-sprite (texture-with (render (get-component e animated-sprite?))
                                             wall-top-bumper-tile))))

  (define (bump-down e)
    (update-entity e animated-sprite?
                   (new-sprite (texture-with (render (get-component e animated-sprite?))
                                             wall-top-bumper-tile)))

    #;(set-posn e
              (posn-add
               (posn 0 0 #;(half (height e)))
               (get-posn e))))

  (define (bump-right e)
    (~> e
        (update-entity _ animated-sprite?
                       (new-sprite (texture-with (render (get-component e animated-sprite?))
                                                 wall-side-bumper-tile)))

        #;(set-posn _
                  (posn-add
                   (posn 0 (if (and topmost-wall-y (= topmost-wall-y (y e)))
                               (- 16)
                               0))
                   (get-posn e)))))

  (define (bump-left e)
    (~> e
        (update-entity _ animated-sprite?
                       (new-sprite (texture-with (render (get-component e animated-sprite?))
                                                 wall-side-bumper-tile)))

        (set-posn _
                  (posn-add
                   (posn 0 (if (and topmost-wall-y (= topmost-wall-y (y e)))
                               (- 16)
                               0))
                   (get-posn e)))))



  

  (define (adj-pos w)
    (set-posn w (posn-subtract (get-posn w)
                               (get-posn r))))


  (define new-walls
    (append (map bump-left left-walls)
            (map bump-up top-walls)
            (map bump-down bottom-walls)
            (map bump-right right-walls)))

  (add-entities-to-room r
                        (map adj-pos new-walls)))



(define (assign-walls-to-rooms walls rooms)
  (map (curry assign-walls-to-room walls) rooms))



(module+ test
  
  (define door (room 1 1 (square 32 'solid 'red)))
  (define r (room 4 4 wood-tile))
  (define rr (beside-room r door r))
  (define rr<->rr (above-room rr door rr))
  (define rrrr (beside-room rr door rr))

  (define bigr       (room 5 5 wood-tile))
  (define bigrr2     (beside-room bigr (nudge-down 0 door) r))
  (define bigrr2<->bigrr2 (above-room bigrr2 door bigrr2))

  (define house (beside-room bigrr2<->bigrr2 door rr))

  (begin
  
  
    (define r2 (room 2 2 wood-tile))
    (define r3 (room 3 4 wood-tile))

    (define r2r3 (beside-room r2 door r3))

    (define r2<->r3 (above-room r2 r3))

    (check-equal? #t (room? r))
    (check-equal? #t (room? rr))

  
    (check-equal? (* 4 32)
                  (room-width r))

    (check-equal? (* 4 32)
                  (room-height r))

    (check-equal? (+ (* 2 4 32) 32)
                  (room-width rr))
  
    (check-equal? (* 4 32)
                  (room-height rr))



    (check-equal? (* 4 32)
                  (room-height r2r3))

    (check-equal? (* 3 32)
                  (room-width r2<->r3))

    (check-equal? (* 5 32)
                  (room-height bigrr2)))
  
  #;(begin
      (flatten (map room->box_ r<->r))
      (map room-width_ r<->r)
      (map room-height_ r<->r)
      (room->box r<->r)

      (room->image r)
      (room->image r<->r)
  
      (room->image (above-room r<->r
                               r<->r
                               #:door-offset 6))))

(define (nudge-down n r)
  (add-to-room-pos r (posn 0 (* 32 n))))

(define (nudge-right n r)
  (add-to-room-pos r (posn (* 32 n) 0)))

#;(begin

    (define door    (place-in-room
                     (make-room 1 1 wood-tile)
                     mat-entity))
    (define r       (make-room 4 4 wood-tile))
    (define r2      (place-in-room
                     (make-room 5 5 wood-tile)
                     bed-entity
                     (nudge-right 1 bed-entity)))
    (define rr      (beside-room r2 (nudge-down 2 door) r))
    (define rr<->rr (above-room rr door rr))

    (define house (beside-room rr<->rr door rr))

    (define big-house (above-room house door house))


    (test-room big-house))


(define (test-room h)

  (apply
   start-game
   #;(room-list->entity-list h)
   (cons
    (basic-hero (posn 100 100))
      
    (room-list->entity-list h)
    )))
