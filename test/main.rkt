#lang racket

(require rackunit)

(require game-engine
         game-engine-demos-common)

(define (ensure-entity procedure-or-entity)
  (if (procedure? procedure-or-entity)
      (procedure-or-entity)
      procedure-or-entity))

(define basic-entity
  (sprite->entity empty-image
                  #:name ""
                  #:position (posn 0 0)))

; ==== ensure-entity test ====
(check-equal? (entity? (ensure-entity basic-entity)) #t "(Ensure-entity basic-entity) should have returned an entity.")


; ==== assets test ====
(check-equal? (entity? (ensure-entity bat)) #t "(ensure-entity bat) should have returned an entity")
(check-equal? (entity? (ensure-entity apples)) #t "(ensure-entity apples) should have returned an entity")

(define broken-asset empty-image)

(define assets-list (list stone-house
                          wood-house
                          brick-house
                          round-tree
                          pine-tree
                          chest
                          crafting-chest
                          cat
                          black-cat
                          white-cat
                          bat
                          slime
                          snake
                          apples
                          ))

(map (λ (asset) (check-equal? (entity? (ensure-entity asset)) #t (~a "(ensure-entity " asset ") should have returned an entity)")))
     assets-list)