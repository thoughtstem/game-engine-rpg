#lang racket

(require rackunit
         game-engine
         "../main.rkt")

(define e
  (sprite->entity (circle 2 "solid" "green")
                  #:name "circle"
                  #:position (posn 0 0)))

(check-equal? e e
              "The (two) function should always return true")