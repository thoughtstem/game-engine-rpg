#lang info

(define version "0.0.1")

;Causes some kind of build issue:
;  cannot require racket/gui/base a second time in the same process
;Commenting out to fix.
;(define scribblings '(("docs/game-engine-rpg.scrbl" ())))

(define compile-omit-paths '(
  "docs"
))

(define deps '(
  "https://github.com/thoughtstem/game-engine.git"
))

