#lang racket

(provide (all-defined-out))


(require 2htdp/image
         game-engine)

; ==== AVATAR SHEETS =====
(define witch-sheet        (bitmap "images/witch-sheet.png"))
(define darkelf-sheet      (bitmap "images/darkelf-sheet.png"))
(define lightelf-sheet     (bitmap "images/lightelf-sheet.png"))
(define madscientist-sheet (bitmap "images/madscientist-sheet.png"))
(define monk-sheet         (bitmap "images/monk-sheet.png"))
(define pirate-sheet       (bitmap "images/pirate-sheet.png"))
(define wizard-sheet       (bitmap "images/wizard-sheet.png"))
(define mystery-sheet      (bitmap "images/mystery-sheet.png"))

(define dragon-sheet      (bitmap "images/dragon-sheet.png"))

; ==== AVATAR SPRITES ====

(define witch-sprite
  (sheet->sprite witch-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 4))

(define darkelf-sprite
  (sheet->sprite darkelf-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 4))

(define lightelf-sprite
  (sheet->sprite lightelf-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 4))

(define madscientist-sprite
  (sheet->sprite madscientist-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 4))

(define monk-sprite
  (sheet->sprite monk-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 4))

(define pirate-sprite
  (sheet->sprite pirate-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 4))

(define wizard-sprite
  (sheet->sprite wizard-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 4))

(define mystery-sprite
  (row->sprite mystery-sheet
                 #:columns 4
                 #:delay 4))


(define dragon-sprite
  (sheet->sprite dragon-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 4))

