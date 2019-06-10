#lang racket

(provide witch-sprite
         darkelf-sprite
         lightelf-sprite
         madscientist-sprite
         monk-sprite
         pirate-sprite
         wizard-sprite
         mystery-sprite
         dragon-sprite

         caitsith-sprite
         darkknight-sprite
         kavi-sprite
         moderngirl-sprite
         moogle-sprite
         pirateboy-sprite
         pirategirl-sprite
         steampunkboy-sprite
         steampunkgirl-sprite

         firedog-sprite
         phoenix-sprite
         prince-sprite
         princess-sprite
         seaserpent-sprite
         )


(require 2htdp/image
         game-engine
         ts-kata-util/assets/main
         )

; ==== AVATAR SHEETS =====

;This will both define and provide all sheets in /avatar-assets/"
;TODO: make this also provide sprites and parse rows and columns from filename
(define-assets-from "avatar-assets")

#|(define witch-sheet        (bitmap "images/witch-sheet.png"))
(define darkelf-sheet      (bitmap "images/darkelf-sheet.png"))
(define lightelf-sheet     (bitmap "images/lightelf-sheet.png"))
(define madscientist-sheet (bitmap "images/madscientist-sheet.png"))
(define monk-sheet         (bitmap "images/monk-sheet.png"))
(define pirate-sheet       (bitmap "images/pirate-sheet.png"))
(define wizard-sheet       (bitmap "images/wizard-sheet.png"))
(define mystery-sheet      (bitmap "images/mystery-sheet.png"))
(define dragon-sheet      (bitmap "images/dragon-sheet.png"))|#

; ==== AVATAR SPRITES ====

; survival
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

; battlearena
(define caitsith-sprite
  (sheet->sprite caitsith-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 2))

(define darkknight-sprite
  (sheet->sprite darkknight-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 2))

(define kavi-sprite
  (sheet->sprite kavi-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 2))

(define moderngirl-sprite
  (sheet->sprite moderngirl-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 2))

(define moogle-sprite
  (sheet->sprite moogle-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 2))

(define pirateboy-sprite
  (sheet->sprite pirateboy-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 2))

(define pirategirl-sprite
  (sheet->sprite pirategirl-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 2))

(define steampunkboy-sprite
  (sheet->sprite steampunkboy-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 2))

(define steampunkgirl-sprite
  (sheet->sprite steampunkgirl-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 2))

;adventure
(define firedog-sprite
  (sheet->sprite firedog-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 5))

(define phoenix-sprite
  (sheet->sprite phoenix-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 5))


(define prince-sprite
  (sheet->sprite prince-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 5))

(define princess-sprite
  (sheet->sprite princess-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 5))

(define seaserpent-sprite
  (sheet->sprite seaserpent-sheet
                 #:columns 4
                 #:rows 4
                 #:row-number 3
                 #:delay 5))