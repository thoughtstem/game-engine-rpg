#lang racket

(provide player-dialog-with
         player-dialog-system)

(require game-engine)

(define (player-dialog-with name
                            #:dialog-list  list-of-dialog
                            #:talk-key     [talk-key 'space]
                            #:select-sound [select-sound #f]
                            #:open-sound   [open-sound #f])
  (define dialog-list-entity
    (dialog-list list-of-dialog
                   (posn 0 0)
                   #:sound select-sound))
  
  (list
   (precompiler dialog-list-entity)
   (on-key talk-key #:rule (ready-to-speak-and-near? name)
           (do-many (set-counter 0)
                    (spawn dialog-list-entity #:relative? #f)
                    (play-sound open-sound)))))


; === NOT USED YET ====
(define (player-dialog-system #:close-key [close-key 'enter])
  (list (counter 0)
        (on-key close-key #:rule player-dialog-open? (get-dialog-selection))
        (on-rule (not/r all-dialog-closed?) (stop-movement))))