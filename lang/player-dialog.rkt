#lang racket

(provide player-dialog-with
         player-dialog-system)

(require game-engine)

(define (npc-has-responses? name)
  (lambda (g e)
    (and (get-storage-data "dialog" (get-entity name g))
         ((listof (listof animated-sprite?)) (get-storage-data "dialog" (get-entity name g))))))

(define (player-dialog-with name
                            #:dialog-list  list-of-dialog
                            #:talk-key     [talk-key 'space]
                            #:selection    [selection 0]
                            #:select-sound [select-sound #f]
                            #:open-sound   [open-sound #f])
  (define dialog-list-entity
    (dialog-list list-of-dialog
                   (posn 0 0)
                   #:selection    selection
                   #:select-sound select-sound))
  
  (list
   (precompiler dialog-list-entity)
   (on-key talk-key #:rule (and/r (ready-to-speak-and-near? name)
                                  (npc-has-responses? name))
           (do-many (set-counter 0)
                    (spawn dialog-list-entity #:relative? #f)
                    (play-sound open-sound)))))


; === NOT USED YET ====
(define (player-dialog-system #:close-key [close-key 'enter])
  (list (counter 0)
        (on-key close-key #:rule player-dialog-open? (get-dialog-selection))
        (on-rule (not/r all-dialog-closed?) (stop-movement))))