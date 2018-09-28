#lang racket

(require game-engine
         game-engine-rpg)


;Player approaches house outdoors
;House has a hyperlink on it
;Player collides with house.
;Dynamically load the transition function (game -> (listof entity?))
;  > Can do this earlier for optimization
;Pass in the current game state, get the new game state.
;  Replace old state with new state.
;  Store old state within the player's world-manager component

;Not yet implemented:
;On subsequent dynamic loads, check the world-manager before doing the load.
;  Local storage for persistent state.  (Can serialize out for save/load?)



(module+ test
  ;TEST!
  (define player
    (add-component
     (basic-hero (posn 200 200))
     (link-follower)))


  (define portal2
    (sprite->entity (overlay (text "House" 18 'white)
                             (square 70 'solid 'brown))
                    #:name "portal2"
                    #:position (posn 100 200)
                    #:components
                    (static)
                    (physical-collider)
                    (link "./Test2")))

  (define portal
    (sprite->entity (overlay (text "Outdoors" 18 'white)
                             (square 70 'solid 'darkgreen))
                    #:name "portal"
                    #:position (posn 300 200)
                    #:components
                    (static)
                    (physical-collider)
                    (link "./Test")))

  (define bg
    (sprite->entity (square 400 'solid 'black)
                    #:name "bg"
                    #:position (posn 0 0)))

  (start-game player
              portal
              portal2
              bg))







