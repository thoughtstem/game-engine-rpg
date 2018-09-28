#lang racket

(provide link
         game->link-follower
         (rename-out [new-world-manager link-follower]))

(require game-engine)

;This will be the component for the doorway/portal/house/edge-of-screen, etc.
(struct link (to) #:transparent)


;This will be the component on the player...  Will check for collisions with links,
;  Will store data for prior worlds visited...
;  Will do the dynamic loading.
;  Will call the world transition functions, etc...
(struct link-follower (data next-entrance-function) #:transparent)

(define (new-world-manager)
  (link-follower #f #f))


(define (game->link-follower g)
  (findf (has-component? link-follower?)
         (game-entities g)))

(define (entrance-function p)
    (define enter (dynamic-require (build-path (string->path p) "main.rkt") 'enter))
    enter)

(define loading-screen
  (sprite->entity (overlay (text "Loading world..." 24 'white)
                           (rectangle 300 100 'solid 'black))
                  #:name "link-loading-screen"
                  #:position (posn 50 50)
                  #:components
                  (after-time 100 die)))

(define (try-resolve-link g e c l)
  (define l-to (link-to l))
  
  (displayln (~a "Trying to follow link to: " l-to))

  (define enter (entrance-function l-to))

  ;Call this enter function later in the game update...
  ;  Store the current world in world manager data hash. (What's the key??)

  (define e-with-updated-follower
    (update-entity e link-follower?
                   (struct-copy link-follower c
                                [data (link-follower-data c)]
                                [next-entrance-function enter])))

  ;Not quite working...
  #;(add-component e-with-updated-follower
                 (spawn-once loading-screen))

  e-with-updated-follower)


(define (reset-link-follower e)
  (if (not (get-component e link-follower?))
      e
      (update-entity e link-follower?
                     (struct-copy link-follower
                                  (get-component e link-follower?)
                                  [next-entrance-function #f]))))

(define (try-switch-worlds g follower enter)
  ;The simplest way to enter.  Just call the entrance function
  ;  on the old game state, and whatever is returned becomes the
  ;  new game...  The old entities are marked to be cleaned up.

  (define doomed
    (map
     (λ(e)
       (update-entity e
                      posn?
                      (posn -1000 -1000)))
     (map (curry die g)
         (game-entities g))))
  
  (define new-born
    (map chipmunkify (flatten (enter g))))

  (define es (map
              reset-link-follower
              (append doomed new-born)))
  
  (struct-copy game g
               [entities es]))

(define (maybe-switch-worlds g)
  (define world-manager-es (filter (has-component? link-follower?) (game-entities g)))
  ;BTW, There really should only be 1 or 0 entities with a world-manager

  (define entrance-functions
    (filter identity
            (map link-follower-next-entrance-function
                 (map (curryr get-component link-follower?)
                      world-manager-es))))

  (if (empty? entrance-functions)
      g
      (try-switch-worlds g
                         (first world-manager-es)
                         (first entrance-functions))))

(define (update-world-manager g e c)
  (define touching (colliding-with e g))
  (define links-touching (filter (has-component? link?) touching))

  (if (not (empty? links-touching))
      (try-resolve-link g e c (get-component (first links-touching) link?))
      e))

(new-component link-follower? update-world-manager)

(new-game-function maybe-switch-worlds)
