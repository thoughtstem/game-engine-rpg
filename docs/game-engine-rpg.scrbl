#lang scribble/manual

@(require racket/base
          racket/list)
@(require game-engine
          "../lang/common.rkt")

@title{RPG Games}

This provides utilities for making RPG games with game-engine.

@section{Houses}

I'll lead with a cool demo to cover the stuff I haven't had
time to document

@racketblock[

(define door    (room 1 1 wood-tile))

(define r       (room 4 4 wood-tile))

(define r-back  (place-in-room
                 (room 4 2 wall-tile)
                 sword-wall-hanging))

(define (add-back-wall r)
  (above-room (place-in-room
               (room (/ (width r) 32)
                     2
                     wall-tile)
               sword-wall-hanging) r))

(define little-house
  (beside-room (add-back-wall (place-in-room r bed))
               (nudge-down 3 door)
               (add-back-wall (place-in-room r (apple-barrel)))))

(test-room little-house)
]

@image[#:scale 1]{./images/little-house.png}

Here are some basics.

Interiors of houses are collections of rooms.  You
construct simple rooms with the @racket[room] function.  You
combine simple rooms into more complex rooms with the @racket[room-above]
@racket[room-beside] functions.

@defproc[(room [width number?] 
               [height number?]
               [floor-texture image?])
         room?]

Makes a new room.  Note that the width and height are in tiles,
not pixels.  Tiles are 32x32 pixels. 

Example:

@racketblock[
(room 1 1 wood-tile)
]

@(room->image (room 1 1 wood-tile))

Or more interestingly:

@racketblock[
(room 3 3 wood-tile)
]

@(room->image (room 3 3 wood-tile))

@defproc[(room-beside 
               [room room?]
               ...)
         room?]

Makes a new room with the supplied rooms placed beside each other.
Any overlapping walls will be removed.  


@racketblock[
  (define r (room 3 3 wood-tile))
  (define door (room 1 1 wood-tile))
  (beside-room (r door r))
]

@(room->image (let [(r (room 3 3 wood-tile))
                    (door (room 1 1 wood-tile))]
                (beside-room r door r)))


If you want to control where that door ends up, you can use the @racket[nudge-down]
function.

@racketblock[
  (define r (room 3 3 wood-tile))
  (define door (room 1 1 wood-tile))
  (beside-room (r (nudge-down 1 door) r))
]

@(room->image (let [(r (room 3 3 wood-tile))
                    (door (room 1 1 wood-tile))]
                (beside-room r (nudge-down 1 door) r)))


@defproc[(room-above
               [room room?]
               ...)
         room?]

Makes a new room with the supplied rooms placed above each other.
Any overlapping walls will be removed.  

See above.


@defproc[(room-list->entity-list
               [room (or/c room?
                           (listof room?))])
         (listof entity?)]

This is the function you use to convert a @racket[room?] to something
you can pass into @racket[start-game], e.g.:

@racketblock[
  (start-game
    player
    enemy
    (room-list->entity-list r))
]


@section{Hyperlinks}

You can link from one game to another game fairly easily.
What you'll need is:
 
1) A player entity with a @racket[(link-follower)] component

2) A link entity (e.g. a door or a portal) with a @racket[(link "PATH")] component.  Where @racket["PATH"] should reference another folder that contains a main.rkt file.

3) The main.rkt file should provide a function called @racket[enter] whose type is @racket[(-> game? (listof entity?))].  This function 
will be dynamically required when the player touches the entity with the @racket[link] component.  The returned list of entities
will will become the new game state.  The old game is passed in and can be used to dynamically adjust the list of entities (e.g.
for carrying the player entity into the new game).

A relatively simple example can be found in this repo:

https://github.com/thoughtstem/RacketCon2018



