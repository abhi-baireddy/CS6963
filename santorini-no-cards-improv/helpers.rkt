#lang racket

(require json)
(require "constants.rkt")
(provide (all-defined-out))

;;random position for initial placement of tokens
(define (random-pos existing-posns)
  (local [(define posn (list (random 1 (add1 MAX_ROW)) (random 1 (add1 MAX_COL))))]
    (cond [(false? (member posn existing-posns)) posn]
          [else (random-pos existing-posns)])))


;;get value at cell [r,c] in board
(define (val-at in row-index col-index)
  (list-ref (list-ref in (sub1 row-index)) (sub1 col-index)))




;;get a player's tokens
(define (player-tokens players player-number)
  (list-ref players player-number))


;;adds two vectors
(define (add-vec posn-1 posn-2)
  (list (+ (list-ref posn-1 FIRST) (list-ref posn-2 FIRST))
        (+ (list-ref posn-1 SECOND) (list-ref posn-2 SECOND))))


;;init-board -- initialize spaces to all zeroes, 
(define (init-spaces)
  (for/list ([i (in-range 0 MAX_ROW)])
    (for/list ([j (in-range 0 MAX_COL)]) 
      0)))
;;check if a posn is inside the board
(define (inside-board? posn)
  (and (and (>= (list-ref posn 0) 1)
            (>= (list-ref posn 1) 1))
       (and (<= (list-ref posn 0) MAX_ROW)
            (<= (list-ref posn 1) MAX_COL))))


;;get level of posn in board
(define (level-of posn in)
  (val-at in (first posn) (second posn)))

;;check if level of neighbor is at most 1 more than current position's level
(define (level-ok? posn neighbor spaces)
  (and (not (= 4 (level-of neighbor spaces))) 
       (<= (- (level-of neighbor spaces) (level-of posn spaces)) 1)))


;;check if the position is already occupied
(define (not-occupied? posn players)
  ;;(false? (member posn player-posns))
  (define first-player-tokens (player-tokens players FIRST))
  (define second-player-tokens (player-tokens players SECOND))
  (and (not (member posn first-player-tokens))
       (not (member posn second-player-tokens))))


;;get neighbors of posn
(define (get-neighbors player-posn)
  (map (lambda (vec)
         (add-vec vec player-posn))
       neighbor-vectors))


;;creates a hash out of players, spaces, and turn
(define (make-board players spaces turn)
  (hasheq 'players players 'spaces spaces 'turn turn))


;; filters list of neighbors and returns a list of possible positions for the token to move to
(define (valid-neighbors token players-tokens spaces)
  (filter (lambda (neighbor)
            (and (not-occupied? neighbor players-tokens)
                 (level-ok? token neighbor spaces)))
          (filter inside-board?
                  (get-neighbors token))))

;; returns a list of neighbors that are build valid -- not at level 4 and inside board and not occupied
(define (valid-build-neighbors token players-tokens spaces)
  (filter (lambda (neighbor)
            (and (not-occupied? neighbor players-tokens)
                 (not-complete? neighbor spaces)))
          (filter inside-board?
                  (get-neighbors token))))


;; returns a list of positions that a token at first-position can move to
;; so that it becomes a neighbor of the second position
(define (common-neighbors token-position position-to-reach players spaces)
  (define token-neighbors (valid-neighbors token-position
                                           players
                                           spaces))
  (define neighbors-of-position-to-reach (filter inside-board?
                                                 (get-neighbors position-to-reach)))
  (set->list (set-intersect (list->set token-neighbors)
                            (list->set neighbors-of-position-to-reach))))

;; returns a random element from the list
(define (select-random from)
  (list-ref from
            (random 0
                    (length from))))

;; checks if a token at first position can move to at least one of the neighbors of second position
(define (build-reachable? first-position second-position players spaces)
  (define first-position-valid-neighbors (valid-neighbors first-position
                                                          players
                                                          spaces))
  (define second-position-neighbors (filter inside-board?
                                            (get-neighbors second-position)))
  (not (empty? (set->list (set-intersect (list->set first-position-valid-neighbors)
                                         (list->set second-position-neighbors))))))


;; finds neighbors of the two tokens of the player-number (FIRST or SECOND) player
(define (get-all-neighbors-of-player player-number players spaces)
  (define player-tokens (list-ref players player-number))
  (define first-token (list-ref player-tokens FIRST))
  (define second-token (list-ref player-tokens SECOND))
  (define first-token-neighbors (list->set (valid-neighbors first-token
                                               players
                                               spaces)))
  (define second-token-neighbors (list->set (valid-neighbors second-token
                                                players
                                                spaces)))

  ;; merge both neighbor lists and return the merged list
  (set->list (set-union first-token-neighbors
                        second-token-neighbors)))


;; checks if two positions are neighbors of each other
(define (are-neighbors? first-position second-position)
  (define neighbors-of-first-position (get-neighbors first-position))
  (cond [(false? (member second-position neighbors-of-first-position)) #f]
        [else #t]))

;; takes a list of posns and a level
;; and keeps only those posns that are at the level specified
(define (filter-by-level posns spaces level)
  (for/list ([posn posns]
    #:when (= (level-of posn spaces)
              level))
    posn))

;; checks if a given position is reachable by any of the opponent tokens in move step
(define (reachable-by-opponent? posn players spaces)
  (if (index-of posn
                (get-all-neighbors-of-player SECOND players spaces))
      #t
      #f))

;; checks if tower at neighbor is not complete
(define (not-complete? neighbor spaces)
  (not (= (level-of neighbor spaces) 4)))