#lang racket

(require json)
(require "constants.rkt")
(provide (all-defined-out))


;;init-board -- initialize spaces to all zeroes, 
(define (init-spaces)
  (for/list ([i (in-range 0 MAX_ROW)])
    (for/list ([j (in-range 0 MAX_COL)]) 
      0)))

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



;;creates a hash given a player's tokens and card
(define (make-player-dict card tokens)
  (hasheq 'card card 'tokens tokens))

;;creates a hash out of players, spaces, and turn
(define (make-board cards players spaces turn)
  (hasheq 'players (list (make-player-dict (first cards) (first players))
                         (make-player-dict (second cards) (second players)))
          'spaces spaces
          'turn turn))

;;get-both-player-tokens returns a list of both the players' tokens like we had in no cards version
(define (get-both-player-tokens players)
  (list (hash-ref (first players) 'tokens)
        (hash-ref (second players) 'tokens)))

;;get-both-player-cards returns a list of two cards; first card is of the first player, second card is of the second player
(define (get-both-player-cards players)
  (list (hash-ref (first players) 'card)
        (hash-ref (second players) 'card)))

;;valid-neighbors filters list of neighbors and returns a list of possible positions for the token to move to
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

;; returns list of unoccupied neighbors ignoring the level difference
(define (get-unoccupied-neighbors token players-tokens spaces)
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

;;adds two vectors
(define (add-vec posn-1 posn-2)
  (list (+ (list-ref posn-1 FIRST) (list-ref posn-2 FIRST))
        (+ (list-ref posn-1 SECOND) (list-ref posn-2 SECOND))))


;;check if a posn is inside the board
(define (inside-board? posn)
  (and (and (>= (list-ref posn 0) 1)
            (>= (list-ref posn 1) 1))
       (and (<= (list-ref posn 0) MAX_ROW)
            (<= (list-ref posn 1) MAX_COL))))


;;get level of posn in board
(define (level-of where in)
  (val-at in (first where) (second where)))

;;check if level of neighbor is at most 1 more than current position's level
(define (level-ok? posn neighbor spaces)
  (and (not (= 4 (level-of neighbor spaces))) 
       (<= (- (level-of neighbor spaces) (level-of posn spaces)) 1)))


;;check if the position is already occupied
(define (not-occupied? posn players)
  (define first-player-tokens (player-tokens players FIRST))
  (define second-player-tokens (player-tokens players SECOND))
  (and (not (member posn first-player-tokens))
       (not (member posn second-player-tokens))))

;;get neighbors of posn
(define (get-neighbors player-posn)
  (map (lambda (vec)
         (add-vec vec player-posn))
       neighbor-vectors))

;; returns first (or second) player's first token and second token
(define (get-player-tokens player-number tokens)
  (define player-tokens (if (= player-number FIRST)
                            (first tokens)
                            (second tokens)))
  (define first-token (first player-tokens))
  (define second-token (second player-tokens))
  (values first-token second-token))


;; returns the number of occurences of item in a list or the like
(define (count-of what in )
  (count (lambda (item)
           (= what item))
         in))

;; returns neighbors of neighbors of the given position
(define (neighbors-of-neighbors posn players spaces)
  (define valid-move-neighbors (valid-neighbors posn
                                                players
                                                spaces))
  (define neighbors-of-move-neighbors
    (for/list ([neighbor valid-move-neighbors])
      (valid-neighbors neighbor
                       players
                       spaces)))
  ;;neighbors-of-move-neighbors will be a list of lists of positions
  ;; convert it into a list of positions (one-dimensional)
  ;; then remove current position and first order neighbors before returning
  
  (define flat-neighbors-of-neighbors
    (for*/list ([lst neighbors-of-move-neighbors]
                [neighbor lst])
      neighbor))
  (remove-duplicates (remove* (append (list posn) valid-move-neighbors)
                              flat-neighbors-of-neighbors)))

;; return true if first token has more level 2 neighbors than second token
(define (has-more-level-two-neighbors? first-token second-token players spaces)
  (define first-token-neighbors-levels (map (lambda (posn)
                                              (level-of posn spaces))
                                            (valid-neighbors first-token
                                                             players
                                                             spaces)))
  (define second-token-neighbors-levels (map (lambda (posn)
                                               (level-of posn spaces))
                                             (valid-neighbors second-token
                                                              players
                                                              spaces)))
  (> (count-of LEVEL-TWO first-token-neighbors-levels)
     (count-of LEVEL-TWO second-token-neighbors-levels)))

;; called when a level three neighbor is found
(define (move-and-win win-posn cards players spaces turn)
  (define-values (current-player-first-token current-player-second-token) (get-player-tokens players FIRST))
  ;; win-posn will be a neighbor of at least one of the tokens
  (define-values (picked-token-new-posn other-token) (if (are-neighbors? current-player-first-token win-posn)
                                                         (values win-posn current-player-second-token)
                                                         (values win-posn current-player-first-token)))
  (define current-player-new-posns (list picked-token-new-posn other-token))
  (define opponent-tokens (second players))
  ;; write the new state back
  (write-json (make-board (reverse cards)
                          (list opponent-tokens current-player-new-posns)
                          spaces
                          (add1 turn)))
  (flush-output))