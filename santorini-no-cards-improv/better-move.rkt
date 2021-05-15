#lang racket

(require json)
(require "helpers.rkt")
(require "better-build.rkt")
(require "constants.rkt")

(provide (all-defined-out))

;;move-step -- takes in current payers tokens and board state and returns next state
;;next state has different player-tokens list corresponding to the new positions of the current player but spaces will look same
(define (move-step players spaces turn)
  
  (define current-player-tokens (player-tokens players FIRST))
  (define opponent-tokens (player-tokens players SECOND))
  
  (define first-token (list-ref current-player-tokens FIRST))
  (define second-token (list-ref current-player-tokens SECOND))
  
  (define-values (new-position-of-picked-token picked-token) (pick-token-to-move players spaces))
  (define other-token (if (equal? first-token picked-token)
                          second-token
                          first-token))
  (define updated-first-player-tokens (list new-position-of-picked-token other-token))

  (define updated-player-tokens (append (list opponent-tokens)
                                        (list updated-first-player-tokens)))
  
  (cond [(= (val-at spaces
                    (first new-position-of-picked-token)
                    (second new-position-of-picked-token))
            LEVEL-THREE)
         (begin
           (write-json (make-board updated-player-tokens spaces (add1 turn)))
           (flush-output))]
        [else (build-step updated-player-tokens new-position-of-picked-token spaces turn)]))



(define (pick-token-to-move players spaces)
  ;; first list in players list is always the curent player's list of tokens
  (define current-player-token (player-tokens players FIRST))
  ;; second list in players list is the list of opponent's tokens
  (define current-player-all-neighbors (get-all-neighbors-of-player FIRST
                                                                    players
                                                                    spaces))
  (define current-player-first-token (list-ref current-player-token FIRST))
  (define current-player-second-token (list-ref current-player-token SECOND))
  (define neighbors-levels (map (lambda (neighbor)
                                  (val-at spaces (first neighbor)
                                          (second neighbor)))
                                current-player-all-neighbors))
  
  (define win-position (index-of neighbors-levels LEVEL-THREE))
  (cond [(false? win-position) (prevent-opponent-from-winning players spaces)]
        [else (if (and (level-ok? current-player-first-token (list-ref current-player-all-neighbors win-position) spaces)
                       (are-neighbors? current-player-first-token (list-ref current-player-all-neighbors win-position)))
                  ;; return new position and old position of the selected token to the caller
                  (values (list-ref current-player-all-neighbors win-position)
                          current-player-first-token)
                  (values (list-ref current-player-all-neighbors win-position)
                          current-player-second-token))]))


;; look at the opponent's one-step neighbors and see if there is a three
;; check if that three is "build reachable" by any of  my tokens
;; i.e if any of my tokens can be moved to a neighbor cell of that position
;; if yes, move your token there and build on the three
;; if not, do something else
(define (prevent-opponent-from-winning players spaces)
  (define current-player-tokens (list-ref players FIRST))
  (define current-player-first-token (list-ref current-player-tokens FIRST))
  (define current-player-second-token (list-ref current-player-tokens SECOND))
  
  (define opponent-player-tokens (list-ref players SECOND))
  
  ;; opponent's potential move positions
  (define opponent-player-all-neighbors (get-all-neighbors-of-player SECOND
                                                                     players
                                                                     spaces))
  (define opponent-neighbors-levels (map (lambda (neighbor)
                                           (val-at spaces (first neighbor)
                                                   (second neighbor)))
                                         opponent-player-all-neighbors))
  
  (define opponent-win-positions
    ;; loop over list of levels and collect the position whose level is three
    
    
    (for/list ([idx (range 0 (length opponent-neighbors-levels))]
               #:when (equal? (list-ref opponent-neighbors-levels idx) LEVEL-THREE))
      (list-ref opponent-player-all-neighbors idx))
    )

  ;; get all positions in opponent-win-position that are build reachable by first player
  (define potential-move-positions
    (for/list ([idx (range 0 (length opponent-win-positions))]
               #:when (or (build-reachable? current-player-first-token
                                            (list-ref opponent-win-positions idx)
                                            players
                                            spaces)
                          (build-reachable? current-player-second-token
                                            (list-ref opponent-win-positions idx)
                                            players
                                            spaces)))
      (list-ref opponent-win-positions idx)))
  ;; if there are no such positions, do something else
  ;; otherwise, get a big list of neighbors of all of these level-three positions
  ;; then randomly choose a position to move one of the tokens to
  (cond [(empty? potential-move-positions) (pick-random-pos-to-move players spaces)]
        [else
         (begin
           (define list-of-list-of-positions
             (for/list ([posn potential-move-positions])
               (get-neighbors posn)))
           ;; flatten the list
           (define list-of-positions
             (for*/list ([posns-lst list-of-list-of-positions]
                         [posn posns-lst])
               posn))

           (define (pick-a-pos current-player-tokens list-of-positions)
             (if (empty? list-of-positions)
                 (values empty empty)
                 (let* ([first-pos (first list-of-positions)]
                        [first-token (list-ref current-player-tokens FIRST)]
                        [second-token (list-ref current-player-tokens SECOND)]
                        [first-token-valid-neighbors (common-neighbors first-token first-pos players spaces)]
                        [second-token-valid-neighbors (common-neighbors second-token first-pos players spaces)])
                   (cond [(empty? list-of-positions) (values empty empty)]
                         [(not (empty? first-token-valid-neighbors)) (values (select-random first-token-valid-neighbors) first-token)]
                         [(not (empty? second-token-valid-neighbors)) (values (select-random second-token-valid-neighbors) second-token)]
                         [else (pick-a-pos current-player-tokens (rest list-of-positions))]))))

           (pick-a-pos current-player-tokens list-of-positions))]))


;; pick a good random position to move to
;; this is claled iff there is no reachable level 3 position
;; of if the opponent can't be blocked
(define (pick-random-pos-to-move players spaces)
  (define current-player-tokens (player-tokens players FIRST))
  (define current-player-first-token (list-ref current-player-tokens FIRST))
  (define current-player-second-token (list-ref current-player-tokens SECOND))

  (define current-player-all-neighbors (get-all-neighbors-of-player FIRST
                                                                    players
                                                                    spaces))

  
  (define picked-token (select-random current-player-tokens))
  ;; if picked-token has no valid neighbors, pick this token
  (define other-token (first (remove picked-token current-player-tokens)))
  (define picked-token-neighbors (valid-neighbors picked-token
                                                  players
                                                  spaces))
  
  (if (empty? picked-token-neighbors)
      (values (select-random (valid-neighbors other-token
                                              players
                                              spaces))
              other-token)
      (values (select-random picked-token-neighbors)
              picked-token)))

