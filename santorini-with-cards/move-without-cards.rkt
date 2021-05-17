#lang racket

(require json)
(require "helpers.rkt")
(require "build-with-cards.rkt")
(require "constants.rkt")

(provide (all-defined-out))


(define (move-without-cards cards players spaces turn)
  (define-values (first-token second-token) (get-player-tokens FIRST))
  (define-values (new-pos-of-picked-token picked-token) (prevent-opponent-from-winning players spaces))
  (define opponent-tokens (second players))
  (define other-token (if (equal? first-token picked-token)
                          second-token
                          first-token))
  (define current-player-new-posns (list new-pos-of-picked-token other-token))
  (define updated-players (list current-player-new-posns
                                opponent-tokens))
  (build-step new-pos-of-picked-token cards updated-players spaces turn))
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
