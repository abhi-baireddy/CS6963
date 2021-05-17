#lang racket

(require json)
(require "../helpers.rkt")
(require "../constants.rkt")
(require "../move-without-cards.rkt")
(require "../build-without-cards.rkt")

(provide (all-defined-out))
;; The moved token can optionally move a second time (i.e., the same token),
;; as long as the first move doesn’t win,
;; and as long as the second move doesn’t return to the original space.
(define (move-artemis cards players spaces turn)
  (define-values (first-token second-token) (get-player-tokens FIRST players))
  (define first-token-neighbors (valid-neighbors first-token
                                                 players
                                                 spaces))
  (define second-token-neighbors (valid-neighbors second-token
                                                  players
                                                  spaces))
  (define neighbors-of-first-token-neighbors (neighbors-of-neighbors first-token
                                                                     players
                                                                     spaces))
  
  (define neighbors-of-second-token-neighbors (neighbors-of-neighbors second-token
                                                                      players
                                                                      spaces))

  ;; this works best if there is a level three neighbor in any of the neighbors of neighbors
  ;; if there is indeed such a position, move there and win
  ;; otherwise move as in no cards version
  (define levels-of-first-token-neighbors-of-neighbors (map (lambda (posn)
                                                              (level-of posn spaces))
                                                            neighbors-of-first-token-neighbors))
  
  (define levels-of-second-token-neighbors-of-neighbors (map (lambda (posn)
                                                               (level-of posn spaces))
                                                             neighbors-of-second-token-neighbors))

  (define index-of-win-pos-first-token (index-of levels-of-first-token-neighbors-of-neighbors
                                                 LEVEL-THREE))
  (define index-of-win-pos-second-token (index-of levels-of-second-token-neighbors-of-neighbors
                                                  LEVEL-THREE))

  

  (cond [(not (false? index-of-win-pos-first-token))
         (begin
           (write-json
            (make-board (reverse cards)
                        (list (second players)
                              (list (list-ref neighbors-of-first-token-neighbors
                                              index-of-win-pos-first-token)
                                    second-token))
                        spaces
                        (add1 turn)))
           (flush-output))]
        [(not (false? index-of-win-pos-second-token))
         (begin
           (write-json
            (make-board (reverse cards)
                        (list (second players)
                              (list (list-ref neighbors-of-second-token-neighbors
                                              index-of-win-pos-second-token)
                                    first-token))
                        spaces
                        (add1 turn)))
           (flush-output))]
        [else
         (move-without-cards cards
                             players
                             spaces
                             turn)]))

