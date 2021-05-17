#lang racket
(require "../helpers.rkt")
(require "../constants.rkt")
(require "../move-without-cards.rkt")
(require "../build-without-cards.rkt")

(provide (all-defined-out))

;; A token’s move can optionally swap places with an adjacent opponent token,
;; as long as the token would be able to move to the opponent’s space if the opponent token were not there;
;; otherwise, the move must be to an unoccupied space as usual.
(define (move-apollo cards players spaces turn)
 

  ;; first check if any of the opponent's tokens are neighbors of the current player's tokens
  (define current-player-tokens (first players))
  (define opponent-tokens (second players))
  ;; get neighbors of both the current player's tokens and see if they contain any of the second player's tokens
  (define-values (first-token second-token) (get-player-tokens players FIRST))
  (define-values (opponent-first-token opponent-second-token) (get-player-tokens players SECOND))
  
  (define first-token-neighbors (filter (lambda (neighbor)
                                          (and (level-ok? first-token neighbor spaces)
                                               (inside-board? neighbor)))
                                        (get-neighbors first-token)))
  (define second-token-neighbors (filter (lambda (neighbor)
                                           (and (level-ok? second-token neighbor spaces)
                                                (inside-board? neighbor)))
                                         (get-neighbors second-token)))
  
  (define current-player-all-neighbors (set-union (list->set first-token-neighbors)
                                                  (list->set second-token-neighbors)))
  (define common-positions (set-intersect current-player-all-neighbors
                                          (list->set opponent-tokens)))
  
  ;; if opponent's tokens are not in the neighbor positions of current player then move as usual
  (cond [(set-empty? common-positions) (move-without-cards cards
                                                           players
                                                           spaces
                                                           turn)]
        ;; otherwise see if swapping position actually helps
        ;; swap if opponent token has more level 2 neighbors than the player token
        
        [else
         (cond [(and (are-neighbors? first-token
                                     opponent-first-token)
                     (level-ok? first-token
                                opponent-first-token)) (cond [(has-more-level-two-neighbors? opponent-first-token
                                                                                             first-token
                                                                                             players
                                                                                             spaces)
                                                              (build-without-cards opponent-first-token
                                                                                   cards
                                                                                   (list (list opponent-first-token
                                                                                               second-token)
                                                                                         (list first-token
                                                                                               opponent-second-token))
                                                                                   spaces
                                                                                   turn)])])
         (cond [(and (are-neighbors? first-token
                                     opponent-second-token)
                     (level-ok? first-token
                                opponent-second-token)) (cond [(has-more-level-two-neighbors? opponent-second-token
                                                                                              first-token
                                                                                              players
                                                                                              spaces)
                                                               (build-without-cards opponent-second-token
                                                                                    cards
                                                                                    (list (list opponent-second-token
                                                                                                second-token)
                                                                                          (list first-token
                                                                                                opponent-first-token))
                                                                                    spaces
                                                                                    turn)])])
         (cond [(and (are-neighbors? second-token
                                     opponent-first-token)
                     (level-ok? second-token
                                opponent-first-token)) (cond [(has-more-level-two-neighbors? opponent-first-token
                                                                                             second-token
                                                                                             players
                                                                                             spaces)
                                                              (build-without-cards opponent-first-token
                                                                                   cards
                                                                                   (list (list opponent-first-token
                                                                                               first-token)
                                                                                         (list second-token
                                                                                               opponent-second-token))
                                                                                   spaces
                                                                                   turn)])])
         (cond [(and (are-neighbors? second-token
                                     opponent-second-token)
                     (level-ok? second-token
                                opponent-second-token)) (cond [(has-more-level-two-neighbors? opponent-second-token
                                                                                              second-token
                                                                                              players
                                                                                              spaces)
                                                               (build-without-cards opponent-second-token
                                                                                    cards
                                                                                    (list (list opponent-second-token
                                                                                                first-token)
                                                                                          (list second-token
                                                                                                opponent-first-token))
                                                                                    spaces
                                                                                    turn)])]
               [else (move-without-cards cards
                                         players
                                         spaces
                                         turn)])]))