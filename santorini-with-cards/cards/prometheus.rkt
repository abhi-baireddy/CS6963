#lang racket
(require "../helpers.rkt")
(require "../constants.rkt")
(require "../move-without-cards.rkt")
(require "../build-without-cards.rkt")
(require "../build-helpers.rkt")

(provide (all-defined-out))

;; A token can optionally build before moving, but then the move is constrained to the same level or lower
;; (i.e., the level of the token’s new space can be no larger than the level of the token’s old space).
;; The moved token must still build after moving.
(define (move-prometheus players cards spaces turn)

  ;; easy to see when not to use prometheus -- when token picked to move has only higher level neighbors
  (define-values (first-token second-token) (get-player-tokens players FIRST))
  (define first-token-neighbors (valid-neighbors first-token
                                                 players
                                                 spaces))
  (define second-token-neighbors (valid-neighbors second-token
                                                  players
                                                  spaces))
  ;; get the common neighbors of current player's tokens
  (define common-neighbors (set->list (set-intersect (list->set first-token-neighbors)
                                                     (list->set second-token-neighbors))))
  ;; keep only the common neighbors that are not reachable by any of the opponent's tokens
  (define safe-common-neighbors (filter (lambda (neighbor)
                                          (not (reachable-by-opponent? neighbor
                                                                       players
                                                                       spaces)))
                                        common-neighbors))
  ;; keep only neighbors that are at the same level or lower and not complete
  (define first-token-filtered-neighbors (filter (lambda (neighbor)
                                                   (and (<= (level-of neighbor spaces)
                                                            (level-of first-token))
                                                        (not-complete? neighbor spaces)))
                                                 first-token-neighbors))
  
  (define second-token-filtered-neighbors (filter (lambda (neighbor)
                                                    (and (<= (level-of neighbor spaces)
                                                             (level-of first-token))
                                                         (not-complete? neighbor spaces)))
                                                  second-token-neighbors))
  ;; use Prometheus only if there is at least one safe common neighbor and at least one of the tokens has
  ;; more than one filtered neighbors
  (cond [(not (empty? safe-common-neighbors))
         (begin
           (define level-one-common-neighbors (filter (lambda (neighbor)
                                                        (= (level-of neighbor spaces) LEVEL-ONE))
                                                      safe-common-neighbors))
           (define level-two-common-neighbors (filter (lambda (neighbor)
                                                        (= (level-of neighbor spaces) LEVEL-TWO))
                                                      safe-common-neighbors))
           (cond [(not (empty? first-token-filtered-neighbors))
                  (begin
                    (define first-token-new-posn (select-random first-token-filtered-neighbors))
                    ;; build at one of the level two or level one posns
                    (cond [(not (empty? level-two-common-neighbors))
                           (build-without-cards cards
                                                (list (list first-token-new-posn second-token)
                                                      (second players))
                                                (build-at (select-random level-two-common-neighbors)
                                                          spaces)
                                                turn)]
                          [(not (empty? level-one-common-neighbors))
                           (build-without-cards cards
                                                (list (list first-token-new-posn second-token)
                                                      (second players))
                                                (build-at (select-random level-one-common-neighbors)
                                                          spaces)
                                                turn)]))])

           (cond [(not (empty? second-token-filtered-neighbors))
                  (begin
                    (define second-token-new-posn (select-random second-token-filtered-neighbors))
                    ;; build at one of the level two or level one posns
                    (cond [(not (empty? level-two-common-neighbors))
                           (build-without-cards cards
                                                (list (list second-token-new-posn first-token)
                                                      (second players))
                                                (build-at (select-random level-two-common-neighbors)
                                                          spaces)
                                                turn)]
                          [(not (empty? level-one-common-neighbors))
                           (build-without-cards cards
                                                (list (list second-token-new-posn first-token)
                                                      (second players))
                                                (build-at (select-random level-one-common-neighbors)
                                                          spaces)
                                                turn)]))]
                 [else (move-without-cards cards
                                           players
                                           spaces
                                           turn)]))]

        [else (move-without-cards cards
                                  players
                                  spaces
                                  turn)]))
