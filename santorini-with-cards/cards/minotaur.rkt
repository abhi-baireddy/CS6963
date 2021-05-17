#lang racket
(require "../helpers.rkt")
(require "../constants.rkt")
(require "../move-without-cards.rkt")
(require "../build-without-cards.rkt")
(provide (all-defined-out))

;; A token’s move can optionally enter the space of an opponent’s token,
;; but only if the token can be pushed back to an unoccupied space,
;; and only as long as the token would be able to move to the opponent’s space
;; if the opponent token were not there.
;; The unoccupied space where the opponent’s token is pushed can be at any level less than 4. 
(define (move-minotaur cards players spaces turn)
  ;; use this card if current player has no option but to push the opponent
  ;; i.e there is only no possible move positions for both the tokens but one of
  ;; the neighbors is occupied by an opponent
  
  
  (define-values (first-token second-token) (get-player-tokens FIRST))
  (define-values (opponent-first-token opponent-second-token) (get-player-tokens SECOND))
  (define all-neighbors-of-current-player (get-all-neighbors-of-player FIRST players spaces))
  (cond [(not (empty? all-neighbors-of-current-player) (move-without-cards cards
                                                                           players
                                                                           spaces
                                                                           turn))]
        [else (begin
                (define opponent-first-token-neighbors (valid-neighbors opponent-first-token
                                                                        players
                                                                        spaces))
                (define opponent-second-token-neighbors (valid-neighbors opponent-second-token
                                                                         players
                                                                         spaces))
                
                (cond [(and (> (length opponent-first-token-neighbors) 1)
                            (are-neighbors? first-token opponent-first-token))
                       (begin
                         (define first-token-new-posn (select-random opponent-first-token-neighbors))
                         (define opponent-first-token-new-posn (select-random (remove first-token-new-posn
                                                                                      opponent-first-token-neighbors)))
                         (build-without-cards cards
                                              (list (list first-token-new-posn
                                                          second-token)
                                                    (list opponent-first-token-new-posn
                                                          opponent-second-token))
                                              spaces
                                              turn))])
                (cond [(and (> (length opponent-first-token-neighbors) 1)
                            (are-neighbors? second-token opponent-first-token))
                       (begin
                         (define second-token-new-posn (select-random opponent-first-token-neighbors))
                         (define opponent-first-token-new-posn (select-random (remove second-token-new-posn
                                                                                      opponent-first-token-neighbors)))
                         (build-without-cards cards
                                              (list (list second-token-new-posn
                                                          first-token)
                                                    (list opponent-first-token-new-posn
                                                          opponent-second-token))
                                              spaces
                                              turn))])
                (cond [(and (> (length opponent-second-token-neighbors) 1)
                            (are-neighbors? first-token opponent-first-token))
                       (begin
                         (define first-token-new-posn (select-random opponent-second-token-neighbors))
                         (define opponent-second-token-new-posn (select-random (remove first-token-new-posn
                                                                                      opponent-second-token-neighbors)))
                         (build-without-cards cards
                                              (list (list first-token-new-posn
                                                          second-token)
                                                    (list opponent-second-token-new-posn
                                                          opponent-first-token))
                                              spaces
                                              turn))])
                (cond [(and (> (length opponent-second-token-neighbors) 1)
                            (are-neighbors? second-token opponent-second-token))
                       (begin
                         (define second-token-new-posn (select-random opponent-second-token-neighbors))
                         (define opponent-second-token-new-posn (select-random (remove second-token-new-posn
                                                                                      opponent-second-token-neighbors)))
                         (build-without-cards cards
                                              (list (list second-token-new-posn
                                                          first-token)
                                                    (list opponent-second-token-new-posn
                                                          opponent-first-token))
                                              spaces
                                              turn))]
                      [else (move-without-cards cards
                                                players
                                                spaces
                                                turn)]))]))

