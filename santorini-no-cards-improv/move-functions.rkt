#lang racket

(require json)
(require "build-functions.rkt")
(require "helpers.rkt")
(require "constants.rkt")
(provide (all-defined-out))



;;move-step -- takes in current payers tokens and board state and returns next state
;;next state has different player-tokens list corresponding to the new positions of the current player but spaces will look same
(define (move-step players spaces turn)
  ;;first player in the players array is the current player
  (define first-player-tokens (player-tokens players FIRST))
  (define picked-token (random 0 2))
  (define neighbors-of-picked-token (valid-neighbors
                                     (player-tokens first-player-tokens picked-token)
                                     (append (player-tokens players FIRST)
                                             (player-tokens players SECOND))
                                     spaces))
  (define token-to-move (if (empty? neighbors-of-picked-token)
                            (modulo (add1 picked-token)
                                    (length first-player-tokens))
                            picked-token))

  (define potential-move-positions (if (= token-to-move picked-token)
                                       neighbors-of-picked-token
                                       (valid-neighbors
                                        (player-tokens first-player-tokens token-to-move)
                                        (append (player-tokens players FIRST)
                                                (player-tokens players SECOND))
                                        spaces)))
  (define new-position-of-token-to-move (pick-move-position potential-move-positions spaces))
  (define new-player-tokens (list new-position-of-token-to-move
                                  (list-ref first-player-tokens
                                            (modulo (add1 token-to-move)
                                                    (length first-player-tokens)))))
  (define updated-players (list (player-tokens players SECOND)
                                new-player-tokens))
  ;; if the token moved to a space whose level is 3, then it won so no need to build
  ;; otherwise build
  (cond [(= (val-at spaces
                    (first new-position-of-token-to-move)
                    (second new-position-of-token-to-move))
            LEVEL-THREE) (write-json (make-board updated-players spaces (add1 turn)))]
        [else (build-step updated-players new-position-of-token-to-move spaces turn)]))


;;pick a position to move -- takes a list of valid neighbors and sees if there is a neighbor with a level three tower
;;if yes, moves to that position and wins
(define (pick-move-position neighbors spaces)
  (let* ([levels (map (lambda (neighbor)
                       (val-at spaces (first neighbor)
                               (second neighbor)))
                     neighbors)]
        [picked-pos-index (index-of levels LEVEL-THREE)])
    (cond [(false? picked-pos-index) (list-ref neighbors (random 0 (length neighbors)))]
          [else (list-ref neighbors picked-pos-index)])))

