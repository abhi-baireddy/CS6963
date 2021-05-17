#lang racket
(require json)
(require "move-with-cards.rkt")
(require "helpers.rkt")



;;game entry-point
(define (run-game)
  (local [(define input-board (read-json))]
    (cond
      [(list? input-board) (setup input-board)]
      [else (make-move input-board)])))


;;select initial positions to place current player's tokens 
(define (setup players)
  ;;players is an array of two dicts; first element of players is mine
  ;;setup picks two random positions from the board and makes the pre player a player by adding a 'tokens' key
  ;;with these random positions as value to that key
  (let* ([current-pre-player (first players)]
         [other-player (second players)]
         [other-player-tokens (cond [(hash-has-key? other-player 'tokens) (hash-ref other-player 'tokens)]
                                    [else empty])]
         [first-token (random-pos other-player-tokens)]
         [second-token (random-pos (append other-player-tokens (list first-token)))]
         [current-player (hasheq 'card (hash-ref current-pre-player 'card) 'tokens (list first-token second-token))])

    (write-json(list other-player current-player))
    (flush-output)))



;;game loop
(for ([i (in-naturals)])
  (run-game)
  (flush-output))  