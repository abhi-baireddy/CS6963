#lang racket

(require "helpers.rkt")
(require "constants.rkt")
(require "build-helpers.rkt")
(require "build-without-cards.rkt")
(require "cards/atlas.rkt")
(require "cards/demeter.rkt")
(require "cards/hephastus.rkt")
(require json)
(provide (all-defined-out))



(define (build-step moved-token cards players spaces turn)
  ;; if current player has a build card then handle build card cases
  ;; otherwise proceed as in no cards version
  (define current-player-card (first cards))
  (if (list? (member build-cards current-player-card))
      (build-with-cards moved-token cards players spaces turn)
      (build-without-cards moved-token cards players spaces turn)))


;; build with cards is called if current player has a build card
(define (build-with-cards moved-token cards players spaces turn)
  (define current-player-card (first cards))
  (case current-player-card
    [("Atlas") (build-atlas moved-token cards players spaces turn)]
    [("Demeter") (build-demeter moved-token cards players spaces turn)]
    [("Hephastus") (build-hephastus moved-token cards players spaces turn)]))








