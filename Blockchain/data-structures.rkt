#lang racket

(provide (all-defined-out))


;; chain is simply a list of blocks -- the blockchain
(struct chain (blocks)
  #:prefab)


;; a block stores a nonce (a number), data (a string),
;; hash (of the string formed by concatenating all the fields of the block other than this field; this is a hex-string),
;; prev-hash (hash of the preceeding block in the blockchain; also a hex-string)
(struct block (nonce
               balances
               txn
               hash
               prev-hash)
  #:prefab)

;; transactions struct has sender recipient amount
(struct transaction (sender
                     recipient
                     amount)
  #:prefab)