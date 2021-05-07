#lang racket
(require sha)
(require json)
(require racket/serialize)
(require "data-structures.rkt")
(provide (all-defined-out))


(define CONSTANT-ZERO 0)
;; experimented with 1, 2, 3, and 4 zeroes
;; mining seems to take at least 4 seconds if this value is 5
;; so looks like a good small value
(define NUM-LEADING-ZEROES 4)

;; the number of zeroes we want the hash of a block to start with (mining)
(define TARGET-START-OF-HASH "0000")

;; hash and prev-hash fields are set to these values when a block is created for the first time
(define INIT-NONCE 0)
(define INIT-HASH 0)
(define INIT-PREV-HASH "0")
;; when a peer is initialized for the first time, starts with 50 coins
(define INIT-BAL 50)
;; when the peer is initialized for the first time, first transaction is dummy
(define FIRST-TXN (transaction 0 0 0))


;; initializes a block with the given data
(define (init-block port-no)
  (block INIT-NONCE (list (cons port-no INIT-BAL)) FIRST-TXN INIT-HASH INIT-PREV-HASH))


;; takes a block (almost always a newly created block)
;; and sets its hash field to the hash of the block (computed by hash-of-block function)
(define (set-hash-of-block node)
  (block (block-nonce node)
         (block-balances node)
         (block-txn node)
         (hash-of-block node)
         (block-prev-hash node)))


;; computes the hash of a block
;; hash of a block is hash of the string formed by concatenating
;; all the other fields of the block
(define (hash-of-block node)
  (bytes->hex-string (sha256 (bytes-append
                              (string->bytes/utf-8 (number->string (block-nonce node)))
                              (string->bytes/utf-8 (~a (serialize (block-balances node))))
                              (string->bytes/utf-8 (~a (serialize (block-txn node))))
                              (string->bytes/utf-8 (block-prev-hash node))))))


;; a block is only valid if the value of it's hash field
;; mathches the hash computed by hash-of-block
;; this is used to check if the block has been tampered with by someone
(define (is-valid-block? node)
  (equal? (hash-of-block node)
          (block-hash node)))


;; a blockchain is only valid iff every block of it is valid
;; and the prev-hash fields of all blocks are as expected 
(define (is-valid-chain? blockchain)
  (cond [(empty? (chain-blocks blockchain)) #t]
        [(empty? (rest (chain-blocks blockchain))) (and (is-valid-block? (first (chain-blocks blockchain)))
                                                        (equal? (block-prev-hash (first (chain-blocks blockchain)))
                                                                INIT-PREV-HASH))]
        [else (is-valid-chain-helper? (first (chain-blocks blockchain))
                                      (rest (chain-blocks blockchain)))]))


(define (is-valid-chain-helper? first-node rest-of-nodes)
  (cond [(empty? rest-of-nodes) (is-valid-block? first-node)]
        [else (and (is-valid-block? first-node)
                   (and (equal? (block-hash first-node)
                                (block-prev-hash (first rest-of-nodes)))
                        (is-valid-chain-helper? (first rest-of-nodes)
                                                (rest rest-of-nodes))))]))


;; a block is said to be mined if it's hash starts with a certain number of zeroes (5 in this case)
(define (is-block-mined? node)
  (equal? (substring (block-hash node) CONSTANT-ZERO NUM-LEADING-ZEROES)
          TARGET-START-OF-HASH))


;; a block is mined by finding a nonce that
;; makes the hash of the block start with a certain number of zeroes (5 in this case)
;; we find the appropriate nonce by brute-force
(define (mine-block node nonce)
  (cond [(is-block-mined? node) node]
        [else (letrec ([new-node (block nonce
                                        (block-balances node)
                                        (block-txn node)
                                        (block-hash node)
                                        (block-prev-hash node))]
                       [rehashed-new-node (set-hash-of-block new-node)])
                (mine-block rehashed-new-node (+ nonce 1)))]))


;; function to add a block to the blockchain
;; a fully created individual block needs to be mined before adding it to the
;; end of the blockchain
(define (add-block blockchain node)
  (cond [(empty? (chain-blocks blockchain))
         (chain (append empty (list (mine-block node INIT-NONCE))))]
        [else (letrec ([current-blocks (chain-blocks blockchain)]
                       [new-block (block (block-nonce node)
                                         (block-balances node)
                                         (block-txn node)
                                         (block-hash node)
                                         (block-hash (last current-blocks)))]
                       [new-block-rehashed (set-hash-of-block new-block)])
                (chain (append current-blocks
                               (list (mine-block new-block-rehashed INIT-NONCE)))))]))


;; print contents of a node
(define (print-block node)
  (display (~a "\nNonce: " (block-nonce node)
               "\nBalances:\n" (balances->string (block-balances node))
               "\nTransaction: " (transaction->string (block-txn node))
               "\nHash: " (block-hash node)
               "\nPrevious Hash: " (block-prev-hash node)
               "\n")))

;; get string form of transaction
(define (transaction->string txn)
  (~a "["
      " From: " (transaction-sender txn)
      ", To: " (transaction-recipient txn)
      ", Amount: " (transaction-amount txn)
      " ]"))

;; get string form of balances
(define (balances->string balances)
  (define string-form
    (for/list ([entry balances])
      (~a (car entry) ": " (cdr entry))))
  (string-join string-form "\n"))


;; loop over the blocks and print each
(define (print-blockchain blockchain)
  (displayln (~a "\t\t\tBLOCKCHAIN START" ))
  (for ([node (chain-blocks blockchain)])
    (displayln (make-string 60 #\-))
    (print-block node))
  (displayln (make-string 60 #\-))
  (displayln (~a "\t\t\tBLOCKCHAIN END" ))) 