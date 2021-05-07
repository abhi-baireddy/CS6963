#lang racket

(require "network-constants.rkt")
(require "data-structures.rkt")
(require "blockchain.rkt")
(require racket/serialize)
(provide (all-defined-out))


;; responds to a new connection request by reading the client's port no, then the client's list of peers
;; then sends its own set of peers -- server side
(define (new-connection-handler in out)
  (define client-port-no (deserialize (read in)))
  (define peers-connected-to-client (deserialize (read in)))
  (define my-peers (get-current-peers))
  ;;send my list of peers to client
  (write (serialize my-peers) out)
  (flush-output out)
  ;; let main loop know my peers set has been updated as a new peer just connected to me
  (channel-put updated-peers-channel
               (set-union peers-connected-to-client
                          (set-union my-peers (set client-port-no))))
  (displayln (~a "Successfully connected to " client-port-no))
  (broadcast-data my-peers
                  NEW_PEER_ADDED
                  (set-union peers-connected-to-client
                             (set client-port-no))))


;; add-new-peer is called when the server is notified of a new peer's joining to the network
(define (add-new-peer in out)
  (define new-peers (deserialize (read in)))
  ;; get current peers set
  (define current-peers (get-current-peers))
  (channel-put updated-peers-channel
               (set-union current-peers new-peers))
  (displayln (~a "Added newly joined peer and its peers to my peers set")))


;; remove the peer that is about to disconnect
(define (remove-peer in out)
  (define peer-to-remove (deserialize (read in)))
  (define current-peers (get-current-peers))
  (channel-put updated-peers-channel
               (set-remove current-peers peer-to-remove))
  (displayln (~a peer-to-remove " disconnected from network")))


;; looks at the new transaction and determines if its allowed
;; then creates a new block with updated balances and this new transaction
;; mines the new block and adds it to its blockchain
(define (process-transaction in out)
  (define new-transaction (deserialize (read in)))
  (define sender (transaction-sender new-transaction))
  (define recipient (transaction-recipient new-transaction))
  (define amount (transaction-amount new-transaction))
  (channel-put get-current-blockchain-channel reply-channel)
  (define my-blockchain (channel-get reply-channel))
  (define balances (block-balances (last (chain-blocks my-blockchain))))
  ;; determine if the sender has sufficient funds
  (define sender-balance (get-balance balances sender))
  (cond [(< sender-balance amount) (reject-transaction sender recipient amount sender-balance)]
        [else (accept-transaction sender recipient amount balances)]))


;; loops through the list of balances and returns the balance of the peer
(define (get-balance balances peer)
  (cond [(empty? balances) ZERO]
        [(equal? (car (first balances)) peer) (cdr (first balances))]
        [else (get-balance (rest balances) peer)]))


;; update balances, create a new block, mine it and add it to local blockchain
(define (accept-transaction sender recipient amount balances)
  ;; update balances
  (define new-balances (update-balances sender recipient amount balances))
  ;; get current blockchain
  (define current-blockchain (get-current-blockchain))
  ;;create a new block
  (define last-block (last (chain-blocks current-blockchain)))
  (define new-block (block ZERO
                           new-balances
                           (transaction sender
                                        recipient
                                        amount)
                           INIT-HASH
                           INIT-PREV-HASH))
  ;; add new block to blockchain
  (define new-blockchain (add-block current-blockchain (set-hash-of-block new-block)))
  ; update current blockchain
  (channel-put update-blockchain-channel new-blockchain)
  (define my-peers (get-current-peers))
  (broadcast-data my-peers UPDATE_BLOCKCHAIN new-blockchain))


;; update sender's and receiver's balances and return a new list
(define (update-balances sender recipient amount balances)
  (for/list ([balance balances])
    (cond [(equal? (car balance) sender) (cons sender
                                               (- (cdr balance)
                                                  amount))]
          [(equal? (car balance) recipient) (cons recipient
                                                  (+ (cdr balance)
                                                     amount))]
          [else balance])))


;; prints a rejection message
(define (reject-transaction sender recipient amount sender-balance)
  (displayln (~a sender " has only " sender-balance
                 "coins. Can't send " amount " coins to " recipient
                 ". Rejecting this transaction")))


;; updates current balances with the new peer's balance by
;; adding a new transaction block to its blockchain
(define (record-new-peer in out)
  (define new-peer-port (deserialize (read in)))
  (define new-peer-blockchain (deserialize (read in)))
  (define other-peer-init-block (first (chain-blocks new-peer-blockchain)))
  ;; get my blockchain
  (define my-blockchain (get-current-blockchain))
  (define latest-block (last (chain-blocks my-blockchain)))
  (define balances (block-balances latest-block))
  (define updated-balances (append balances
                                   (block-balances other-peer-init-block)))
  ;; create a new block
  (define new-block (block INIT-NONCE
                           updated-balances
                           (transaction new-peer-port new-peer-port INIT-BALANCE)
                           INIT-HASH
                           INIT-PREV-HASH))
  (define new-block-hashed (set-hash-of-block new-block))
  (define updated-blockchain (add-block my-blockchain new-block-hashed))
  (channel-put update-blockchain-channel updated-blockchain)
  (close-ports in out)
  ;; now broadcast my updated blockchain to all my peers including newly connected peer
  (define my-peers (get-current-peers))
  (broadcast-data my-peers UPDATE_BLOCKCHAIN updated-blockchain))


;; reconnect to an old peer that disconnected from the network
(define (reconnect-to-peer in out)
  ;; send my blockchain
  (channel-put get-current-blockchain-channel reply-channel)
  (define my-blockchain (channel-get reply-channel))
  (write (serialize my-blockchain) out)
  (flush-output out)
  (close-ports in out))


;; update my blockchain if length of my blockchain is less than the one I just received
(define (update-blockchain in out)
  (define new-blockchain (deserialize (read in)))
  (channel-put get-current-blockchain-channel reply-channel)
  (define my-blockchain (channel-get reply-channel))
  (if (< (length (chain-blocks my-blockchain))
         (length (chain-blocks new-blockchain)))
      (channel-put update-blockchain-channel new-blockchain)
      (~a "My blockchain is already up to date!"))
  (close-ports in out))

