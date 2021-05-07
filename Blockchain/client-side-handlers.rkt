#lang racket

(provide (all-defined-out))

(require "network-constants.rkt")
(require "data-structures.rkt")
(require racket/serialize)


;; connect to the peer running at specified port
(define (connect-to-peer reconnecting? current-peer other-peer-port)
  (define-values (in out) (tcp-connect localhost other-peer-port))
  ;; message type is 'connection-request'
  (define message-type CONN_REQ)
  ;; send message type to server
  (write (serialize message-type) out)
  (flush-output out)
  (define my-port-no (peer-port current-peer))
  ;; send my port number to the other peer so that it adds me to its list of peers
  (write (serialize my-port-no) out)
  (flush-output out)
  (define my-peers (peer-connected-peers current-peer))
  ;; send my peers to server so that it merges my peer list and its own peer list
  (write (serialize my-peers) out)
  (flush-output out)
  ;; get back the list of peers of the other peer so that I can update my own list of peers
  (define peers-connected-to-other-peer (deserialize (read in)))
  ;; let the main loop know that my peers set has been updated via the updated-peers-channel
  (define my-updated-peer-set (set-union (peer-connected-peers current-peer)
                                         (set-union peers-connected-to-other-peer
                                                    (set other-peer-port))))
  (channel-put updated-peers-channel
               my-updated-peer-set)
  (displayln (~a "Successfully connected to " other-peer-port))
  ;; client must also tell its peers about the new peer and new peer's peers
  (broadcast-data my-peers
                  NEW_PEER_ADDED
                  (set-union peers-connected-to-other-peer
                             (set other-peer-port)))
  (close-ports in out)
  (if reconnecting?
      (sync-blockchain my-port-no other-peer-port)
      (send-init-blockchain my-port-no other-peer-port (get-current-blockchain))))

;; handles the case when a peer disonncects from the network and connects back again after some time
(define (sync-blockchain current-peer other-peer-port)
  ;; only exchange blockchains and keep the longest one
  (define-values (in out) (tcp-connect localhost other-peer-port))
  ;; send message type
  (write (serialize SYNC_BLOCKCHAIN) out)
  (flush-output out)
  ;; get blockchain from the other peer
  (define other-peer-blockchain (deserialize (read in)))
  ;; get my blockchain
  (define my-blockchain (get-current-blockchain))
  (if (< (length (chain-blocks my-blockchain))
         (length (chain-blocks other-peer-blockchain)))
      (channel-put update-blockchain-channel other-peer-blockchain)
      (~a "My blockchain is already up to date!"))
  (close-ports in out))


;; sends newly initialized peer's blokchain (which has only one node) to the peer it just connected to
(define (send-init-blockchain my-port other-peer-port blockchain)
  (define-values (in out) (tcp-connect localhost other-peer-port))
  (write (serialize INIT_BLOCKCHAIN) out)
  (flush-output out)
  (write (serialize my-port) out)
  (flush-output out)
  (write (serialize blockchain) out)
  (flush-output out)
  (close-ports in out))

;; broadcasts new transaction struct to all peers
(define (make-transaction current-peer-struct to amount)
  (define from (peer-port current-peer-struct))
  (define new-transaction (transaction from
                                       to
                                       amount))
  (broadcast-data (peer-connected-peers current-peer-struct)
                  NEW_TXN
                  new-transaction))