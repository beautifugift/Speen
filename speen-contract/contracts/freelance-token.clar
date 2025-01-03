;; Define the contract owner
(define-constant contract-owner tx-sender)

;; Define the token
(define-fungible-token freelance-token)

;; Error codes
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-amount (err u102))

;; Data variables
(define-map user-stakes principal uint)
(define-map user-rewards principal uint)

;; Read-only functions

(define-read-only (get-balance (account principal))
  (ft-get-balance freelance-token account))

(define-read-only (get-stake (account principal))
  (default-to u0 (map-get? user-stakes account)))

(define-read-only (get-rewards (account principal))
  (default-to u0 (map-get? user-rewards account)))

;; Public functions

;; Mint new tokens (only contract owner)
(define-public (mint (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ft-mint? freelance-token amount recipient)))

;; Pay transaction fees
(define-public (pay-fee (amount uint))
  (ft-burn? freelance-token amount tx-sender))

;; Reward users
(define-public (reward-user (user principal) (amount uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set user-rewards user (+ (get-rewards user) amount))
    (ft-mint? freelance-token amount user)))

;; Stake tokens
(define-public (stake (amount uint))
  (begin
    (asserts! (>= (ft-get-balance freelance-token tx-sender) amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    (map-set user-stakes tx-sender (+ (get-stake tx-sender) amount))
    (ft-burn? freelance-token amount tx-sender)))

;; Unstake tokens
(define-public (unstake (amount uint))
  (begin
    (asserts! (>= (get-stake tx-sender) amount) err-insufficient-balance)
    (asserts! (> amount u0) err-invalid-amount)
    (map-set user-stakes tx-sender (- (get-stake tx-sender) amount))
    (ft-mint? freelance-token amount tx-sender)))

;; Claim rewards
(define-public (claim-rewards)
  (let ((reward-amount (get-rewards tx-sender)))
    (begin
      (asserts! (> reward-amount u0) err-invalid-amount)
      (map-set user-rewards tx-sender u0)
      (ft-mint? freelance-token reward-amount tx-sender))))