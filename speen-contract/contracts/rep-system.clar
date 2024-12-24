;; Define constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-already-reviewed (err u101))

;; Define data maps
(define-map reviews 
  { reviewer: principal, reviewee: principal } 
  { rating: uint, comment: (string-utf8 280) })

(define-map reputation-scores principal uint)

(define-map nft-badges principal uint)

;; Define non-fungible token for badges
(define-non-fungible-token reputation-badge uint)

;; Counter for NFT badge IDs
(define-data-var badge-id-nonce uint u0)

;; Function to leave a review
(define-public (leave-review (reviewee principal) (rating uint) (comment (string-utf8 280)))
  (let ((review-key { reviewer: tx-sender, reviewee: reviewee }))
    (asserts! (is-none (map-get? reviews review-key)) err-already-reviewed)
    (map-set reviews review-key { rating: rating, comment: comment })
    (ok (update-reputation-score reviewee rating))))

;; Function to get a review
(define-read-only (get-review (reviewer principal) (reviewee principal))
  (map-get? reviews { reviewer: reviewer, reviewee: reviewee }))

;; Function to update reputation score
(define-private (update-reputation-score (user principal) (new-rating uint))
  (let ((current-score (default-to u0 (map-get? reputation-scores user))))
    (map-set reputation-scores user (+ current-score new-rating))
    (try! (check-and-issue-badge user))
    (ok true)))

;; Function to get reputation score
(define-read-only (get-reputation-score (user principal))
  (default-to u0 (map-get? reputation-scores user)))

;; Function to check and issue badge
(define-private (check-and-issue-badge (user principal))
  (let ((score (get-reputation-score user)))
    (if (and (>= score u100) (is-none (map-get? nft-badges user)))
      (issue-badge user)
      (ok true))))

;; Function to issue NFT badge
(define-private (issue-badge (recipient principal))
  (let ((new-badge-id (+ (var-get badge-id-nonce) u1)))
    (try! (nft-mint? reputation-badge new-badge-id recipient))
    (var-set badge-id-nonce new-badge-id)
    (map-set nft-badges recipient new-badge-id)
    (ok true)))

;; Function to check if user has a badge
(define-read-only (has-badge (user principal))
  (is-some (map-get? nft-badges user)))