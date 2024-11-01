;; Arbitration DAO: On-Chain Dispute Resolution Mechanism

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-DISPUTE (err u101))
(define-constant ERR-INSUFFICIENT-STAKE (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-DISPUTE-CLOSED (err u104))
(define-constant ERR-INVALID-VOTE (err u105))

;; Define fungible token for staking and rewards
(define-fungible-token arbitration-token)

;; Data structures
(define-map disputes
    { dispute-id: uint }
    {
        creator: principal,
        description: (string-utf8 500),
        status: (string-utf8 20),
        votes-for: uint,
        votes-against: uint,
        total-stake: uint,
        resolution-fee: uint
    }
)

(define-map arbiter-votes
    { dispute-id: uint, arbiter: principal }
    { vote: (string-utf8 10), stake: uint }
)

(define-map evidence
    { dispute-id: uint, evidence-id: uint }
    { submitter: principal, evidence-hash: (buff 32) }
)

;; Variables
(define-data-var dispute-nonce uint u0)
(define-data-var evidence-nonce uint u0)
(define-data-var min-stake uint u100) ;; Minimum stake required to vote
(define-data-var contract-owner principal tx-sender)
(define-data-var authorized-arbiters (list 100 principal) (list tx-sender))

;; Private functions
(define-private (is-authorized-arbiter (caller principal))
    (is-some (index-of (var-get authorized-arbiters) caller))
)

;; Public functions

;; Create a new dispute
(define-public (create-dispute (description (string-utf8 500)) (resolution-fee uint))
    (let
        ((dispute-id (+ (var-get dispute-nonce) u1)))
        (map-set disputes
            { dispute-id: dispute-id }
            {
                creator: tx-sender,
                description: description,
                status: u"open",
                votes-for: u0,
                votes-against: u0,
                total-stake: u0,
                resolution-fee: resolution-fee
            }
        )
        (var-set dispute-nonce dispute-id)
        (ok dispute-id)
    )
)

;; Submit evidence for a dispute
(define-public (submit-evidence (dispute-id uint) (evidence-hash (buff 32)))
    (let
        ((evidence-id (+ (var-get evidence-nonce) u1))
         (dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-INVALID-DISPUTE)))
        (asserts! (is-eq (get status dispute) u"open") ERR-DISPUTE-CLOSED)
        (map-set evidence
            { dispute-id: dispute-id, evidence-id: evidence-id }
            { submitter: tx-sender, evidence-hash: evidence-hash }
        )
        (var-set evidence-nonce evidence-id)
        (ok evidence-id)
    )
)

;; Vote on a dispute
(define-public (vote-on-dispute (dispute-id uint) (vote (string-utf8 10)) (stake uint))
    (let
        ((dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-INVALID-DISPUTE))
         (current-vote (map-get? arbiter-votes { dispute-id: dispute-id, arbiter: tx-sender })))
        (asserts! (is-authorized-arbiter tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status dispute) u"open") ERR-DISPUTE-CLOSED)
        (asserts! (>= stake (var-get min-stake)) ERR-INSUFFICIENT-STAKE)
        (asserts! (is-none current-vote) ERR-ALREADY-VOTED)
        (asserts! (or (is-eq vote u"for") (is-eq vote u"against")) ERR-INVALID-VOTE)
        
        (try! (ft-transfer? arbitration-token stake tx-sender (as-contract tx-sender)))
        
        (map-set arbiter-votes
            { dispute-id: dispute-id, arbiter: tx-sender }
            { vote: vote, stake: stake }
        )
        
        (map-set disputes
            { dispute-id: dispute-id }
            (merge dispute {
                votes-for: (if (is-eq vote u"for")
                    (+ (get votes-for dispute) u1)
                    (get votes-for dispute)),
                votes-against: (if (is-eq vote u"against")
                    (+ (get votes-against dispute) u1)
                    (get votes-against dispute)),
                total-stake: (+ (get total-stake dispute) stake)
            })
        )
        
        (ok true)
    )
)

;; Resolve a dispute
(define-public (resolve-dispute (dispute-id uint))
    (let
        ((dispute (unwrap! (map-get? disputes { dispute-id: dispute-id }) ERR-INVALID-DISPUTE)))
        (asserts! (is-authorized-arbiter tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status dispute) u"open") ERR-DISPUTE-CLOSED)
        
        (let
            ((total-votes (+ (get votes-for dispute) (get votes-against dispute)))
             (resolution (if (> (get votes-for dispute) (get votes-against dispute)) u"resolved-for" u"resolved-against"))
             (winning-stake (if (is-eq resolution u"resolved-for")
                                (get votes-for dispute)
                                (get votes-against dispute)))
             (reward-per-stake (/ (get resolution-fee dispute) winning-stake)))
            
            (map-set disputes
                { dispute-id: dispute-id }
                (merge dispute { status: resolution })
            )
            
            ;; Distribute rewards
            (try! (distribute-rewards dispute-id resolution reward-per-stake))
            
            (ok resolution)
        )
    )
)

;; Private function to distribute rewards
(define-private (distribute-rewards (dispute-id uint) (resolution (string-utf8 20)) (reward-per-stake uint))
    (let
        ((arbiters (unwrap! (get-arbiters-who-voted dispute-id) ERR-INVALID-DISPUTE)))
        (fold distribute-reward-to-arbiter
            arbiters
            { total-distributed: u0, dispute-id: dispute-id, resolution: resolution, reward-per-stake: reward-per-stake }
        )
        (ok true)
    )
)

;; Helper function to distribute reward to a single arbiter
(define-private (distribute-reward-to-arbiter 
    (arbiter principal)
    (acc { total-distributed: uint, dispute-id: uint, resolution: (string-utf8 20), reward-per-stake: uint })
)
    (let
        ((vote (unwrap! (map-get? arbiter-votes { dispute-id: (get dispute-id acc), arbiter: arbiter }) acc)))
        (if (is-eq (get vote vote) (get resolution acc))
            (let
                ((reward (* (get stake vote) (get reward-per-stake acc))))
                (match (as-contract (ft-transfer? arbitration-token
                    reward
                    tx-sender
                    arbiter))
                    success (merge acc { total-distributed: (+ (get total-distributed acc) reward) })
                    error acc)
            )
            acc
        )
    )
)

;; Helper function for checking and adding arbiters to the list
(define-private (check-and-add-arbiter
    (arbiter principal)
    (acc { arbiters: (list 100 principal), dispute-id: uint })
)
    (let 
        (
            (vote (map-get? arbiter-votes { dispute-id: (get dispute-id acc), arbiter: arbiter }))
            (current-arbiters (get arbiters acc))
        )
        (if (is-some vote)
            (merge acc { 
                arbiters: (match (as-max-len? (append current-arbiters arbiter) u100)
                    success-result success-result
                    current-arbiters)
            })
            acc
        )
    )
)

;; Read-only functions

;; Get dispute details
(define-read-only (get-dispute-details (dispute-id uint))
    (map-get? disputes { dispute-id: dispute-id })
)

;; Get evidence for a dispute
(define-read-only (get-evidence (dispute-id uint) (evidence-id uint))
    (map-get? evidence { dispute-id: dispute-id, evidence-id: evidence-id })
)

;; Get arbiter's vote for a dispute
(define-read-only (get-arbiter-vote (dispute-id uint) (arbiter principal))
    (map-get? arbiter-votes { dispute-id: dispute-id, arbiter: arbiter })
)

;; Get all arbiters who voted on a dispute (single implementation)
(define-read-only (get-arbiters-who-voted (dispute-id uint))
    (ok (get arbiters (fold check-and-add-arbiter
        (var-get authorized-arbiters)
        { arbiters: (list), dispute-id: dispute-id }
    )))
)

;; Helper function for filtering arbiters that takes a single parameter
(define-private (not-equal-to (current principal)) 
    (not (is-eq tx-sender current))
)

;; Add an authorized arbiter
(define-public (add-authorized-arbiter (arbiter principal))
    (let
        ((current-arbiters (var-get authorized-arbiters)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (asserts! (< (len current-arbiters) u100) ERR-NOT-AUTHORIZED)
        (var-set authorized-arbiters (unwrap! (as-max-len? (append current-arbiters arbiter) u100) ERR-NOT-AUTHORIZED))
        (ok true)
    )
)

;; Remove an authorized arbiter
(define-public (remove-authorized-arbiter (arbiter principal))
    (let
        ((current-arbiters (var-get authorized-arbiters)))
        (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-NOT-AUTHORIZED)
        (var-set authorized-arbiters 
            (filter not-equal-to current-arbiters))
        (ok true)
    )
)

;; Mint initial tokens to contract deployer
(define-private (mint-initial-supply)
    (ft-mint? arbitration-token u1000000000 tx-sender)
)

;; Initialize contract
(begin
    (try! (mint-initial-supply))
)