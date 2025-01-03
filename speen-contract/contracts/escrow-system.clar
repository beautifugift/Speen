;; Freelance Escrow Smart Contract

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-MILESTONE (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-CONTRACT-COMPLETED (err u103))
(define-constant ERR-CONTRACT-CANCELLED (err u104))

;; Contract data structures
(define-map milestones
    {contract-id: uint, milestone-id: uint}
    {
        description: (string-utf8 100),
        amount: uint,
        completed: bool
    }
)

(define-map contracts
    {contract-id: uint}
    {
        client: principal,
        freelancer: principal,
        total-amount: uint,
        total-milestones: uint,
        completed-milestones: uint,
        is-cancelled: bool
    }
)

;; Storage variables
(define-data-var next-contract-id uint u0)

;; Helper function to add individual milestones
(define-private (add-milestone 
    (contract-id uint)
    (milestone-id uint)
    (description (string-utf8 100))
    (amount uint)
)
    (map-set milestones 
        {contract-id: contract-id, milestone-id: milestone-id}
        {
            description: description,
            amount: amount,
            completed: false
        }
    )
)

;; Create a new contract
(define-public (create-contract 
    (freelancer principal)
    (total-amount uint)
    (milestone-descriptions (list 10 (string-utf8 100)))
    (milestone-amounts (list 10 uint))
)
    (let 
        (
            (contract-id (+ (var-get next-contract-id) u1))
            (sender tx-sender)
            (total-milestones (len milestone-descriptions))
        )
        ;; Validate input
        (asserts! (is-eq (len milestone-descriptions) (len milestone-amounts)) ERR-INVALID-MILESTONE)
        (asserts! (> total-amount u0) ERR-INSUFFICIENT-BALANCE)
        (asserts! (is-eq total-amount (fold + milestone-amounts u0)) ERR-INVALID-MILESTONE)

        ;; Transfer total contract amount to contract
        (try! (stx-transfer? total-amount sender (as-contract tx-sender)))

        ;; Create contract entry
        (map-set contracts 
            {contract-id: contract-id}
            {
                client: sender,
                freelancer: freelancer,
                total-amount: total-amount,
                total-milestones: total-milestones,
                completed-milestones: u0,
                is-cancelled: false
            }
        )

        ;; Create milestones
        (map add-milestone 
            (list contract-id contract-id contract-id contract-id contract-id 
                  contract-id contract-id contract-id contract-id contract-id)
            (list u0 u1 u2 u3 u4 u5 u6 u7 u8 u9)
            milestone-descriptions
            milestone-amounts
        )

        ;; Update next contract ID
        (var-set next-contract-id contract-id)

        (ok contract-id)
    )
)

;; Complete a milestone
(define-public (complete-milestone 
    (contract-id uint)
    (milestone-id uint)
)
    (let 
        (
            (contract (unwrap! (map-get? contracts {contract-id: contract-id}) ERR-NOT-AUTHORIZED))
            (milestone (unwrap! (map-get? milestones {contract-id: contract-id, milestone-id: milestone-id}) ERR-INVALID-MILESTONE))
        )
        ;; Validate that sender is the freelancer
        (asserts! (is-eq tx-sender (get freelancer contract)) ERR-NOT-AUTHORIZED)
        
        ;; Validate contract is not cancelled
        (asserts! (not (get is-cancelled contract)) ERR-CONTRACT-CANCELLED)

        ;; Mark milestone as completed
        (map-set milestones 
            {contract-id: contract-id, milestone-id: milestone-id}
            (merge milestone {completed: true})
        )

        ;; Update contract completed milestones
        (map-set contracts 
            {contract-id: contract-id}
            (merge contract {completed-milestones: (+ (get completed-milestones contract) u1)})
        )

        ;; Release milestone payment
        (as-contract 
            (stx-transfer? 
                (get amount milestone) 
                (as-contract tx-sender) 
                (get freelancer contract)
            )
        )
    )
)

;; Cancel contract
(define-public (cancel-contract (contract-id uint))
    (let 
        (
            (contract (unwrap! (map-get? contracts {contract-id: contract-id}) ERR-NOT-AUTHORIZED))
        )
        ;; Validate that sender is the client
        (asserts! (is-eq tx-sender (get client contract)) ERR-NOT-AUTHORIZED)

        ;; Mark contract as cancelled
        (map-set contracts 
            {contract-id: contract-id}
            (merge contract {is-cancelled: true})
        )

        ;; Refund remaining funds to client
        (as-contract 
            (stx-transfer? 
                (- (get total-amount contract) 
                   (* 
                     (get completed-milestones contract) 
                     (/ (get total-amount contract) (get total-milestones contract))
                   )
                )
                (as-contract tx-sender)
                (get client contract)
            )
        )
    )
)

;; View contract details
(define-read-only (get-contract-details (contract-id uint))
    (map-get? contracts {contract-id: contract-id})
)

;; View milestone details
(define-read-only (get-milestone-details (contract-id uint) (milestone-id uint))
    (map-get? milestones {contract-id: contract-id, milestone-id: milestone-id})
)