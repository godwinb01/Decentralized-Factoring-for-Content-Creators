;; Decentralized Factoring for Content Creators
;; Smart contract enabling content creators to monetize future earnings immediately

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_INVOICE_NOT_FOUND (err u102))
(define-constant ERR_INVOICE_ALREADY_FACTORED (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_INVOICE_EXPIRED (err u105))
(define-constant ERR_PAYMENT_ALREADY_MADE (err u106))
(define-constant ERR_INVALID_DISCOUNT_RATE (err u107))
(define-constant MIN_INVOICE_AMOUNT u1000000) ;; 1 STX minimum
(define-constant MAX_DISCOUNT_RATE u20) ;; 20% max discount
(define-constant PLATFORM_FEE_RATE u2) ;; 2% platform fee

;; Data Variables
(define-data-var next-invoice-id uint u1)
(define-data-var total-factored-amount uint u0)
(define-data-var platform-treasury uint u0)

;; Invoice structure
(define-map invoices uint {
    creator: principal,
    client: principal,
    amount: uint,
    discount-rate: uint,
    factored-amount: uint,
    due-date: uint,
    created-at: uint,
    factored: bool,
    paid: bool,
    factor: (optional principal)
})

;; Creator profiles with reputation scoring
(define-map creator-profiles principal {
    total-invoices: uint,
    successful-payments: uint,
    total-earned: uint,
    reputation-score: uint,
    verified: bool
})

;; Factor liquidity pool
(define-map factor-balances principal uint)

;; Events
(define-map invoice-events uint {
    event-type: (string-ascii 20),
    timestamp: uint,
    details: (string-ascii 100)
})

;; Read-only functions
(define-read-only (get-invoice (invoice-id uint))
    (map-get? invoices invoice-id)
)

(define-read-only (get-creator-profile (creator principal))
    (default-to 
        {total-invoices: u0, successful-payments: u0, total-earned: u0, reputation-score: u0, verified: false}
        (map-get? creator-profiles creator)
    )
)

(define-read-only (get-factor-balance (factor principal))
    (default-to u0 (map-get? factor-balances factor))
)

(define-read-only (calculate-factored-amount (amount uint) (discount-rate uint))
    (let ((discount (/ (* amount discount-rate) u100)))
        (- amount discount)
    )
)

(define-read-only (calculate-reputation-score (total-invoices uint) (successful-payments uint))
    (if (is-eq total-invoices u0)
        u0
        (/ (* successful-payments u100) total-invoices)
    )
)

(define-read-only (get-platform-treasury)
    (var-get platform-treasury)
)

(define-read-only (get-total-factored-amount)
    (var-get total-factored-amount)
)

;; Private functions
(define-private (update-creator-reputation (creator principal) (successful bool))
    (let ((profile (get-creator-profile creator)))
        (map-set creator-profiles creator
            (merge profile {
                total-invoices: (+ (get total-invoices profile) u1),
                successful-payments: (if successful 
                    (+ (get successful-payments profile) u1)
                    (get successful-payments profile)
                ),
                reputation-score: (calculate-reputation-score 
                    (+ (get total-invoices profile) u1)
                    (if successful 
                        (+ (get successful-payments profile) u1)
                        (get successful-payments profile)
                    )
                )
            })
        )
    )
)

(define-private (charge-platform-fee (amount uint))
    (let ((fee (/ (* amount PLATFORM_FEE_RATE) u100)))
        (var-set platform-treasury (+ (var-get platform-treasury) fee))
        fee
    )
)

;; Public functions

;; Create invoice for future content earnings
(define-public (create-invoice (client principal) (amount uint) (discount-rate uint) (due-date uint))
    (let ((invoice-id (var-get next-invoice-id)))
        (asserts! (>= amount MIN_INVOICE_AMOUNT) ERR_INVALID_AMOUNT)
        (asserts! (<= discount-rate MAX_DISCOUNT_RATE) ERR_INVALID_DISCOUNT_RATE)
        (asserts! (> due-date block-height) ERR_INVOICE_EXPIRED)
        
        (map-set invoices invoice-id {
            creator: tx-sender,
            client: client,
            amount: amount,
            discount-rate: discount-rate,
            factored-amount: (calculate-factored-amount amount discount-rate),
            due-date: due-date,
            created-at: block-height,
            factored: false,
            paid: false,
            factor: none
        })
        
        (map-set invoice-events invoice-id {
            event-type: "CREATED",
            timestamp: block-height,
            details: "Invoice created by content creator"
        })
        
        (var-set next-invoice-id (+ invoice-id u1))
        (update-creator-reputation tx-sender false)
        (ok invoice-id)
    )
)

;; Factor an invoice (provide immediate liquidity)
(define-public (factor-invoice (invoice-id uint))
    (let ((invoice-opt (map-get? invoices invoice-id)))
        (match invoice-opt
            invoice (begin
                (asserts! (not (get factored invoice)) ERR_INVOICE_ALREADY_FACTORED)
                (asserts! (> (get due-date invoice) block-height) ERR_INVOICE_EXPIRED)
                (asserts! (>= (get-factor-balance tx-sender) (get factored-amount invoice)) ERR_INSUFFICIENT_FUNDS)
                
                (let ((factored-amount (get factored-amount invoice))
                      (platform-fee (charge-platform-fee factored-amount))
                      (net-amount (- factored-amount platform-fee)))
                    
                    ;; Transfer funds from factor to creator
                    (try! (stx-transfer? net-amount tx-sender (get creator invoice)))
                    
                    ;; Update factor balance
                    (map-set factor-balances tx-sender 
                        (- (get-factor-balance tx-sender) factored-amount))
                    
                    ;; Update invoice
                    (map-set invoices invoice-id 
                        (merge invoice {factored: true, factor: (some tx-sender)}))
                    
                    ;; Record event
                    (map-set invoice-events invoice-id {
                        event-type: "FACTORED",
                        timestamp: block-height,
                        details: "Invoice factored by liquidity provider"
                    })
                    
                    (var-set total-factored-amount 
                        (+ (var-get total-factored-amount) factored-amount))
                    
                    (ok true)
                )
            )
            ERR_INVOICE_NOT_FOUND
        )
    )
)

;; Client pays the original invoice amount
(define-public (pay-invoice (invoice-id uint))
    (let ((invoice-opt (map-get? invoices invoice-id)))
        (match invoice-opt
            invoice (begin
                (asserts! (is-eq tx-sender (get client invoice)) ERR_UNAUTHORIZED)
                (asserts! (not (get paid invoice)) ERR_PAYMENT_ALREADY_MADE)
                
                (let ((amount (get amount invoice))
                      (platform-fee (charge-platform-fee amount))
                      (net-amount (- amount platform-fee)))
                    
                    ;; Handle payment based on factoring status
                    (try! (if (get factored invoice)
                        ;; Pay to factor if invoice was factored
                        (match (get factor invoice)
                            factor-principal (stx-transfer? net-amount tx-sender factor-principal)
                            ERR_INVOICE_NOT_FOUND
                        )
                        ;; Pay directly to creator if not factored
                        (stx-transfer? net-amount tx-sender (get creator invoice))
                    ))
                    
                    ;; Update invoice as paid
                    (map-set invoices invoice-id (merge invoice {paid: true}))
                    
                    ;; Update creator reputation
                    (update-creator-reputation (get creator invoice) true)
                    
                    ;; Update creator earnings
                    (let ((creator-profile (get-creator-profile (get creator invoice))))
                        (map-set creator-profiles (get creator invoice)
                            (merge creator-profile {
                                total-earned: (+ (get total-earned creator-profile) net-amount)
                            })
                        )
                    )
                    
                    ;; Record event
                    (map-set invoice-events invoice-id {
                        event-type: "PAID",
                        timestamp: block-height,
                        details: "Invoice payment completed"
                    })
                    
                    (ok true)
                )
            )
            ERR_INVOICE_NOT_FOUND
        )
    )
)

;; Add liquidity to factor pool
(define-public (add-liquidity (amount uint))
    (begin
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set factor-balances tx-sender 
            (+ (get-factor-balance tx-sender) amount))
        (ok true)
    )
)

;; Remove liquidity from factor pool
(define-public (remove-liquidity (amount uint))
    (begin
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (>= (get-factor-balance tx-sender) amount) ERR_INSUFFICIENT_FUNDS)
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        (map-set factor-balances tx-sender 
            (- (get-factor-balance tx-sender) amount))
        (ok true)
    )
)

;; Verify creator (admin function)
(define-public (verify-creator (creator principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (let ((profile (get-creator-profile creator)))
            (map-set creator-profiles creator 
                (merge profile {verified: true}))
        )
        (ok true)
    )
)

;; Emergency functions (admin only)
(define-public (pause-contract)
    (begin 
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (ok true)
    )
)

(define-public (withdraw-platform-fees (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= amount (var-get platform-treasury)) ERR_INSUFFICIENT_FUNDS)
        (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
        (var-set platform-treasury (- (var-get platform-treasury) amount))
        (ok true)
    )
)