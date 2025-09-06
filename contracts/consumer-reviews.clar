;; Consumer Reviews Contract
;; Enables end consumers to review and rate fair trade products after purchase

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-BATCH-NOT-FOUND (err u402))
(define-constant ERR-ALREADY-REVIEWED (err u403))
(define-constant ERR-INVALID-RATING (err u404))
(define-constant ERR-REVIEW-NOT-FOUND (err u405))
(define-constant ERR-OWNER-ONLY (err u406))
(define-constant ERR-INVALID-PURCHASE (err u407))

(define-constant contract-owner tx-sender)

;; Data variables
(define-data-var next-review-id uint u1)
(define-data-var review-period-blocks uint u14400) ;; ~100 days for review window
(define-data-var min-rating uint u1)
(define-data-var max-rating uint u10)

;; Consumer purchase verification
(define-map consumer-purchases
  { consumer: principal, batch-id: uint }
  {
    purchase-date: uint,
    purchase-verified: bool,
    purchase-amount: uint,
    retailer: principal
  }
)

;; Product reviews data
(define-map product-reviews
  { review-id: uint }
  {
    batch-id: uint,
    consumer: principal,
    quality-rating: uint,
    ethics-rating: uint,
    sustainability-rating: uint,
    overall-rating: uint,
    review-text: (string-ascii 200),
    verified-purchase: bool,
    helpful-votes: uint,
    review-date: uint
  }
)

;; Review aggregations per batch
(define-map batch-review-summary
  uint
  {
    total-reviews: uint,
    avg-quality: uint,
    avg-ethics: uint,
    avg-sustainability: uint,
    avg-overall: uint,
    last-review-date: uint
  }
)

;; Review helpfulness votes
(define-map review-votes
  { review-id: uint, voter: principal }
  {
    helpful: bool,
    vote-date: uint
  }
)

;; Consumer reputation tracking
(define-map consumer-reputation
  principal
  {
    total-reviews: uint,
    helpful-review-count: uint,
    reputation-score: uint,
    account-created: uint
  }
)

;; Track consumer reviews per batch (for duplicate prevention)
(define-map consumer-batch-reviews
  { consumer: principal, batch-id: uint }
  {
    review-id: uint,
    submitted: bool
  }
)

;; Read-only functions

(define-read-only (get-review (review-id uint))
  (map-get? product-reviews { review-id: review-id })
)

(define-read-only (get-batch-reviews-summary (batch-id uint))
  (map-get? batch-review-summary batch-id)
)

(define-read-only (get-consumer-reputation (consumer principal))
  (map-get? consumer-reputation consumer)
)

(define-read-only (has-consumer-purchased (consumer principal) (batch-id uint))
  (is-some (map-get? consumer-purchases { consumer: consumer, batch-id: batch-id }))
)

(define-read-only (get-purchase-details (consumer principal) (batch-id uint))
  (map-get? consumer-purchases { consumer: consumer, batch-id: batch-id })
)

(define-read-only (get-review-config)
  {
    review-period-blocks: (var-get review-period-blocks),
    min-rating: (var-get min-rating),
    max-rating: (var-get max-rating),
    next-review-id: (var-get next-review-id)
  }
)

(define-read-only (has-consumer-reviewed (consumer principal) (batch-id uint))
  (is-some (map-get? consumer-batch-reviews { consumer: consumer, batch-id: batch-id }))
)

;; Public functions

(define-public (verify-purchase 
  (consumer principal) 
  (batch-id uint) 
  (retailer principal) 
  (amount uint)
)
  (begin
    ;; Only contract owner can verify purchases
    (asserts! (is-eq tx-sender contract-owner) ERR-NOT-AUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-PURCHASE)
    
    (map-set consumer-purchases
      { consumer: consumer, batch-id: batch-id }
      {
        purchase-date: stacks-block-height,
        purchase-verified: true,
        purchase-amount: amount,
        retailer: retailer
      }
    )
    
    (ok true)
  )
)

(define-public (submit-review 
  (batch-id uint)
  (quality-rating uint)
  (ethics-rating uint)
  (sustainability-rating uint)
  (review-text (string-ascii 200))
)
  (let (
    (review-id (var-get next-review-id))
    (purchase-info (unwrap! (map-get? consumer-purchases { consumer: tx-sender, batch-id: batch-id }) ERR-INVALID-PURCHASE))
    (purchase-date (get purchase-date purchase-info))
    (review-deadline (+ purchase-date (var-get review-period-blocks)))
    (overall-rating (/ (+ (+ quality-rating ethics-rating) sustainability-rating) u3))
  )
    ;; Validate review is within allowed timeframe
    (asserts! (<= stacks-block-height review-deadline) ERR-NOT-AUTHORIZED)
    
    ;; Validate ratings are within range
    (asserts! (and (>= quality-rating (var-get min-rating)) (<= quality-rating (var-get max-rating))) ERR-INVALID-RATING)
    (asserts! (and (>= ethics-rating (var-get min-rating)) (<= ethics-rating (var-get max-rating))) ERR-INVALID-RATING)
    (asserts! (and (>= sustainability-rating (var-get min-rating)) (<= sustainability-rating (var-get max-rating))) ERR-INVALID-RATING)
    
    ;; Check if consumer already reviewed this batch
    (asserts! (not (has-consumer-reviewed tx-sender batch-id)) ERR-ALREADY-REVIEWED)
    
    ;; Create the review
    (map-set product-reviews
      { review-id: review-id }
      {
        batch-id: batch-id,
        consumer: tx-sender,
        quality-rating: quality-rating,
        ethics-rating: ethics-rating,
        sustainability-rating: sustainability-rating,
        overall-rating: overall-rating,
        review-text: review-text,
        verified-purchase: true,
        helpful-votes: u0,
        review-date: stacks-block-height
      }
    )
    
    ;; Mark that consumer has reviewed this batch
    (map-set consumer-batch-reviews
      { consumer: tx-sender, batch-id: batch-id }
      {
        review-id: review-id,
        submitted: true
      }
    )
    
    ;; Update batch review summary
    (try! (update-batch-review-summary batch-id quality-rating ethics-rating sustainability-rating overall-rating))
    
    ;; Update consumer reputation
    (try! (update-consumer-reputation tx-sender))
    
    (var-set next-review-id (+ review-id u1))
    (ok review-id)
  )
)

(define-public (vote-review-helpful (review-id uint) (helpful bool))
  (let (
    (review-info (unwrap! (map-get? product-reviews { review-id: review-id }) ERR-REVIEW-NOT-FOUND))
    (existing-vote (map-get? review-votes { review-id: review-id, voter: tx-sender }))
  )
    ;; Ensure user hasn't already voted on this review
    (asserts! (is-none existing-vote) ERR-ALREADY-REVIEWED)
    
    ;; Record the vote
    (map-set review-votes
      { review-id: review-id, voter: tx-sender }
      {
        helpful: helpful,
        vote-date: stacks-block-height
      }
    )
    
    ;; Update helpful vote count if positive
    (if helpful
      (map-set product-reviews
        { review-id: review-id }
        (merge review-info { helpful-votes: (+ (get helpful-votes review-info) u1) })
      )
      true
    )
    
    (ok true)
  )
)

;; Private helper functions

(define-private (update-batch-review-summary 
  (batch-id uint) 
  (quality uint) 
  (ethics uint) 
  (sustainability uint) 
  (overall uint)
)
  (let (
    (current-summary (default-to 
      { total-reviews: u0, avg-quality: u0, avg-ethics: u0, avg-sustainability: u0, avg-overall: u0, last-review-date: u0 }
      (map-get? batch-review-summary batch-id)
    ))
    (new-total (+ (get total-reviews current-summary) u1))
    (prev-total (get total-reviews current-summary))
    (new-avg-quality (if (is-eq prev-total u0) quality (/ (+ (* (get avg-quality current-summary) prev-total) quality) new-total)))
    (new-avg-ethics (if (is-eq prev-total u0) ethics (/ (+ (* (get avg-ethics current-summary) prev-total) ethics) new-total)))
    (new-avg-sustainability (if (is-eq prev-total u0) sustainability (/ (+ (* (get avg-sustainability current-summary) prev-total) sustainability) new-total)))
    (new-avg-overall (if (is-eq prev-total u0) overall (/ (+ (* (get avg-overall current-summary) prev-total) overall) new-total)))
  )
    (map-set batch-review-summary batch-id
      {
        total-reviews: new-total,
        avg-quality: new-avg-quality,
        avg-ethics: new-avg-ethics,
        avg-sustainability: new-avg-sustainability,
        avg-overall: new-avg-overall,
        last-review-date: stacks-block-height
      }
    )
    (ok true)
  )
)

(define-private (update-consumer-reputation (consumer principal))
  (let (
    (current-rep (default-to 
      { total-reviews: u0, helpful-review-count: u0, reputation-score: u0, account-created: stacks-block-height }
      (map-get? consumer-reputation consumer)
    ))
    (new-total (+ (get total-reviews current-rep) u1))
    (new-score (if (> new-total u0) (/ (* (get helpful-review-count current-rep) u100) new-total) u0))
  )
    (map-set consumer-reputation consumer
      (merge current-rep 
        { 
          total-reviews: new-total,
          reputation-score: new-score
        }
      )
    )
    (ok true)
  )
)

;; Administrative functions

(define-public (configure-review-settings (period-blocks uint) (min-rate uint) (max-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) ERR-OWNER-ONLY)
    (asserts! (> period-blocks u0) ERR-INVALID-RATING)
    (asserts! (and (> min-rate u0) (< min-rate max-rate)) ERR-INVALID-RATING)
    
    (var-set review-period-blocks period-blocks)
    (var-set min-rating min-rate)
    (var-set max-rating max-rate)
    (ok true)
  )
)
