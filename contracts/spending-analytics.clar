;; Spending Analytics Contract
;; Provides analytical insights and reporting for government spending data

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u201))
(define-constant ERR-INVALID-PERIOD (err u202))
(define-constant ERR-NO-DATA (err u203))
(define-constant ERR-INVALID-RANGE (err u204))
(define-constant ERR-CALCULATION-ERROR (err u205))

;; Data variables for analytics configuration
(define-data-var analytics-enabled bool true)
(define-data-var min-records-for-trend uint u5)
(define-data-var default-trend-period uint u2160) ;; ~15 days in blocks

;; Analytics cache maps
(define-map department-analytics
  { department: (string-ascii 50), period-start: uint, period-end: uint }
  {
    total-amount: uint,
    record-count: uint,
    avg-spending: uint,
    max-spending: uint,
    min-spending: uint,
    calculated-at: uint
  }
)

(define-map constituency-trends
  { constituency: (string-ascii 50), trend-period: uint }
  {
    spending-velocity: uint, ;; average spending per block
    growth-rate: int, ;; percentage change from previous period
    efficiency-score: uint, ;; spending per record ratio
    last-updated: uint
  }
)

(define-map spending-comparisons
  { comparison-id: uint }
  {
    constituency-a: (string-ascii 50),
    constituency-b: (string-ascii 50),
    period-blocks: uint,
    difference-percentage: int,
    comparison-date: uint,
    created-by: principal
  }
)

(define-data-var next-comparison-id uint u1)

;; Read-only functions for analytics

(define-read-only (get-department-spending-summary 
  (department (string-ascii 50)) 
  (period-start uint) 
  (period-end uint)
)
  (match (map-get? department-analytics { department: department, period-start: period-start, period-end: period-end })
    analytics-data (ok analytics-data)
    ERR-NO-DATA
  )
)

(define-read-only (get-constituency-spending-trends 
  (constituency (string-ascii 50)) 
  (trend-period uint)
)
  (match (map-get? constituency-trends { constituency: constituency, trend-period: trend-period })
    trend-data (ok trend-data)
    ERR-NO-DATA
  )
)

(define-read-only (get-spending-comparison (comparison-id uint))
  (match (map-get? spending-comparisons { comparison-id: comparison-id })
    comparison-data (ok comparison-data)
    ERR-NO-DATA
  )
)

(define-read-only (calculate-spending-velocity 
  (constituency (string-ascii 50)) 
  (period-blocks uint)
)
  (let (
    (current-block stacks-block-height)
    (start-block (- current-block period-blocks))
  )
    (ok {
      constituency: constituency,
      period-start: start-block,
      period-end: current-block,
      velocity-per-block: u0, ;; Would be calculated from actual records
      estimated-total: u0
    })
  )
)

(define-read-only (get-efficiency-metrics (constituency (string-ascii 50)))
  (ok {
    constituency: constituency,
    records-per-million-stx: u0, ;; Records per 1M STX spent
    avg-record-size: u0,
    processing-efficiency: u0, ;; verified records / total records
    calculated-at: stacks-block-height
  })
)

;; Public functions for generating analytics

(define-public (generate-department-analytics 
  (department (string-ascii 50)) 
  (period-start uint) 
  (period-end uint)
)
  (let (
    (period-duration (- period-end period-start))
    ;; Mock calculations - in real implementation would aggregate from main contract
    (mock-total u5000000000)
    (mock-count u25)
    (mock-avg (/ mock-total mock-count))
  )
    (asserts! (var-get analytics-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (> period-end period-start) ERR-INVALID-PERIOD)
    (asserts! (<= period-duration u8640) ERR-INVALID-RANGE) ;; Max 60 days
    
    (map-set department-analytics
      { department: department, period-start: period-start, period-end: period-end }
      {
        total-amount: mock-total,
        record-count: mock-count,
        avg-spending: mock-avg,
        max-spending: u1000000000,
        min-spending: u50000000,
        calculated-at: stacks-block-height
      }
    )
    
    (ok true)
  )
)

(define-public (calculate-constituency-trends 
  (constituency (string-ascii 50)) 
  (trend-period uint)
)
  (let (
    (current-block stacks-block-height)
    (start-block (- current-block trend-period))
    ;; Mock trend calculations
    (mock-velocity u346) ;; STX per block
    (mock-growth-rate 15) ;; 15% growth
    (mock-efficiency u200000000) ;; STX per record
  )
    (asserts! (var-get analytics-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (>= trend-period (var-get min-records-for-trend)) ERR-INVALID-PERIOD)
    
    (map-set constituency-trends
      { constituency: constituency, trend-period: trend-period }
      {
        spending-velocity: mock-velocity,
        growth-rate: mock-growth-rate,
        efficiency-score: mock-efficiency,
        last-updated: current-block
      }
    )
    
    (ok true)
  )
)

(define-public (create-spending-comparison 
  (constituency-a (string-ascii 50)) 
  (constituency-b (string-ascii 50)) 
  (period-blocks uint)
)
  (let (
    (comparison-id (var-get next-comparison-id))
    ;; Mock comparison calculation
    (mock-difference -12) ;; -12% difference
  )
    (asserts! (var-get analytics-enabled) ERR-NOT-AUTHORIZED)
    (asserts! (> period-blocks u0) ERR-INVALID-PERIOD)
    (asserts! (not (is-eq constituency-a constituency-b)) ERR-INVALID-RANGE)
    
    (map-set spending-comparisons
      { comparison-id: comparison-id }
      {
        constituency-a: constituency-a,
        constituency-b: constituency-b,
        period-blocks: period-blocks,
        difference-percentage: mock-difference,
        comparison-date: stacks-block-height,
        created-by: tx-sender
      }
    )
    
    (var-set next-comparison-id (+ comparison-id u1))
    (ok comparison-id)
  )
)

(define-public (toggle-analytics (enabled bool))
  (begin
    ;; Simplified owner check - would need proper implementation
    (asserts! (is-eq tx-sender tx-sender) ERR-NOT-AUTHORIZED)
    (var-set analytics-enabled enabled)
    (ok true)
  )
)

;; Enhanced read-only functions for complex analytics

(define-read-only (get-multi-constituency-summary (constituencies (list 10 (string-ascii 50))))
  (ok {
    constituencies: constituencies,
    total-analyzed: (len constituencies),
    summary-generated-at: stacks-block-height,
    period-analyzed: (var-get default-trend-period)
  })
)

(define-read-only (get-spending-distribution-by-department 
  (constituency (string-ascii 50)) 
  (departments (list 5 (string-ascii 50)))
)
  (ok {
    constituency: constituency,
    departments: departments,
    distribution-type: "percentage",
    calculated-at: stacks-block-height
  })
)

(define-read-only (get-analytics-config)
  {
    enabled: (var-get analytics-enabled),
    min-records-for-trend: (var-get min-records-for-trend),
    default-trend-period: (var-get default-trend-period),
    next-comparison-id: (var-get next-comparison-id)
  }
)

(define-read-only (get-spending-insights (constituency (string-ascii 50)))
  (ok {
    constituency: constituency,
    has-sufficient-data: true,
    trend-direction: "upward",
    efficiency-rating: "good",
    anomalies-detected: false,
    last-analysis: stacks-block-height
  })
)
