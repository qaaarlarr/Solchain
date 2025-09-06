;; Solar Project Performance Tracking & Verification System
;; Tracks real-world solar project performance data with oracle verification

;; Error constants
(define-constant err-not-authorized (err u130))
(define-constant err-project-not-found (err u131))
(define-constant err-oracle-not-authorized (err u132))
(define-constant err-invalid-data (err u134))
(define-constant err-report-too-early (err u136))
(define-constant err-no-performance-data (err u138))

;; Data variables
(define-data-var performance-report-count uint u0)
(define-data-var min-reporting-interval uint u144)

;; Performance metrics tracking
(define-map project-performance-data uint
  {
    projected-monthly-output: uint,
    actual-monthly-output: uint,
    performance-ratio: uint,
    total-reports: uint,
    last-report-block: uint,
    verified-by: (optional principal)
  })

;; Performance reports from oracles
(define-map performance-reports (tuple (project-id uint) (report-id uint))
  {
    oracle: principal,
    energy-output: uint,
    reporting-period-start: uint,
    reporting-period-end: uint,
    weather-factor: uint,
    equipment-status: (string-ascii 50),
    reported-at: uint,
    verified: bool,
    verification-score: uint
  })

;; Authorized oracle management
(define-map authorized-oracles principal
  {
    authorized: bool,
    verification-score: uint,
    total-reports: uint,
    accurate-reports: uint
  })

;; Project milestone tracking
(define-map project-milestones uint
  {
    installation-complete: bool,
    first-energy-production: (optional uint),
    milestone-rewards-paid: uint
  })

;; Authorize performance oracle
(define-public (authorize-performance-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender tx-sender) err-not-authorized)
    (map-set authorized-oracles oracle
      {
        authorized: true,
        verification-score: u100,
        total-reports: u0,
        accurate-reports: u0
      })
    (ok true)))

;; Report performance data
(define-public (report-performance
  (project-id uint)
  (energy-output uint)
  (reporting-period-start uint)
  (reporting-period-end uint)
  (weather-factor uint)
  (equipment-status (string-ascii 50)))
  (let ((oracle-data (unwrap! (map-get? authorized-oracles tx-sender) err-oracle-not-authorized))
        (report-id (+ (var-get performance-report-count) u1)))
    (asserts! (get authorized oracle-data) err-oracle-not-authorized)
    (asserts! (and (>= weather-factor u1) (<= weather-factor u100)) err-invalid-data)
    (asserts! (> reporting-period-end reporting-period-start) err-invalid-data)
    (asserts! (> energy-output u0) err-invalid-data)
    
    ;; Create performance report
    (map-set performance-reports (tuple (project-id project-id) (report-id report-id))
      {
        oracle: tx-sender,
        energy-output: energy-output,
        reporting-period-start: reporting-period-start,
        reporting-period-end: reporting-period-end,
        weather-factor: weather-factor,
        equipment-status: equipment-status,
        reported-at: stacks-block-height,
        verified: false,
        verification-score: u0
      })
    
    ;; Update project performance data
    (update-project-performance project-id energy-output)
    
    ;; Update oracle statistics
    (map-set authorized-oracles tx-sender
      (merge oracle-data 
        {
          total-reports: (+ (get total-reports oracle-data) u1)
        }))
    
    (var-set performance-report-count report-id)
    (ok report-id)))

;; Verify performance report
(define-public (verify-performance-report (project-id uint) (report-id uint) (verification-score uint))
  (let ((report-key (tuple (project-id project-id) (report-id report-id)))
        (report-data (unwrap! (map-get? performance-reports report-key) err-project-not-found))
        (oracle-data (unwrap! (map-get? authorized-oracles tx-sender) err-oracle-not-authorized)))
    (asserts! (get authorized oracle-data) err-oracle-not-authorized)
    (asserts! (not (is-eq tx-sender (get oracle report-data))) err-not-authorized)
    (asserts! (and (>= verification-score u1) (<= verification-score u100)) err-invalid-data)
    
    ;; Update report verification
    (map-set performance-reports report-key
      (merge report-data 
        {
          verified: true,
          verification-score: verification-score
        }))
    
    (ok true)))

;; Update project performance data
(define-private (update-project-performance (project-id uint) (energy-output uint))
  (let ((performance-data (default-to
          {
            projected-monthly-output: u1000,
            actual-monthly-output: u0,
            performance-ratio: u0,
            total-reports: u0,
            last-report-block: u0,
            verified-by: none
          }
          (map-get? project-performance-data project-id))))
    (let ((new-total-output (+ (get actual-monthly-output performance-data) energy-output))
          (new-report-count (+ (get total-reports performance-data) u1))
          (new-average (/ new-total-output new-report-count))
          (new-performance-ratio (if (> (get projected-monthly-output performance-data) u0)
            (/ (* new-average u100) (get projected-monthly-output performance-data))
            u0)))
      (map-set project-performance-data project-id
        (merge performance-data
          {
            actual-monthly-output: new-total-output,
            performance-ratio: new-performance-ratio,
            total-reports: new-report-count,
            last-report-block: stacks-block-height
          }))
      true)))

;; Calculate performance-based reward multiplier
(define-public (calculate-performance-multiplier (project-id uint))
  (let ((performance-data (unwrap! (map-get? project-performance-data project-id) err-no-performance-data)))
    (let ((performance-ratio (get performance-ratio performance-data)))
      (ok (if (>= performance-ratio u120) 
            u150
            (if (>= performance-ratio u110)
              u125
              (if (>= performance-ratio u90)
                u100
                (if (>= performance-ratio u70)
                  u75
                  u50))))))))

;; Update milestone achievements
(define-public (update-project-milestone (project-id uint) (milestone-type (string-ascii 20)))
  (let ((milestones (default-to
          {
            installation-complete: false,
            first-energy-production: none,
            milestone-rewards-paid: u0
          }
          (map-get? project-milestones project-id))))
    (asserts! (get authorized (default-to {authorized: false} (map-get? authorized-oracles tx-sender))) err-not-authorized)
    
    (if (is-eq milestone-type "installation")
      (map-set project-milestones project-id
        (merge milestones {installation-complete: true}))
      (if (is-eq milestone-type "first-production")
        (map-set project-milestones project-id
          (merge milestones {first-energy-production: (some stacks-block-height)}))
        true))
    
    (ok true)))

;; Read-only functions
(define-read-only (get-project-performance (project-id uint))
  (map-get? project-performance-data project-id))

(define-read-only (get-performance-report (project-id uint) (report-id uint))
  (map-get? performance-reports (tuple (project-id project-id) (report-id report-id))))

(define-read-only (get-oracle-info (oracle principal))
  (map-get? authorized-oracles oracle))

(define-read-only (get-project-milestones (project-id uint))
  (map-get? project-milestones project-id))

(define-read-only (get-oracle-reliability (oracle principal))
  (match (get-oracle-info oracle)
    oracle-data
    (if (> (get total-reports oracle-data) u0)
      (ok (/ (* (get accurate-reports oracle-data) u100) (get total-reports oracle-data)))
      (ok u0))
    (ok u0)))

(define-read-only (get-project-performance-summary (project-id uint))
  (let ((performance-data (get-project-performance project-id))
        (milestones (get-project-milestones project-id)))
    (ok {
      performance-data: performance-data,
      milestones: milestones,
      reports-count: (match performance-data
        data (get total-reports data)
        u0)
    })))

(define-read-only (is-project-performing-well (project-id uint))
  (match (get-project-performance project-id)
    performance-data
    (>= (get performance-ratio performance-data) u90)
    false))

(define-read-only (get-performance-verification-status (project-id uint) (report-id uint))
  (match (get-performance-report project-id report-id)
    report-data
    (ok {
      verified: (get verified report-data),
      verification-score: (get verification-score report-data),
      oracle: (get oracle report-data),
      energy-output: (get energy-output report-data)
    })
    err-project-not-found))

;; Revoke oracle authorization
(define-public (revoke-oracle (oracle principal))
  (begin
    (asserts! (is-eq tx-sender tx-sender) err-not-authorized)
    (match (get-oracle-info oracle)
      oracle-data
      (map-set authorized-oracles oracle (merge oracle-data {authorized: false}))
      true)
    (ok true)))
