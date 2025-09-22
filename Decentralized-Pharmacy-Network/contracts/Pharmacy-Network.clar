;; Decentralized Pharmacy Network Smart Contract
;; P2P medication sharing and prescription verification system

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-prescription (err u103))
(define-constant err-medication-not-available (err u104))
(define-constant err-insufficient-quantity (err u105))
(define-constant err-prescription-expired (err u106))
(define-constant err-already-exists (err u107))
(define-constant err-invalid-parameters (err u108))

;; Data Variables
(define-data-var contract-active bool true)
(define-data-var next-prescription-id uint u1)
(define-data-var next-medication-id uint u1)
(define-data-var next-pharmacy-id uint u1)

;; Data Maps
;; Licensed pharmacies in the network
(define-map pharmacies
  { pharmacy-id: uint }
  {
    owner: principal,
    name: (string-ascii 100),
    license-number: (string-ascii 50),
    location: (string-ascii 200),
    active: bool,
    verification-score: uint,
    registered-at: uint
  }
)

;; Verified prescriptions
(define-map prescriptions
  { prescription-id: uint }
  {
    patient: principal,
    doctor: principal,
    medication-name: (string-ascii 100),
    dosage: (string-ascii 50),
    quantity: uint,
    issued-date: uint,
    expiry-date: uint,
    verification-hash: (buff 32),
    fulfilled: bool,
    fulfilling-pharmacy: (optional uint)
  }
)

;; Available medications in network
(define-map medications
  { medication-id: uint }
  {
    pharmacy-id: uint,
    name: (string-ascii 100),
    manufacturer: (string-ascii 100),
    batch-number: (string-ascii 50),
    quantity-available: uint,
    price-per-unit: uint,
    expiry-date: uint,
    requires-prescription: bool,
    active: bool
  }
)

;; P2P sharing requests
(define-map sharing-requests
  { request-id: uint }
  {
    requester: principal,
    medication-name: (string-ascii 100),
    quantity-needed: uint,
    max-price: uint,
    prescription-id: (optional uint),
    location-preference: (string-ascii 200),
    created-at: uint,
    fulfilled: bool,
    fulfiller: (optional principal)
  }
)

;; Doctor verification registry
(define-map verified-doctors
  { doctor: principal }
  {
    name: (string-ascii 100),
    license-number: (string-ascii 50),
    specialization: (string-ascii 100),
    verified: bool,
    verified-by: principal,
    verification-date: uint
  }
)

;; Tracking variables
(define-data-var next-request-id uint u1)

;; Read-only functions

;; Get pharmacy details
(define-read-only (get-pharmacy (pharmacy-id uint))
  (map-get? pharmacies { pharmacy-id: pharmacy-id })
)

;; Get prescription details
(define-read-only (get-prescription (prescription-id uint))
  (map-get? prescriptions { prescription-id: prescription-id })
)

;; Get medication details
(define-read-only (get-medication (medication-id uint))
  (map-get? medications { medication-id: medication-id })
)

;; Get sharing request details
(define-read-only (get-sharing-request (request-id uint))
  (map-get? sharing-requests { request-id: request-id })
)

;; Check if doctor is verified
(define-read-only (is-doctor-verified (doctor principal))
  (match (map-get? verified-doctors { doctor: doctor })
    doctor-data (get verified doctor-data)
    false
  )
)

;; Get contract status
(define-read-only (get-contract-status)
  (var-get contract-active)
)

;; Public functions

;; Register a new pharmacy
(define-public (register-pharmacy (name (string-ascii 100)) 
                                  (license-number (string-ascii 50))
                                  (location (string-ascii 200)))
  (let ((pharmacy-id (var-get next-pharmacy-id)))
    (asserts! (var-get contract-active) err-unauthorized)
    (asserts! (> (len name) u0) err-invalid-parameters)
    (asserts! (> (len license-number) u0) err-invalid-parameters)
    
    (map-set pharmacies
      { pharmacy-id: pharmacy-id }
      {
        owner: tx-sender,
        name: name,
        license-number: license-number,
        location: location,
        active: true,
        verification-score: u0,
        registered-at: block-height
      }
    )
    
    (var-set next-pharmacy-id (+ pharmacy-id u1))
    (ok pharmacy-id)
  )
)

;; Add medication to pharmacy inventory
(define-public (add-medication (pharmacy-id uint)
                               (name (string-ascii 100))
                               (manufacturer (string-ascii 100))
                               (batch-number (string-ascii 50))
                               (quantity uint)
                               (price-per-unit uint)
                               (expiry-date uint)
                               (requires-prescription bool))
  (let ((medication-id (var-get next-medication-id))
        (pharmacy (unwrap! (map-get? pharmacies { pharmacy-id: pharmacy-id }) err-not-found)))
    
    (asserts! (var-get contract-active) err-unauthorized)
    (asserts! (is-eq tx-sender (get owner pharmacy)) err-unauthorized)
    (asserts! (get active pharmacy) err-unauthorized)
    (asserts! (> quantity u0) err-invalid-parameters)
    (asserts! (> expiry-date block-height) err-invalid-parameters)
    
    (map-set medications
      { medication-id: medication-id }
      {
        pharmacy-id: pharmacy-id,
        name: name,
        manufacturer: manufacturer,
        batch-number: batch-number,
        quantity-available: quantity,
        price-per-unit: price-per-unit,
        expiry-date: expiry-date,
        requires-prescription: requires-prescription,
        active: true
      }
    )
    
    (var-set next-medication-id (+ medication-id u1))
    (ok medication-id)
  )
)

;; Issue a prescription (doctors only)
(define-public (issue-prescription (patient principal)
                                   (medication-name (string-ascii 100))
                                   (dosage (string-ascii 50))
                                   (quantity uint)
                                   (expiry-date uint)
                                   (verification-hash (buff 32)))
  (let ((prescription-id (var-get next-prescription-id)))
    
    (asserts! (var-get contract-active) err-unauthorized)
    (asserts! (is-doctor-verified tx-sender) err-unauthorized)
    (asserts! (> quantity u0) err-invalid-parameters)
    (asserts! (> expiry-date block-height) err-invalid-parameters)
    
    (map-set prescriptions
      { prescription-id: prescription-id }
      {
        patient: patient,
        doctor: tx-sender,
        medication-name: medication-name,
        dosage: dosage,
        quantity: quantity,
        issued-date: block-height,
        expiry-date: expiry-date,
        verification-hash: verification-hash,
        fulfilled: false,
        fulfilling-pharmacy: none
      }
    )
    
    (var-set next-prescription-id (+ prescription-id u1))
    (ok prescription-id)
  )
)

;; Create P2P sharing request
(define-public (create-sharing-request (medication-name (string-ascii 100))
                                       (quantity-needed uint)
                                       (max-price uint)
                                       (prescription-id (optional uint))
                                       (location-preference (string-ascii 200)))
  (let ((request-id (var-get next-request-id)))
    
    (asserts! (var-get contract-active) err-unauthorized)
    (asserts! (> quantity-needed u0) err-invalid-parameters)
    
    ;; If prescription ID provided, verify it belongs to requester
    (match prescription-id
      presc-id (let ((prescription (unwrap! (map-get? prescriptions { prescription-id: presc-id }) err-not-found)))
                 (asserts! (is-eq tx-sender (get patient prescription)) err-unauthorized)
                 (asserts! (not (get fulfilled prescription)) err-invalid-prescription)
                 (asserts! (> (get expiry-date prescription) block-height) err-prescription-expired)
                 true)
      true)
    
    (map-set sharing-requests
      { request-id: request-id }
      {
        requester: tx-sender,
        medication-name: medication-name,
        quantity-needed: quantity-needed,
        max-price: max-price,
        prescription-id: prescription-id,
        location-preference: location-preference,
        created-at: block-height,
        fulfilled: false,
        fulfiller: none
      }
    )
    
    (var-set next-request-id (+ request-id u1))
    (ok request-id)
  )
)

;; Fulfill prescription at pharmacy
(define-public (fulfill-prescription (prescription-id uint) (pharmacy-id uint))
  (let ((prescription (unwrap! (map-get? prescriptions { prescription-id: prescription-id }) err-not-found))
        (pharmacy (unwrap! (map-get? pharmacies { pharmacy-id: pharmacy-id }) err-not-found)))
    
    (asserts! (var-get contract-active) err-unauthorized)
    (asserts! (is-eq tx-sender (get owner pharmacy)) err-unauthorized)
    (asserts! (get active pharmacy) err-unauthorized)
    (asserts! (not (get fulfilled prescription)) err-invalid-prescription)
    (asserts! (> (get expiry-date prescription) block-height) err-prescription-expired)
    
    (map-set prescriptions
      { prescription-id: prescription-id }
      (merge prescription {
        fulfilled: true,
        fulfilling-pharmacy: (some pharmacy-id)
      })
    )
    
    (ok true)
  )
)

;; Verify doctor (contract owner only)
(define-public (verify-doctor (doctor principal)
                              (name (string-ascii 100))
                              (license-number (string-ascii 50))
                              (specialization (string-ascii 100)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (var-get contract-active) err-unauthorized)
    
    (map-set verified-doctors
      { doctor: doctor }
      {
        name: name,
        license-number: license-number,
        specialization: specialization,
        verified: true,
        verified-by: tx-sender,
        verification-date: block-height
      }
    )
    
    (ok true)
  )
)

;; Update medication quantity
(define-public (update-medication-quantity (medication-id uint) (new-quantity uint))
  (let ((medication (unwrap! (map-get? medications { medication-id: medication-id }) err-not-found))
        (pharmacy (unwrap! (map-get? pharmacies { pharmacy-id: (get pharmacy-id medication) }) err-not-found)))
    
    (asserts! (var-get contract-active) err-unauthorized)
    (asserts! (is-eq tx-sender (get owner pharmacy)) err-unauthorized)
    (asserts! (get active medication) err-medication-not-available)
    
    (map-set medications
      { medication-id: medication-id }
      (merge medication { quantity-available: new-quantity })
    )
    
    (ok true)
  )
)

;; Emergency contract pause (owner only)
(define-public (toggle-contract-status)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set contract-active (not (var-get contract-active)))
    (ok (var-get contract-active))
  )
)