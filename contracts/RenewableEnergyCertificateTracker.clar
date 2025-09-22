;; RenewableEnergyTracker - Green energy certificate and carbon credit system

(define-map energy-certificates uint {
  producer: principal,
  energy-source: (string-utf8 64),
  capacity-details: (string-utf8 256),
  generation-period: uint,
  installation-site: (string-utf8 64),
  carbon-verified: bool
})

(define-map producer-certificates principal (list 100 uint))
(define-map carbon-auditors principal bool)
(define-data-var certificate-sequence uint u0)

;; Error constants
(define-constant err-unauthorized-producer (err u400))
(define-constant err-unauthorized-auditor (err u401))
(define-constant err-certificate-missing (err u402))
(define-constant err-access-forbidden (err u403))
(define-constant err-certificate-limit-reached (err u404))
(define-constant err-invalid-address-format (err u405))
(define-constant err-empty-energy-source (err u406))
(define-constant err-empty-capacity-details (err u407))
(define-constant err-invalid-generation-period (err u408))
(define-constant err-empty-installation-site (err u409))
(define-constant err-invalid-certificate-id (err u410))

;; System administrator
(define-constant system-admin tx-sender)

;; Register carbon auditor
(define-public (register-carbon-auditor (auditor principal))
  (begin
    (asserts! (is-eq tx-sender system-admin) err-access-forbidden)
    (asserts! (not (is-eq auditor 'SP000000000000000000002Q6VF78)) err-invalid-address-format)
    (ok (map-set carbon-auditors auditor true))
  ))

;; Issue energy certificate
(define-public (issue-energy-certificate
  (energy-source (string-utf8 64))
  (capacity-details (string-utf8 256))
  (generation-period uint)
  (installation-site (string-utf8 64)))
  (let
    ((certificate-id (var-get certificate-sequence))
     (producer tx-sender)
     (current-certificates (default-to (list) (map-get? producer-certificates producer))))
    
    (asserts! (> (len energy-source) u0) err-empty-energy-source)
    (asserts! (> (len capacity-details) u0) err-empty-capacity-details)
    (asserts! (> generation-period u0) err-invalid-generation-period)
    (asserts! (> (len installation-site) u0) err-empty-installation-site)
    (asserts! (< (len current-certificates) u100) err-certificate-limit-reached)
    
    (map-set energy-certificates certificate-id {
      producer: producer,
      energy-source: energy-source,
      capacity-details: capacity-details,
      generation-period: generation-period,
      installation-site: installation-site,
      carbon-verified: false
    })
    
    (let
      ((updated-certificates (unwrap-panic (as-max-len? (concat (list certificate-id) current-certificates) u100))))
      (map-set producer-certificates producer updated-certificates)
    )
    
    (var-set certificate-sequence (+ certificate-id u1))
    (ok certificate-id)))

;; Verify carbon credits
(define-public (verify-carbon-credits (certificate-id uint))
  (begin
    (asserts! (< certificate-id (var-get certificate-sequence)) err-invalid-certificate-id)
    (let
      ((certificate (unwrap! (map-get? energy-certificates certificate-id) err-certificate-missing)))
      (asserts! (default-to false (map-get? carbon-auditors tx-sender)) err-unauthorized-auditor)
      (ok (map-set energy-certificates certificate-id (merge certificate {carbon-verified: true})))
    )
  ))

;; Get certificate information
(define-read-only (get-certificate-info (certificate-id uint))
  (map-get? energy-certificates certificate-id))

;; Get producer certificates
(define-read-only (get-producer-certificates (producer principal))
  (default-to (list) (map-get? producer-certificates producer)))

;; Check auditor status
(define-read-only (is-carbon-auditor (address principal))
  (default-to false (map-get? carbon-auditors address)))
