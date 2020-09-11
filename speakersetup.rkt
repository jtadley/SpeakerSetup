#lang racket

(define SPEAKER_HEIGHT 26)
(define SPEAKER_WIDTH 15)
(define SPEAKER_DEPTH 16)
(define TWEETER_POSN_TOP 7)
(define TWEETER_POSN_SIDE 8)
(define TWEETER_DISTANCE_WALL 30)

(define SPEAKER_DISTANCE_APART 75)

(define EAR_DISTANCE_WALL 135)
(define EAR_DISTANCE_GROUND 38)

(define STAND_HEIGHT 10)

;; -------------------------------------------------------------

(define INCR 0.01)

(define π pi)

;; -------------------------------------------------------------

(define αγω-from-β
  (λ (β)
    (let*
        ([ψ TWEETER_POSN_SIDE]
         [σ TWEETER_POSN_TOP]
         [μ TWEETER_DISTANCE_WALL]
         [φ SPEAKER_HEIGHT]
         [l SPEAKER_WIDTH]
         [ρ SPEAKER_DEPTH]
         [ε SPEAKER_DISTANCE_APART]
         [δ EAR_DISTANCE_WALL]
         [θ (atan (/ (/ ε 2) (- δ μ)))]
         [τ (/ (/ ε 2) (sin θ))]
         [α θ]
         [γ (- μ (- (+ (* ψ (sin θ)) (* ρ (sin (- (/ π 2) α)))) (* (- π σ) (sin β) (cos α))))]
         [ω (+ γ (* l (sin α)))])
      (list α γ ω))))

(define wall-angle-from-tiltback-degrees
  (λ (β)
    (let ([res (αγω-from-β (degrees->radians β))])
      (cons (radians->degrees (car res)) (cdr res))
      `((angle . ,(radians->degrees (car res)))
        (inner-distance . ,(cadr res))
        (outer-distance . ,(caddr res))))))

(wall-angle-from-tiltback-degrees 10)

;; -------------------------------------------------------------

(define zero-function
  (λ (f start end incr goal)
    (letrec
        ([helper
          (λ (cur best)
            (cond
              [(zero? cur) (helper (+ cur incr) best)]
              [(> cur end) best]
              [else
               (let
                   ([val (abs (- (f cur) goal))])
                 ;; (println val)
                 (helper (+ cur incr) (if (< val (cdr best)) (cons cur val) best)))]))])
      (car (helper start (cons +inf.0 +inf.0))))))

(define β-from-η
  (λ (η)
    (let*
        ([ψ TWEETER_POSN_SIDE]
         [σ TWEETER_POSN_TOP]
         [μ TWEETER_DISTANCE_WALL]
         [φ SPEAKER_HEIGHT]
         [ρ SPEAKER_DEPTH]
         [ε SPEAKER_DISTANCE_APART]
         [δ EAR_DISTANCE_WALL]
         [χ EAR_DISTANCE_GROUND]
         [θ (atan (/ (/ ε 2) (- δ μ)))]
         [τ (/ (/ ε 2) (sin θ))]
         [function (λ (β)
                     (let*
                         ([η^ (- η (* ρ (sin β)))]
                          [y (- ρ (* (- φ σ) (tan β)))]
                          [z (* y (cos β))]
                          [v (- z (* η^ (tan β)))]
                          [t (/ (+ φ (- σ) (/ η^ (cos β))) (sin β))]
                          [l (+ t v τ)])
                       (* l (tan β))))]
         [β (zero-function function (- (/ π 2)) (/ π 2) INCR χ)])
      (list β (- η (* ρ (sin β)))))))

(define tiltback-from-stand-height
  (λ (η)
    (let ([res (β-from-η η)])
      `((tilt-back-angle . ,(radians->degrees (car res)))
        (back-height . ,(cadr res))))))

(tiltback-from-stand-height 10)

;; -------------------------------------------------------------

(define η-from-β
  (λ (β)
    (let*
        ([ψ TWEETER_POSN_SIDE]
         [σ TWEETER_POSN_TOP]
         [μ TWEETER_DISTANCE_WALL]
         [φ SPEAKER_HEIGHT]
         [ρ SPEAKER_DEPTH]
         [ε SPEAKER_DISTANCE_APART]
         [δ EAR_DISTANCE_WALL]
         [χ EAR_DISTANCE_GROUND]
         [θ (atan (/ (/ ε 2) (- δ μ)))]
         [τ (/ (/ ε 2) (sin θ))]
         [function (λ (η)
                     (let*
                         ([η^ (- η (* ρ (sin β)))]
                          [y (- ρ (* (- φ σ) (tan β)))]
                          [z (* y (cos β))]
                          [v (- z (* η^ (tan β)))]
                          [t (/ (+ φ (- σ) (/ η^ (cos β))) (sin β))]
                          [l (+ t v τ)])
                       (* l (tan β))))]
         [η (zero-function function 0 100 INCR χ)])
      (list η (- η (* ρ (sin β)))))))

(define stand-height-from-tiltback-degrees
  (λ (β)
    (let ([res (η-from-β (degrees->radians β))])
      `((front-height . ,(car res))
        (back-height . ,(cadr res))))))

(stand-height-from-tiltback-degrees 5)
