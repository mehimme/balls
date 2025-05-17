;;; Copyright (C) 2025 mehimme <mehimme@proton.me>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; combine dynamics from user input, gravity, friction, collisions
;;;
;;; Code:

(define-module (physics)
  #:use-module (entities)
  #:use-module (math vector)
  #:export (step-world! dt))

(define g 200.0)

;; coefficient of restitution: 0 = inelastic, 1 = elastic
(define e-ball-ball 0.85) ;; mostly elastic
(define μ 0.02) ;; rolling resistance
(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz
(define held-speed 200.0)

(define (apply-left-right-input-velocity! world curr-held-ent)
  (let ((speed (* held-speed
                  (+ (if (world-curr-held-left? world) -1.0 0.0)
                     (if (world-curr-held-right? world) 1.0 0.0)))))
    (set-vec2-x! (ball-v curr-held-ent) speed)))

(define (apply-gravity! ball)
  (let* ((v_ball (ball-v ball))
         (v_ball_y (vec2-y v_ball)))
    ;; semi-implicit euler (velocity first)
    (set-vec2-y! v_ball (+ v_ball_y
                           (* g
                              (/ dt 1000.0))))))

(define (dist-ball-wall ball wall)
  (let* ((n-wall (wall-n wall))
         (v-wall-x (vec2-x (wall-v wall)))
         (v-wall-y (vec2-y (wall-v wall)))
         (p-wall (wall-p wall))
         (p-ball (ball-p ball)))
    (if (or (< (vec2-x n-wall) 0.0) (< (vec2-y n-wall) 0.0))
        (vec2-dot (vec2-sub p-ball p-wall)
                  n-wall)
        (vec2-dot (vec2-sub
                   (vec2-sub p-ball p-wall)
                   (vec2-mul-scalar n-wall
                                    (if (< v-wall-x v-wall-y)
                                        v-wall-x v-wall-y)))
                  n-wall))))

(define (apply-ball-wall-collision! ball wall)
  (let* ((r-ball (ball-r ball))
         (v-ball (ball-v ball))
         (m-ball (ball-m ball))
         (n-wall (wall-n wall))
         (e-wall (wall-e wall))
         (vn-ball (vec2-dot v-ball n-wall))
         (Δ-ball-wall (dist-ball-wall ball wall)))
    (if (and (< Δ-ball-wall r-ball) (<= vn-ball 0.0))
        (let ((vn-impulse (vec2-mul-scalar ;; wall applies impulse in normal direction due to collision
                           n-wall
                           (- (* (+ 1 e-wall) vn-ball)))))
          ;; (pk 'we-shouldn't-be-here #t)
          ;; (pk 'Δ-ball-wall Δ-ball-wall)
          ;; (pk 'vn-ball vn-ball)
          ;; (pk 'vn-impulse-x (vec2-x vn-impulse))
          ;; (pk 'vn-impulse-y (vec2-y vn-impulse))
          ;; apply velocity due to impulse
          (vec2-add! v-ball vn-impulse)
          ;; (pk 'Δ-ball-wall Δ-ball-wall)
          ;; (pk 'vn-ball vn-ball)
          ;; (pk 'vn-impulse-x (vec2-x vn-impulse))
          ;; (pk 'vn-impulse-y (vec2-y vn-impulse))
          )
        #t)))

;; (define (get-ball-ball-collisions world)
;;   ;; spatial hash

;;   )

(define (integrate! ball)
  (let* ((v_ball (ball-v ball))
         (v_ball_x (vec2-x v_ball))
         (v_ball_y (vec2-y v_ball))
         (p_ball (ball-p ball))
         (p_ball_x (vec2-x p_ball))
         (p_ball_y (vec2-y p_ball))
         (θ_ball (ball-θ ball))
         (ω_ball (ball-ω ball)))
    (set-vec2-x! p_ball (+ p_ball_x
                           (* v_ball_x
                              (/ dt 1000.0))))
    (set-vec2-y! p_ball (+ p_ball_y
                           (* v_ball_y
                              (/ dt 1000.0))))
    (ball-θ-set! ball (+ θ_ball
                         (* ω_ball
                            (/ dt 1000.0))))))

(define (step-world! world)
  (let* ((held-ents (world-held-ents world))
         (curr-held-ent (vector-ref held-ents 0))
         (dropped-ents (world-dropped-ents world))
         (walls (world-walls world)))

    ;; update velocity from left/right inputs to currently held ent
    (apply-left-right-input-velocity! world curr-held-ent)

    ;; update velocity due to gravity on each dropped ent in world
    (for-each (lambda (dropped-ent)
                (apply-gravity! dropped-ent))
              dropped-ents)

    ;; collect all likely collisions (spatial hash)

    ;; narrow to actual collisions
    ;; ball-ball

    ;; entity-wall
    (let loop ((i 0)) ;; for each wall
      (if (< i (vector-length walls))
          (let ((wall (vector-ref walls i)))
            (for-each (lambda (dropped-ent)
                        ;; (pk 'wall-idx i)
                        ;; (pk 'prev-p-x (vec2-x (ball-p dropped-ent)))
                        ;; (pk 'prev-p-y (vec2-y (ball-p dropped-ent)))
                        ;; (pk 'prev-v-x (vec2-x (ball-v dropped-ent)))
                        ;; (pk 'prev-v-y (vec2-y (ball-v dropped-ent)))
                        (apply-ball-wall-collision! dropped-ent wall)
                        ;; (pk 'new-p-x (vec2-x (ball-p dropped-ent)))
                        ;; (pk 'new-p-y (vec2-y (ball-p dropped-ent)))
                        ;; (pk 'new-v-x (vec2-x (ball-v dropped-ent)))
                        ;; (pk 'new-v-y (vec2-y (ball-v dropped-ent)))
                        )
                      dropped-ents)
            (loop (+ i 1)))
          #t))

    ;; resolve all collisions

    ;; apply rolling resistance

    ;; update position of all dropped ents and currently held ent
    (integrate! curr-held-ent)
    (for-each (lambda (dropped-ent)
                (integrate! dropped-ent))
              dropped-ents)))
