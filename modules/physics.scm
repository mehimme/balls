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
  #:use-module (math)
  #:use-module (math vector)
  #:export (step-world! dt))

;; coefficient of restitution: 0 = inelastic, 1 = elastic
(define e-ball-ball 0.85) ;; mostly elastic
(define μ 0.25) ;; ball-wall sliding friction
(define g 200.0) ;; gravity
(define dt (/ 1000.0 60.0)) ; aim for updating at 60Hz
(define held-speed 200.0)

(define (apply-left-right-input-velocity! world curr-held-ent)
  (let* ((speed (* held-speed
                   (+ (if (world-curr-held-left? world) -1.0 0.0)
                      (if (world-curr-held-right? world) 1.0 0.0))))
         (p (ball-p curr-held-ent))
         (r (ball-r curr-held-ent))
         (p-x (vec2-x p)))
    (set-vec2-x! (ball-v curr-held-ent) speed)))

    ;; (if (and (> (- p-x r) (* 0.3 640.0)) (< (+ p-x r) (* 0.7 640.0)))
        ;; (set-vec2-x! (ball-v curr-held-ent) 0.0))))

(define (apply-gravity! ball)
  (let* ((v_ball (ball-v ball))
         (v_ball_y (vec2-y v_ball)))
    (set-vec2-y! v_ball (+ v_ball_y     ;; semi-implicit euler (velocity first)
                           (* g
                              (/ dt 1000.0))))))

(define (dist-ball-wall ball wall)
  "distance from ball center to relevant wall edge given direction of wall normal"
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
  (let* ((Δ-ball-wall (dist-ball-wall ball wall))
         (r-ball (ball-r ball)))
    (if (< Δ-ball-wall r-ball) ;; ball intersects wall
        (let* ((v-ball (ball-v ball))
               (n-wall (wall-n wall))
               (e-wall (wall-e wall))
               (vn-ball-mag (vec2-dot v-ball n-wall)) ;; normal component of v-ball
               (vt-ball (vec2-sub v-ball (vec2-mul-scalar n-wall vn-ball-mag))) ;; tangential component of v-ball
               (vt-ball-mag (vec2-magnitude vt-ball))
               (vn-impulse-mag (- (* (+ 1 e-wall) vn-ball-mag)))
               (vn-impulse (vec2 0.0 0.0))  ;; wall->ball normal impulse due to collision
               (vt-impulse (vec2 0.0 0.0))) ;; wall->ball tangential impulse due to sliding friction

          (if (< vn-ball-mag 0.0) ;; ball is moving into wall
              (set! vn-impulse (vec2-mul-scalar n-wall vn-impulse-mag))) ;; bounce off walls
          (if (> vt-ball-mag 0.0) ;; ball is moving tangential to wall
              (let* ((nt-ball (vec2-normalize vt-ball)) ;; tangential velocity unit vector
                     (vt-impulse-max (* μ (abs vn-impulse-mag))))
                (set! vt-impulse (vec2-mul-scalar ;; oppose sliding direction
                                  nt-ball
                                  (clamp (- vt-ball-mag) (- vt-impulse-max) vt-impulse-max)))))
          (vec2-add! v-ball vn-impulse)
          (vec2-add! v-ball vt-impulse)))))

(define (apply-ball-ball-collision! a b)
  (let* ((d (vec2-sub (ball-p a) (ball-p b))) ;; normal displacement
         (Δ (vec2-magnitude d))
         (r-a (ball-r a))
         (r-b (ball-r b)))
    (if (< Δ (+ r-a r-b)) ;; balls intersect
        (let* ((inv-m-a (/ 1 (ball-m a)))
               (inv-m-b (/ 1 (ball-m b)))
               (v-a (ball-v a))
               (v-b (ball-v b))
               (n (vec2-normalize d)) ;; unit normal
               (rv (vec2-sub v-a v-b)) ;; rel velocity
               (rvn-mag (vec2-dot rv n)) ;; normal component of rel velocity
               (vn-impulse-mag (/ (- (* (+ 1 e-ball-ball) rvn-mag)) (+ inv-m-a inv-m-b)))
               (vn-impulse (vec2 0.0 0.0))) ;; a->b normal impulse due to collision
          (if (< rvn-mag 0.0) ;; balls are moving into each other
              (set! vn-impulse (vec2-mul-scalar n vn-impulse-mag))) ;; bounce off each other
          (vec2-add! v-a (vec2-mul-scalar vn-impulse inv-m-a))
          (vec2-sub! v-b (vec2-mul-scalar vn-impulse inv-m-b))))))

(define (integrate! ball)
  (let* ((v_ball (ball-v ball))
         (v_ball_x (vec2-x v_ball))
         (v_ball_y (vec2-y v_ball))
         (p_ball (ball-p ball))
         (p_ball_x (vec2-x p_ball))
         (p_ball_y (vec2-y p_ball)))
    (set-vec2-x! p_ball (+ p_ball_x
                           (* v_ball_x
                              (/ dt 1000.0))))
    (set-vec2-y! p_ball (+ p_ball_y
                           (* v_ball_y
                              (/ dt 1000.0))))))

(define (apply-ball-wall-correction! ball wall)
  (let ((Δ-ball-wall (dist-ball-wall ball wall))
        (r-ball (ball-r ball)))
    (if (< Δ-ball-wall r-ball)
        (let* ((v-ball (ball-v ball))
               (v-ball-x (vec2-x v-ball))
               (v-ball-y (vec2-y v-ball))
               (n-wall (wall-n wall))
               (n-wall-x (vec2-x n-wall))
               (n-wall-y (vec2-y n-wall)))
          ;; ball intersects wall and is still moving into wall
          (if (> (abs n-wall-x) (abs n-wall-y))
              (if (< (* v-ball-x n-wall-x) 0.0) (set-vec2-x! v-ball 0.0))
              (if (< (* v-ball-y n-wall-y) 0.0) (set-vec2-y! v-ball 0.0)))))))

(define (apply-ball-ball-correction! a b)
  (let* ((d (vec2-sub (ball-p a) (ball-p b))) ;; normal displacement
         (Δ (vec2-magnitude d))
         (r-a (ball-r a))
         (r-b (ball-r b)))
    (if (< Δ (+ r-a r-b))
        (let* ((inv-m-a (/ 1 (ball-m a)))
               (inv-m-b (/ 1 (ball-m b)))
               (v-a (ball-v a))
               (v-b (ball-v b))
               (n (vec2-normalize d)) ;; unit normal
               (rv (vec2-sub v-a v-b)) ;; rel velocity
               (v-an-mag (vec2-dot v-a n))
               (v-bn-mag (vec2-dot v-b n))
               (rvn-mag (vec2-dot rv n))) ;; normal component of rel velocity
          ;; balls intersect and are still moving into each other
          (if (< v-an-mag 0.0) (vec2-sub! v-a (vec2-mul-scalar n (* 1.01 v-an-mag))))
          (if (> v-bn-mag 0.0) (vec2-sub! v-b (vec2-mul-scalar n (* 1.01 v-bn-mag))))))))

(define (step-world! world)
  (let* ((held-ents (world-held-ents world))
         (curr-held-ent (vector-ref held-ents 0))
         (dropped-ents (world-dropped-ents world))
         (walls (world-walls world)))

    ;; update velocity from left/right inputs to currently held ent
    (apply-left-right-input-velocity! world curr-held-ent)

    ;; update velocity due to gravity on each dropped ent in world
    (buf-do dropped-ents
            (lambda (dropped-ent)
              (apply-gravity! dropped-ent)))

    ;; update velocity due to entity-wall collisions
    (do ((i 0 (+ i 1))) ((= i (vector-length walls)))
      (let ((wall (vector-ref walls i)))
        (buf-do dropped-ents
                (lambda (dropped-ent)
                  (apply-ball-wall-collision! dropped-ent wall)))))
    ;; update velocity due to entity-entity collisions
    (buf-buf-do dropped-ents
                (lambda (ent-a ent-b)
                  (apply-ball-ball-collision! ent-a ent-b)))

    ;; entity-entity penetration correction
    (buf-buf-do dropped-ents
                (lambda (ent-a ent-b)
                  (apply-ball-ball-correction! ent-a ent-b)))

    ;; wall penetration correction
    (do ((i 0 (+ i 1))) ((= i (vector-length walls)))
      (let ((wall (vector-ref walls i)))
        (buf-do dropped-ents
                (lambda (dropped-ent)
                  (apply-ball-wall-correction! dropped-ent wall)))))

    ;; update position of all dropped ents and currently held ent
    (integrate! curr-held-ent)
    (buf-do dropped-ents
            (lambda (dropped-ent)
              (integrate! dropped-ent)))))
