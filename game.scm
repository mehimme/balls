;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
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
;;; Prove theorems by dropping watermelons!
;;;
;;; Code:

(use-modules (dom canvas)
             (dom document)
             (dom element)
             (dom event)
             (dom image)
             (dom media)
             (dom window)
             (hoot ffi)
             (hoot hashtables)
             (ice-9 match)
             (math)
             (math ffi)
             (math rect)
             (math vector)
             (physics)
             (entities)
             (entities tactics)
             (entities theorems))

;; Assets
(define image:ball         (make-image "assets/images/ball.png"))

;; Game data
(define game-width    640.0)
(define game-height   480.0)

(define (make-random-ent ents)
  (let* ((len (length ents))
         (idx (inexact->exact (floor (* (random) len)))))
    ((list-ref ents idx) (/ game-width 2) (* 0.1 game-height))))

(define (make-world-1)
  (let* ((tactics (list induction))
         (theorems (list add_zero add_succ))
         (held-ents (vector (make-random-ent (append tactics theorems)) #f #f #f #f #f))
         (dropped-ents (list))
         (goals 'make-goals-1)
         (walls (vector (make-wall (vec2 (* 0.3 game-width) (* 0.1 game-height))
                                   (vec2 1.0 0.0)
                                   (vec2 10.0 (* 0.8 game-height)) 0.85)
                        (make-wall (vec2 (* 0.7 game-width) (* 0.1 game-height))
                                   (vec2 -1.0 0.0)
                                   (vec2 10.0 (* 0.8 game-height)) 0.85)
                        (make-wall (vec2 (* 0.3 game-width) (* 0.9 game-height))
                                   (vec2 0.0 -1.0)
                                   (vec2 (+ (* 0.4 game-width) 10.0) 10.0) 0.0))))
    (make-world 'play held-ents dropped-ents goals
                #f #f #f
                tactics theorems walls)))

;; Game state
(define *world* (make-world-1))

(define (win! world)
  (world-state-set! world 'win))

(define (lose! world)
  (world-state-set! world 'lose))


(define (drop-ent! world)
  (let* ((held-ents (world-held-ents world))
         (curr-held-ent (vector-ref held-ents 0))
         (dropped-ents (world-dropped-ents world)))
    (if (world-drop? world)
        (begin ;; move currently held ent to dropped-ents
          (if (null? dropped-ents)
              (world-dropped-ents-set! world (list curr-held-ent))
              (world-dropped-ents-set! world (append! dropped-ents (list curr-held-ent))))
          (ball-dropped-set! curr-held-ent #t)
          (world-drop-set! world #f))
        #t)))

(define (update)
  (match (world-state *world*)
    ('play
     (let* ((tactics (world-tactics *world*))
            (theorems (world-theorems *world*))
            (ents (append tactics theorems))
            (held-ents (world-held-ents *world*))
            (curr-held-ent (vector-ref held-ents 0))
            (dropped-ents (world-dropped-ents *world*)))

       ;; if currently held entity has been dropped, generate a new one and hold it
       (if (ball-dropped? curr-held-ent) (vector-set! held-ents 0 (make-random-ent ents)))

       ;; drop currently held entity if requested
       (drop-ent! *world*)

       ;; evolve dynamics of all entities in world
       (step-world! *world*)))
    (_ #t))
  (timeout update-callback dt))
(define update-callback (procedure->external update))

;; Rendering
(define number->string*
  (let ((cache (make-eq-hashtable))) ; assuming fixnums only
    (lambda (x)
      (or (hashtable-ref cache x)
          (let ((str (number->string x)))
            (hashtable-set! cache x str)
            str)))))

(define (draw prev-time)
  (let* ((held-ents (world-held-ents *world*))
         (curr-held-ent (vector-ref held-ents 0))
         (dropped-ents (world-dropped-ents *world*))
         (walls (world-walls *world*)))

    ;; Draw background
    (set-fill-color! context "#140c1c")
    (fill-rect context 0.0 0.0 game-width game-height)

    ;; Draw walls
    (set-fill-color! context "#30d3c8")
    (let loop ((i 0))
      (if (< i (vector-length walls))
          (let* ((wall (vector-ref walls i))
                 (p (wall-p wall))
                 (v (wall-v wall)))
            (fill-rect context (vec2-x p) (vec2-y p) (vec2-x v) (vec2-y v))
            (loop (+ i 1)))
          #t))

    ;; Draw curr-held-ent
    (let* ((r-ball (ball-r curr-held-ent))
           (⌀ (* 2 r-ball))
           (x (- (vec2-x (ball-p curr-held-ent)) r-ball))
           (y (- (vec2-y (ball-p curr-held-ent)) r-ball)))
      (draw-image context image:ball
                  0.0 0.0 ⌀ ⌀
                  x y ⌀ ⌀))

    ;; Draw dropped-ents
    (for-each (lambda (dropped-ent)
                (let* ((r-ball (ball-r dropped-ent))
                       (⌀ (* 2 r-ball))
                       (x (- (vec2-x (ball-p dropped-ent)) r-ball))
                       (y (- (vec2-y (ball-p dropped-ent)) r-ball)))
                  (draw-image context image:ball
                              0.0 0.0 ⌀ ⌀
                              x y ⌀ ⌀)))
              dropped-ents)

    ;; Print relevant information
    (set-fill-color! context "#ffffff")
    (set-font! context "bold 12px monospace")
    (set-text-align! context "left")
    (fill-text context "ball-p:" 16.0 36.0)
    (fill-text context (number->string* (vec2-x (ball-p curr-held-ent))) 16.0 50.0)
    (fill-text context (number->string* (vec2-y (ball-p curr-held-ent))) 16.0 64.0)
    (fill-text context (ball-label curr-held-ent) 16.0 78.0)
    (fill-text context (number->string* (length dropped-ents)) 16.0 92.0)

    ;; (match (level-state *level*)
    ;;   ('win
    ;;    (set-text-align! context "center")
    ;;    (fill-text context "YAY YOU DID IT!!!" (/ game-width 2.0) (/ game-height 2.0)))
    ;;   ('lose
    ;;    (set-text-align! context "center")
    ;;    (fill-text context "OH NO, GAME OVER :(" (/ game-width 2.0) (/ game-height 2.0)))
    ;;   (_ #t))
    )
  (request-animation-frame draw-callback))
(define draw-callback (procedure->external draw))

;; Input
(define key:left "ArrowLeft")
(define key:right "ArrowRight")
(define key:confirm "Enter")
(define key:space "Space")

(define (on-key-down event)
  (let ((key (keyboard-event-code event)))
    (match (world-state *world*)
      ('play
       (cond
        ((string=? key key:left)
         (world-curr-held-left-set! *world* #t))
        ((string=? key key:right)
         (world-curr-held-right-set! *world* #t))))
      ((or 'win 'lose)
       (when (string=? key key:confirm)
         (set! *world* (make-world-1)))))))

(define (on-key-up event)
  (let ((key (keyboard-event-code event)))
    (match (world-state *world*)
      ('play
       (cond
        ((string=? key key:left)
         (world-curr-held-left-set! *world* #f))
        ((string=? key key:right)
         (world-curr-held-right-set! *world* #f))))
      (_ #t))))

(define (on-key-press event)
  (let ((key (keyboard-event-code event)))
    (match (world-state *world*)
      ('play
       (cond
        ((string=? key key:space)
         (world-drop-set! *world* #t))))
      (_ #t))))

;; Canvas and event loop setup
(define canvas (get-element-by-id "canvas"))
(define context (get-context canvas "2d"))
(set-element-width! canvas (inexact->exact game-width))
(set-element-height! canvas (inexact->exact game-height))
(add-event-listener! (current-document) "keydown"
                     (procedure->external on-key-down))
(add-event-listener! (current-document) "keyup"
                     (procedure->external on-key-up))
(add-event-listener! (current-document) "keypress"
                     (procedure->external on-key-press))
(request-animation-frame draw-callback)
(timeout update-callback dt)
