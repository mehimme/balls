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
;;; TODO
;;;
;;; Code:

(define-module (entities)
  #:use-module (srfi srfi-9)
  #:export
  (<ball>
   make-ball
   ball?
   ;; mutable
   ball-p ball-v ball-dropped? ball-merged?
   ball-p-set! ball-v-set! ball-dropped-set! ball-merged-set!
   ;; immutable
   ball-r ball-m ball-I ball-color ball-label

   <goal>
   make-goal
   goal?
   ;; mutable
   goal-p      goal-v
   goal-p-set! goal-v-set!
   goal-r      goal-m      goal-I      goal-color      goal-label
   goal-r-set! goal-m-set! goal-I-set! goal-color-set! goal-label-set!

   <wall>
   make-wall
   wall?
   ;; immutable
   wall-p wall-n wall-v wall-e
   wall-color

   <buf>
   make-buf
   buf?
   buf-vec buf-used buf-used-set! buf-push!
   buf-do buf-buf-do

   <world>
   make-world
   world?
   ;; mutable
   world-state      world-held-ents      world-dropped-ents      world-goals
   world-state-set! world-held-ents-set! world-dropped-ents-set! world-goals-set!
   world-curr-held-left?     world-curr-held-right?     world-drop?
   world-curr-held-left-set! world-curr-held-right-set! world-drop-set!
   ;; immutable
   world-tactics world-theorems world-walls))

(define wall-color "#020887") ;; dark blue

(define-record-type <ball>
  (make-ball p v dropped? merged? r m color label)
  ball?
  (p ball-p ball-p-set!)               ;; position
  (v ball-v ball-v-set!)               ;; velocity
  (dropped? ball-dropped? ball-dropped-set!)
  (merged? ball-merged? ball-merged-set!)
  (r ball-r)                           ;; radius
  (m ball-m)                           ;; mass
  (color ball-color)
  (label ball-label))

(define-record-type <goal>
  (make-goal p v r m color label)
  goal?
  (p goal-p goal-p-set!)               ;; position
  (v goal-v goal-v-set!)               ;; velocity
  (r goal-r goal-r-set!)               ;; radius
  (m goal-m goal-m-set!)               ;; mass
  (color goal-color goal-color-set!)
  (label goal-label goal-label-set!))

(define-record-type <wall>
  (make-wall p n v e)
  wall?
  (p wall-p) ;; top-left point
  (n wall-n) ;; unit normal vector (in collision direction)
  (v wall-v)
  (e wall-e)) ;; coefficient of restitution: 0 = inelastic, 1 = elastic

(define-record-type <buf>
  (make-buf vec used)
  buf?
  (vec buf-vec)
  (used buf-used buf-used-set!))

(define (buf-push! buf x)
  (let* ((vec (buf-vec buf))
         (used (buf-used buf)))
    (if (= used (vector-length vec))
        (error "buffer full" buf x)
        (begin
          (vector-set! vec used x)
          (buf-used-set! buf (+ used 1))))))

(define (buf-do buf expr)
  (do ((i 0 (+ i 1)))
      ((= i (buf-used buf)))
    (let ((x (vector-ref (buf-vec buf) i)))
      (expr x))))

(define (buf-buf-do buf expr)
  (do ((i 0 (+ i 1)))
      ((= i (buf-used buf)))
    (let ((x_i (vector-ref (buf-vec buf) i)))
      (do ((j 0 (+ j 1)))
          ((= j (buf-used buf)))
        (let ((x_j (vector-ref (buf-vec buf) j)))
          (if (not (= i j)) (expr x_i x_j)))))))

(define-record-type <world>
  (make-world state held-ents dropped-ents goals curr-held-left? curr-held-right? drop? tactics theorems walls)
  world?
  (state world-state world-state-set!)
  (held-ents world-held-ents world-held-ents-set!)
  (dropped-ents world-dropped-ents world-dropped-ents-set!)
  (goals world-goals world-goals-set!)
  (curr-held-left? world-curr-held-left? world-curr-held-left-set!)
  (curr-held-right? world-curr-held-right? world-curr-held-right-set!)
  (drop? world-drop? world-drop-set!)
  (tactics world-tactics)
  (theorems world-theorems)
  (walls world-walls))
