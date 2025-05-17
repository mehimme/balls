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
   ball-p ball-v ball-θ ball-ω ball-dropped? ball-p-set! ball-v-set! ball-θ-set! ball-ω-set! ball-dropped-set!
   ;; immutable
   ball-r ball-m ball-I ball-color ball-label

   <goal>
   make-goal
   goal?
   ;; mutable
   goal-p      goal-v      goal-θ      goal-ω
   goal-p-set! goal-v-set! goal-θ-set! goal-ω-set!
   goal-r      goal-m      goal-I      goal-color      goal-label
   goal-r-set! goal-m-set! goal-I-set! goal-color-set! goal-label-set!

   <wall>
   make-wall
   wall?
   ;; immutable
   wall-p wall-n wall-v wall-e

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

(define-record-type <ball>
  (make-ball p v θ ω dropped? r m I color label)
  ball?
  (p ball-p ball-p-set!)               ;; position
  (v ball-v ball-v-set!)               ;; velocity
  (θ ball-θ ball-θ-set!)               ;; angular position
  (ω ball-ω ball-ω-set!)               ;; angular velocity
  (dropped? ball-dropped? ball-dropped-set!)
  (r ball-r)                           ;; radius
  (m ball-m)                           ;; mass
  (I ball-I)                           ;; moment of inertia
  (color ball-color)
  (label ball-label))

(define-record-type <goal>
  (make-goal p v θ ω r m I color label)
  goal?
  (p goal-p goal-p-set!)               ;; position
  (v goal-v goal-v-set!)               ;; velocity
  (θ goal-θ goal-θ-set!)               ;; angular position
  (ω goal-ω goal-ω-set!)               ;; angular velocity
  (r goal-r goal-r-set!)               ;; radius
  (m goal-m goal-m-set!)               ;; mass
  (I goal-I goal-I-set!)               ;; moment of inertia
  (color goal-color goal-color-set!)
  (label goal-label goal-label-set!))

(define-record-type <wall>
  (make-wall p n v e)
  wall?
  (p wall-p) ;; top-left point
  (n wall-n) ;; unit normal vector (in collision direction)
  (v wall-v)
  (e wall-e)) ;; coefficient of restitution: 0 = inelastic, 1 = elastic

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
