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

(define-module (entities goals)
  #:use-module (entities)
  #:use-module (math vector)
  #:export (+two +four +succzero +succsucczero))

;; merge rules:
;; goals can only merge with theorems, not with each other

(define goal-color "#FF6B6B") ;; red
(define goal-r 22.0)
(define goal-m (* 10 goal-r))

(define (add_zero p_0x p_0y)
  (let* ((p_0 (vec2 p_0x p_0y))
         (v_0 (vec2 0.0 0.0)))
    (make-ball p_0 v_0 #f #f goal-r goal-m goal-color "+,+0 -> +")))

(define (add_succ p_0x p_0y)
  (let* ((p_0 (vec2 p_0x p_0y))
         (v_0 (vec2 0.0 0.0)))
    (make-ball p_0 v_0 #f #f goal-r goal-m goal-color "+,+succ -> succ+,succ+")))
