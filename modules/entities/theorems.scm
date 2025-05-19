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

(define-module (entities theorems)
  #:use-module (entities)
  #:use-module (math vector)
  #:export
  (four-succ_three
   three-succ_two
   two-succ_one
   one-succ_zero

   add_zero
   add_succ))

(define thrm-color "#32936F") ;; green
(define thrm-r 30.0)
(define thrm-m (* 10 thrm-r))

(define (four-succ_three p_0x p_0y)
  (let* ((p_0 (vec2 p_0x p_0y))
         (v_0 (vec2 0.0 0.0))
         (r 11.0)
         (m 100.0))
    (make-ball p_0 v_0 #f #f thrm-r thrm-m thrm-color "+4 → +succ3")))

(define (three-succ_two p_0x p_0y)
  (let* ((p_0 (vec2 p_0x p_0y))
         (v_0 (vec2 0.0 0.0))
         (r 11.0)
         (m 100.0))
    (make-ball p_0 v_0 #f #f thrm-r thrm-m thrm-color "+3 → +succ2")))

(define (two-succ_one p_0x p_0y)
  (let* ((p_0 (vec2 p_0x p_0y))
         (v_0 (vec2 0.0 0.0))
         (r 11.0)
         (m 100.0))
    (make-ball p_0 v_0 #f #f thrm-r thrm-m thrm-color "+2 → +succ1")))

(define (one-succ_zero p_0x p_0y)
  (let* ((p_0 (vec2 p_0x p_0y))
         (v_0 (vec2 0.0 0.0))
         (r 11.0)
         (m 100.0))
    (make-ball p_0 v_0 #f #f thrm-r thrm-m thrm-color "+1 → +succ0")))

(define (add_zero p_0x p_0y)
  (let* ((p_0 (vec2 p_0x p_0y))
         (v_0 (vec2 0.0 0.0))
         (r 11.0)
         (m 100.0))
    (make-ball p_0 v_0 #f #f thrm-r thrm-m thrm-color "+,+0 → +")))

(define (add_succ p_0x p_0y)
  (let* ((p_0 (vec2 p_0x p_0y))
         (v_0 (vec2 0.0 0.0))
         (r 11.0)
         (m 100.0))
    (make-ball p_0 v_0 #f #f thrm-r thrm-m thrm-color "+,+succ → succ+,succ+")))
