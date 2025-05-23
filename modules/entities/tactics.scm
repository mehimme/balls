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

(define-module (entities tactics)
  #:use-module (entities)
  #:use-module (math vector)
  #:export (induction))

;; implicit tactics:
;;   rfl:
;;     equality (applied on contact between equal left and right goals)
;;   rw (specifically nth_rewrite [n] [h]):
;;     substitute in assumption (applied on contact between assumption and compatible goal)

(define tact-color "#006DAA") ;; blue
(define tact-r 15.0)
(define tact-m (* 10 tact-r))

(define (induction p_0x p_0y)
  (let* ((p_0 (vec2 p_0x p_0y))
         (v_0 (vec2 0.0 0.0)))
    (make-ball p_0 v_0 #f #f tact-r tact-m tact-color "induction")))
