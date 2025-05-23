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
;;; Vectors, in the linear algebra sense.
;;;
;;; Code:

(define-module (math vector)
  #:use-module (math)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (vec2
            vec2?
            vec2-x
            vec2-y
            vec2-add
            vec2-sub
            vec2-mul-scalar
            vec2-magnitude
            vec2-normalize
            vec2-dot
            set-vec2-x!
            set-vec2-y!
            with-vec2
            vec2-add!
            vec2-sub!
            vec2-mul-scalar!
            vec2-normalize!
            vec2-clamp!))

;; For speed, a vec2 is a wrapper around a bytevector so that we can
;; use unboxed floats.
(define-record-type <vec2>
  (make-vec2 bv)
  vec2?
  (bv vec2-bv))

(set-record-type-printer! <vec2>
  (lambda (v port)
    (format port "#<vec2 ~a ~a>" (vec2-x v) (vec2-y v))))

(define f64-ref  bytevector-ieee-double-native-ref)
(define f64-set! bytevector-ieee-double-native-set!)

(define (vec2 x y)
  (let ((v (make-vec2 (make-bytevector 16))))
    (set-vec2-x! v x)
    (set-vec2-y! v y)
    v))

(define (vec2-x v)
  (f64-ref (vec2-bv v) 0))

(define (vec2-y v)
  (f64-ref (vec2-bv v) 8))

(define (vec2-add v w)
  (let ((s (make-vec2 (make-bytevector 16))))
    (set-vec2-x! s (+ (vec2-x v) (vec2-x w)))
    (set-vec2-y! s (+ (vec2-y v) (vec2-y w)))
    s))

(define (vec2-sub v w)
  (let ((s (make-vec2 (make-bytevector 16))))
    (set-vec2-x! s (- (vec2-x v) (vec2-x w)))
    (set-vec2-y! s (- (vec2-y v) (vec2-y w)))
    s))

(define (vec2-mul-scalar v x)
  (let ((s (make-vec2 (make-bytevector 16))))
    (set-vec2-x! s (* (vec2-x v) x))
    (set-vec2-y! s (* (vec2-y v) x))
    s))

(define (vec2-magnitude v)
  (sqrt (+ (* (vec2-x v) (vec2-x v)) (* (vec2-y v) (vec2-y v)))))

(define (vec2-normalize v)
  (unless (and (= (vec2-x v) 0.0) (= (vec2-y v) 0.0))
    (let ((m (vec2-magnitude v))
          (s (make-vec2 (make-bytevector 16))))
      (set-vec2-x! s (/ (vec2-x v) m))
      (set-vec2-y! s (/ (vec2-y v) m))
      s)))

(define (vec2-dot v w)
  (+ (* (vec2-x v) (vec2-x w)) (* (vec2-y v) (vec2-y w))))

(define (set-vec2-x! v x)
  (f64-set! (vec2-bv v) 0 x))

(define (set-vec2-y! v y)
  (f64-set! (vec2-bv v) 8 y))

(define (vec2-add! v w)
  (set-vec2-x! v (+ (vec2-x v) (vec2-x w)))
  (set-vec2-y! v (+ (vec2-y v) (vec2-y w))))

(define (vec2-sub! v w)
  (set-vec2-x! v (- (vec2-x v) (vec2-x w)))
  (set-vec2-y! v (- (vec2-y v) (vec2-y w))))

(define (vec2-mul-scalar! v x)
  (set-vec2-x! v (* (vec2-x v) x))
  (set-vec2-y! v (* (vec2-y v) x)))

(define (vec2-normalize! v)
  (unless (and (= (vec2-x v) 0.0) (= (vec2-y v) 0.0))
    (let ((m (vec2-magnitude v)))
      (set-vec2-x! v (/ (vec2-x v) m))
      (set-vec2-y! v (/ (vec2-y v) m)))))

(define (vec2-clamp! v xmin ymin xmax ymax)
  (set-vec2-x! v (clamp (vec2-x v) xmin xmax))
  (set-vec2-y! v (clamp (vec2-y v) ymin ymax)))
