;;; -*- Gerbil -*-
(import :std/error
        :std/sugar)
(export #t)

;;; Your library support code
;;; ...

(define (median lst)
  (let ((sorted-lst (sort lst <)))
    (if (odd? (length sorted-lst))
        (list-ref sorted-lst (quotient (length sorted-lst) 2))
        (/ (+ (list-ref sorted-lst (quotient (length sorted-lst) 2))
              (list-ref sorted-lst (- (quotient (length sorted-lst) 2) 1)))
           2))))

(def (mode lst)
  (let ((freq (hash)))
    (for-each (lambda (item)
                (hash-put! freq item (lambda (x) (+ x 1)) 0))
              lst)
    (let loop ((max-val #f) (max-count 0))
      (hash-for-each
       (lambda (key count)
         (if (> count max-count)
             (loop key count)))
       freq)
      max-val)))

(def (mean lst)
  (/ (apply + lst) (length lst)))

(def (variance lst)
  (let ((m (mean lst)))
    (/ (apply + (map (lambda (x) (expt (- x m) 2)) lst))
       (- (length lst) 1))))

(def (std-deviation lst)
  (sqrt (variance lst)))

(def (kurtosis lst)
  (let* ((n (length lst))
         (m (mean lst))
         (s (std-deviation lst))
         (numerator (* n (apply + (map (lambda (x) (expt (/ (- x m) s) 4)) lst)))))
    (- (/ (* numerator (+ n 1)) (* (- n 1) (- n 2) (- n 3)))
       (/ (* 3 (expt (- n 1) 2)) (* (- n 2) (- n 3)))))
  )


(def (fft x)
  "Cooley-Tukey fft"
  (let* ((N (length x))
         (half-N (quotient N 2)))
    (if (= N 1)
        x
        (let* ((even (fft (list-tabulate half-N (lambda (k) (list-ref x (* 2 k))))))
               (odd (fft (list-tabulate half-N (lambda (k) (list-ref x (+ 1 (* 2 k)))))))
               (combined (make-vector N)))
          (for (i 0 half-N)
            (let* ((t (exp (* -2.0 pi i 0+1i) (/ i N)))
                   (odd-term (* t (list-ref odd i))))
              (vector-set! combined i (+ (list-ref even i) odd-term))
              (vector-set! combined (+ i half-N) (- (list-ref even i) odd-term))))
          combined)))
  )

(def (z-scores lst)
  (let ((m (mean lst))
        (std-dev (standard-deviation lst)))
    (map (lambda (x) (/ (- x m) std-dev)) lst)))
