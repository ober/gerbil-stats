;;; -*- Gerbil -*-
(import :std/error
        :std/sugar)
(export #t)

;;; Your library support code
;;; ...

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
