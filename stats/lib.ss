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

(def (quartile lst q)
  (let* ((sorted-lst (sort lst <))
         (len (length sorted-lst))
         (pos (+ (* q (- len 1)) 1)))
    (if (integer? pos)
      (list-ref sorted-lst (1- pos))
      (let* ((lower (list-ref sorted-lst (floor pos)))
             (upper (list-ref sorted-lst (ceiling pos))))
        (/ (+ lower upper) 2)))))

(def (interquartile-range lst)
  (- (quartile lst 0.75)
     (quartile lst 0.25)))

(def (linear-regression x y)
  (let* ((n (length x))
         (mean-x (mean x))
         (mean-y (mean y))
         (ss-xy (sum (map * (map - x (make-list n mean-x)) (map - y (make-list n mean-y)))))
         (ss-xx (sum (map (lambda (xi) (expt (- xi mean-x) 2)) x)))
         (b1 (/ ss-xy ss-xx))
         (b0 (- mean-y (* b1 mean-x))))
    (list b0 b1)))


(def (correlation x y)
  (let* ((mean-x (mean x))
         (mean-y (mean y))
         (n (length x))
         (sum-xy (sum (map * (map - x (make-list n mean-x)) (map - y (make-list n mean-y)))))
         (sum-x2 (sum (map (lambda (x) (expt (- x mean-x) 2)) x)))
         (sum-y2 (sum (map (lambda (y) (expt (- y mean-y) 2)) y))))
    (/ sum-xy (sqrt (* sum-x2 sum-y2)))))

(def (percentile lst p)
  (let* ((sorted-lst (sort lst <))
         (index (floor (* p (/ (length lst) 100)))))
    (list-ref sorted-lst index)))

(def (box-plot-stats lst)
  (let* ((q1 (quartile lst 0.25))
         (median (quartile lst 0.5))
         (q3 (quartile lst 0.75))
         (iqr (interquartile-range lst))
         (lower-whisker (max (min lst) (- q1 (* 1.5 iqr))))
         (upper-whisker (min (max lst) (+ q3 (* 1.5 iqr)))))
    (list lower-whisker q1 median q3 upper-whisker)))

;; dbscan
(def (distance p1 p2)
  "Euclidian distance function"
  (sqrt (apply + (map (lambda (x y) (expt (- x y) 2)) p1 p2))))

(define (epsilon-neighborhood point points eps)
  (filter (lambda (p)
	    (<= (distance point p) eps)) points))

(define (expand-cluster core-point points eps minPts cluster-id assigned-clusters)
  (let ((seeds (epsilon-neighborhood core-point points eps)))
    (for-each (lambda (seed)
                (let ((seed-status (vector-ref assigned-clusters seed)))
                  (cond
                   ((or (eq? seed-status 'unvisited) (eq? seed-status 'noise))
                    (vector-set! assigned-clusters seed cluster-id)
                    (let ((new-neighbors (epsilon-neighborhood seed points eps)))
                      (when (>= (length new-neighbors) minPts)
                        (set! seeds (append seeds new-neighbors))))))))
              seeds)))

(def (dbscan points eps minPts)
  (let ((assigned-clusters (make-vector (length points) 'unvisited))
        (cluster-id 0))
    (for-each (lambda (point)
                (when (eq? (vector-ref assigned-clusters point) 'unvisited)
                  (let ((neighbors (epsilon-neighborhood point points eps)))
                    (if (< (length neighbors) minPts)
                      (vector-set! assigned-clusters point 'noise)
                      (begin
                        (set! cluster-id (+ cluster-id 1))
                        (expand-cluster point neighbors points eps minPts cluster-id assigned-clusters)))))
		points)
	      assigned-clusters)))
