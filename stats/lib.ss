;;; -*- Gerbil -*-
(import :std/error
	:std/iter
	:std/srfi/125
	:std/sort
        :std/sugar)
(export #t)

(def (sum lst)
  (foldl + 0 lst))

(def (median lst)
  (inexact (let ((sorted-lst (sort lst <)))
    (if (odd? (length sorted-lst))
      (list-ref sorted-lst (quotient (length sorted-lst) 2))
      (/ (+ (list-ref sorted-lst (quotient (length sorted-lst) 2))
            (list-ref sorted-lst (- (quotient (length sorted-lst) 2) 1)))
         2)))))

(define (count-occurrences item lst)
  (foldl (lambda (elem acc) (if (= elem item) (+ acc 1) acc)) 0 lst))

(define (mode lst)
  (let loop ((remaining lst) (highest-count 0) (current-modes '()))
    (if (null? remaining)
        current-modes
        (let* ((current-item (car remaining))
               (item-count (count-occurrences current-item lst)))
          (cond
            ((> item-count highest-count)
             (loop (cdr remaining) item-count (list current-item)))
            ((= item-count highest-count)
             (loop (cdr remaining) highest-count (cons current-item current-modes)))
            (else (loop (cdr remaining) highest-count current-modes)))))))

(def (mean lst)
  (inexact (/ (apply + lst)
	      (length lst))))

(def (variance lst)
  (let ((m (mean lst)))
    (/ (apply + (map (lambda (x) (expt (- x m) 2)) lst))
       (- (length lst) 1))))

(def (standard-deviation lst)
  (sqrt (variance lst)))

(def (kurtosis lst)
  (let* ((n (length lst))
         (m (mean lst))
         (s (standard-deviation lst))
         (numerator (* n (apply + (map (lambda (x) (expt (/ (- x m) s) 4)) lst)))))
    (- (/ (* numerator (+ n 1)) (* (- n 1) (- n 2) (- n 3)))
       (/ (* 3 (expt (- n 1) 2)) (* (- n 2) (- n 3)))))
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
      (let* ((lower (list-ref sorted-lst (exact (floor pos))))
             (upper (list-ref sorted-lst (exact (ceiling pos)))))
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

;; ;; dbscan
;; (def (distance p1 p2)
;;   "Euclidian distance function"
;;   (sqrt (apply + (map (lambda (x y) (expt (- x y) 2)) p1 p2))))

;; (def (epsilon-neighborhood point points eps)
;;   (filter (lambda (p)
;; 	    (<= (distance point p) eps)) points))

;; (def (expand-cluster core-point points eps minPts cluster-id assigned-clusters)
;;   (let ((seeds (epsilon-neighborhood core-point points eps)))
;;     (for-each (lambda (seed)
;;                 (let ((seed-status (vector-ref assigned-clusters seed)))
;;                   (cond
;;                    ((or (eq? seed-status 'unvisited) (eq? seed-status 'noise))
;;                     (vector-set! assigned-clusters seed cluster-id)
;;                     (let ((new-neighbors (epsilon-neighborhood seed points eps)))
;;                       (when (>= (length new-neighbors) minPts)
;;                         (set! seeds (append seeds new-neighbors))))))))
;;               seeds)))

;; (def (dbscan points eps minPts)
;;   (let ((assigned-clusters (make-vector (length points) 'unvisited))
;;         (cluster-id 0))
;;     (for-each (lambda (point)
;;                 (when (eq? (vector-ref assigned-clusters point) 'unvisited)
;;                   (let ((neighbors (epsilon-neighborhood point points eps)))
;;                     (if (< (length neighbors) minPts)
;;                       (vector-set! assigned-clusters point 'noise)
;;                       (begin
;;                         (set! cluster-id (+ cluster-id 1))
;;                         (expand-cluster point neighbors points eps minPts cluster-id assigned-clusters)))))
;; 		points)
;; 	      assigned-clusters)))

;; (def (fft-convolve x y)
;;   (let* ((N (length x))
;; 	 (M (length y))
;; 	 (L (+ N M -1))
;; 	 (x-fft (fft (append x (make-list (- L N) 0+0i))))
;; 	 (y-fft (fft (append y (make-list (- L M) 0+0i))))
;; 	 (xy-fft (map * x-fft y-fft)))
;;     (map (lambda (x) (real-part x)) (fft xy-fft))))

;; (def (dbscan-plot points eps minPts)
;;   (let ((clusters (dbscan points eps minPts)))
;;     (for-each (lambda (point)
;; 		(let ((cluster (vector-ref clusters point)))
;; 		  (cond
;; 		   ((eq? cluster 'noise)
;; 		    (plot-point point 0 0 0))
;; 		   ((eq? cluster 1)
;; 		    (plot-point point 1 0 0))
;; 		   ((eq? cluster 2)
;; 		    (plot-point point 0 1 0))
;; 		   ((eq? cluster 3)
;; 		    (plot-point point 0 0 1))
;; 		   ((eq? cluster 4)
;; 		    (plot-point point 1 1 0))
;; 		   ((eq? cluster 5)
;; 		    (plot-point point 1 0 1))
;; 		   ((eq? cluster 6)
;; 		    (plot-point point 0 1 1))
;; 		   ((eq? cluster 7)
;; 		    (plot-point point 1 1 1))))))
;; 	      points))

;; (def (fft-convolve-plot x y)
;;   (let* ((N (length x))
;; 	 (M (length y))
;; 	 (L (+ N M -1))
;; 	 (x-fft (fft (append x (make-list (- L N) 0+0i))))
;; 	 (y-fft (fft (append y (make-list (- L M) 0+0i))))
;; 	 (xy-fft (map * x-fft y-fft))
;; 	 (xy (map (lambda (x) (real-part x)) (fft xy-fft))))
;;     (plot xy)))

;; (def (plot lst)
;;   (let ((n (length lst)))
;;     (for (i 0 n)
;;       (plot-point i (list-ref lst i) 0 0))))

;; (def (plot-point x y r g b)
;;   (let ((canvas (get-canvas)))
;;     (canvas-set! canvas x y r g b)))


;; (def (get-canvas width height)
;;   (let ((canvas (make-canvas width height)))
;;     (canvas-clear! canvas 0 0 0)
;;     canvas))

;; (def (canvas-clear! canvas r g b)
;;   (for (x 0 (canvas-width canvas))
;;     (for (y 0 (canvas-height canvas))
;;       (canvas-set! canvas x y r g b))))

;; (def (canvas-set! canvas x y r g b)
;;   (let ((index (+ (* y (canvas-width canvas)) x)))
;;     (vector-set! (canvas-pixels canvas) index r)
;;     (vector-set! (canvas-pixels canvas) (+ index 1) g)
;;     (vector-set! (canvas-pixels canvas) (+ index 2) b)))

;; (def (canvas-pixels canvas)
;;   (vector-ref canvas 0))

;; (def (canvas-width canvas)
;;   (vector-ref canvas 1))

;; (def (canvas-height canvas)
;;   (vector-ref canvas 2))

;; (def (canvas-size canvas)
;;   (vector-ref canvas 3))

;; (def (canvas-to-image canvas)
;;   (let* ((width (canvas-width canvas))
;; 	 (height (canvas-height canvas))
;; 	 (image (make-image width height)))
;;     (for (x 0 width)
;;       (for (y 0 height)
;; 	(image-set! image x y (canvas-get canvas x y))))
;;     image))

;; (def (image-set! image x y color)
;;   (let ((index (+ (* y (image-width image)) x)))
;;     (vector-set! (image-pixels image) index (color-red color))
;;     (vector-set! (image-pixels image) (+ index 1) (color-green color))
;;     (vector-set! (image-pixels image) (+ index 2) (color-blue color))))

;; (def (image-pixels image)
;;   (vector-ref image 0))

;; (def (image-width image)
;;   (vector-ref image 1))

;; (def (color-red color)
;;   (vector-ref color 0))

;; (def (color-green color)
;;   (vector-ref color 1))

;; (def (color-blue color)
;;   (vector-ref color 2))

;; (def (make-image width height)
;;   (let ((image (make-vector (+ (* width height) 2) 0)))
;;     (vector-set! image 0 (make-vector (* width height) 0))
;;     (vector-set! image 1 width)
;;     (vector-set! image 2 height)
;;     image)
;; )

;; (def (list-tabulate n f)
;;   (let lp ((i 0) (lst '()))
;;     (if (= i n)
;;       (reverse lst)
;;       (lp (+ i 1) (cons (f i) lst)))))

;; (def (fft x)
;;   "Cooley-Tukey fft"
;;   (let* ((N (length x))
;;          (half-N (quotient N 2)))
;;     (if (= N 1)
;;       x
;;       (let* ((even (fft (list-tabulate half-N (lambda (k) (list-ref x (* 2 k))))))
;;              (odd (fft (list-tabulate half-N (lambda (k) (list-ref x (+ 1 (* 2 k)))))))
;;              (combined (make-vector N)))
;;         (for (i 0 half-N)
;;           (let* ((t (exp (* -2.0 pi i 0+1i) (/ i N)))
;;                  (odd-term (* t (list-ref odd i))))
;;             (vector-set! combined i (+ (list-ref even i) odd-term))
;;             (vector-set! combined (+ i half-N) (- (list-ref even i) odd-term))))
;;         combined)))
;;   )

(def (read-numbers)
  (let loop ((numbers '()))
    (let ((input (read-line)))
      (if (eof-object? input)
        (reverse numbers)
        (loop (cons (string->number input) numbers))))))

(def (histogram lst)
  (let ((freq (hash)))
    (for-each
      (lambda (item)
	(hash-update! freq item (lambda (x) (+ x 1)) 0))
      lst)
    (hash->list freq)))

(def (skewness numbers)
  (let* ((n (length numbers))
         (mu (mean numbers))
         (sigma (standard-deviation numbers))
         (skew-sum (reduce + 0 (map (lambda (x) (expt (/ (- x mu) sigma) 3)) numbers))))
    (/ skew-sum n)))

(def (reduce f init lst)
  (foldl f init lst))
