;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/getopt
	:std/format
        ./lib)
(export main)

;; build manifest; generated during the build
;; defines version-manifest which you can use for exact versioning
(include "../manifest.ss")

(def (main . args)
  (call-with-getopt gerbil-stats-main args
    program: "gerbil-stats"
    help: "A one line description of your program"
    ;; commands/options/flags for your program; see :std/getopt
    ;; ...
    ))

(def* gerbil-stats-main
  ((opt)
   (gerbil-stats-main/options opt))
  ((cmd opt)
   (gerbil-stats-main/command cmd opt)))

;;; Implement this if your CLI doesn't have commands
(def (gerbil-stats-main/options opt)
  (let ((lst (read-numbers)))
    (displayln "mode: " (mode lst))
    (displayln "mean: " (mean lst))
    (displayln "sum: " (sum lst))
    (displayln "median: " (median lst))
    (displayln "variance: " (variance lst))
    (displayln "std-dev: " (standard-deviation lst))
    (displayln "kurtosis: " (kurtosis lst))
    (displayln "skew: " (skewness lst))
    (displayln "z-scores: " (z-scores lst))
    ;; (displayln "quantile 25%: " (quartile lst 0.25))
    ;; (displayln "box-plot-stats: " (box-plot-stats lst))
    ;; (displayln "fft: " (fft lst))
    (displayln "histogram: " (histogram lst))
    ))

;;; Implement this if your CLI has commands
(def (gerbil-stats-main/command cmd opt)
  (error "Implement me!"))
