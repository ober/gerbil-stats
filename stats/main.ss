;;; -*- Gerbil -*-
(import :std/error
        :std/sugar
        :std/getopt
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
  (error "Implement me!"))

;;; Implement this if your CLI has commands
(def (gerbil-stats-main/command cmd opt)
  (error "Implement me!"))
