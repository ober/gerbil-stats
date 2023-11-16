#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/build-script)

(defbuild-script
  '("stats/lib"
    (exe: "stats/main" bin: "stats")))
