#!/usr/bin/env bash
# Compare every translation's output with its MLton original, at the SML's
# own iteration counts
cd /home/benjamin/projects/webs-hackage-bench/sml
for x in "Ack ack 1" "Tak tak 1" "Cpstak cpstak 1" "YFact y-fact 100000" "Nqueens nqueens 1" \
         "Mandelbrot mandelbrot 1" "Quicksort quicksort 1000" "Filter filter 10000" \
         "Derivative derivative 10000" "Streams streams 100000" "Tardis tardis 10000"; do
  tools/check.sh $x
done
