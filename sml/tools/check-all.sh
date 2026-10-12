#!/usr/bin/env bash
# Compare every translation's output with its MLton original, at the SML's
# own iteration counts
cd /home/benjamin/projects/webs-hackage-bench/sml
# MLton references built from tools/mlton/*.sml
for f in tools/mlton/*.sml; do
  [ "${f%.sml}.mlton" -nt "$f" ] || ~/lib/mlton/build/bin/mlton -output "${f%.sml}.mlton" "$f"
done
for x in "Ack ack 1" "Tak tak 1" "Cpstak cpstak 1" "YFact y-fact 100000" "Nqueens nqueens 1" \
         "Mandelbrot mandelbrot 1" "Quicksort quicksort 1000" "Filter filter 10000" \
         "Derivative derivative 10000" "Streams streams 100000" "Tardis tardis 10000" \
         "Interpreter interpreter 10000" "ParserComb parser-comb 10000" \
         "TwentyFour twenty-four 10000" "JsonDecode json-decode 100001" "CpsConvert cps-convert 100000" \
         "KnuthBendix knuth-bendix 2" "Raytracer raytracer 1" \
         "RatioRegions ratio-regions 1 tools/mlton/ratio-regions.mlton"; do
  tools/check.sh $x
done
