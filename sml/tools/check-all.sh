#!/usr/bin/env bash
# Compare every translation's output with its MLton original, at the SML's
# own iteration counts
cd /home/benjamin/projects/webs-hackage-bench/sml
# MLton references built from tools/mlton/*.sml
for f in tools/mlton/*.sml tools/mlton/ho/*.sml; do
  [ "${f%.sml}.mlton" -nt "$f" ] || ~/lib/mlton/build/bin/mlton -output "${f%.sml}.mlton" "$f"
done
for x in "Ack ack 1" "Tak tak 1" "Cpstak cpstak 1" "YFact y-fact 100000" "Nqueens nqueens 1" \
         "Mandelbrot mandelbrot 1" "Quicksort quicksort 1000" "Filter filter 10000" \
         "Derivative derivative 10000" "Streams streams 100000" "Tardis tardis 10000" \
         "Interpreter interpreter 10000" "ParserComb parser-comb 10000" \
         "TwentyFour twenty-four 10000" "JsonDecode json-decode 100001" "CpsConvert cps-convert 100000" \
         "KnuthBendix knuth-bendix 2" "Raytracer raytracer 1" \
         "RatioRegions ratio-regions 1 $PWD/tools/mlton/ratio-regions.mlton" \
         "KCfa k-cfa 10001" "McRay mc-ray 1"; do
  tools/check.sh $x
done
# higher-order/: MLton references built from tools/mlton/ho (same loop counts)
for x in Defunctionalization:defunctionalization ReturnedClosures:returned-closures \
         FunctionComposition:function-composition MutualRecursionHo:mutual-recursion-ho \
         TreeFoldHo:tree-fold-ho CurryUncurry:curry-uncurry ClosureCapture:closure-capture \
         HigherOrderList:higher-order-list ChurchEncoding:church-encoding CpsExplicit:cps-explicit \
         CallbackState:callback-state EnvironmentSharing:environment-sharing \
         PolymorphicHo:polymorphic-ho ContinuationCombinators:continuation-combinators \
         ApplicativeFunctor:applicative-functor TreeTraversalHo:tree-traversal-ho \
         RegisterAllocator:register-allocator DpllSat:dpll-sat; do
  tools/check.sh ${x%%:*} ${x#*:} "" $PWD/tools/mlton/ho/${x#*:}.mlton
done
# from-mlton/: references built from tools/mlton/<name>.sml (MLton's own
# programs with a driver that prints a result), compared at N = 1
for x in Zebra:zebra Logic:logic SmithNormalForm:smith-normal-form Tsp:tsp; do
  tools/check.sh ${x%%:*} ${x#*:} 1 $PWD/tools/mlton/${x#*:}.mlton
done
