#!/usr/bin/env bash
# check.sh Name sml-name [N]: compile progs/Name.hs (plain -O2), run it with
# iteration count N (default: its own), compare with the MLton executable's
# output (GC summary lines dropped)
B=/home/benjamin/projects/webs-hackage-bench/sml
S=/home/benjamin/projects/3cps/3cps-main/test/programs/llvm-sml
G=/home/benjamin/projects/ghc-webs/_build/stage1/bin/ghc
O=$B/tools/out; mkdir -p $O
cd $B/progs
$G -O2 -rtsopts -i. $1.hs -outputdir $O/$1 -o $O/$1.exe > $O/$1.compile 2>&1 || { echo "$1: COMPILE ERROR"; grep -A5 error $O/$1.compile | head -15; exit 1; }
/usr/bin/time -f "%e s %M KB" -o $O/$1.time $O/$1.exe $3 > $O/$1.hs.out 2>&1
timeout 120 $S/$2.mlton.out > $O/$1.ml.out 2>&1
grep -vE "^(Major|Minor) GCs:|Bytes Allocated:|^GC time|^max|^total|^num |^bytes |^\*+ round [0-9]+ \*+$" $O/$1.ml.out > $O/$1.ml.clean
if cmp -s $O/$1.hs.out $O/$1.ml.clean; then echo "$1: matches MLton ($(wc -l < $O/$1.hs.out) lines) $(cat $O/$1.time)"
# Programs that print per round print one round's lines: compare the distinct lines
elif cmp -s <(sort -u $O/$1.hs.out) <(sort -u $O/$1.ml.clean); then echo "$1: matches MLton (distinct lines; $(wc -l < $O/$1.hs.out) vs $(wc -l < $O/$1.ml.clean)) $(cat $O/$1.time)"
else echo "$1: DIFFERS from MLton $(cat $O/$1.time)"; diff <(sort $O/$1.hs.out | uniq -c) <(sort $O/$1.ml.clean | uniq -c) | head -6; fi
