#!/bin/bash
dist/build/bench/bench --list | fgrep strict | fgrep -v Char8 | xargs dist/build/bench/bench -r comparison.csv
(head -n 1 comparison.csv && tail -n +2 comparison.csv | sort -n -t, -k 3) > sorted_comparison.csv
rm comparison.csv
