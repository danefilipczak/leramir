#!/bin/sh

echo "Running JVM tests"
clojure -X:jvm-test :dirs "[\"test\" \"src\"]" :patterns "[\".*\"]"
