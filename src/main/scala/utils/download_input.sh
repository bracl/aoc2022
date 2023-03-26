#!/bin/bash

mkdir -p "/Users/bradleyking/dev/aoc2022/src/main/scala/input"
mkdir -p "/Users/bradleyking/dev/aoc2022/src/main/scala/solutions"

#DAY=$( date +"%d" | sed 's/^0*//' )
DAY="    "
echo "Pulling input for day $DAY"
OUTPUT="/Users/bradleyking/dev/aoc2022/src/main/scala/input/$DAY.txt"
EXAMPLE="/Users/bradleyking/dev/aoc2022/src/main/scala/input/$DAY.example.txt"
touch $EXAMPLE
echo "Writing to file: $OUTPUT"

URL="https://adventofcode.com/2022/day/$DAY/input"
SESSION="53616c7465645f5f074938c88e65ca656f144653908c6984cab736eadc55c61b7dcc4bf82f653922120962238dcfe6809a2e634cd1c12f8126c5d96e7bf3503b"

curl -A "@bracl via curl" --location --request GET "$URL" --header "Cookie: session=$SESSION" -o "$OUTPUT"
echo "Happy Solving :)"

DESTINATION="/Users/bradleyking/dev/aoc2022/src/main/scala/solutions/day$DAY.scala"
cp "/Users/bradleyking/dev/aoc2022/src/main/scala/utils/template" $DESTINATION
sed -i.bak s/DAY/$DAY/g $DESTINATION
sed -i.bak s/utils$/solutions/g $DESTINATION
rm $DESTINATION.bak
