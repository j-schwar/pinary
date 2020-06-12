#!/bin/bash

if [[ -z $1 ]]; then
  echo "Please supply an input file"
  exit 1
fi

FILE="$1"
ORIGINAL_SIZE=$(wc -c "$FILE" | awk '{ print $1 }')

printf "%s - %d bytes\n" $FILE $ORIGINAL_SIZE

RADICES=(10 16 32 64 94)

function ptc {
  echo "scale = 2; $1 * 100 / $2" | bc
}

function encode {
  local SIZE=$(pinary encode --base $1 $FILE | wc -c)
  local PERCENT=$(ptc $SIZE $ORIGINAL_SIZE)
  printf "radix: %3s, size: %6d bytes (%6.2f %%)\n" $1 $SIZE $PERCENT
}

for RADIX in ${RADICES[@]}; do
  encode $RADIX
done
