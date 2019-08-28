#!/bin/bash

set -x

for f in *.gz
do
  gunzip -c $f > $(basename $f .gz)
done
