#!/bin/bash

./main_simple4_actke.CPU.x  > diff.CPU.txt 2>&1 
./main_simple4_actke.GPU.x  > diff.GPU.txt 2>&1 
vim -d diff.?PU.txt 

