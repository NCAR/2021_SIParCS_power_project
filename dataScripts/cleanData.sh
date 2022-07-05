#!/bin/bash
jobnode=$(echo $1 | sed 's/^[a-z]*//')

#conda activate npl
./prepoutput.sh powerout$jobnode
python3 fixData.py $jobnode
