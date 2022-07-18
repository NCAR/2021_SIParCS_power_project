#!/bin/bash
jobnode=$1

#conda activate npl
./prepoutput.sh $jobnode
python3 fixData.py $jobnode
