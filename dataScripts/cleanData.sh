#!/bin/bash

#conda activate npl

for file in `ls | grep $1`; do ./prepoutput.sh $file; python3 fixData.py $file.csv; done
