#!/bin/bash

#conda activate npl

for file in `ls | grep "${1}_"`
do
  ./prepoutput.sh $file
  python3 fixData.py $file.csv
done
