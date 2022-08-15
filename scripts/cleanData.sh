#!/bin/bash

#conda activate npl

for jid in `cat jobIDlist.txt | uniq`
do
  for file in `ls | grep "${jid}_"`
  do
    ./prepoutput.sh $file
    python3 fixData.py $file.csv
  done
  python3 collateData.py $jid
done
