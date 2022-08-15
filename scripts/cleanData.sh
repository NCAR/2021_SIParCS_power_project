#!/bin/bash

for jid in `cat jobIDlist.txt | uniq`
do
  for file in `ls ${jid}_*`
  do
    ./prepoutput.sh $file
    python3 fixData.py $file.csv
  done
  python3 collateData.py $jid
done

for jid in `cat failedJobIDlist.txt | uniq`
do
  sed -i "/$jid/d" Data.csv
  rm ${jid}*
done

rm jobIDlist.txt
rm failedJobIDlist.txt
touch jobIDlist.txt
touch failedJobIDlist.txt
