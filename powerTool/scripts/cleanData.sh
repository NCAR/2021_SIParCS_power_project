#!/bin/bash

datadir=$(echo $PWD | sed 's%scripts%data%')
touch $datadir/Data.csv
touch failedJobIDlist.txt
for jid in `cat jobIDlist.txt | uniq`
do
  for file in `ls $datadir/${jid}_*`
  do
    ./prepoutput.sh $file
    python3 fixData.py $file.csv
  done
  python3 collateData.py $datadir $jid
done

for jid in `cat failedJobIDlist.txt | uniq`
do
  sed -i "/$jid/d" $datadir/Data.csv
  rm $datadir/${jid}*
done

rm jobIDlist.txt
rm failedJobIDlist.txt
