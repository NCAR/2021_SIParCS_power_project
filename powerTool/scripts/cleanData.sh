#!/bin/bash

datadir=$(echo $PWD | sed 's%scripts%data%')
mkdir -p $datadir
touch $datadir/Data.csv
touch $PWD/failedJobIDlist.txt
for jid in `cat $PWD/jobIDlist.txt | uniq`
do
  for file in `ls $datadir/${jid}_*`
  do
    ./prepoutput.sh $file
    python3 fixData.py $file.csv
  done
  python3 collateData.py $datadir $jid
done

for jid in `cat $PWD/failedJobIDlist.txt | uniq`
do
  sed -i "/$jid/d" $datadir/Data.csv
  rm $datadir/${jid}*
done

rm $PWD/jobIDlist.txt
rm $PWD/failedJobIDlist.txt
