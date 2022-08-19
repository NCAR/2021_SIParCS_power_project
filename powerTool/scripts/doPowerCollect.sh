#!/bin/bash

powerdir=/glade/work/$USER/powerTool
PID=`echo  $PBS_JOBID | head -c 7`
echo $PBS_JOBNAME > $powerdir/data/${PID}Data.csv
echo $PID >> $powerdir/scripts/jobIDlist.txt
for node in `cat $PBS_NODEFILE | uniq`; do ssh -f $node "$powerdir/scripts/powerDataCollect.sh $powerdir/data/$PID" ; done
