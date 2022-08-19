#!/bin/bash
hostid=$(hostname)
datadir=$(echo $PWD | sed 's%scripts%data%')
while true; do sudo ipmitool dcmi power reading >> $datadir/${1}_$hostid; sleep 1; done 
