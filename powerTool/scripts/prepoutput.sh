#!/bin/bash

jobnode=$(echo $1 | sed 's/^[a-z]*//')

sed -i '/I/!d' $jobnode #delete lines without 'Instant' or 'IMPI timestamp'
sed -i 's/Jan/1/g;s/Feb/2/g;s/Mar/3/g;s/Apr/4/g;s/May/5/g;s/Jun/6/g;s/Jul/7/g;s/Aug/8/g;s/Sep/9/g;s/Oct/10/g;s/Nov/11/g;s/Dec/12/g' $jobnode #replace month name with number
sed -i 's/^[A-Za-z \t:]*//;s/[A-Za-z \t:]*$//' $jobnode #trim unwanted characters
paste -sd ',\n' $jobnode >> ${jobnode}temp #replace newlines with commas, combining reading with time
mv ${jobnode}temp $jobnode
sed -i 's/  /,/g;s/[ :]/,/g' $jobnode #change to full csv format
mv $jobnode $jobnode.csv #add suffix to file
