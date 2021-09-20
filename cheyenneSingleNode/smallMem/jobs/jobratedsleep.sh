#/bin/bash

#PBS -N sleeprated
#PBS -A SCSG0001
#PBS -l walltime=00:05:00
#PBS -l select=cpufreq=rated
#PBS -q regular
#PBS -o /glade/work/sdiamond/output
#PBS -e /glade/scratch/sdiamond/temp/error
#PBS -l select=1:nodetype=smallmem:ncpus=36:mpiprocs=36
#PBS -m abe
#PBS -M sdiamond@ucar.edu

export TMPDIR=/glade/scratch/$USER/temp
mkdir -p $TMPDIR

echo "sleeprated"
hostname
echo -n "" >$TMPDIR/powerout$PBS_JOBID
while true; do sudo ipmitool dcmi power reading >>$TMPDIR/powerout$PBS_JOBID; sleep 1; done &
pid=$!
kstart=$(date +"%s")
sleep 180
kend=$(date +"%s")
kill $pid

echo $kstart
grep Instant $TMPDIR/powerout$PBS_JOBID | rev | cut -d " " -f 2 | rev
echo $kend