#/bin/bash

#PBS -N mg2extraturbo
#PBS -A SCSG0001
#PBS -l walltime=00:03:00

#PBS -q regular
#PBS -o /glade/work/sdiamond/output
#PBS -e /glade/scratch/sdiamond/temp/error
#PBS -l select=1:ncpus=36:mpiprocs=36
#PBS -m abe
#PBS -M sdiamond@ucar.edu

export TMPDIR=/glade/scratch/$USER/temp
mkdir -p $TMPDIR

cat /proc/cpuinfo > /glade/work/sdiamond/infoout/info1$PBS_JOBID
echo "mg2extraturbo"
hostname
echo -n "" >$TMPDIR/powerout$PBS_JOBID
cat /proc/cpuinfo > /glade/work/sdiamond/infoout/info2$PBS_JOBID
while true; do sudo ipmitool dcmi power reading >>$TMPDIR/powerout$PBS_JOBID; sleep 1; done &
cat /proc/cpuinfo > /glade/work/sdiamond/infoout/info3$PBS_JOBID
pid=$!
kstart=$(date +"%s")
cat /proc/cpuinfo > /glade/work/sdiamond/infoout/info4$PBS_JOBID
sleep 15
cat /proc/cpuinfo > /glade/work/sdiamond/infoout/info5$PBS_JOBID
mpiexec_mpt ./mg2kernel.exe >$TMPDIR/kernelout$PBS_JOBID
sleep 15
kend=$(date +"%s")
kill $pid

echo $kstart
grep Instant $TMPDIR/powerout$PBS_JOBID | rev | cut -d " " -f 2 | rev
echo $kend
