#/bin/bash

#PBS -N waccmturbo
#PBS -A SCSG0001
#PBS -l walltime=00:02:30

#PBS -q regular
#PBS -o /glade/work/sdiamond/output
#PBS -e /glade/scratch/sdiamond/temp/error
#PBS -l select=1:ncpus=36:mpiprocs=36
#PBS -m abe
#PBS -M sdiamond@ucar.edu

export TMPDIR=/glade/scratch/$USER/temp
mkdir -p $TMPDIR

echo $PBS_JOBID >> /glade/work/sdiamond/output/jobIDlist
echo "waccmturbo"
hostname
echo -n "" >$TMPDIR/powerout$PBS_JOBID
while true; do sudo ipmitool dcmi power reading >>$TMPDIR/powerout$PBS_JOBID; sleep 1; done &
pid=$!
kstart=$(date +"%s")
mpiexec_mpt ./waccmkernel.exe >$TMPDIR/kernelout$PBS_JOBID
kend=$(date +"%s")
kill $pid

echo $kstart
grep Instant $TMPDIR/powerout$PBS_JOBID | rev | cut -d " " -f 2 | rev
echo $kend
