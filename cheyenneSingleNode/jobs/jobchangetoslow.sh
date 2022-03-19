#/bin/bash

#PBS -N changetoslow
#PBS -A SCSG0001
#PBS -l walltime=00:00:30
#PBS -q regular
#PBS -j oe
#PBS -k oe
#PBS -l select=1:ncpus=36:mpiprocs=36:cpufreq=slow
#PBS -m abe
#PBS -M sdiamond@ucar.edu

export TMPDIR=/glade/scratch/$USER/temp
mkdir -p $TMPDIR

sleep 2

