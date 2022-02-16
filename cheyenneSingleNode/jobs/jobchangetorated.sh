#/bin/bash

#PBS -N changetorated
#PBS -A SCSG0001
#PBS -l walltime=00:00:30
#PBS -l select=cpufreq=rated
#PBS -q regular
#PBS -j oe
#PBS -k oe
#PBS -l select=1:ncpus=36:mpiprocs=36
#PBS -m abe
#PBS -M sdiamond@ucar.edu

export TMPDIR=/glade/scratch/$USER/temp
mkdir -p $TMPDIR

sleep 2

