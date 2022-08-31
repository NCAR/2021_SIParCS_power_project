#2021_SIParCS_power_project

The purpose of this project was to develop and impliment a methodology for gathering power/energy usage data during execution. 
The implimentation of this methodology is contained in the powerTool/scripts directory in this repository. 
To use the tool, copy the powerTool directory into your work directory, and add a line at the beginning of your batch script that runs the doPowerCollect.sh script in the powerTool/scripts directory.
You will need to have permission to run 'sudo ipmitool dcmi power reading' on the system you are using and you need to make sure the hardware running the job supports that command.
During execution, if not already extent, a new powerTool/data directory will be made that is populated by the raw output of the data collection tool.
After your jobs have finished, cd to powerTool/scripts, activate npl, and run ./cleanData.sh


Now in the powerTool/data directory there will be three kinds of files:
one file per node, containing instantaneous power readings for each second during execution with timestamps, called \<JobID\>\_\<nodename\>.csv
one file per job, containing the name of the job and one line for each node with the total time the node was active and the total energy used by the node, called \<JobID\>Data.csv
and a single Data.csv file, containing one line per job with the job id, the name of the job, the total wall time, and the total energy for the job


cleanData.sh: Runs data cleaning scripts for each job in jobIDlist.txt, in order: prepoutput.sh, fixData.py, then collateData.py. Also, for every job ID in failedJobIDlist.txt removes all data from the failed job

collateData.py: Gets per node data from <JobID>Data.csv, finds the largest wall time, sums the total energy, and writes this per job data to Data.csv

doPowerCollect.sh: Writes job ID to jobIDlist.txt, runs powerDataCollect.sh in the background on each node through ssh

fixData.py: Checks for and removes repeated data points, integrates over existing data to find total energy, finds total time, interpolates missing data points, writes interpolated data back to \<JobID\>\_\<nodename\>.csv, and writes totals to \<JobID\>Data.csv. Also checks for power reading failures, and writes job ID to failedJobIDlist.txt and prints the name of the job to standard out if found

fixTime.py: Converts reading times from datetime format to Unix timestamp

powerDataCollect.sh: Runs data collection command once a second

prepoutput.sh: Prepares raw output files for processing, removes unwanted lines from output and converts to csv format, then runs fixTime.py
