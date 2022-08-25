#!/bin/ env python
import datetime
import pytz
from pytz import timezone
import csv
import sys

mount = timezone('US/Mountain')
dataFile = sys.argv[1]
pwd = dataFile.rsplit("/",1)[0]
fileName = dataFile.rsplit("/",1)[1]
jID = fileName.split("_")[0]
jNode = fileName.split("_")[1].split(".")[0]
csvFile = open(dataFile)
data = np.loadtxt(csvFile, delimiter=",", dtype="int")
csvFile.close()

power = []
times = []
for j in range(len(data)):
    dt = mount.localize(datetime.datetime(data[j][6],data[j][1],data[j][2],data[j][3],data[j][4],data[j][5]))
    utcdt = dt.astimezone(pytz.utc)
    times.append(int(utcdt.timestamp()))
    power.append(data[j][0])

fout=open(dataFile, 'w')
for k in range(len(power)):
    fout.write(str(times[k])+','+str(power[k])+'\n')
fout.close()
