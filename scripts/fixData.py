#!/bin/ env python
import datetime
import pytz
from pytz import timezone
import csv
import numpy as np
import sys
from scipy import interpolate
from scipy import integrate

mount = timezone('US/Mountain')
dataFile = sys.argv[1]
jID = dataFile.split("_")[0]
jNode = dataFile.split("_")[1].split(".")[0]
csvFile = open(dataFile)
data = np.loadtxt(csvFile, delimiter=",", dtype="int")

power = []
times = []
for j in range(len(data)):
    dt = mount.localize(datetime.datetime(data[j][6],data[j][1],data[j][2],data[j][3],data[j][4],data[j][5]))
    utcdt = dt.astimezone(pytz.utc)
    times.append(int(utcdt.timestamp()))
    power.append(data[j][0])

cTimes = times.copy()
m = 0
count = 0
fault = 0
for n in range(len(cTimes)-1):
    if cTimes[n] == cTimes[n+1]:
        fault = cTimes[n]
        count = cTimes.count(fault)
        for r in range(count-1):
            times.pop(n-m)
            power.pop(n-m)
        m += count

totJ = integrate.simps(power,times)
totT = times[len(times)-1] - times[0]

spl = interpolate.splrep(times,power)
check = 0
while check == 0:
    check = 1
    for t in range(len(times)-1):
        if (times[t]+1) != times[t+1]:
            times.insert(t+1,times[t]+1)
            power.insert(t+1,interpolate.splev(times[t]+1, spl))
            check = 0

fout=open(dataFile, 'w')
for k in range(len(power)):
    fout.write(str(times[k])+','+str(round(float(power[k])))+'\n')
fout.close()

if totJ == 0:
    print("dcmi power reading failure: "+ dataFile.split(".")[0])
    hout=open("failedJobIDlist.txt", 'a')
    hout.write(jID +'\n')
    hout.close()
else:
    gout=open(jID + "Data.csv", 'a')
    gout.write(jNode +','+ str(totT) +','+ str(round(totJ)) +'\n')
    gout.close()
