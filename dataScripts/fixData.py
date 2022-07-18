#!/bin/ env python
import datetime
import pytz
from pytz import timezone
import csv
import numpy as np
import sys
from scipy import interpolate

mount = timezone('US/Mountain')
dataFile = sys.argv[1]
csvfile = open(dataFile)
data = np.loadtxt(csvfile, delimiter=",", dtype="int")

power = []
times = []
for j in range(0, len(data)):
    dt = mount.localize(datetime.datetime(data[j][6],data[j][1],data[j][2],data[j][3],data[j][4],data[j][5]))
    utcdt = dt.astimezone(pytz.utc)
    times.append(int(utcdt.timestamp()))
    power.append(data[j][0])

spl = interpolate.splrep(times,power)
check = 0
while check == 0:
    check = 1
    for t in range(1,len(times)-1):
        if (times[t]-1) != times[t-1]:
            times.insert(t,times[t-1]+1)
            power.insert(t,interpolate.splev(times[t-1]+1, spl))
            check = 0

fout=open(dataFile, 'w')
for k in range(0, len(power)):
    fout.write(str(times[k])+','+str(power[k])+'\n')
fout.close()
