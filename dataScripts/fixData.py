#!/bin/ env python
import datetime
import pytz
from pytz import timezone
import csv
import numpy as np
import sys

mount = timezone('US/Mountain')
dataFile = sys.argv[1]
csvfile = open(dataFile)
data = np.loadtxt(csvfile, delimiter=",", dtype="int")

fout=open(dataFile, 'w')
for j in range(0, len(data)):
    dt = mount.localize(datetime.datetime(data[j][6],data[j][1],data[j][2],data[j][3],data[j][4],data[j][5]))
    utcdt = dt.astimezone(pytz.utc)
    timestamp = int(utcdt.timestamp())
    fout.write(str(data[j][0])+','+str(timestamp)+'\n')
fout.close()
