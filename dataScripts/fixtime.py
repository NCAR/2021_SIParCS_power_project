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

print(data)

newData = np.zeros((len(data),2), dtype="int")
for j in range(0, len(data)):
    dt = mount.localize(datetime.datetime(data[j][6],data[j][1],data[j][2],data[j][3],data[j][4],data[j][5]))
    utcdt = dt.astimezone(pytz.utc)
    timestamp = int(utcdt.timestamp())
    newData[j] = [data[j][0], timestamp]

print(newData)
