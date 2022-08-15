#!/bin/ env python
import csv
import numpy as np
import sys

jID = sys.argv[1]
datafile = jID+"Data.csv"

data = []
m=open(datafile, 'r')
for line in m:
     data.append(line.strip())
m.close()

jName = data.pop(0)
tMax = 0
pSum = 0
n = 0

for k in data:
    newT = int(k.split(",")[1])
    if newT > tMax:
        tMax = newT
    pSum += int(k.split(",")[2])
    n += 1

f=open("Data.csv", 'a')
f.write(jName +','+ jID +','+ str(tMax) +','+ str(pSum) +'\n')
f.close()
