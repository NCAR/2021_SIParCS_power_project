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
tSum = 0
pSum = 0
n = 0

for k in data:
    tSum += int(k.split(",")[1])
    pSum += int(k.split(",")[2])
    n += 1

tAve = round(tSum/n)

f=open("Data.csv", 'a')
f.write(jName +','+ str(tAve) +','+ str(pSum) +'\n')
f.close()
