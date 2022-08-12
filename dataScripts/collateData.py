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
nSum = 0

for n in data:
    nSum += int(n.split(",")[1])

f=open("Data.csv", 'a')
f.write(jName +','+ str(nSum) +'\n')
f.close()
