#!/bin/ env python
import csv
import numpy as np
import sys
from scipy import interpolate
from scipy import integrate

dataFile = sys.argv[1]
pwd = dataFile.rsplit("/",1)[0]
fileName = dataFile.rsplit("/",1)[1]
jID = fileName.split("_")[0]
jNode = fileName.split("_")[1].split(".")[0]
csvFile = open(dataFile)
data = np.loadtxt(csvFile, delimiter=",", dtype="int")
csvFile.close()

times = []
power = []
for j in range(len(data)):
    times.append(data[j][0])
    power.append(data[j][1])

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
    yout=open(pwd +"/"+ jID + "Data.csv", 'r')
    jName = yout.readline().strip()
    yout.close()
    print("dcmi power reading failure: "+ fileName.split(".")[0] +" - "+ jName)
    hout=open("failedJobIDlist.txt", 'a')
    hout.write(jID +'\n')
    hout.close()
else:
    gout=open(pwd +"/"+ jID + "Data.csv", 'a')
    gout.write(jNode +','+ str(totT) +','+ str(round(totJ)) +'\n')
    gout.close()
