try:
    import influxdb
except:
    print("influx fail")

import sys
import urllib3
urllib3.disable_warnings()

if len(sys.argv) < 1:
	print("Usage: %s [Jobidlist]" % sys.argv[0])
	exit(1)

jobIDlist = sys.argv[1]

IDlist = []
m=open(jobIDlist, 'r')
for line in m:
	IDlist.append(line.strip())
m.close()

for jobID in IDlist:
 nodeFile = jobID+".OU"
 nodeData = []
 
 f=open(nodeFile, 'r')
 for line in f:
     nodeData.append(line.strip())
 f.close()
 
 outName = nodeData.pop(0)
 outputFile = "Data.csv"
 IRUname = nodeData.pop(0).partition("n")[0]+"c"
 
 kStart = nodeData.pop(0)
 kEnd = nodeData.pop(len(nodeData)-1)
 
 totalTime = int(kEnd) - int(kStart)
 nodeSum = 0
 for d in nodeData:
     nodeSum += int(d)
 nodeAverage = nodeSum/len(nodeData)
 nodeTotal = nodeAverage*totalTime

 fout=open(outputFile, 'a')
 fout.write(outName+","+str(nodeTotal)+",0,"+str(totalTime)+"\n")
 fout.close()
