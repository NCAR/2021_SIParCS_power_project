try:
    import influxdb
except:
    print("influx fail")

import sys
import urllib3
urllib3.disable_warnings()

if len(sys.argv) < 4:
	print("Usage: %s [influxhost] [influx username] [influx password] [Jobid]" % sys.argv[0])
	exit(1)

influxhost = sys.argv[1]
influxuser = sys.argv[2]
influxpass = sys.argv[3]
jobID = sys.argv[4]

influxIdle = 6598.75 #replace this with measured average idle for 35 nodes

dcmiFile = jobID+".chadmin1.ib0.cheyenne.ucar.edu.OU"
dcmiData = []

f=open(dcmiFile, 'r')
for line in f:
    dcmiData.append(line.strip())
f.close()

outputFile = dcmiData.pop(0)+"Data.csv"
IRUname = dcmiData.pop(0).partition("n")[0]+"c"

kStart = dcmiData.pop(0)
kEnd = dcmiData.pop(len(dcmiData)-1)

influx = influxdb.InfluxDBClient(host=influxhost, username=influxuser, password=influxpass, ssl=True, verify_ssl=False)
results = influx.query(database="ch", query="select \"Value\" from Pout_psu where \"IRU\" = '"+IRUname+"' and time > "+kStart+"s and time < "+kEnd+"s")

influxValues = []
influxTimes = []
for record in results.get_points():
    influxValues.append(record['Value'])
for record in results.get_points():
    influxTimes.append(record['time'])
influx.close()

j=0
valueSum=0
influxTimes.append(0)
timePoint = influxTimes[0]
influxData = []
while len(influxTimes) != 0:
    if influxTimes[0] == timePoint:
        valueSum += influxValues.pop(0)
        influxTimes.pop(0)
        j += 1
    else:
        influxData.append(((valueSum/j)*9)-influxIdle)
        valueSum = 0
        j = 0
        if influxTimes[0] == 0:
            influxTimes.pop(0)
        else:
            timePoint = influxTimes[0]

totalTime = int(kEnd) - int(kStart)
dcmiSum = 0
for d in dcmiData:
    dcmiSum += int(d)
dcmiAverage = dcmiSum/len(dcmiData)
dcmiTotal = dcmiAverage*totalTime

influxSum = 0
for i in influxData:
    influxSum += i
influxAverage = influxSum/len(influxData)
influxTotal = influxAverage*totalTime

fout=open(outputFile, 'a')
fout.write(str(dcmiTotal)+","+str(influxTotal)+","+str(totalTime)+"\n")
fout.close()
