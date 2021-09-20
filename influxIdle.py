try:
    import influxdb
except:
    print("influx fail")

import sys
import urllib3
urllib3.disable_warnings()

if len(sys.argv) < 6:
    print("Usage: %s [influxhost] [influx username] [influx password] [IRUname] [Start time] [End time]" % sys.argv[0])
    exit(1)

influxhost = sys.argv[1]
influxuser = sys.argv[2]
influxpass = sys.argv[3]
IRUname = sys.argv[4]
kStart = sys.argv[5]
kEnd = sys.argv[6]

influx = influxdb.InfluxDBClient(host=influxhost, username=influxuser, password=influxpass, ssl=True, verify_ssl=False)
results = influx.query(database="ch", query="select \"Value\" from Pout_psu where \"IRU\" = '"+IRUname+"' and time > "+kStart+"s and time < "+kEnd+"s")

influxValues = []
influxTimes = []
for record in results.get_points():
    influxValues.append(record['Value'])
for record in results.get_points():
    influxTimes.append(record['time'])

print(influxValues)

j=0
k=0
valueSum = 0
timePoint = influxTimes[0]
influxData = []
while len(influxTimes) != 0:
    if influxTimes[0] == timePoint:
        valueSum += influxValues.pop(0)
        influxTimes.pop(0)
        j += 1
    else:
        timePoint = influxTimes[0]
        influxData.append((valueSum/j)*9)
        valueSum = 0
        k += 1
        j = 0

influx.close()

influxSum = 0
for i in influxData:
    influxSum += i
influxAverage = influxSum/len(influxData)

influx35Average = (influxAverage/36)*35

print(influxAverage, influx35Average)
