files = ["waccmdefaultData.csv", "waccmratedData.csv", "waccmslowData.csv", "mg2ratedData.csv", "mg2defaultData.csv", "mg2slowData.csv", "clubbdefaultData.csv", "clubbratedData.csv", "clubbslowData.csv"]

for f in files:
	s=open(f, 'w')
	s.write("dcmiTotal, influxTotal, totalTime\n")
	s.close()
