files = ["waccmturboData.csv", "waccmratedData.csv", "waccmslowData.csv", "mg2ratedData.csv", "mg2turboData.csv", "mg2slowData.csv", "clubbturboData.csv", "clubbratedData.csv", "clubbslowData.csv"]

for f in files:
	s=open(f, 'w')
	s.write("nodeTotal, psuTotal, totalTime\n")
	s.close()
