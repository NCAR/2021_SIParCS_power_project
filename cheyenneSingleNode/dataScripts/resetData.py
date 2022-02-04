files = ["waccmturbolargeData.csv", "waccmratedlargeData.csv", "waccmslowlargeData.csv", "mg2ratedlargeData.csv", "mg2turbolargeData.csv", "mg2slowlargeData.csv", "clubbturbolargeData.csv", "clubbratedlargeData.csv", "clubbslowlargeData.csv","sleepslowlargeData.csv","sleepratedlargeData.csv","sleepturbolargeData.csv","waccmturbosmallData.csv", "waccmratedsmallData.csv", "waccmslowsmallData.csv", "mg2ratedsmallData.csv", "mg2turbosmallData.csv", "mg2slowsmallData.csv", "clubbturbosmallData.csv", "clubbratedsmallData.csv", "clubbslowsmallData.csv","sleepslowsmallData.csv","sleepratedsmallData.csv","sleepturbosmallData.csv","waccmslowData.csv","waccmratedData.csv","waccmturboData.csv","clubbslowData.csv","clubbratedData.csv","clubbturboData.csv","mg2slowData.csv","mg2ratedData.csv","mg2turboData.csv","sleepslowData.csv","sleepratedData.csv","sleepturboData"]

for f in files:
	s=open(f, 'w')
	s.write("")
	s.close()

j=open("jobIDlist", 'w')
j.write("")
j.close()
