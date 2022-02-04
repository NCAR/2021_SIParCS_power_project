IDlist = []
m=open("jobIDlist", 'r')
for line in m:
	IDlist.append(line.strip())
m.close()

for jobID in IDlist:
	jobFile = jobID+".OU"
	
	check = []
	f=open(jobFile, 'r')
	for line in f:
		check.append(line)
	f.close()

	if len(check) < 5:
		print(jobFile, check[0])

