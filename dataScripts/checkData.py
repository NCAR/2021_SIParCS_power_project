IDlist = []
m=open("jobIDlist", 'r')
for line in m:
	IDlist.append(line.strip())
m.close()

for jobID in IDlist:
	jobFile = jobID+".OU"
	check = []
	sum=0
	
	f=open(jobFile, 'r')
	for line in f:
		check.append(line)
	f.close()

	if len(check) < 10:
		print(jobFile, check[0])

	for i in range(3,len(check)-1):
		sum += int(check[i])

	if sum == 0:
		print(jobFile, check[0])
