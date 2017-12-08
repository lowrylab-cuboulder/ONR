#Script to process files for use in analysis software
#input file needs format: date (YYYY/MM/DD) time(HH:MM:SS), temp., (act.)
#output files:
#	FILE_proc.txt: same format as above, but 30 second sampling is removed, and experiment day is added
#	FILE_act.awd: activity file prepped for CL
#	FILE_temp.awd: temperature file prepped for CL
#
#in version 3 added ZT 
#	it's important that there is no missing time data. There is no built in
#	mechanism to match zt with real time. It just assumes that every line is
#	another minute.
#
#in version 3.1
#	corrected the some errors wirting the 'proc' file
#	added an option to run as batch operation

import sys
import datetime
from os import listdir
from os import system

if len(sys.argv) > 1:
	mfile = sys.argv[1]
	batch = 0
else:
	batch = 1
eday = int(input('enter experiment start day: '))-1 #lazy way to avoid the fact that when writing starts eday is incrimented
ztStart = input('enter ZT start (NN:NN:NN):')
######need to customize this
#sdate = input('enter start date in format dd-mon-yyyy: ')

#make the start date a list object and return datetime object
def getDate(fname):
	f = open(fname, 'r')
	for line in f:
		if ztStart in line:
			sdate = line.split(',')[0][0:10]
			break
	sdateList = sdate.split('/')
	zt = datetime.datetime(int(sdateList[0]),int(sdateList[1]),int(sdateList[2]),0,0,0)
	f.close()
	return zt

#make a file thats useful for R or other analysis tools
def writeProcFile(fname, ztStart, zt, eday):
	f=open(fname,'r')
	of=open(fname[:-4]+'_proc.csv','w')
	of.write('eday,date,real_time,zt,temp,act\n')
	writeToken = 0
	for line in f:
		lDate = line.split(',')[0]
		if line[11:19] == ztStart:
			eday += 1
			writeToken = 1
		if writeToken == 1 and lDate.split(' ')[1][6:] == '00': #this filters out recordings on fractions of minutes
			of.write(str(eday)+',')
			of.write(lDate.split(' ')[0]+','+lDate.split(' ')[1])
			of.write(','+zt.time().__str__())
			zt = zt+datetime.timedelta(minutes=1)
			for i in range(1,len(line.split(','))):
				of.write(','+line.split(',')[i].lstrip())
	f.close()
	of.close()

#make files for ClockWork
def writeHeader(file,fname,time,zt):
	file.write(fname[:-4]+'\n')
	file.write(zt.day.__str__()+'/'+zt.month.__str__()+'/'+zt.year.__str__()+'\n')
	file.write(time+'\n')
	file.write(str(4)+'\n')
	file.write(str(100)+'\n')
	file.write(fname[:-4]+'\n')
	file.write('M'+'\n')
	
def CLwriter(fname, zt):
	f = open(fname[:-4]+'_proc.csv','r')
	lines=[]
	for line in f:
		lines.append(line)
	time = lines[1].split(',')[2]
	if len(lines[0].split(',')) == 6:
		tempf = open(fname[:-4]+'_temp.AWD','w')
		writeHeader(tempf,fname,time,zt)
		actf = open(fname[:-4]+'_act.AWD','w')
		writeHeader(actf,fname,time,zt)
	elif len(lines[0].split(',')) == 5:
		tempf = open(fname[:-4]+'_temp.AWD','w')
		writeHeader(tempf,fname,time,zt)
	else:
		sys.exit('something weird happened to the processed file')

	for i in range(1,len(lines)):
		tempf.write(lines[i].split(',')[4]+'\n')
		if 'actf' in vars():
			actf.write(lines[i].split(',')[5])
	f.close()
	tempf.close()
	try:
		actf.close()
	except NameError:
		print('looks like there is no activity data')
		
#concatenate all the data
#format: eday, zt, M[NN].act, M[NN].temp
def makeMaster():
	csvArray = [f for f in listdir() if '.csv' in f and 'Mouse' in f]
	for i in range(0,len(csvArray)):
		if i == 0:
			with open('tmp1.csv','w') as tmp1:
				id = csvArray[i][5:7]
				tmp1.write('eday,zt,'+id+'_temp,'+id+'_act\n')
				with open(csvArray[i], 'r') as f:
					next(f)
					for line in f:
						lines = line.split(',')
						oline = lines[0]
						for j in [3,4,5]:
							oline += ',' + lines[j]
						tmp1.write(oline)
		else:
			with open('tmp1.csv','r') as tmp1, open('tmp2.csv','w') as tmp2, open(csvArray[i],'r') as f:
				print('working on '+csvArray[i])
				next(f)
				id = csvArray[i][5:7]
				tmp2.write(next(tmp1)[:-1]+','+id+'_temp,'+id+'_act\n')
				for line in f:
					oline = str()
					lines = line.split(',')
					for j in [4,5]:
						oline += ',' + lines[j]
					try:
						tmp2.write(next(tmp1)[:-1]+oline)
					except StopIteration:
						pass
			with open('tmp1.csv','w') as tmp1, open('tmp2.csv','r') as tmp2:
				for line in tmp2:
					tmp1.write(line)		
	with open('tmp1.csv','r') as tmp1, open('Master.csv','w') as fmaster:
		for line in tmp1:
			fmaster.write(line)

def main(mfile):
	zt = getDate(mfile)
	writeProcFile(mfile,ztStart,zt,eday)
	CLwriter(mfile,zt)

if batch == 1:
	rawArray = [f for f in listdir() if f[-3:] == 'txt']
	for mfile in rawArray:
		main(mfile)
	makeMaster()
	system('del tmp1.csv, tmp2.csv')
else:
	main(mfile)
	if input("do you want to make a master file? (y/n)") == 'y':
		makeMaster()
		system('del tmp1.csv, tmp2.csv')

