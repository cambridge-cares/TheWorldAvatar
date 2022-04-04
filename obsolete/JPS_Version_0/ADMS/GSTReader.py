import time
import math

## This piece of python script converts the output of ADMS in gst format into json format 
## which can be directly visualized as contour maps in plotly.js


start_time = time.time()
path = 'test.gst' # set the gst file to convert to json format 
with open(path) as f:
	lines = f.readlines()
counter = 0
rows = []
row = []

day = '186'
hour = '13'

currentDay = '186'

dayNo = 0
for line in lines[1:]:
	if(day==currentDay):
		string = ''
		for char in line:
			string = string + char
		item = string.split(',')[7].strip()
		day =  string.split(',')[1].strip()
		hour =  string.split(',')[2].strip()
		
		print('day',day)
		print('hour',hour)
		
		item = float(item);
		row.append(item);
		counter = counter + 1
		if(counter%201 == 0):
			rows.append(row)
			row = []
	
	else:
		#print('-------'+line[5]);
		string = ''
		for char in line:
			string = string + char
			#print(char)
		if(float(string.split(',')[7].strip()) == 0):
			item = '0'
		else:
			item = str(math.log10(float(string.split(',')[7].strip())))
		day =  string.split(',')[1].strip()
		hour =  string.split(',')[2].strip()
		
		print('day',day)
		print('hour',hour)
		currentDay = day
		dayNo = dayNo + 1 

	
resultFile = open('result.json', 'w') # start to write to json file
index = 0
resultFile.write('[')
for row in (rows):
	#print(row)
	if(index == 0 ):
		
		resultFile.write("%s\n" %row + ',' )
	elif (index == len(rows) - 1):
		resultFile.write("%s\n" %row + ']' )
	else:
		resultFile.write("%s\n" %row + ',' )
	index = index + 1


counter = 0
rows = []
row = []
		



