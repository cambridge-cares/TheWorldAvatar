import os
import re
		
# Author: Zhou Xiaochi
# This simple search function demonstrates 
#	- how to find a pattern using regular expression 
#	- how to iterate through all lines in a file and how to modify a certain line
	
def search(filename, pattern):
	with open(filename) as file:
		tempLines = file.readlines()
		for idx,line in enumerate(tempLines):
			
			m = re.search('www.theworldavatar.com/BMS',line)
			if m:
				pattern = m.group(0)
				print('\tpattern: ', pattern)
				line = line.replace(pattern,'bms.theworldavatar.com/BMS')
				tempLines[idx] = line
				
		file.close()
	with open(filename, 'w') as file:
		for newLine in tempLines:
			file.write(newLine)
		file.close()

			
# This block iterate through all the owl files within the current folder('./')
# Python's default os library also support more complicated actions such as iteratively go through all subfolders of a folder
 			
for filename in os.listdir('./'):
	if filename.endswith(".owl"): 
		print(filename)	
		search(filename, 'nothing')
		continue
	else:
		continue
