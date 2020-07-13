import sys
import re
from urllib.parse import urlencode, quote_plus, quote
import urllib.request
import json
import requests
import re
import os
# inputs are 
#	filename

#　step 1: choose map from type

	
	

	
def readAttributesFromOwlFile(filename):

	api =  "http://www.theworldavatar.com:82/getAttrList?"
# ======================== Read Arg 1 =========================
	para = {'uri': 'http://www.theworldavatar.com/OntoEN/' + filename + '.owl'}
	request = requests.post(api,json = para)
	array = json.loads(request.text)
	newArray = sorted(array, key=lambda k: k['name']) 
	
	result = {}
	for item in newArray:
		name = item['name']
		value = item['value']
		result[name] = value
	
	return result
	
	
#def formNewLine(IRIs,)	



def identifyType(filename):

	if(re.fullmatch('EBus-[0-9]*',filename)):
		type = 'Bus'
		
	elif(re.fullmatch('MGDL-[0-9]*',filename)):
		type = 'Branch'
	elif(re.fullmatch('EGEN-[0-9]*',filename)):
		type = 'Gen'
	else:
		type = ''
	return type


def readCVSMap(csvFilename,index):
	results = []
	with open(csvFilename) as file:
		templates = file.readlines()[1].split(',')
		counter_ = indexToSkip
		for template in templates:
			result = {'name':template%index,'index': counter_}
			results.append(result)
			counter_ = counter_ + 1
			
	return results
		
		
def createNewLine(IRIs,results):

	stringArray = []

	for IRI in IRIs[indexToSkip:]:
		key = IRI['name']
		value = (results[key])
		index = IRI['index']

		
		stringArray.append(value)
	
	string = '\t'.join(stringArray)
	return(string)
	
def writeNewLineToTxtFile(filename,newLine,index):
	index = int(index) - 1

	with open(filename) as fileToRead:
		lines = fileToRead.readlines()
		
	newLineArray = newLine.split('\t')
	oldLineArray = lines[index].split('\t')
 
	delta = len(oldLineArray) - len(newLineArray)
	
	for i in range(delta, len(oldLineArray)):
		oldLineArray[i] = newLineArray[i - delta]

		
	newLine = '\t'.join(oldLineArray) + '\n'
	lines[index] = newLine
		
		
	with open(filename,'w') as fileToWrite:
		fileToWrite.writelines(lines)
		
		







	
csvFileMap = {'Branch':'map.csv', 'Gen': 'generatorMap.csv', 'Bus': 'busMap.csv'}
indexToSkipMap = {'Branch': 0, 'Gen': 0, 'Bus': 1}
targetFileName = {'Branch': 'branch.txt', 'Gen': 'gen.txt', 'Bus': 'bus.txt'}




filename = str(sys.argv[1])
type = identifyType(filename)

if(type==''):
	print('Out of scope')
else:
	results = readAttributesFromOwlFile(filename)
	csvFilename = csvFileMap[type]
	indexToSkip = indexToSkipMap[type]
	
	# index is number of line in the txt file
	index = filename.split('-')[1]
	
	IRIs = readCVSMap(csvFilename,index)
	newLine = createNewLine(IRIs,results)
	writeNewLineToTxtFile(targetFileName[type],newLine,index)
	
	
	
	
		