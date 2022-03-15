#The following example uses Python 3.x syntax
 
#Python with COM requires the pyWin32 extensions
import win32com.client, csv, re, math, os
from win32com.client import VARIANT

# This will import VT_VARIANT
import pythoncom

#dictionary function designed to read .csv file from a provided address and given an array to store the values
def RCSV(address):
	input=[]
	csv_reader = csv.DictReader(open(address, 'r'), delimiter=',', quotechar='"')
	headers = csv_reader.fieldnames
	for line in csv_reader:
		for i in range(len(csv_reader.fieldnames)):
			input.append(line[csv_reader.fieldnames[i]])
	return input, headers
			

#dictionary function which writes a .csv file given its address, an array with values to be saved and an array with headers under which it is supposed to write the values (to be improved)
def WCSV(address, output, headers):
	with open(address, 'w') as csvfile:
		writer = csv.DictWriter(csvfile, fieldnames=headers, lineterminator = '\n')
		writer.writeheader()
		
		#for i in range(len(output[0])):
		writer.writerow({headers[x]: output[x] for x in range(len(headers))})



def Run(InVal,OutType,OutDesc):
	 
	#VARIANT is needed if passing in array of arrays. BOTH the field list
	#and the value list must use this syntax. If not passing in arrays of arrays, the
	#standard list format can be used. Passing out arrays of arrays from SimAuto in the
	#output parameter seems to work OK with Python.
	 
	for j in range(len(InVal)):  
		if j==0:
			InType = 'Load'
			InDesc = ["BusNum", "LoadID", "LoadMW"]
		if j==1:
			InType = 'Load'
			InDesc = ["BusNum", "LoadID", "LoadMVR"]
		if j==2:
			InType = 'Gen'
			InDesc = ["BusNum", "GenID", "GenMW"]
		if j==3:
			InType = 'Gen'
			InDesc = ["BusNum", "GenID", "GenMVR"]
		
		
		FieldArray = VARIANT(pythoncom.VT_VARIANT | pythoncom.VT_ARRAY, InDesc)
		AllValueArray=[None]*len(InVal[j])
		
		for i in range(len(InVal[j])):
			AllValueArray[i]=VARIANT(pythoncom.VT_VARIANT | pythoncom.VT_ARRAY, InVal[j][i])
		
		if len(AllValueArray) == 1: object.ChangeParametersSingleElement(InType, FieldArray, AllValueArray[0])
		else: object.ChangeParametersMultipleElement(InType, FieldArray, AllValueArray)
		
	object.RunScriptCommand("SolvePowerFlow")
	
	OutVal=[]
	Values = [["95"]]
	OutVal.append(object.GetParametersSingleElement(OutType[0], OutDesc[0], [93,0,0,0,0,0,0])[1])
	OutVal.append(object.GetParametersSingleElement(OutType[0], OutDesc[0], [94,0,0,0,0,0,0])[1])
	OutVal.append(object.GetParametersSingleElement(OutType[0], OutDesc[0], [95,0,0,0,0,0,0])[1])
	#OutVal = OutVal[1]
	print(OutVal)
	'''
	OutVal2=[]
	OutVal2 = object.GetParametersMultipleElement(OutType[1], OutDesc[1],'')
	OutVal2 = OutVal2[1]
	'''
	
	return OutVal #, OutVal2


# This will establish the connection
object = win32com.client.Dispatch("pwrworld.SimulatorAuto") 
filename = r"C:\Users\janusz\Desktop\PW-MoDS paper\2015.11 - study of JPS grid from May 2015\Eddy PowerWorld Model - May 2015\JParkSimulator.pwb"
object.OpenCase(filename)

# Reading inputs from a .csv
ADIN='IN.csv'
ADOUT='OUT.csv'

RVal, InH = RCSV(ADIN)

#More information about names of variables and identifiers at http://www.powerworld.com/files/Auxiliary-File-Format-18.pdf
InVal=[[],[],[],[]]


for i in range(len(InH)):
	if re.search('LoadMW',InH[i]) != None:
		for j in range(len(InH[i])):
			if InH[i][-j]=="_":
				InVal[0].append([int(InH[i][1-j:-1]), InH[i][-1], RVal[i]])
				break
			else:
				continue
				
	if re.search('LoadMVR',InH[i]) != None:
		for j in range(len(InH[i])):
			if InH[i][-j]=="_":
				InVal[1].append([int(InH[i][1-j:]), InH[i][-1], RVal[i]])
				break
			else:
				continue
				
	if re.search('GenMW',InH[i]) != None:
		for j in range(len(InH[i])):
			if InH[i][-j]=="_":
				InVal[2].append([int(InH[i][1-j:]), InH[i][-1], RVal[i]])
				break
			else:
				continue
				
	if re.search('GenMVR',InH[i]) != None:
		for j in range(len(InH[i])):
			if InH[i][-j]=="_":
				InVal[3].append([int(InH[i][1-j:]), InH[i][-1], RVal[i]])
				break
			else:
				continue
				

#OutType = ["BUS", "Branch"]
#OutDesc = [["PWBUSNUM", "PWBUSNAME", "PWBUSPUVOLT", "PWBUSANGLE", "PWBUSKVVOLT", "LineMW", "LineMVR"],["BusNumFrom", "BusNumTo", "LineLossMW", "LineLossMVR"]]
OutType = ["BUS", "Branch"]
OutDesc = [["PWBUSNUM", "PWBUSNAME", "PWBUSPUVOLT", "PWBUSANGLE", "PWBUSKVVOLT", "LineMW", "LineMVR"]]
OutTrans = Run(InVal,OutType,OutDesc)

OutVal=[[],[]]

for j in range(len(OutTrans)):
	for i in range(3,len(OutDesc[0])):
		OutVal[0].append('Bus'+str(int(OutTrans[j][0]))+'_'+OutTrans[j][1]+'_'+OutDesc[0][i])
		OutVal[1].append(OutTrans[j][i])
	
'''	
for j in range(len(OutTrans2[0])):
	for i in range(2,len(OutDesc[1])):
		OutVal[0].append('From'+str(int(OutTrans2[0][j]))+'_'+str(int(OutTrans2[1][j]))+'_'+OutDesc[1][i])
		OutVal[1].append(OutTrans2[i][j])
'''	
	
WCSV(ADOUT,OutVal[1],OutVal[0])

#This will close the connection
del object
object = None
