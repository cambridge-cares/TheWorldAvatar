#The following script is designed to read a CSV file with data in a correct format (this is important; more below), alter a PowerWorld simulation accordingly and run it and store the desired outputs (types and locations are hard-coded into the script) in another CSV file. The input files must be formatted in the following manner: row 1 must include headers, row 2 the corresponding values, headers must follow busname_inputtype_busid_typeid for powers of loads and generators and busname_inputtype_busid for nominal voltage of a bus.

#The following example uses Python 3.x syntax
 
#Python with COM requires the pyWin32 extensions
import win32com.client, csv, re, os, pythoncom
from win32com.client import VARIANT

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
		if j==4:
			InType = 'Bus'
			InDesc = ["BusNum", "BusName", "BusNomVolt"]	
		
		
		FieldArray = VARIANT(pythoncom.VT_VARIANT | pythoncom.VT_ARRAY, InDesc)
		AllValueArray=[None]*len(InVal[j])
		
		for i in range(len(InVal[j])):
			AllValueArray[i]=VARIANT(pythoncom.VT_VARIANT | pythoncom.VT_ARRAY, InVal[j][i])
	
		#Alter the simulation according to the input file
		if len(AllValueArray) == 1: object.ChangeParametersSingleElement(InType, FieldArray, AllValueArray[0])
		else: object.ChangeParametersMultipleElement(InType, FieldArray, AllValueArray)
	
	
	object.RunScriptCommand("ResetToFlatStart")
	#Solve the altered simulation
	object.RunScriptCommand("SolvePowerFlow")
	
	#Retrieve the desired outputs
	OutVal=[]
	OutVal = object.GetParametersMultipleElement(OutType[0], OutDesc[0],'')
	OutVal = OutVal[1]
	
	
	return OutVal

#Define addresses of the input, output and simulation files
ADIN='IN.csv'
ADOUT='OUT.csv'
filename = r"D:\Semakau MoDS files\REIDS Semakau June 2016\REIDS Semakau microgrid.PWB"

# This will establish the connection
object = win32com.client.Dispatch("pwrworld.SimulatorAuto") 
object.OpenCase(filename)

# Reading inputs from a .csv
RVal, InH = RCSV(ADIN)

#Process the input data from IN.csv into a format disgestible for RUN function
InVal=[[],[],[],[],[]]


for i in range(len(InH)):
	if re.search('LoadMW',InH[i]) != None:
		pos = []
		for m in re.finditer('_',InH[i]):
			pos.append(m.start())
		InVal[0].append([int(InH[i][pos[1]+1:pos[2]]), InH[i][pos[2]+1:], RVal[i]])
				
	if re.search('LoadMVR',InH[i]) != None:
		pos = []
		for m in re.finditer('_',InH[i]):
			pos.append(m.start())
		InVal[1].append([int(InH[i][pos[1]+1:pos[2]]), InH[i][pos[2]+1:], RVal[i]])

				
	if re.search('GenMW',InH[i]) != None:
		pos = []
		for m in re.finditer('_',InH[i]):
			pos.append(m.start())
		InVal[2].append([int(InH[i][pos[1]+1:pos[2]]), InH[i][pos[2]+1:], RVal[i]])

				
	if re.search('GenMVR',InH[i]) != None:
		pos = []
		for m in re.finditer('_',InH[i]):
			pos.append(m.start())
		InVal[3].append([int(InH[i][pos[1]+1:pos[2]]), InH[i][pos[2]+1:], RVal[i]])

				
	if re.search('BusNomVolt',InH[i]) != None: #'Bus
		pos = []
		for m in re.finditer('_',InH[i]):
			pos.append(m.start())
		InVal[4].append([int(InH[i][pos[1]+1:]), InH[i][:pos[0]], RVal[i]])
				
#Define descriptions and types of data to be retrieved from the simulation
#More information about names of variables and identifiers at http://www.powerworld.com/files/Auxiliary-File-Format-18.pdf
OutType = ["BUS", "Branch"]
OutDesc = [["PWBUSNUM", "PWBUSNAME", "PWBUSPUVOLT", "PWBUSANGLE", "PWBUSKVVOLT", "BusLoadMW", "BusLoadMVR", "BusGenMW", "BusGenMVR"],["BusNumFrom", "BusNumTo", "LineLossMW", "LineLossMVR"]]
#Run the simulation and store outputs in OutTrans array
OutTrans = Run(InVal,OutType,OutDesc)


#Process the data in OutTrans into a form disgestible for WCSV (CSV file writer)
OutVal=[[],[]]
for j in range(len(OutTrans[0])):
	for i in range(2,len(OutDesc[0])):
		OutVal[0].append('Bus'+str(int(OutTrans[0][j]))+'_'+OutTrans[1][j]+'_'+OutDesc[0][i])
		if OutTrans[i][j] == None: OutVal[1].append(0)
		else: OutVal[1].append(OutTrans[i][j])

		
#Write the outputs to a local CSV file
WCSV(ADOUT,OutVal[1],OutVal[0])

#This will close the connection
del object
object = None
