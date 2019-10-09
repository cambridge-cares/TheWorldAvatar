#import relevant libraries
import os, win32api, csv
import win32com.client as win32


#dictionary function designed to read .csv file from a provided address and given an array to store the values			
def RCSV(address):
	csv_reader = csv.DictReader(open(address, 'r'), delimiter=',', quotechar='"')
	headers = csv_reader.fieldnames
	input=[]
	for line in csv_reader:
		for i in range(len(csv_reader.fieldnames)):
			input.append(line[csv_reader.fieldnames[i]])
	return input

#dictionary function which writes a .csv file given its address, an array with values to be saved and an array with headers under which it is supposed to write the values (to be improved)
def WCSV(address, output, headers):
	with open(address, 'w') as csvfile:
		writer = csv.DictWriter(csvfile, fieldnames=headers, lineterminator = '\n')
		writer.writeheader()
		
		#for i in range(len(headers)):
		writer.writerow({headers[x]: output[x] for x in range(len(headers))})
		
		
#function amending chosen inputs in AspenPlus, running a defined simulation and extracting chosen outputs from the simulation
def RUN(input):

	initial = hysys.Flowsheet.Streams.Item(0).MolarFlowValue
	hysys.Flowsheet.Streams.Item(0).MolarFlowValue = input[0]  #NG cbm/s  /35.3147/0.9756*1000


	output=[]
	
	output.append(hysys.Flowsheet.Operations.Item(75).Cell('B3').CellValue) #MeOH kg/s
	output.append(hysys.Flowsheet.Operations.Item(75).Cell('K14').CellValue) #electricity kW
	output.append(abs(hysys.Flowsheet.Operations.Item(75).Cell('E14').CellValue)) #fuel gas kol/hr
	output.append(hysys.Flowsheet.Operations.Item(75).Cell('N14').CellValue)  #HPS kg/s
	output.append(0)  #LPS kg/hr
	output.append(hysys.Flowsheet.Operations.Item(75).Cell('Q14').CellValue) #MPS kg/s
	output.append(hysys.Flowsheet.Operations.Item(75).Cell('H14').CellValue)  #cooling water kg/s
	output.append(hysys.Flowsheet.Operations.Item(75).Cell('T14').CellValue)  #process water kg/s

	hysys.Flowsheet.Streams.Item(0).MolarFlowValue = initial
	
	return output



#define location of the simulation in question and convert it into an appropriate format

SimAddress = win32api.GetLongPathName(r"C:\Users\Janusz\Desktop\Commodity_prices\METHANOL PRODUCTION SIMULATION\Methanol Production Plant Rev 1.hsc")
hysys = win32.GetObject (SimAddress)
	
#Define the name of the input file and run .csv reading function
ADIN = "IN.csv"
IN = RCSV(ADIN)
#run the simulation with the new inputs and sample the outputs as defined in RUN; if it fails reinitialize the simulation and try again (done twice)
try:
	OUT = RUN(IN)
	
except:
	try:
		aspen.Reinit()
		OUT = RUN(IN)
	except:
		aspen.Reinit()
		OUT = RUN(IN)

#define names of outputs to be sampled by the RUN function and printed into OUT.csv file
H=['massF_MethanolOutput_001',	'energyF_Electricity_001',	'moleF_FuelGas_001',	'massF_HighPressureSteam_001',	'massF_LowPressureSteam_001',	'massF_MediumPressureSteam_001',	'massF_CoolingWater_001',	'massF_ProcessWater_001']
#define names of outputs to be sampled by the RUN function and printed into OUT.csv file
ADOUT = "OUT.csv"
WCSV(ADOUT,OUT,H)

