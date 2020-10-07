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
def RUN(input, aspen):

	initial = aspen.Tree.FindNode(r"\Data\Streams\OIL\Input\TOTFLOW\MIXED").Value
	aspen.Tree.FindNode(r"\Data\Streams\OIL\Input\TOTFLOW\MIXED").Value = input[0] #CPO kg/hr
	
	aspen.Reinit()
	aspen.Engine.Run2()
	
	output=[]
	
	output.append(aspen.Tree.FindNode(r"\Data\Streams\FINALPRD\Output\MASSFLMX\MIXED").Value) #FAME kg/hr
	output.append(aspen.Tree.FindNode(r"\Data\Streams\ELECLINE\Output\POWER_OUT").Value) #electricity kW
	output.append(aspen.Tree.FindNode(r"\Data\Streams\FFG\Output\MOLEFLMX\MIXED").Value) #fuel gas kol/hr
	output.append(0)  #HPS kg/hr
	output.append(0)  #LPS kg/hr
	output.append(aspen.Tree.FindNode(r"\Data\Streams\FSTEAM\Output\MASSFLMX\MIXED").Value) #MPS kg/hr
	output.append(aspen.Tree.FindNode(r"\Data\Streams\FCW\Output\MASSFLMX\MIXED").Value)  #cooling water kg/hr
	output.append(aspen.Tree.FindNode(r"\Data\Streams\FPW\Output\MASSFLMX\MIXED").Value)  #process water kg/hr

	aspen.Tree.FindNode(r"\Data\Streams\OIL\Input\TOTFLOW\MIXED").Value = initial
	
	aspen.Reinit()
	aspen.Engine.Run2()
	
	return output



#define location of the simulation in question and convert it into an appropriate format

SimAddress = win32api.GetLongPathName(r"C:\Users\Janusz\Desktop\Commodity_prices\Biodiesel Final Simulation_20160429\Jbiod_WWHR_26042016.apw")
aspen = win32.GetObject (SimAddress)
	
#Define the name of the input file and run .csv reading function
ADIN = "IN.csv"

IN = RCSV(ADIN)
#run the simulation with the new inputs and sample the outputs as defined in RUN; if it fails reinitialize the simulation and try again (done twice)
try:
	OUT = RUN(IN,aspen)
	
except:
	try:
		aspen.Reinit()
		OUT = RUN(IN,aspen)
	except:
		aspen.Reinit()
		OUT = RUN(IN,aspen)

#define names of outputs to be sampled by the RUN function and printed into OUT.csv file
H=['massF_BiodieselOutput_001',	'energyF_Electricity_001',	'moleF_FuelGas_001',	'massF_HighPressureSteam_001',	'massF_LowPressureSteam_001',	'massF_MediumPressureSteam_001',	'massF_CoolingWater_001',	'massF_ProcessWater_001']
#define names of outputs to be sampled by the RUN function and printed into OUT.csv file
ADOUT = "OUT.csv"
WCSV(ADOUT,OUT,H)

