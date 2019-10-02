

import win32api, win32com.client as win32

def read_AspenPlus_data():
	
	if 0==0:
		# Define address of a relevant Aspen Plus model
		SimAddress = win32api.GetLongPathName(r"C:\Users\Janusz\Desktop\Commodity_prices\Biodiesel Final Simulation_20160429\Jbiod_WWHR_23052017.apw") 
		 
		#given the simulation address connect to an existing simulation or create a new COM instance
		aspen = win32.GetObject (SimAddress)

		aspen.Engine.Run2()
		data =(
		str(aspen.Tree.FindNode(r"\Data\Streams\OIL\Output\MASSFLMX\MIXED").Value)+","+    #CPO kg/hr
		str(aspen.Tree.FindNode(r"\Data\Streams\FINALPRD\Output\MASSFLMX\MIXED").Value)+","+ #FAME kg/hr
		str(aspen.Tree.FindNode(r"\Data\Streams\ELECLINE\Output\POWER_OUT").Value)+","+ #electricity kW
		str(aspen.Tree.FindNode(r"\Data\Streams\FFG\Output\MOLEFLMX\MIXED").Value)+","+ #fuel gas kol/hr
		str(0)+","+    #HPS kg/hr
		str(0)+","+    #LPS kg/hr
		str(aspen.Tree.FindNode(r"\Data\Streams\FSTEAM\Output\MASSFLMX\MIXED").Value)+","+    #MPS kg/hr
		str(aspen.Tree.FindNode(r"\Data\Streams\FCW\Output\MASSFLMX\MIXED").Value)+","+    #cooling water kg/hr
		str(aspen.Tree.FindNode(r"\Data\Streams\FPW\Output\MASSFLMX\MIXED").Value)    #process water kg/hr
		)
	else:
		data = "1,2,3,4,5,6,7,8,9"
		
	headers = "massF_CrudePalmOilInput_001,massF_BiodieselOutput_001,energyF_Electricity_001,moleF_FuelGas_001,massF_HighPressureSteam_001,massF_LowPressureSteam_001,massF_MediumPressureSteam_001,massF_CoolingWater_001,massF_ProcessWater_001,"

	print(headers + data)
	
	
if __name__ == "__main__":
	read_AspenPlus_data()
