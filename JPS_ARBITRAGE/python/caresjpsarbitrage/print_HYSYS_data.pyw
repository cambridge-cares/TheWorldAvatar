

import win32api, win32com.client as win32

def read_AspenHYSYS_data():
	
	if 0==0:
		# Define address of a relevant Aspen Plus model
		SimAddress = win32api.GetLongPathName(r"C:\Users\Janusz\Desktop\Commodity_prices\METHANOL PRODUCTION SIMULATION\Methanol Production Plant Rev 1.hsc")		 
		#given the simulation address connect to an existing simulation or create a new COM instance
		hysys = win32.GetObject (SimAddress)

		data =(
		str(hysys.Flowsheet.Operations.Item(75).Cell('B2').CellValue)+","+    #NG mmBTU/s  *35.3147*0.9756/1000
		str(hysys.Flowsheet.Operations.Item(75).Cell('B3').CellValue)+","+ #MeOH kg/s
		str(hysys.Flowsheet.Operations.Item(75).Cell('K14').CellValue)+","+ #electricity kW
		str(abs(hysys.Flowsheet.Operations.Item(75).Cell('E14').CellValue))+","+ #fuel gas kol/hr
		str(hysys.Flowsheet.Operations.Item(75).Cell('N14').CellValue)+","+    #HPS kg/s
		str(0)+","+    #LPS kg/hr
		str(hysys.Flowsheet.Operations.Item(75).Cell('Q14').CellValue)+","+    #MPS kg/s
		str(hysys.Flowsheet.Operations.Item(75).Cell('H14').CellValue)+","+    #cooling water kg/s
		str(hysys.Flowsheet.Operations.Item(75).Cell('T14').CellValue)+","    #process water kg/s
		)
	else:
		data = "1,2,3,4,5,6,7,8,9"
		
	headers = "massF_NaturalGasInput_001,massF_MethanolOutput_001,energyF_Electricity_001,moleF_FuelGas_001,massF_HighPressureSteam_001,massF_LowPressureSteam_001,massF_MediumPressureSteam_001,massF_CoolingWater_001,massF_ProcessWater_001"

	print(headers)
	print(data)
	
	
if __name__ == "__main__":
	read_AspenHYSYS_data()
