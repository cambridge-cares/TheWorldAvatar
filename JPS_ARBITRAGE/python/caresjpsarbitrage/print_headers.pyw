

def print_headers():

	headers = [
	"V_energyF_Electricity_001,",
	"V_Costs_Transport_USGC-NEA_NaturalGas_001,",
	"V_Costs_MediumPressureSteam_001,",
	"V_Costs_CoolingWater_001,",
	"V_massF_BiodieselOutput_001,",
	"V_Costs_Transport_SG-SC_Methanol_001,",
	"V_Price_Storage_NaturalGas_001,",
	"V_Price_CoolingWater_001,",
	"V_Price_Storage_Biodiesel_001,",
	"V_Price_Storage_CrudePalmOil_001,",
	"V_Costs_Storage_CrudePalmOil_001,",
	"V_Earnings_MethanolOutput_001,",
	"V_Price_Storage_Methanol_001,",
	"V_massF_HighPressureSteam_001,",
	"V_Costs_NaturalGasInput_001,",
	"V_Price_Transport_Malaysia-SG_CrudePalmOil_001,",
	"V_Price_Electricity_001,",
	"V_Costs_CrudePalmOilInput_001,",
	"V_massF_LowPressureSteam_001,",
	"V_Costs_Storage_Biodiesel_001,",
	"V_massF_MethanolOutput_001,",
	"V_Price_Transport_SG-SC_Methanol_001,",
	"V_moleF_FuelGas_001,",
	"V_massF_NaturalGasInput_001,",
	"V_Costs_HighPressureSteam_001,",
	"V_USD_to_SGD,",
	"V_Costs_Transport_Malaysia-SG_CrudePalmOil_001,",
	"V_Price_ProcessWater_001,",
	"V_Price_Transport_USGC-NEA_NaturalGas_001,",
	"V_Costs_Storage_Methanol_001,",
	"V_massF_MediumPressureSteam_001,",
	"V_Price_HighPressureSteam_001,",
	"V_Earnings_BiodieselOutput_001,",
	"V_massF_CoolingWater_001,",
	"V_USD_to_CNY,",
	"V_Costs_LowPressureSteam_001,",
	"V_Costs_FuelGas_001,",
	"V_massF_CrudePalmOilInput_001,",
	"V_Price_MediumPressureSteam_001,",
	"V_Costs_Storage_NaturalGas_001,",
	"V_Price_LowPressureSteam_001,",
	"V_Price_Transport_SEA-SC_Biodiesel_001,",
	"V_massF_ProcessWater_001,",
	"V_Price_FuelGas_001,",
	"V_Costs_Electricity_001,",
	"V_Costs_Transport_SEA-SC_Biodiesel_001,",
	"V_Costs_ProcessWater_001"
	]
	
	string =""
	for element in headers:
		string = string + element
	
	print(string)



if __name__ == "__main__":
	print_headers()
