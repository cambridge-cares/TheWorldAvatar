import pandas as pd
import json
import sys

from CostEmissionCalculator import cost_emission_calculation

if __name__ == "__main__":
	
	rootPath = json.loads(sys.argv[1])
	generation_technology_sent = sys.argv[2]
	fuel_type_sent = sys.argv[3]
	
	# DEFINE PARAMETERS
	## define user's input parameters (decide which surrogate model to use for calculation)
	
	#generation_technology = 'ultrasubcritical'
	### choose from ultrasubcritical, supercritical, subcritical, NGCC, IGCC, cogeneration
	
	#fuel_type = 'coal'
	### choose from coal, coal_biomass_cofiring, lignite, anthracite, bituminous, subbituminous, natural gas, oil
	
	## define parameters from powerplant knowledge base (use SPARQL to retrieve information from powerplant .owl files)
	
	# load the powerplant database

	df = pd.read_csv(rootPath + 'data/input/powerplant_database.csv', header='infer', sep=',')
	cols = list(df)
	cols.remove('year')
	
	# use SPARQL query to get these results
	country, capacity, primary_fuel, generation_technology, age, output, fuel_used ='Australia', 114, 'bituminous', 'subcritical', 30, 5000,'coal'
	plant_data = [country, capacity, primary_fuel, generation_technology, age, output, fuel_used]
	plant_df = pd.DataFrame([plant_data], columns = cols)
	
	
	# GET THE REFERENCE EMISSION DATABASE (CHOOSE THE RIGHT REFERENCE DATABASE BY GENERATION TECHNOLOGY AND FUEL)
	generation_technology = generation_technology_sent
	fuel_type = fuel_type_sent
	
	## coal
	if generation_technology == 'ultrasubcritical':
		emission_df = pd.read_csv(rootPath + 'data/input/baseplant/base_ultrasubcritical_PC_coal.csv', header='infer', sep=',')

	if generation_technology == 'supercritical':
		if primary_fuel == 'anthracite':
			emission_df = pd.read_csv(rootPath + 'data/input/baseplant/base_supercritical_PC_anthracite.csv', header='infer', sep=',')
		elif primary_fuel == 'subbituminous':
			emission_df = pd.read_csv(rootPath + 'data/input/baseplant/base_supercritical_PC_subbituminous.csv', header='infer', sep=',')
		elif primary_fuel == 'lignite':
			emission_df = pd.read_csv(rootPath + 'data/input/baseplant/base_supercritical_PC_lignite.csv', header='infer', sep=',')
		elif primary_fuel == 'bituminous' or primary_fuel == 'coal':
			emission_df = pd.read_csv(rootPath + 'data/input/baseplant/base_supercritical_PC_subbituminous.csv', header='infer', sep=',')

	if generation_technology == 'subcritical' or generation_technology == 'cogeneration':
		if primary_fuel == 'anthracite':
			emission_df = pd.read_csv(rootPath + 'data/input/baseplant/base_subcritical_PC_anthracite.csv', header='infer', sep=',')
		elif primary_fuel == 'subbituminous':
			emission_df = pd.read_csv(rootPath + 'data/input/baseplant/base_subcritical_PC_subbituminous.csv', header='infer', sep=',')
		elif primary_fuel == 'lignite':
			emission_df = pd.read_csv(rootPath + 'data/input/baseplant/base_subcritical_PC_lignite.csv', header='infer', sep=',')
		elif primary_fuel == 'coal_biomass':
			emission_df = pd.read_csv(rootPath + 'data/input/baseplant/base_subcritical_PC_coal_biomass.csv', header='infer', sep=',')     
		elif primary_fuel == 'bituminous' or primary_fuel == 'coal':
			emission_df = pd.read_csv(rootPath + 'data/input/baseplant/base_subcritical_PC_subbituminous.csv', header='infer', sep=',')
			
	## natural gas
	if primary_fuel == 'natural_gas':
		emission_df = pd.read_csv(rootPath + 'data/input/baseplant/base_NGCC.csv', header='infer', sep=',')
		emission_df = emission_df.sort_values(by=['age','capacity_MW'], ascending=[True,True])
		
	## oil
	if primary_fuel == 'oil':
		emission_df = pd.read_csv(rootPath + 'data/input/baseplant/base_NGCC_oil.csv', header='infer', sep=',')
		emission_df = emission_df.sort_values(by=['age','capacity_MW'], ascending=[True,True])
	
	# CALCULATE THE EMISSION FOR THE QUERIED POWERPLANT
	result_df = cost_emission_calculation(plant_df, emission_df)
	dict = {}
	dict['df'] = result_df.values.tolist()
	print(json.dumps(dict))