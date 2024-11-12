def gen_fuel_col(gen_fuel):
  #returns a colour code for each generator fuel type.
  #https://htmlcolorcodes.com/
  map_fuel_dict = {
        "BiomassGeneration": "#1E8449",
        "Biomass": "#1E8449",
        "WindGeneration": "#13bef2",
        "Wind": "#13bef2",
        "HydroGeneration": "#1F618D",
        "Hydro": "#1F618D",
        "SolarGeneration": "#ffcc33", 
        "Solar": "#ffcc33",
        "CoalGeneration": "#99A3A4",
        "Coal": "#99A3A4",
        "OilGeneration": "#1B2631",
        "Oil": "#1B2631",
        "NaturalGasGeneration": "#eb8500",
        "NaturalGas": "#eb8500",
        "NuclearGeneration": "#ed2400",
        "Nuclear": "#ed2400",
        "PumpHydroStorage": "#1F618D",
        "PumpHydro": "#1F618D",
        "SourGasGeneration": "#eb8500",
        "SourGas": "#eb8500",
        "Waste_anaerobicdigestion": "#873600",
        "Waste_municipalsolidwaste": "#873600",
        "Waste": "#873600"
        # "Waste_anaerobicdigestion": "#1E8449",
        # "Waste_municipalsolidwaste": "#bc1fff",
        # "Waste": "#873600"
    }
  return map_fuel_dict[gen_fuel]