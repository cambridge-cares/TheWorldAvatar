####################################################
# Original author: Tom Savage (trs3@cam.ac.uk)     #
# Extended by: Wanni Xie (wx243@cam.ac.uk)         #
# Last Update Date: 20 August 2021                 #
####################################################

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
        "SolarGeneration": "#ffd21f",
        "Solar": "#ffd21f",
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
        "Waste_anaerobicdigestion": "#1E8449",
        "Waste_municipalsolidwaste": "#bc1fff",
        "Waste": "#873600"
    }
  return map_fuel_dict[gen_fuel]

def getChoropleth(consumption):
    if consumption > 2000: 
        return "#a50026"
    elif consumption <= 2000 and consumption > 1800:
        return "#d73027"
    elif consumption <= 1800 and consumption > 1600:
        return "#f46d43"
    elif consumption <= 1600 and consumption > 1400:
        return "#fdae61"
    elif consumption <= 1400 and consumption > 1200:
        return "#fee08b"
    elif consumption <= 1200 and consumption > 1000:
        return "#ffffbf"
    elif consumption <= 1000 and consumption > 800:
        return "#d9ef8b"
    elif consumption <= 800 and consumption > 600:
        return "#a6d96a"
    elif consumption <= 600 and consumption > 400:
        return "#66bd63"
    elif consumption <= 400 and consumption > 200:
        return "#1a9850"
    elif consumption <= 200:
        return "#006837"