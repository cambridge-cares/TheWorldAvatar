"""
Data source:https://en.wikipedia.org/wiki/Electricity_sector_in_the_United_Kingdom
The Electricity production distribution in the United Kingdom
"""

totalElectricityProduction = 3.12e+8 ## Unit: MGh
ElectricityProductionDistribution = {
    'http://www.theworldavatar.com/kb/ontoeip/WindOffshore': totalElectricityProduction * 0.131,
    'http://www.theworldavatar.com/kb/ontoeip/WindOnshore': totalElectricityProduction * 0.111,
    'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal': totalElectricityProduction * 0.018,
    'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar': totalElectricityProduction * 0.042,
    'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil': totalElectricityProduction * 0.033, ## and the other types of the generation
    'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas':totalElectricityProduction * 0.357,
    'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#SourGas':totalElectricityProduction * 0.357, ## assume sour gas generatiion has the same annual operation hours as natrual gas generator 
    'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Nuclear':totalElectricityProduction * 0.161,
    'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Biomass':totalElectricityProduction * 0.126,
    'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Hydro':totalElectricityProduction * 0.022
}