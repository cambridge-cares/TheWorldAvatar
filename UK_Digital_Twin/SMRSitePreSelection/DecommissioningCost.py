##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 14 July 2022         #
##########################################

"""
Data source:https://www.rff.org/publications/reports/decommissioning-us-power-plants-decisions-costs-and-key-issues/
The original unit is US$ (year 2016), coverts to £
[Minimum, Mean, Maximum]

Unit: £/MW Capacity
"""

# DecommissioningCost = {
#     'http://www.theworldavatar.com/kb/ontoeip/WindOffshore': [103421.746, 178247.874, 287550.815],
#     'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal': [17654.922, 98363.137, 391771.125],
#     'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar': [-74828.273, 47923.726, 150494.783],
#     'http://www.theworldavatar.com/kb/ontoeip/WindOnshore': [1681.449, 42876.96, 186640.884],
#     'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil': [1681.449, 26060.056, 86586.637],
#     'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas': [840.622, 12609.333, 42031.111]
# }


DecommissioningCost = {
    'http://www.theworldavatar.com/kb/ontoeip/WindOffshore': [0, 103421.746, 178247.874, 287550.815],
    'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Coal': [0, 17654.922, 98363.137, 391771.125],
    'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Solar': [0, -74828.273, 47923.726, 150494.783],
    'http://www.theworldavatar.com/kb/ontoeip/WindOnshore': [0, 1681.449, 42876.96, 186640.884],
    'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Oil': [0, 1681.449, 26060.056, 86586.637],
    'http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NaturalGas': [0, 840.622, 12609.333, 42031.111]
}
