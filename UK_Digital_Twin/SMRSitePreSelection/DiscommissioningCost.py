##########################################
# Author: Wanni Xie (wx243@cam.ac.uk)    #
# Last Update Date: 11 July 2022         #
##########################################

"""
Data source:https://www.rff.org/publications/reports/decommissioning-us-power-plants-decisions-costs-and-key-issues/
The original unit is US$ (year 2016), coverts to £
[Minimum Unit, Mean, Maximum]

Unit: £/MW Capacity
"""

DiscommissioningCost = {
    'http://www.theworldavatar.com/kb/ontoeip/WindOffshore': [103421.746, 178247.874, 287550.815],
    'Coal': [17654.922, 98363.137, 391771.125],
    'Solar': [-74828.273, 47923.726, 150494.783],
    'http://www.theworldavatar.com/kb/ontoeip/WindOnshore': [1681.449, 42876.96, 186640.884],
    'Oil': [1681.449, 26060.056, 86586.637],
    'NaturalGas': [840.622, 12609.333, 42031.111]
}