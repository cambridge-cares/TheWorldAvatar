exportPath = "ExampleExcels\Template-Powerplant-Export.xlsx"
curtailmentPath = "ExampleExcels\Template-Powerplant-Curtailment.xlsx"

###Libraries###
import httplib2
from pprint import pformat

import re
import pandas as pd
import sys
from datetime import datetime, timedelta

#Other Python Files
import TimeStampFunctions

###Other Functions###
def isStringNumeric(check):
    #Check elements of string.
    for i in check:
        if not i in "0987654321.":
            return False
    return True


###Conversion###
def excelToKG(excelData, EIC):
    #Takes the location of the excel and the identifier of a powerplant (wind farm), which is currently the EIC. 
    #excelData should already be opened, as it is no longer opened here. 

    #Open CSV (curtailment or export)
    #excelData = pd.read_excel(excelPath)
    
    times = []
    values = []
    print("len(excelData[EIC]):", len(excelData[EIC]))
    for i in range(7,len(excelData[EIC])):
    #for i in range(7,56): #Test
        if(str(excelData.iloc[i, excelData.columns.get_loc(EIC)])
           and isStringNumeric(str(excelData.iloc[i, excelData.columns.get_loc('Year')]))
           and isStringNumeric(str(excelData.iloc[i, excelData.columns.get_loc('Month')]))
           and isStringNumeric(str(excelData.iloc[i, excelData.columns.get_loc('Day')]))
           and isStringNumeric(str(excelData.iloc[i, excelData.columns.get_loc('Period')])) ):
            values.append(excelData.iloc[i, excelData.columns.get_loc(EIC)])
            times.append(TimeStampFunctions.format_time(str(excelData.iloc[i, excelData.columns.get_loc('Year')]),
                        str(excelData.iloc[i, excelData.columns.get_loc('Month')]),
                        str(excelData.iloc[i, excelData.columns.get_loc('Day')]),
                        str(excelData.iloc[i, excelData.columns.get_loc('Period')])))

    return times, values

if __name__=='__main__':
    excelFile = "data/Template-Powerplant-Export.xlsx"    
    excelToKG(excelFile, "48WSTN0000ABRBON")
