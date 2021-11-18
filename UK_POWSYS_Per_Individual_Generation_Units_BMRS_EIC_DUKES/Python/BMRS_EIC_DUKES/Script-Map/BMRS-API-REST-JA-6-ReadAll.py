#KEY: iwx6raw9m7nqq0f (this should not be in the Version Control). 

#TEST from page 193: https://www.elexon.co.uk/documents/training-guidance/bsc-guidance-notes/bmrs-api-and-data-push-user-guide-2/
#This corresponds to this information: https://www.bmreports.com/bmrs/?q=actgenration/actualgeneration

###Libraries###
import httplib2
from pprint import pformat

import re
import pandas as pd
import sys
from datetime import datetime, timedelta


#Also note that the "post_elexon_basic" function is not used, but might be useful for just querying without processing at all if you are learning / testing. 



####Functions###
def post_elexon_basic(url):
    #Query BMRS. This function is not used, but is good if you are learning. Simply run this with the commented out lines to see the output. 
    http_obj = httplib2.Http()
    resp, content = http_obj.request(
        uri=url,
        method='GET',
        headers={'Content-Type': 'application/xml; charset=UTF-8'},
    )
    print('===Response===')
    print(pformat(resp))
    
    print('===Content===')
    print(pformat(content))
    print(type(pformat(content)))

    print('===Finished===')


def post_elexon(url):
    #Query BMRS. 
    http_obj = httplib2.Http()
    resp, content = http_obj.request(
        uri=url,
        method='GET',
        headers={'Content-Type': 'application/xml; charset=UTF-8'},
    )
    #print('===Response===')
    #print(pformat(resp))
    
    #print('===Content===')
    return str(pformat(content))
    #print(type(pformat(content)))

    #print('===Finished===')


def float_chars(test):
    #Check if all characters in the string "test" are valid for a float.
    valid = "0987654321."
    for i in test:
        if i not in valid:
            return 0
    return 1


def ext_chars(test):
    #Check if all characters in the string "test" are valid for a float.
    valid = "0987654321.-_ "
    for i in test:
        if i not in valid:
            return 0
    return 1


def xmlStringProcessToArray(xmlInfo):
    #Takes the big bulk of text and splits it up into a readable array.
    xmlInfo = xmlInfo.replace("'\n b'","")
    xmlInfo = xmlInfo.split("<item>")
    #Now each element in xmlInfo should contain a potential generator and its output.
    #A dictionary should be created such that each ID obtains an output.
    unitOutput = {}
    for item in xmlInfo:
        #Check if this is a valid generator, with an output.
        #Will check if there is a single ID (NGCBMUnitID) and output (quantity). Thus, these terms should exist twice each, with a <> and </> case. 
        if (2 == len(item.split("<nGCBMUnitID>"))) and (2 == len(item.split("</nGCBMUnitID>"))) and (3 == len(item.split("nGCBMUnitID"))) and (2 == len(item.split("<quantity>"))) and (2 == len(item.split("</quantity>"))) and (3 == len(item.split("quantity"))):
            #Extract the ID and Quantity values. 
            ID = re.search("<nGCBMUnitID>(.+?)</nGCBMUnitID>", str(item))
            quantity = re.search("<quantity>(.+?)</quantity>", str(item))
            #Check if this worked, if so, just use the values extracted, rather than the current information, which includes if the extraction worked. 
            if (ID) and (quantity):
                ID = str(ID.group(1))
                quantity = str(quantity.group(1))
                #Check the value contains only valid characters. 
                if (float_chars(quantity)):
                    #Add these values to the dictionary. 
                    unitOutput[ID] = float(quantity)
                    #print(item)
                    #print(ID)
                    #print(quantity)
    return unitOutput


def run_query(Key, Year, Month, Day, Period):
    #The query function
    Key = str(Key)
    Year = str(Year)
    Month = str(Month)
    Day = str(Day)
    Period = str(Period)
    querystring = 'https://api.bmreports.com/BMRS/B1610/v2?APIKey='+Key+'&SettlementDate='+Year+'-'+Month+'-'+Day+'&Period='+Period+'&NGCBMUnitID=&ServiceType=xml'
    
    #Query All
    #post_elexon_basic(url='https://api.bmreports.com/BMRS/B1610/v2?APIKey=iwx6raw9m7nqq0f&SettlementDate=2021-01-01&Period=1&NGCBMUnitID=&ServiceType=xml',) #All 
    #xmlString = post_elexon(url='https://api.bmreports.com/BMRS/B1610/v2?APIKey=iwx6raw9m7nqq0f&SettlementDate=2021-01-01&Period=1&NGCBMUnitID=&ServiceType=xml',) #All
    xmlString = post_elexon(url=querystring,) #All 
    
    #Specific
    #WHILW #48WSTN1000WHILWQ
    #post_elexon_basic(url='https://api.bmreports.com/BMRS/B1610/v2?APIKey=iwx6raw9m7nqq0f&SettlementDate=2021-01-01&Period=1&NGCBMUnitID=WHILW&ServiceType=xml',) #HEYM2

    #Try to format information
    IDOutput = xmlStringProcessToArray(xmlString) #The ID Output dictionary. 
    #print(IDOutput)
    #print(xmlString)
    
    return IDOutput



###Primary Function###
def live_power(ExcelName, Key, Year, Month, Day, Period):
    #The first input is the name of the excel spreadsheet to use.
    #The Key is the API key for BMRS. 
    #The other are for the time (
        #Year, eg. 2020 (four characters),
        #Month, eg. 01 (two characters, the first being a zero if it is <10, and a number, not the name of the month),
        #Day, eg. 01 (two characters, the first being a zero if it is <10, this being the day into the month, so 1-31),
        #Period, eg. 01 (could be 1 or two characters, as using a zero if it is <10 is optional).  The period is the half hour into the day, from 1-48.
    #The same time is used for the whole query. 
    liveGeneratorData = run_query(Key, Year, Month, Day, Period) #Dict of generation units and their outputs. 
    #Read DUKES Stations from Excel
    data = pd.read_excel(ExcelName) #Dataframe including DUKES stations.

    #Clear the outputs.
    for i in range(0,len(data['Output'])):
        data.iloc[i, data.columns.get_loc('Output')] = 0
        data.iloc[i, data.columns.get_loc('Year')] = Year
        data.iloc[i, data.columns.get_loc('Month')] = Month
        data.iloc[i, data.columns.get_loc('Day')] = Day
        data.iloc[i, data.columns.get_loc('Period')] = Period
    
    #Now go through the dataframe to get the IDs for each DUKES station. 
    for i in range(0,len(data['outputDUKESToBMRSID'])):
        #See if an ID is contained.
        if (data['outputDUKESToBMRSID'][i] != "na") and (data['outputDUKESToBMRSID'][i] != "none") and (data['outputDUKESToBMRSID'][i] != "None") and (data['outputDUKESToBMRSID'][i] != "") and (data['outputDUKESToBMRSID'][i] != "NA") and (data['outputDUKESToBMRSID'][i] != "nan"):
            #Now loop through the generators
            for gen in liveGeneratorData: 
                #Now see if there is a match. This is based on the generator ID containing the station ID, and the generator ID, when specifics are removed, being the same as the station ID.
                if (data['outputDUKESToBMRSID'][i] in gen):
                    if (gen.strip("0987654321.-_ ") == data['outputDUKESToBMRSID'][i]):
                        data.iloc[i, data.columns.get_loc('Output')] = float(data['Output'][i]) + liveGeneratorData[gen]
                    elif ext_chars(gen.replace(data['outputDUKESToBMRSID'][i], "")):
                        #The station name could contain a number, which we can check with a looser method here. Here we see if, after removing the station name from the gen name, if the only remaining characters are numbers or in . -_". 
                        #print("TAKE2: " + data['outputDUKESToBMRSID'][i] + ", " + gen)
                        data.iloc[i, data.columns.get_loc('Output')] = float(data['Output'][i]) + liveGeneratorData[gen]
                    else:
                        #Code should not get here. Print differences for debugging. 
                        print(data['outputDUKESToBMRSID'][i] + ", " + gen)

    #Re-export to excel, with times and outputs. 
    data.to_excel(ExcelName, index = False)



###Main Function###
if __name__ == "__main__":
    live_power('Input.xlsx', '', '2021', '01', '01', '01')

