#TEST from page 193: https://www.elexon.co.uk/documents/training-guidance/bsc-guidance-notes/bmrs-api-and-data-push-user-guide-2/
#This corresponds to this information: https://www.bmreports.com/bmrs/?q=actgenration/actualgeneration

###Libraries###
import httplib2
from pprint import pformat

import re
import pandas as pd
from datetime import datetime, timedelta



#Also note that the "post_elexon_basic" function is not used, but might be useful for just querying without processing at all if you are learning / testing.



####Other###
#Note: the functions used here are not used by the primary functions or any of the functions called therein. This is a seperate process which serves as an add-on to the base code here.
def period_to_time(Period):
    #Returns hh:mm for each Period
    halfHour = {
        1: "00:00",
        2: "00:30",
        3: "01:00",
        4: "01:30",
        5: "02:00",
        6: "02:30",
        7: "03:00",
        8: "03:30",
        9: "04:00",
        10: "04:30",
        11: "05:00",
        12: "05:30",
        13: "06:00",
        14: "06:30",
        15: "07:00",
        16: "07:30",
        17: "08:00",
        18: "08:30",
        19: "09:00",
        20: "09:30",
        21: "10:00",
        22: "10:30",
        23: "11:00",
        24: "11:30",
        25: "12:00",
        26: "12:30",
        27: "13:00",
        28: "13:30",
        29: "14:00",
        30: "14:30",
        31: "15:00",
        32: "15:30",
        33: "16:00",
        34: "16:30",
        35: "17:00",
        36: "17:30",
        37: "18:00",
        38: "18:30",
        39: "19:00",
        40: "19:30",
        41: "20:00",
        42: "20:30",
        43: "21:00",
        44: "21:30",
        45: "22:00",
        46: "22:30",
        47: "23:00",
        48: "23:30"
    }
    return halfHour[int(Period)]


def week_ago():
    #Get a week ago (returns str for year, month, day: 
    weekAgo = datetime.utcnow() - timedelta(days=7)
    #print(weekAgoFormat)
    return str(weekAgo.year), str(weekAgo.month), str(weekAgo.day)


def day_over_a_week_ago():
    #Get a week ago + 1 day (8 days) for a slightly longer time ago (returns str for year, month, day: 
    weekAgo = datetime.utcnow() - timedelta(days=8)
    #print(weekAgoFormat)
    return str(weekAgo.year), str(weekAgo.month), str(weekAgo.day)


def two_weeks_ago():
    #Get a two weeks ago (14 days) here, returns str for year, month, day: 
    weekAgo = datetime.utcnow() - timedelta(days=14)
    #print(weekAgoFormat)
    return str(weekAgo.year), str(weekAgo.month), str(weekAgo.day)


def str0_2(value):
    #Converts the day or month int to a string of length 2. Thus, 12 -> "12", and 1 -> "01", the leading 0 is important.
    #This function thus performs a similar role as str(), but also can add the 0 at the start, and is applied to length 2 instances.
    #Must use for day or month, but use for period is optional, as length 1 and 2 is accepted by the API format the period, but not the month or day. 
    value = str(value)
    if len(value) == 1:
        value = "0"+value
    return value


def format_time(Year, Month, Day, Period):
    #Formats the time
    #EG. "2021-11-26T09:30:00Z"
    return str(Year) + '-' + str0_2(Month) + '-' + str0_2(Day) + "T" + period_to_time(Period) + ":00Z"


def convert_csv_to_triple_dfs(csvName):
    print("Formatting BMRS Data")
    #This puts it in the correct format for the utils kg triples.
    
    #This converts the format of the output. Returns two dfs.
    #This is only for Search == 2 scenarios. 
    
    #CSV Locations
    #\Dropbox (Cambridge CARES)\CoMo shared\wx243\UK_Digital_Twin\OWL File\PowerPlant
    #Simplified Data Link
    # plantfilelocation = 'https://www.dropbox.com/s/arwtg13rjj8ajoi/powerplanttriple%20-%20Simple.csv?dl=1'
    # genfilelocation = 'https://www.dropbox.com/s/a24f76icghonsf9/generatortriple%20-%20Simple.csv?dl=1'
    #Standardised Day Link
    plantfilelocation = 'https://www.dropbox.com/s/iwx5ukrzy4agxdx/powerplanttriple%20-%20All.csv?dl=1'
    genfilelocation = 'https://www.dropbox.com/s/aj9zlr91yuzqctp/generatortriple%20-%20All.csv?dl=1'
    
    #Read csv
    data = pd.read_csv(csvName) #Dataframe including DUKES stations. 
    powerplant_data = pd.read_csv(plantfilelocation) #Fixed powerplant name
    generator_data = pd.read_csv(genfilelocation) #Fixed generator name

    p_count = 0
    g_count = 0
    
    #Get Time
    Year = data['Year'][1]
    Month = data['Month'][1]
    Day = data['Day'][1]
    #Period = data['Period'][1] #All 48 are used. 
    
    for i in range(0, len(powerplant_data['*'])):
        powerplant_data.iloc[i, powerplant_data.columns.get_loc('powerplanteic')] = ""
        powerplant_data.iloc[i, powerplant_data.columns.get_loc('time')] = ""
        powerplant_data.iloc[i, powerplant_data.columns.get_loc('power')] = ""
    for i in range(0, len(generator_data['*'])):
        generator_data.iloc[i, generator_data.columns.get_loc('generatoreic')] = ""
        generator_data.iloc[i, generator_data.columns.get_loc('time')] = ""
        generator_data.iloc[i, generator_data.columns.get_loc('power')] = ""

    #For each powerplant/generator
    for i in range(0, len(data['Registered Resource EIC code'])):
        #Now convert the data over
        #####Will use period for now, but might want to switch for TimeStamp string#####
        #For each of the 48 periods
        for Period in range(1,49):
            #If powerplant
            if data['Type (powerplant(station) or generator(unit))'][i] == "powerplant":
                powerplant_data.iloc[p_count, powerplant_data.columns.get_loc('powerplanteic')] = "PowerPlant_" + str(data['outputpowerplant =IF(ISNA(VLOOKUP(C2,CP$2:CR$1184,3,FALSE)),"",VLOOKUP(C2,CP$2:CR$1184,3,FALSE))'][i])
                powerplant_data.iloc[p_count, powerplant_data.columns.get_loc('time')] = format_time(Year, Month, Day, Period) #str(Period)
                powerplant_data.iloc[p_count, powerplant_data.columns.get_loc('power')] = data[('Output' + str(Period))][i]
                p_count += 1
            #If generator
            if data['Type (powerplant(station) or generator(unit))'][i] == "generator":
                generator_data.iloc[g_count, generator_data.columns.get_loc('generatoreic')] = "PowerGenerator_" + str(data['Registered Resource EIC code'][i])
                generator_data.iloc[g_count, generator_data.columns.get_loc('time')] = format_time(Year, Month, Day, Period) #str(Period)
                generator_data.iloc[g_count, generator_data.columns.get_loc('power')] = data[('Output' + str(Period))][i]
                g_count += 1
    
    #powerplant_data.to_csv(plantfilelocation, index = False)
    #generator_data.to_csv(genfilelocation, index = False)
    
    return powerplant_data, generator_data



###Functions###
def outputless_assignment(csvName):
    #This function may or may not be used in 
    #Notes stations and generators for what is mapped.
    data = pd.read_csv(csvName)    

    for i in range(0,len(data['outputDUKESToBMRSID'])):
        #See if an ID is contained.
        if (((str(data['ConfidenceResult'][i]) == "1") or (str(data['ManualConfidence'][i]) == "1")) and data['outputDUKESToBMRSID'][i] != "na") and (data['outputDUKESToBMRSID'][i] != "none") and (data['outputDUKESToBMRSID'][i] != "None") and (data['outputDUKESToBMRSID'][i] != "") and (data['outputDUKESToBMRSID'][i] != "NA") and (data['outputDUKESToBMRSID'][i] != "nan"):
            #So if this is a mapped DUKES Powerplant (station) proceed. 
            for k in range(0,len(data['Registered Resource EIC code'])):
                #Add this to the station.
                if (str(data['outputDUKESToBMRSEIC'][i]) == str(data['Registered Resource EIC code'][k])):
                    data.iloc[k, data.columns.get_loc('Type (powerplant(station) or generator(unit))')] = "powerplant"
                #Set this to the generator.
                elif (str(data['Registered Resource Name'][k]).strip("0987654321.-_ ") == str(data['outputDUKESToBMRSID'][i])):
                    data.iloc[k, data.columns.get_loc('Type (powerplant(station) or generator(unit))')] = "generator"
                    data.iloc[k, data.columns.get_loc('Connected (if generator(unit))')] = data['outputDUKESToBMRSEIC'][i]
                elif ext_chars(str(data['Registered Resource Name'][k]).replace(str(data['outputDUKESToBMRSID'][i]), "")): 
                    data.iloc[k, data.columns.get_loc('Type (powerplant(station) or generator(unit))')] = "generator"
                    data.iloc[k, data.columns.get_loc('Connected (if generator(unit))')] = data['outputDUKESToBMRSEIC'][i]
    
    data.to_csv(csvName, index = False)
    

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
    #post_elexon_basic(url='https://api.bmreports.com/BMRS/B1610/v2?APIKey=<PROVIDE_API_KEY>&SettlementDate=2021-01-01&Period=1&NGCBMUnitID=&ServiceType=xml',) #All 
    #xmlString = post_elexon(url='https://api.bmreports.com/BMRS/B1610/v2?APIKey=<PROVIDE_API_KEY>&SettlementDate=2021-01-01&Period=1&NGCBMUnitID=&ServiceType=xml',) #All
    xmlString = post_elexon(url=querystring,) #All 
    
    #Specific
    #WHILW #48WSTN1000WHILWQ
    #post_elexon_basic(url='https://api.bmreports.com/BMRS/B1610/v2?APIKey=<PROVIDE_API_KEY>&SettlementDate=2021-01-01&Period=1&NGCBMUnitID=WHILW&ServiceType=xml',) #HEYM2

    #Try to format information
    IDOutput = xmlStringProcessToArray(xmlString) #The ID Output dictionary. 
    #print(IDOutput)
    #print(xmlString)
    
    return IDOutput

def empty_query_response(liveGeneratorData):
    #See if there are a few outputs recieved. If not, this should not replace the old data.
    if (len(liveGeneratorData) > 3):
        return 0 #It is NOT empty.
    return 1 #It is empty

def check_valid_time(Key, Year, Month, Day, Period):
    #Very similar to the Primary Function (live_power), but does not proceed to proces the data...
    #... instead, it only checks if data for a given time was recieved.
    #Returns 0 if data is not recieved, and 1 if it was. 
    liveGeneratorData = run_query(Key, Year, Month, Day, Period) #Dict of generation units and their outputs.
    if empty_query_response(liveGeneratorData):
        #See if anything was recieved in this query.
        return 0
    return 1

def incriment_time(TryYear, TryMonth, TryDay, TryPeriod):
    #Incriment the checked time by 1 period.
    TryPeriod += 1 #1 for a 1 period increase, in testing make this 48 to go up in days. 
    if TryPeriod > 48:
        #Have incrimented to a new day. 
        TryPeriod = 1
        date = datetime(TryYear, TryMonth, TryDay)
        date += timedelta(days=1)
        TryYear = int(date.year)
        TryMonth = int(date.month)
        TryDay = int(date.day)
    return TryYear, TryMonth, TryDay, TryPeriod

def find_recent_time(Key, Year, Month, Day, Period):
    #Keep incrimenting (not in excess of the current day), to try to find the most recent time since the one given as a starting point (Year, Month, Day Period).
    TryYear = int(Year)
    TryMonth = int(Month)
    TryDay = int(Day)
    TryPeriod = int(Period)
    today = datetime.today() #Loop breaks if it tries to look past this date, as data won't exist for the future, given that it notes past data. 
    while ((check_valid_time(Key, str(TryYear), str0_2(TryMonth), str0_2(TryDay), str0_2(TryPeriod)) == 1) and (today > datetime(TryYear, TryMonth, TryDay))):
        #This time works, so accept it. 
        Year = str(TryYear)
        Month = str0_2(TryMonth)
        Day = str0_2(TryDay)
        Period = str0_2(TryPeriod)
        #Incriment the tried time again.
        TryYear, TryMonth, TryDay, TryPeriod = incriment_time(TryYear, TryMonth, TryDay, TryPeriod)
    return Year, Month, Day, Period


###Primary Function###
def live_power(csvName, Key, Year, Month, Day, Period, Search):
    #The first input is the name of the csv spreadsheet to use.
    #The Key is the API key for BMRS. 
    #The other are for the time (
        #Year, eg. 2020 (four characters),
        #Month, eg. 01 (two characters, the first being a zero if it is <10, and a number, not the name of the month),
        #Day, eg. 01 (two characters, the first being a zero if it is <10, this being the day into the month, so 1-31),
        #Period, eg. 01 (could be 1 or two characters, as using a zero if it is <10 is optional).  The period is the half hour into the day, from 1-48.
        #Search (0 or 1), if set to 0, the only time queried will be the one given. If it is 1, it will continue to try until it obtains the most recent time (incrimenting by periods).

    if Search == 2:
        print("Querying BMRS and Updating Data Record of BMRS Data (if new data exists)")
        #This will automatically be 8 days ago. 
        #Year, Month, Day = week_ago() #Week Ago
        Year, Month, Day = two_weeks_ago() #14 Days Ago (to be safe)
        Year = str(Year)
        Month = str(Month)
        Day = str(Day)
    
    #As only a length of 2 is accepted for the day or month, (period can be either), i.e. "01" used instead of "1" ("20" would stay as "20"), we can make sure they are converted if they aren't already in this form.
    Month = str0_2(Month)
    Day = str0_2(Day)
    Period = str0_2(Period) #Optional step, but do it for consistency. 

    #Loop variable (only really relevant for Search == 2, can ignore for 0 and 1, and just presume it runs once (which it does)).
    if Search == 0 or Search == 1:
        loop = 1 + 1 #+1 for the loop to not immediately terminate, as it starts at 1.
    elif Search == 2:
        loop = 48 + 1 #+1 for the loop to not immediately terminate, as it starts at 1. 
    
    #The same time is used for the whole query.
    if Search == 1:
        Year, Month, Day, Period = find_recent_time(Key, Year, Month, Day, Period)
    
    if Search == 1 or Search == 0:
        liveGeneratorData = run_query(Key, Year, Month, Day, Period) #Dict of generation units and their outputs.
        if empty_query_response(liveGeneratorData):
            #See if anything was recieved in this query.
            return 0
    elif Search == 2:
        for i in range(1,loop):
            Period = i
            print("Check: " + str(Period))
            liveGeneratorData = run_query(Key, Year, Month, Day, Period) #Dict of generation units and their outputs. 
            if empty_query_response(liveGeneratorData):
                #See if anything was recieved in this query.
                return 0
    
    #Read DUKES Stations from csv
    data = pd.read_csv(csvName) #Dataframe including DUKES stations.

    #Clear the outputs.
    for i in range(0,len(data['Output'])):
        data.iloc[i, data.columns.get_loc('Output')] = 0
        data.iloc[i, data.columns.get_loc('Year')] = Year
        data.iloc[i, data.columns.get_loc('Month')] = Month
        data.iloc[i, data.columns.get_loc('Day')] = Day
        data.iloc[i, data.columns.get_loc('Period')] = Period
    if Search == 2:
        manuals = {} #Only used if Search == 2 later. 
        manualsDone = {}
        for i in range(0,len(data['Output1'])):
            if str(data['Manual'][i]) == "0" or str(data['Manual'][i]) == "0.0" or str(data['Manual'][i]) == "False":
                data.iloc[i, data.columns.get_loc('Type (powerplant(station) or generator(unit))')] = "None"
                data.iloc[i, data.columns.get_loc('Connected (if generator(unit))')] = "None"
            elif str(data['Manual'][i]) == "1" or str(data['Manual'][i]) == "1.0" or str(data['Manual'][i]) == "True":
                #List of Manual Mappings
                manuals[str(data['Registered Resource Name'][i])] = int(i)
                manualsDone[str(data['Registered Resource Name'][i])] = 0
            for p in range(1,loop):
                data.iloc[i, data.columns.get_loc(('Output'+str(p)))] = 0
    
    #Now go through the dataframe to get the IDs for each DUKES station.
    for p in range(1,loop):
        if Search == 2:
            Period = p
            print("Input: " + str(p))
            liveGeneratorData = run_query(Key, Year, Month, Day, Period)
        for i in range(0,len(data['outputDUKESToBMRSID'])):
            #See if an ID is contained.
            if (((str(data['ConfidenceResult'][i]) == "1") or (str(data['ManualConfidence'][i]) == "1")) and data['outputDUKESToBMRSID'][i] != "na") and (data['outputDUKESToBMRSID'][i] != "none") and (data['outputDUKESToBMRSID'][i] != "None") and (data['outputDUKESToBMRSID'][i] != "") and (data['outputDUKESToBMRSID'][i] != "NA") and (data['outputDUKESToBMRSID'][i] != "nan"):
                #Now loop through the generators
                for gen in liveGeneratorData:
                    #If gen is in manuals will need to set the generator output, and add to the powerplant output (mapping is not required, as it is already done manually). 
                    #MANUAL
                    if gen in manuals:
                        #This will repeat multiple times due to the automatic ones including the DUKES scan for mapping. So just need to do this once (this is what manualsDone is used for).
                        if manualsDone[gen] < p:
                            #Now set manualsDone for this period
                            manualsDone[gen] = p
                            #Set the output value for the generator. 
                            data.iloc[int(manuals[gen]), data.columns.get_loc(('Output' + str(Period)))] = liveGeneratorData[gen]
                            #Add the output to the powerplant. 
                            for kPP in range(0,len(data['Registered Resource EIC code'])):
                                #Try to find powerplant.
                                if data['Connected (if generator(unit))'][manuals[gen]] == data['Registered Resource EIC code'][kPP]:
                                    data.iloc[kPP, data.columns.get_loc(('Output' + str(Period)))] = float(data[('Output' + str(Period))][kPP]) + liveGeneratorData[gen]
                            #Done for this manual generator. 
                    
                    #AUTOMATIC
                    #Now for the non-manuals (automatic ones), where matching will need to be done by algorithm. 
                    #Now see if there is a match. This is based on the generator ID containing the station ID, and the generator ID, when specifics are removed, being the same as the station ID.
                    if ((data['outputDUKESToBMRSID'][i] in gen) and (gen not in manuals)): #Manuals will be considered seperately, there are only manuals if Search == 2
                        if (Search == 0 or Search == 1) and (gen.strip("0987654321.-_ ") == data['outputDUKESToBMRSID'][i]):
                            data.iloc[i, data.columns.get_loc('Output')] = float(data['Output'][i]) + liveGeneratorData[gen]
                        elif (Search == 2) and ((gen.strip("0987654321.-_ ") == data['outputDUKESToBMRSID'][i])):
                            #data.iloc[i, data.columns.get_loc(('Output' + str(Period)))] = float(data[('Output' + str(Period))][i]) + liveGeneratorData[gen]
                            for k in range(0,len(data['Registered Resource EIC code'])):
                                #Set this to the generator. 
                                if (str(data['Registered Resource Name'][k]) == gen) or ((str(data['NGC BM Unit ID'][k]) != "") and (str(data['NGC BM Unit ID'][k]) == gen)):
                                    data.iloc[k, data.columns.get_loc(('Output' + str(Period)))] = liveGeneratorData[gen] #SET generator's Output in the Record. 
                                    data.iloc[k, data.columns.get_loc('Type (powerplant(station) or generator(unit))')] = "generator" #Set generator. 
                                    data.iloc[k, data.columns.get_loc('Connected (if generator(unit))')] = data['outputDUKESToBMRSEIC'][i] #Define powerplant for this generator. 
                                #Add this to the station.
                                elif (data['outputDUKESToBMRSEIC'][i] == data['Registered Resource EIC code'][k]):
                                    data.iloc[k, data.columns.get_loc(('Output' + str(Period)))] = float(data[('Output' + str(Period))][k]) + liveGeneratorData[gen] #ADD generator's output to its powerplant's output. 
                                    data.iloc[k, data.columns.get_loc('Type (powerplant(station) or generator(unit))')] = "powerplant" #Set powerplant. 
                                
                        elif ext_chars(gen.replace(data['outputDUKESToBMRSID'][i], "")): #Now we just repeat the above for non-manuals, but with another algorithm to attempt mapping. 
                            #The station name could contain a number, which we can check with a looser method here. Here we see if, after removing the station name from the gen name, if the only remaining characters are numbers or in . -_". 
                            #print("TAKE2: " + data['outputDUKESToBMRSID'][i] + ", " + gen)
                            if Search == 0 or Search == 1:
                                data.iloc[i, data.columns.get_loc('Output')] = float(data['Output'][i]) + liveGeneratorData[gen]
                            elif Search == 2:
                                #data.iloc[i, data.columns.get_loc(('Output' + str(Period)))] = float(data[('Output' + str(Period))][i]) + liveGeneratorData[gen]
                                for k in range(0,len(data['Registered Resource EIC code'])):
                                    #Set this to the generator. 
                                    if (str(data['Registered Resource Name'][k]) == gen) or ((str(data['NGC BM Unit ID'][k]) != "") and (str(data['NGC BM Unit ID'][k]) == gen)):
                                        data.iloc[k, data.columns.get_loc(('Output' + str(Period)))] = liveGeneratorData[gen]
                                        data.iloc[k, data.columns.get_loc('Type (powerplant(station) or generator(unit))')] = "generator"
                                        data.iloc[k, data.columns.get_loc('Connected (if generator(unit))')] = data['outputDUKESToBMRSEIC'][i]
                                    #Add this to the station.
                                    elif (data['outputDUKESToBMRSEIC'][i] == data['Registered Resource EIC code'][k]):
                                        data.iloc[k, data.columns.get_loc(('Output' + str(Period)))] = float(data[('Output' + str(Period))][k]) + liveGeneratorData[gen]
                                        data.iloc[k, data.columns.get_loc('Type (powerplant(station) or generator(unit))')] = "powerplant"
                        else:
                            #Code should not get here. Print differences for debugging. 
                            print(data['outputDUKESToBMRSID'][i] + ", " + gen)
    
    outputless_assignment(csvName) #For the mapped generators / stations without outputs. 
    
    #Re-export to csv, with times and outputs. 
    data.to_csv(csvName, index = False)

    #Save to another place. #####
    newName = "./datastorage/" + str(Year) + "-" + str(Month) + "-" + str(Day) + ".csv"
    data.to_csv(newName, index = False)
    return 1


def Auto_Call(Key, AutoFile):
    #Automatically calls the primary function with Search setting == 2.
    #This means that rather than choosing a specific time, it picks 8 days ago and sweeps for all 48 periods (half hours) of the day. 
    #Thus, the Year, Month, Day, and Period inputs don't matter. With Search == 2.
    #CSV names are also set here and for the triple conversions. 
    live_power(AutoFile, Key, '2020', '20', '02', '02', 2) #Note, the prior date does not matter, it will be replaced by the day, a week ago so long as the last input is 2. 
    dfa, dfb = convert_csv_to_triple_dfs(AutoFile)
    return dfa, dfb

def download_bmrs_data():
    Key = '<PROVIDE_API_KEY>' #Add Here if needed, but remove before push. 
    #live_power('https://www.dropbox.com/s/43vdtji8rf1zspr/Input-Template.csv?dl=1', Key, '2021', '11', '14', '24', 2)
    Auto_Call(Key, 'https://www.dropbox.com/s/tupqp1nu017xiyw/Input-Template-Auto.csv?dl=1')
    #NOTE: With Manual Mapping Exemption now (i.e. if the value in the "Manual" column is '1', then it does not overwrite the generator to plant mapping and leaves it.
    print("Data formatting completed.")

###Main Function###
if __name__ == "__main__":
    Key = '<PROVIDE_API_KEY>' #Add Here if needed, but remove before push. 
    #live_power('https://www.dropbox.com/s/43vdtji8rf1zspr/Input-Template.csv?dl=1', Key, '2021', '11', '14', '24', 2)
    Auto_Call(Key, 'https://www.dropbox.com/s/tupqp1nu017xiyw/Input-Template-Auto.csv?dl=1')
    #NOTE: With Manual Mapping Exemption now (i.e. if the value in the "Manual" column is '1', then it does not overwrite the generator to plant mapping and leaves it. 

